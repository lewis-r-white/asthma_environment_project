
#LOADING PACKAGES
library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(purrr)
library(fs)
library(sf)
library(tmap)
library(dplyr)
library(broom)
library(MASS)
library(ggbeeswarm)
library(ggpubr)
library(sjPlot)
library(magick)


rootdir <- ("/Users/lewiswhite/MEDS/eds_222/asthma_environment_project")

datadir <- file.path(rootdir,"data") 

setwd(file.path(rootdir,"R"))





#READING IN THE DATA

#hospitalizations by county
hospitalizations <- read_csv(file.path(datadir,"asthma_hospitalizations.csv")) %>%
  clean_names()

#asthma prevalence by county
asthma_prevalence <- read_csv(file.path(datadir, "asthma_prevalence.csv")) %>%
  clean_names() 

#country demographic info
county_demographics <- read_csv(file.path(datadir, "county_demographics.csv")) %>%
  clean_names() 

#creating a list of the county yearly aqi data files 
aqi_files <- list.files(path = datadir, pattern="annual_aqi*", full.names=TRUE, recursive=FALSE)

#looping through the files, reading them in and binding the rows to make one dataframe 
california_aqi <- aqi_files %>%
  map(function (path) {
    read_csv(path)
  }) %>%
  bind_rows() %>%
  clean_names() %>%
  filter(state =="California")

#county spatial data 
county_shapes <- read_sf(file.path(datadir, "CA_Counties", "CA_Counties_TIGER2016.shp")) %>%
  clean_names() 

#smoke pm2 data
smoke <- read_csv(file.path(datadir, "smoke_pm2_data.csv"))






#CLEAN THE DATA

#filter to desired rows/columns from hospitalizations dataset
hosp_clean <- hospitalizations %>%
  filter(strata == "Total population",
         county != "California") %>%
  dplyr::select(county, year, number_of_hospitalizations, age_adjusted_hospitalization_rate) 

#filter to desired rows/columns from hospitalizations dataset
asthma_prev_clean <- asthma_prevalence %>%
  dplyr::select(county:comment) %>%
  filter(strata == "Total population",
         county != "California") %>%
  dplyr::select(county, years, current_prevalence) %>%
  mutate(current_prevalence = as.numeric(str_remove(current_prevalence, "%"))) #adjust percent variable to be numeric so it can be more easily used in analysis 

#filter rows, select columns, adjust columns, and create new columns in the demographics dataset. The race specific demographic info is an estimate for the overall 2010 to 2020 decade. 
county_dem_clean <- county_demographics %>%
  filter(state == "CA") %>%
  dplyr::select(county,
                ethnicities_american_indian_and_alaska_native_alone:ethnicities_white_alone_not_hispanic_or_latino,
                income_median_houseold_income, #median income 12 month median income collected from 2015 to 2019
                population_2020_population, population_population_per_square_mile) %>%
  rename("median_income" = "income_median_houseold_income",
         "pop2020" = "population_2020_population",
         "pop_density_sq_m" = "population_population_per_square_mile") %>%
  mutate(median_income_in_thousands = median_income / 1000) %>%
  mutate(percent_poc = ethnicities_american_indian_and_alaska_native_alone + ethnicities_asian_alone + ethnicities_black_alone + ethnicities_hispanic_or_latino + ethnicities_native_hawaiian_and_other_pacific_islander_alone + ethnicities_two_or_more_races) %>% #For future analysis, I created a percent person of color variable to see whether asthma prevalence/hospitalizations was related to minority group status. 
  mutate(county = str_remove(county, " County")) %>%
  dplyr::select(!ethnicities_american_indian_and_alaska_native_alone:ethnicities_white_alone_not_hispanic_or_latino)


#select columns of interest and create new columns for aqi data
cali_aqi_clean <- california_aqi %>%
  dplyr::select(-state, -(days_co:days_pm10)) %>%
  mutate(good_or_moderate = good_days + moderate_days,
         unhealthy_or_worse = unhealthy_days + very_unhealthy_days + hazardous_days,
         unhealthy_or_worse_per365 = (unhealthy_or_worse / days_with_aqi)*365) 

#select column for geospatial data and rename county column to match other datasets
county_shape_clean <- county_shapes %>%
  dplyr::select(name) %>%
  rename("county" = "name")

smoke_clean <- smoke %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  filter(GEOID > 6000,
         GEOID < 6116) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  mutate(GEOID = paste0("0", GEOID)) %>%
  mutate(date = lubridate::ymd(date)) %>%
  mutate(year(date))

smoke_clean_2015_to_2019 <- smoke_clean %>%
  filter(date > '2014-12-31',
         date < '2020-01-01') %>%
  rename("year" = "year(date)")

smoke_year_summaries <- smoke_clean_2015_to_2019 %>%
  group_by(GEOID, year) %>%
  summarize(mean_smokePM = mean(smokePM_pred),
            third_quartile_smoke_PM = quantile(smokePM_pred, 0.75),
            max_smoke_PM = max(smokePM_pred)) %>%
  left_join(county_shapes, by = c("GEOID" = "geoid")) %>%
  st_drop_geometry() %>%
  dplyr::select(GEOID, year, mean_smokePM, third_quartile_smoke_PM, 
                max_smoke_PM, name, namelsad) %>%
  rename("county" = "name")






# JOIN DATASETS FOR ANALYSIS

#add aqi and demographic data to the hospitalizations data set
hosp_full <- left_join(hosp_clean, cali_aqi_clean) %>%
  left_join(smoke_year_summaries) %>%
  left_join(county_dem_clean) %>%
  mutate(hosp_per_100k = (number_of_hospitalizations / pop2020) * 100000) %>%
  mutate(max_aqi = replace(max_aqi, max_aqi > 500, 500)) #since the EPA's AQI qualitative scale peaks at 500, I adjusted AQI values above 500 to just be 500 








## Joining the spatial data so spatial plots can be made

hosp_full_spatial <- hosp_full %>%
  left_join(county_shape_clean) %>%
  st_as_sf()

hosp_year_summaries <- hosp_full %>%
  group_by(county) %>%
  summarise(mean_hosp = mean(hosp_per_100k, na.rm = TRUE),
            mean_max_aqi = mean(max_aqi, na.rm = TRUE),
            mean_median_aqi = mean(median_aqi, na.rm = TRUE),
            mean_smoke = mean(mean_smokePM, na.rm = TRUE),
            max_smoke = max(max_smoke_PM, na.rm = TRUE),
            mean_income = mean(median_income_in_thousands, na.rm = TRUE),
            mean_pop_density = mean(pop_density_sq_m, na.rm = TRUE)) %>%
  mutate(log_pop_density = log(mean_pop_density)) %>%
  left_join(county_shape_clean) %>%
  st_as_sf()


asthma_prev_spatial <- asthma_prev_clean %>%
  left_join(county_shape_clean) %>%
  st_as_sf()





### Spatial plots and animation!

#HOSPITALIZATIONS
hosp_ggplot <- ggplot() +
  geom_sf(data = hosp_year_summaries, 
          aes(fill = mean_hosp), col = 'black', size = 0.5) +
  labs(title = "Hospitalization Rate from Asthma between \n 2015–2019 for California Counties") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        legend.direction = "vertical",
        panel.border = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(1, 0.75),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  scale_fill_stepsn(colors = c("#fff0d6", 
                               "#ffffb2", 
                               "#fecc5c", 
                               "#fd8d3c", 
                               "#f03b20", 
                               "#bd0026"),
                    na.value = "grey50",
                    guide = guide_colorbar(title = "Hospitalization Rate \n (per 100,000)"))




#MEDIAN INCOME
median_income_ggplot <- ggplot() +
  geom_sf(data = hosp_year_summaries, 
          aes(fill = mean_income), col = 'black', size = 0.5) +
  labs(title = "Median Income for California Counties") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        legend.direction = "vertical",
        panel.border = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(1, 0.75),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  scale_fill_stepsn(colors = c("#fff0d6", 
                               "#ffffb2", 
                               "#fecc5c", 
                               "#fd8d3c", 
                               "#f03b20", 
                               "#bd0026"),
                    na.value = "grey50",
                    guide = guide_colorbar(title = "Median Income (in $1,000s)"))


#POPULATION DENSITY  
pop_density_ggplot <- ggplot() +
  geom_sf(data = hosp_year_summaries, 
          aes(fill = log_pop_density), col = 'black', size = 0.5) +
  labs(title = "Normalized (log transformed) Population \n Density in California Counties") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        legend.direction = "vertical",
        panel.border = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(1, 0.75),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  scale_fill_stepsn(colors = c("#fff0d6", 
                               "#ffffb2", 
                               "#fecc5c", 
                               "#fd8d3c", 
                               "#f03b20", 
                               "#bd0026"),
                    na.value = "grey50",
                    guide = guide_colorbar(title = "Log Population Density \n (per square mile)"))


# AQI

max_aqi_ggplot <- ggplot() +
  geom_sf(data = hosp_year_summaries, 
          aes(fill = mean_max_aqi), col = 'black', size = 0.5) +
  labs(title = "Average Maximum AQI from 2015-2019 \n for California Counties") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        legend.direction = "vertical",
        panel.border = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(1, 0.75),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  scale_fill_stepsn(colors = c("#fff0d6", 
                               "#ffffb2", 
                               "#fecc5c", 
                               "#fd8d3c", 
                               "#f03b20", 
                               "#bd0026"),
                    na.value = "grey50",
                    guide = guide_colorbar(title = "Maximum AQI"))

# SMOKE PM 2.5

mean_smoke_ggplot <- ggplot() +
  geom_sf(data = hosp_year_summaries, 
          aes(fill = mean_smoke), col = 'black', size = 0.5) +
  labs(title = "Median PM 2.5 from Smoke \n for California Counties") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'right',
        legend.direction = "vertical",
        panel.border = element_blank(),
        axis.text = element_blank()) +
  theme(legend.position = c(1, 0.75),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  scale_fill_stepsn(colors = c("#fff0d6", 
                               "#ffffb2", 
                               "#fecc5c", 
                               "#fd8d3c", 
                               "#f03b20", 
                               "#bd0026"),
                    na.value = "grey50",
                    guide = guide_colorbar(title = "Mean PM 2.5 \n from Smoke"))




## Saving the images to create the animation!

#HOSP
ggsave(filename = "hosp_map.png", plot=hosp_ggplot, 
       width=6, height=4, units = "in")

hosp_map_png <- image_read("hosp_map.png")



#INCOME
ggsave(filename = "median_income_map.png", plot=median_income_ggplot, 
       width=6, height=4, units = "in")

income_map_png <- image_read("median_income_map.png")



#POP DENSITY
ggsave(filename = "pop_density_map.png", plot=pop_density_ggplot, 
       width=6, height=4, units = "in")

density_map_png <- image_read("pop_density_map.png")


#AQI
ggsave(filename = "max_aqi_map.png", plot=max_aqi_ggplot, 
       width=6, height=4, units = "in")

aqi_map_png <- image_read("max_aqi_map.png")


#SMOKE
ggsave(filename = "mean_smoke_map.png", plot=mean_smoke_ggplot, 
       width=6, height=4, units = "in")

smoke_map_png <- image_read("mean_smoke_map.png")






#Making the gif!

gif_images <- c(hosp_map_png, income_map_png, density_map_png, aqi_map_png, smoke_map_png)

image_append(image_scale(gif_images, "x200"))

asthma_animation <- image_animate(image_scale(gif_images, "400x400"), fps = 0.5, dispose = "previous")

image_write(asthma_animation, "asthma_vars.gif")



