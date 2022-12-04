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




#MODELING THE DATA
# stepwise multiple regression model

null.lm <- lm(hosp_per_100k ~ 1, data = hosp_full)

full.lm <- lm(hosp_per_100k ~ log(max_aqi) + median_income_in_thousands + median_aqi + log(pop_density_sq_m) + sqrt(mean_smokePM) + max_smoke_PM, data = hosp_full)

MASS::stepAIC(full.lm, scope = list(lower = null.lm, upper = full.lm), direction = "backward", k = log(60))  #Use backward elimination process to determine which variables are relevant


asthma_mod <- lm(hosp_per_100k ~ log(max_aqi) + sqrt(mean_smokePM) + log(pop_density_sq_m) + median_income_in_thousands, data = hosp_full)

asthma_hosp_table <- tab_model(asthma_mod,
                               pred.labels = c("Intercept", "Log Max Aqi", "SQRT Mean Smoke", 
                                               "Log Pop Density", "Median Income (in $1000s)"),
                               dv.labels = c("Hospitalization Rate (per 100k)"),
                               string.ci = "Conf. Int (95%)",
                               string.p = "P-value",
                               title = "Table 1. Linear Model Results",
                               digits = 4)





#CHECKING PREDICTIONS

final_model <- lm(hosp_per_100k ~ log(max_aqi) + median_income_in_thousands + log(pop_density_sq_m) + sqrt(mean_smokePM), data = hosp_full)

predictions <- augment(final_model)

ggplot(data = predictions, mapping = aes(x = .fitted, y = hosp_per_100k)) +
  geom_point() +
  geom_abline(slope = 1, color = "red", lwd = 1) +
  xlim(20, 70) +
  annotate("text",
           x = 67, 
           y = 83, 
           label = "Line of perfect",
           color = "red",
           size = 3.5) +
  annotate("text",
           x = 67,
           y = 79,
           label = "predictions",
           color = "red",
           size = 3.5) +
  labs(x = "Predicted Hospitalizations (per 100k)",
       y = "True Hospitalizations (per 100k)",
       title = "Comparing our model predictions to the true value of hospitalizations",
       subtitle = "The model trends in the right direction, but tends to over predict low hospitalization rates and underpredict low hospitalization rates. A perfect model would follow the line in red.") +
  theme_classic()





## Hypothesis test that hospitalizations would be different (likely higher) in 2018 than in 2016 due to increased fire acreage burnt. 

#2016: 669,534 burnt
#2018: 	1,975,086 burnt

hosp_2016_2018 <- hosp_full %>%
  filter(year == 2016 | year == 2018) %>%
  mutate(year = as.factor(year))

fire_test <- t.test(hosp_per_100k ~ year, data = hosp_2016_2018)

tab_model(fire_test,
          string.ci = c("Conf. Int (95%)"),
          string.p = "P-value",
          dv.labels = c("Hospitalization Rate"),
          pred.labels = "2016 – 2018",
          title = "Table 2: Hospitalization Rate and Wildfires: Welch Two Sample t-test")


#testing to confirm which year is first 
hosp_2016_2018 %>% 
  filter(year == "2016") %>%
  mutate(hosp_per_100k = as.numeric(hosp_per_100k)) %>%
  summarise(mean = mean(hosp_per_100k, na.rm = TRUE))

hosp_2016_2018 %>% 
  filter(year == "2018") %>%
  mutate(hosp_per_100k = as.numeric(hosp_per_100k)) %>%
  summarise(mean = mean(hosp_per_100k, na.rm = TRUE))




beeswarm_boxplot <- ggplot(data = hosp_2016_2018, mapping = aes(x = year, y = hosp_per_100k)) +
  geom_beeswarm(color = "red", alpha = .5) + 
  geom_boxplot(alpha = 0.3) +
  theme_classic() +
  labs(x = "Year",
       y = "Hospitalizations (per 100k)",
       title = "California County Hospitalization Rate from Asthma in 2016 and 2018",
       subtitle = "The distribution of hospitalization rates appear similar from 2016 and 2018")

beeswarm_boxplot_t_test <- beeswarm_boxplot + 
  stat_compare_means(method = "t.test", label.x = 1.35, label.y = 65)


