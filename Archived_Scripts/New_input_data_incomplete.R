# This file pulls data in from NWIS as well as CSV files located in "Input_Files"
# Variables for joined data and joined trimmed data are created here

# Libraries:
library(pacman)
p_load(dplyr,dataRetrieval,lubridate,tidyr,ggplot2,viridis,readxl,imputeTS,tsibble,sjPlot,pROC,gridExtra,broom,gtsummary)

# Sourcing functions needed for trimming data:
source("Input_Files/00_functions.R")


######## Pulling in outlet flow from NWIS and adding cumulative flow:
# LV Site Number:
lv_no <- '401733105392404'

# define parameter for discharge (00060)
param <- '00060'

# get daily values from NWIS
lv_dat <- readNWISdv(siteNumbers = lv_no, parameterCd = param,
                     startDate = '1983-10-01', endDate = '2024-09-30')

# rename columns using renameNWISColumns from package dataRetrieval
# this renames the column for Flow from the parameter ID to "Flow"
lv_dat <- renameNWISColumns(lv_dat)

# Removing column with USGS code for observations
lv_dat <- select(lv_dat, -contains('_cd'))

# Adding the water year to the df
lv_dat <- addWaterYear(lv_dat)

# calculating cumulative discharge for each year by first grouping by water year,
# and then using the "cumsum" function. Add day of water year for plotting purposes.
cumulative_dat <- group_by(lv_dat, waterYear) %>%
  mutate(cumulative_dis = cumsum(Flow), 
         wy_doy = seq(1:n()))

# ungroup the dataframe
cumulative_dat_ungroup <- cumulative_dat %>%
  ungroup() %>%
  as.data.frame()

# rename the df, remove the site number column, make sure dates are in date format. This is the final df
cumulative_flow_df <- cumulative_dat_ungroup %>% select(-site_no) %>% mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d"))



######## Pulling in Temp and Conductivity from NWIS:
# setting parameters for temp and cond
parameterCd <- c('00095','00010')
# making the request from NWIS
lv_dat_outlet_input <- readWQPqw(paste0("USGS-", lv_no), parameterCd)

#Clean up dataframe
lv_dat_outlet <- lv_dat_outlet_input %>%
  select(ActivityStartDate, ActivityConductingOrganizationText, CharacteristicName, ResultMeasureValue) %>%
  pivot_wider(names_from = CharacteristicName, values_from = ResultMeasureValue, values_fn = mean) %>%
  rename(cond_uScm="Specific conductance",
         temperature_C_raw="Temperature, water",
         Date = "ActivityStartDate") %>%
  select(-ActivityConductingOrganizationText) %>%
  mutate(wy_doy = hydro.day(Date)) %>%
  addWaterYear() %>% 
  distinct(Date, .keep_all = TRUE) %>%
  as_tsibble(., key = waterYear, index = Date) %>% #time series tibble
  fill_gaps() #%>%  #makes the missing data implicit
# View(lv_dat_outlet) # THIS IS WEEKLY DATA FOR THE WHOLE TIME SERIES

# Imputed data:
# Filling gaps in weekly data with a max gap of interpolation as 7 days
lv_out_impute <- lv_dat_outlet %>%
  mutate(cond_uScm_impute = imputeTS::na_interpolation(cond_uScm, maxgap = 7),
         temperature_C_impute = imputeTS::na_interpolation(temperature_C_raw, maxgap = 7))

# Binding the weekly data and imputed data with cumulative flow
## Weekly:
flow_temp_cond_weekly <- full_join(cumulative_flow_df, lv_dat_outlet, by = "Date")
#View(flow_temp_cond_weekly)
## Imputed:
flow_temp_cond_impute <- full_join(cumulative_flow_df, lv_out_impute, by = "Date")
#View(flow_temp_cond_impute)

######## Pulling in Daily Temp and Cond from Graham with USGS:
# These CSVs contain data from 2019-2023
out_cond_dat <- read.csv("Input_Files/Loch_O_daily_conductivity.csv")
out_temp_dat <- read.csv("Input_Files/Loch_O_daily_temperature.csv")
# Joining those two using FULL join because there are less conductivity observations
out_condTemp_dat19_23 <- full_join(out_cond_dat,out_temp_dat, by = "Date")
# This CSV contains data from 2011-2019
out_condTemp_dat11_19 <- read.csv("Input_Files/LochDaily_TempCond_2011-2019.csv")
#View(out_condTemp_dat11_19)

out_condTemp_allDates <- merge(out_condTemp_dat11_19, out_condTemp_dat19_23, all = TRUE)
#View(out_condTemp_allDates)

# Adding water year and water year doy to the daily data
out_cond_temp_daily <- out_condTemp_allDates %>% mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d")) %>% mutate(wy_doy = hydro.day(Date)) %>% addWaterYear() %>% 
  distinct(Date, .keep_all = TRUE)
#View(out_cond_temp_daily)






######## Bring in OBSERVED ice presence on The Loch

# this is just the observed ice-off dates from 2013-2023 with date of ice-off, date, and waterYear
obs_ice_off_dates <- read_xlsx("Input_Files/ice_off_dates20240918.xlsx")
#View(obs_ice_off_dates)

# this is a df with the wy_doy date of 100% ice and the wy_doy date with 0% ice for 2013-2023
obs_ice_melt_windows <- read_xlsx("Input_Files/ice_100_to_0_dates_20241114.xlsx")
#View(obs_ice_melt_windows)

# Reading CSV for ice duration with 0% ice as ice-off
ice_off_binary <- read.csv("Input_Files/binary_iceOff_20241001.csv")  %>% select(c(Date,ice.0.1.,wy_doy)) %>% rename(ice_or_no=ice.0.1.) %>% mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d"))
# View(ice_off_binary)

# recode ice_or_no into 2 classes
ice_off_binary$ice_presence <- ifelse(ice_off_binary$ice_or_no == 0,
                                      0,
                                      1
)
# set labels for ice
ice_off_binary$ice_presence <- factor(ice_off_binary$ice_presence,
                                      levels = c(1, 0),
                                      labels = c("ice", "no ice")
)

#View(ice_off_binary)
#str(ice_off_binary$ice_presence)
#levels(ice_off_binary$ice_presence)

# making a df for only ice_presence and Date for simpler joining
ice_presence_df <- ice_off_binary %>% select(Date, ice_or_no, ice_presence)

######## Combining Dfs:
# combine binary ice on or off with DAILY temp and conductivity
out_dat_and_ice_daily <- full_join(out_cond_temp_daily,ice_presence_df, by = "Date")
#View(out_dat_and_ice_daily)

# combine daily conductivity and temperature with ice presence and cumulative flow
## This df will be used in models for daily observations. It is trimmed to only include observations that are inside
## the time frame of ice observations -> (drop_na(ice_or_no))
flow_temp_cond_daily_ice <- left_join(out_dat_and_ice_daily,cumulative_flow_df,by="Date") %>% select(-c(wy_doy.y,waterYear.y)) %>% rename(c(wy_doy = wy_doy.x,waterYear = waterYear.x)) %>% drop_na(ice_or_no)
#View(flow_temp_cond_daily_ice)

# combine weekly conductivity and temperature observations with ice presence (this includes cumulative flow already)
flow_temp_cond_weekly_ice <- full_join(flow_temp_cond_weekly, ice_presence_df, by = "Date") %>% select(Date, wy_doy.x, waterYear.x, Flow, cumulative_dis, cond_uScm, temperature_C_raw, ice_or_no,ice_presence) %>% rename(wy_doy=wy_doy.x,waterYear=waterYear.x)
#View(flow_temp_cond_weekly_ice)

# combine imputed conductivity and temperature observations with ice presence (this includes cumulative flow already)
flow_temp_cond_imputed_ice <- full_join(flow_temp_cond_impute, ice_presence_df, by = "Date") %>% select(Date, wy_doy.x, waterYear.x, Flow, cumulative_dis, cond_uScm_impute, temperature_C_impute, ice_or_no,ice_presence) %>% rename(wy_doy=wy_doy.x,waterYear=waterYear.x)
#View(flow_temp_cond_imputed_ice)


######## Trimming Dfs for the spring

# trimming the data frames for windows:
## 1982 - 2024
imputed_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_imputed_ice, c(170,288)) # March 18 - July 15
## 1982-2024
weekly_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_weekly_ice, c(170,288)) # March 18 - July 15
## 2014-2023
daily_data_trimmed <- filter_by_year_and_doy(flow_temp_cond_daily_ice, c(170,288)) # March 18 - July 15




####### Trimming Dfs again to match daily variables for model comparison (training time-frame)

# imputed un-trimmed:
flow_temp_cond_imputed_ice_14_23 <- flow_temp_cond_imputed_ice %>% filter(waterYear >= 2014 & waterYear <= 2023)
# weekly un-trimmed:
flow_temp_cond_weekly_ice_14_23 <- flow_temp_cond_weekly_ice %>% filter(waterYear >= 2014 & waterYear <= 2023)

# imputed trimmed:
imputed_data_trimmed_14_23 <- imputed_data_trimmed %>% filter(waterYear >= 2014 & waterYear <= 2023)
# weekly trimmed:
weekly_data_trimmed_14_23 <- weekly_data_trimmed %>% filter(waterYear >= 2014 & waterYear <= 2023)


####### Randomly sampling 70% of the data with observed ice presence:
# choosing random years for the training and validation datasets:
set.seed(10)
obs_years_list <- as_tibble(unique(imputed_data_trimmed_14_23$waterYear))
randomYears <- obs_years_list %>% slice_sample(n=6)

# filtering imputed data into training dataset:
imputed_test_df <- imputed_data_trimmed_14_23 %>% filter(waterYear %in% randomYears$value)
#View(imputed_test_df)

# filtering imputed data into validation dataset:
imputed_validation_df <- imputed_data_trimmed_14_23 %>% filter(!(waterYear %in% randomYears$value))
#View(imputed_validation_df)

# filtering weekly data into training dataset:
weekly_test_df <- weekly_data_trimmed_14_23 %>% filter(waterYear %in% randomYears$value)


# filtering weekly data into validation dataset:
weekly_validation_df <- weekly_data_trimmed_14_23 %>% filter(!(waterYear %in% randomYears$value))

# filtering daily data into training dataset
daily_test_df <- daily_data_trimmed %>% filter(waterYear %in% randomYears$value)

# filtering daily data into validation dataset:
daily_validation_df <- daily_data_trimmed %>% filter(!(waterYear %in% randomYears$value))

####### Final Models:
daily_final_model_test <- glm(ice_presence~cumulative_dis+Temperature_C, data = daily_test_df, family = binomial)

weekly_final_model_test <- glm(ice_presence~cumulative_dis+temperature_C_raw, data = weekly_test_df, family = binomial)

imputed_final_model_test <- glm(ice_presence~cumulative_dis+temperature_C_impute, data = imputed_test_df, family = binomial)

  