#######################################
# 00 Data Munging 
#######################################
# # KAG & BDG 2026-06-15

# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

library(here)
source(here::here("source", "00_libraries.R"))
source(here::here("source", "00_functions.R"))

# __________________________________________________
# 01. Flow 
# __________________________________________________
# Pulling in outlet flow from NWIS and adding cumulative flow:
    # LV Site Number:
    lv_no <- '401733105392404'

    # define parameter for discharge (00060)
    param <- '00060'

    # get daily values from NWIS
    lv_dat <- readNWISdv(siteNumbers = lv_no, parameterCd = param,
                        startDate = '1983-10-01', endDate = '2025-09-30')

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
    cumulative_flow_df <- cumulative_dat_ungroup %>% 
      select(-site_no) %>% 
      mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d"))

# __________________________________________________
# 02. Temp and Conductivity 
# __________________________________________________

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
    # View(lv_dat_outlet) # This is weekly data from 1982-2023, but missing temp & cond after 2019

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
    out_cond_temp_daily <- out_condTemp_allDates %>% mutate(Date = as.Date(Date, tz = "MST", format = "%Y-%m-%d")) %>% 
      mutate(wy_doy = hydro.day(Date)) %>% addWaterYear() %>% 
      distinct(Date, .keep_all = TRUE)
    #View(out_cond_temp_daily)


###### Subsetting weekly temp and cond observations from the daily observations to fill in gap from 2019 onwards
    # Create a weekly dataset from the WY2020-2024 data
    TLoch_weekly_19_23 <- out_cond_temp_daily %>% 
      mutate(dayOfWeek = wday(Date, label = TRUE, abbr = FALSE)) %>% 
      filter(dayOfWeek == "Tuesday")
    TLoch_weekly_19_23<- TLoch_weekly_19_23 %>% select(-dayOfWeek) %>% rename(temperature_C_raw = Temperature_C)

    # Joining weekly data from 2019-2023 with the weekly data from 1982-2023 to fill gaps:
    updated_lv_dat_outlet <- lv_dat_outlet %>%
      left_join(TLoch_weekly_19_23, by = "Date", suffix = c("_old", "_new")) %>%
      mutate(
        # Replace missing values in lv_dat_outlet with values from TLoch_weekly_19_23
        temperature_C_raw = ifelse(is.na(temperature_C_raw_old), temperature_C_raw_new, temperature_C_raw_old),
        cond_uScm = ifelse(is.na(cond_uScm_old), cond_uScm_new, cond_uScm_old)
      ) %>%
      # Keep only original column names
      select(Date, temperature_C_raw, cond_uScm)

    TCond_weekly_all <- updated_lv_dat_outlet %>% mutate(wy_doy = hydro.day(Date)) %>% rename(waterYear = waterYear_old, temperature_C_weekly = temperature_C_raw,cond_uScm_weekly=cond_uScm)
    ##### THIS IS WEEKLY DATA WITH FILLED GAPS FOR THE ENTIRE TIME SERIES (1982-2023) - TCond_weekly_all

    # Filling gaps in weekly data with a max gap of interpolation as 7 days
    TCond_imputed_all <- TCond_weekly_all %>%
      mutate(cond_uScm_impute = imputeTS::na_interpolation(cond_uScm_weekly, maxgap = 7),
            temperature_C_impute = imputeTS::na_interpolation(temperature_C_weekly, maxgap = 7))
    #### THIS IS THE IMPUTED DATA FOR THE ENTIRE TIME SERIES (1982-2023) - TCond_imputed_all

# __________________________________________________
# 02. Put together flow, conductivity, and temperature and export as a single file 
# __________________________________________________

    # Binding the weekly data and imputed data with cumulative flow
    ## Weekly:
    flow_temp_cond_weekly <- full_join(cumulative_flow_df, TCond_weekly_all, by = "Date")
    #View(flow_temp_cond_weekly)

    ## Imputed:
    flow_temp_cond_impute <- full_join(cumulative_flow_df, TCond_imputed_all, by = "Date")
    #View(flow_temp_cond_impute)

    # save outputs 
    write.csv(flow_temp_cond_impute, "derived_data/flow_temp_cond_impute.csv")
    write.csv(flow_temp_cond_weekly, "derived_data/flow_temp_cond_weekly.csv")
