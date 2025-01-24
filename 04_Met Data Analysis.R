source("Input_Files/00_functions.R")
source("Input_Files/01_Data_Input.R")

## First, bring in the dfs for met data and outlet data as well as predicted days of ice-off:

# Bear Lake Snow Telemetry Data:
SWE_stats <- read.csv("Input_Files/Bear_SWE_stats.csv")

# Weather station data (temp, wind):
weatherData <- read.csv("Input_Files/subdaily_met_1991to2022.csv") %>% select(date_time, waterYear, T_air_2_m, T_air_6_m, WSpd_2_m, WSpd_6_m, SWin_2m6m_mean)

# hindcasted dates of ice-off
hindcasted_dates <- read.csv("Input_Files/hindcasted_ice_off_dates.csv") %>% select(-"X")

# hindcasted dates and SWE
hindcast_SWE <- full_join(SWE_stats,hindcasted_dates, by="waterYear") %>% rename(predicted_ice_off_wy_doy=first_no_ice_wy_doy)

## Weather needs to be daily - here I find the mean daily air temp and wind speed
weather_daily <- weatherData %>%
  mutate(Date = as.Date(date_time)) %>% # Extract the date part
  group_by(Date) %>% # Group by date
  summarise(
    T_air_2_m_mean = mean(T_air_2_m, na.rm = TRUE),
    T_air_6_m_mean = mean(T_air_6_m, na.rm = TRUE),
    WSpd_2_m_mean = mean(WSpd_2_m, na.rm = TRUE),
    WSpd_6_m_mean = mean(WSpd_6_m, na.rm = TRUE),
    SWin_2m6m_daily_mean = mean(SWin_2m6m_mean, na.rm = TRUE)
  ) %>% addWaterYear()
## Now I'll calculate the cumulative temperature of different time windows
# CHANGE THESE DATES TO MAKE EACH DATAFRAME
start_month_day <- "04-01"
end_month_day <- "06-01"

# Add year-based grouping and calculate cumulative sums
april_may_cumul_temp <- weather_daily %>%
  mutate(
    year = format(Date, "%Y"),                     # Extract the year
    month_day = format(Date, "%m-%d")              # Extract month and day
  ) %>%
  filter(month_day >= start_month_day & month_day < end_month_day) %>% # Filter by date window
  group_by(year) %>%                               # Group by year
  mutate(
    T_air_2_m_AprilMaycumul = cumsum(T_air_2_m_mean),     # Cumulative sum for T_air_2_m_mean
    T_air_6_m_AprilMaycumul = cumsum(T_air_6_m_mean)      # Cumulative sum for T_air_6_m_mean
  ) %>%
  ungroup()  

march_cumul_temp %>% 
  ggplot(aes(x=Date, y=T_air_2_m_Marchcumul))+
  geom_point()+
  facet_wrap(.~year, scales="free_x")

# joining them all in probably the worst way possible
cumulativeAirTemp <- full_join(march_cumul_temp,april_cumul_temp,by=c("Date","year")) %>% 
  select(Date,year,waterYear.x,T_air_2_m_mean.x,T_air_2_m_Marchcumul,T_air_2_m_Aprilcumul) %>%
  rename(c(waterYear=waterYear.x,T_air_2_m_mean=T_air_2_m_mean.x))

cumulativeAirTemp1 <- full_join(cumulativeAirTemp,may_cumul_temp,by=c("Date","year")) %>% 
  select(Date,year,waterYear.x,T_air_2_m_mean.x,T_air_2_m_Marchcumul,T_air_2_m_Aprilcumul,T_air_2_m_Maycumul) %>%
  rename(c(waterYear=waterYear.x,T_air_2_m_mean=T_air_2_m_mean.x))

cumulativeAirTemp2 <- full_join(cumulativeAirTemp1,march_april_cumul_temp,by=c("Date","year")) %>% 
  select(Date,year,waterYear.x,T_air_2_m_mean.x,T_air_2_m_Marchcumul,T_air_2_m_Aprilcumul,T_air_2_m_Maycumul,T_air_2_m_MarchAprilcumul) %>%
  rename(c(waterYear=waterYear.x,T_air_2_m_mean=T_air_2_m_mean.x))

cumulativeAirTemp3 <- full_join(cumulativeAirTemp2,march_may_cumul_temp,by=c("Date","year")) %>% 
  select(Date,year,waterYear.x,T_air_2_m_mean.x,T_air_2_m_Marchcumul,T_air_2_m_Aprilcumul,T_air_2_m_Maycumul,T_air_2_m_MarchAprilcumul,T_air_2_m_MarchMaycumul) %>%
  rename(c(waterYear=waterYear.x,T_air_2_m_mean=T_air_2_m_mean.x))

cumulativeAirTemp4 <- full_join(cumulativeAirTemp3,april_may_cumul_temp,by=c("Date","year")) %>% 
  select(Date,year,waterYear.x,T_air_2_m_mean.x,T_air_2_m_Marchcumul,T_air_2_m_Aprilcumul,T_air_2_m_Maycumul,T_air_2_m_MarchAprilcumul,T_air_2_m_MarchMaycumul,T_air_2_m_AprilMaycumul) %>%
  rename(c(waterYear=waterYear.x,T_air_2_m_mean=T_air_2_m_mean.x))

cumulativeAirTempAll <- cumulativeAirTemp4 %>% mutate(year = as.integer(year))

# hooray! now we have mean air temp and cumulative air temp for many different windows. Let's merge it all together:
airTemp_hindcast_SWE <- full_join(cumulativeAirTempAll,hindcast_SWE,by="year") %>% rename(waterYear=waterYear.x)

# now we have a messy dataframe that has everything in it, let's run some models and see what happens.

tempModel <- lm(predicted_ice_off_wy_doy~T_air_2_m_mean, data = airTemp_hindcast_SWE)
summary(tempModel)


View(cumulativeAirTempAll)



weatherData %>% mutate(wy_doy = hydro.day(date_time))

cumulative_dat <- group_by(lv_dat, waterYear) %>%
  mutate(cumulative_dis = cumsum(Flow), 
         wy_doy = seq(1:n()))





# Now Put them all together
hindcast_SWE <- full_join(SWE_stats,hindcasted_dates, by="waterYear") %>% rename(predicted_ice_off_wy_doy=first_no_ice_wy_doy)