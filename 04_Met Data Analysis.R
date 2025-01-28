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
airTemp_hindcast_SWE <- full_join(cumulativeAirTempAll,hindcast_SWE,by="year") %>% rename(waterYear=waterYear.x) %>% select(-c(first_snow_acc,first_snow_acc_doy,cont_snow_acc,cont_snow_acc_doy,first_snow_melt,first_snow_melt_doy,last_snow_melt,last_snow_melt_doy,waterYear.y,first_snow_acc_wydoy,cont_snow_acc_wydoy,first_snow_melt_wydoy,last_snow_melt_wydoy,max_swe_date))
airTemp_hindcast_SWE$month <- format(airTemp_hindcast_SWE$Date, "%m")

# now we have a messy dataframe that has everything in it, let's run some models and see what happens.
###### MODELS AND CORRELATIONS:
tempModel <- lm(predicted_ice_off_wy_doy~T_air_2_m_mean, data = airTemp_hindcast_SWE)
summary(tempModel)

tempCumulMarchModel <- lm(predicted_ice_off_wy_doy~T_air_2_m_Marchcumul, data = airTemp_hindcast_SWE)
summary(tempCumulMarchModel)

# This doesn't work because of the NAs?
ggpairs(airTemp_hindcast_SWE)+theme_bw()

ggpairs(airTemp_hindcast_SWE, columns=c(4,14))+theme_bw()

## Statistically significant correlations: April Cumulative Temp (-0.361***), May Cumulative Temp: (-0.299***),
## March-April Cumulative Temp (-0.089***), March-May Cumulative Temperature (-0.227***), April-May Cumulative Temp (-0.474***)
## Max SWE (0.406***), Max SWE doy (0.551***)

## The only variables that were not statistically correlated with the predicted date of ice-off 
## were Mean 2m Air Temp and March Cumulative Temp

###### Change Over Time:
# March Cumul Temp
marchPlot <-
  ggplot(airTemp_hindcast_SWE,aes(x=Date,y=T_air_2_m_Marchcumul))+
  geom_point()+
  geom_smooth(method=lm)
marchPlot
# April Cumul Temp
aprilPlot <-
  ggplot(airTemp_hindcast_SWE,aes(x=Date,y=T_air_2_m_Aprilcumul))+
  geom_point()+
  geom_smooth(method=lm)
aprilPlot
# May Cumul Temp
mayPlot <-
  ggplot(airTemp_hindcast_SWE,aes(x=Date,y=T_air_2_m_Maycumul))+
  geom_point()+
  geom_smooth(method=lm)
mayPlot
# March- May Cumul Temp:
marchMayPlot <-
  ggplot(airTemp_hindcast_SWE,aes(x=Date,y=T_air_2_m_MarchMaycumul))+
  geom_point()+
  geom_smooth(method=lm)
marchMayPlot
# Mean Daily Temp over time:
weather_daily$month <- format(weather_daily$Date, "%m") %>% as.integer(weather_daily$month)

dailyTempPlot <- 
  ggplot(weather_daily %>% filter(month>=3&month<=5),aes(x=month,y=T_air_2_m_mean))+
  geom_point()+
  facet_wrap(~waterYear)
dailyTempPlot

##### 20th Percentile Dates:

# Function to find wy_doy for specific percentiles
find_percentiles <- function(data, percentiles) {
  total_dis <- max(data$cumulative_dis, na.rm = TRUE) # Total discharge for the waterYear
  targets <- percentiles * total_dis                  # Target values for the percentiles
  
  # Find wy_doy for each target
  data.frame(
    percentile = percentiles,
    wy_doy = sapply(targets, function(target) {
      data$wy_doy[which.min(abs(data$cumulative_dis - target))]
    })
  )
}

# Apply the function for each waterYear
cumul_dis_percentiles <- cumulative_dat_ungroup %>%
  group_by(waterYear) %>%
  group_modify(~ find_percentiles(.x, c(0.2, 0.5, 0.8)))

percentile_hindcast_dates <- left_join(cumul_dis_percentiles,hindcasted_dates,by="waterYear") %>% rename(wy_doy_perc=wy_doy) %>% filter(waterYear!=2024)

# Correlation plots
ggpairs(percentile_hindcast_dates %>% filter(percentile==0.2) %>% select(-percentile)) # corr: 0.414**

ggpairs(percentile_hindcast_dates %>% filter(percentile==0.5) %>% select(-percentile)) # corr: 0.288

ggpairs(percentile_hindcast_dates %>% filter(percentile==0.8) %>% select(-percentile)) # corr: 0.023

# Plot of 20th percentile wy_doy over time
ggplot(percentile_hindcast_dates %>% filter(percentile==0.2), aes(x=waterYear,y=wy_doy_perc))+
  geom_point()


##### Pierson Dates:
pierson_dates <- read_xlsx("Input_Files/Peirson_IcePhenoDates_AVG_20250120.xlsx")
pierson_dates <- pierson_dates %>% mutate(Ice_Off_Peirson = as.Date(Ice_Off_Peirson)) %>%  mutate(wy_doy_pierson_off = hydro.day(Ice_Off_Peirson))
pierson_dates <- pierson_dates %>% mutate(Ice_On_Peirson = as.Date(Ice_On_Peirson)) %>%  mutate(wy_doy_pierson_on = hydro.day(Ice_On_Peirson))

pierson_hindcast_dates <- left_join(pierson_dates,hindcasted_dates,by="waterYear")

# pierson vs. my model dates

ggplot(pierson_hindcast_dates, aes(x=first_no_ice_wy_doy,y=wy_doy_pierson_off))+
  geom_point(aes(first_no_ice_wy_doy, color = waterYear))+
  geom_point(aes(wy_doy_pierson_off, color = waterYear))+
  scale_color_gradient(low="blue",high="red")
