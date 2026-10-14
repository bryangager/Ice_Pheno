# Looking at some data for ice-on

# load em in
source("source/00_libraries.R")

#--------------------------------
### 01 Getting Data Ready:
#--------------------------------

# Load in data for ice on and ice off 
met_only <- read.csv("derived_data/00_met_daily_fullyr.csv") %>% select(-X)
hydro_only <- read.csv("derived_data/00_hydro_daily_fullyr.csv")  %>% select(-X) # This has 239 duplicate dates
ice_only <- read.csv("derived_data/00_ice_daily_fullyr.csv") %>% select(-X)

# hydro data has duplicate because of wydoy stuff, needs to be fixed in data munging script but this works for now
hydro_only_fixed <- hydro_only %>%
  group_by(Date) %>%
  summarise(
    calYear = first(na.omit(calYear)),
    waterYear = first(na.omit(waterYear)),
    wy_doy = max(wy_doy, na.rm = TRUE),
    cond_uScm = first(na.omit(cond_uScm)),
    water_temp_C = first(na.omit(water_temp_C)),
    Flow = first(na.omit(Flow)),
    cumulative_dis = first(na.omit(cumulative_dis)),
    .groups = "drop"
  )

anyDuplicated(hydro_only_fixed$Date)
# Add Ice data to create the three data frames you are going to work with 
met_data_full_timeseries <- full_join(ice_only, met_only, by = "Date") %>% select(-c(calYear.x,waterYear.x,wy_doy.x)) %>% 
  rename(c(calYear = calYear.y, waterYear=waterYear.y, wy_doy=wy_doy.y))
hydro_data_full_timeseries <- full_join(ice_only, hydro_only_fixed, by = "Date") %>% select(-c(calYear.x,waterYear.x,wy_doy.x)) %>% 
  rename(c(calYear = calYear.y, waterYear=waterYear.y, wy_doy=wy_doy.y))
sink_data_full_timeseries <- full_join(hydro_data_full_timeseries, met_data_full_timeseries, by = "Date") %>% select(-c(calYear.x,waterYear.x,wy_doy.x, ice.x)) %>% 
  rename(c(calYear = calYear.y, waterYear=waterYear.y, wy_doy=wy_doy.y, ice=ice.y))
sink_data_real_full_time_series <- full_join(met_only,hydro_only_fixed) # Including ice_only trims it to 2013

# Trim data frames to only spring and only since 2014
met_data_fall <- filter_by_year_and_doy(met_data_full_timeseries, c(1,76))  %>% # October 1 - December 15
  filter(waterYear >= 2014 & waterYear <= 2024)

hydro_data_fall <- filter_by_year_and_doy(hydro_data_full_timeseries, c(1,76))  %>% # October 1 - December 15
  filter(waterYear >= 2014 & waterYear <= 2024)

sink_data_fall <- filter_by_year_and_doy(sink_data_full_timeseries, c(1,76))  %>% # October 1 - December 15
  filter(waterYear >= 2014 & waterYear <= 2024)

sink_data_real_full_time_series_fall <- filter_by_year_and_doy(sink_data_real_full_time_series, c(1,76))

# Ice on observed dates:
# Initialize an empty data frame to store the results
ice_on_observed <- data.frame()

# Iterate through each unique waterYear
for(year in unique(ice_only$waterYear)) {
  
  # Filter the data for the current year where predicted_ice == "no ice"
  year_data <- ice_only %>% 
    filter(waterYear == year, ice == 1) %>%
    arrange(wy_doy)  # Sort by wy_doy to find the first day
  
  # If there is any day where predicted_ice == "no ice"
  if (nrow(year_data) > 0) {
    # Get the first day (earliest day) in that year where predicted_ice == "no ice"
    first_ice_wy_doy <- year_data %>% slice(1)
    
    # Create a new row with the waterYear and first wy_doy
    result_row <- data.frame(
      waterYear = year,
      first_ice_wy_doy = first_ice_wy_doy$wy_doy
    )
    
    # Append the result_row to the result_df
    ice_on_observed <- bind_rows(ice_on_observed, result_row)
  }
}

### Merging ice-on dates with sink data for plotting:
# Just Observed years
sink_ice_on_window <- sink_data_real_full_time_series_fall %>%
  inner_join(ice_on_observed, by = "waterYear") %>%
  mutate(
    days_from_ice_on = wy_doy - first_ice_wy_doy
  ) %>%
  filter(
    days_from_ice_on >= -30,
    days_from_ice_on <= 15
  )
# All years using met model hindcasted dates
sink_ice_on_window_all_years <- sink_data_real_full_time_series_fall %>%
  inner_join(all_three_hinds_ice_on_df %>% filter(model=="met"), by = "waterYear") %>%
  mutate(
    days_from_ice_on = wy_doy - ice_on_dowy
  ) %>%
  filter(
    days_from_ice_on >= -30,
    days_from_ice_on <= 15
  )

### Making labels for Ice-On dates:
# Observed years
ice_on_labels <- ice_on_observed %>%
  mutate(
    ice_on_date = as.Date(
      paste0(waterYear - 1, "-10-01")
    ) + first_ice_wy_doy - 1,
    label = paste("Ice-on:", format(ice_on_date, "%b %d"))
  )
# All years (using sink model hindcast dates)
ice_on_labels_all_years <- all_three_hinds_ice_on_df %>% filter(model=="met") %>%
  mutate(
    ice_on_date = as.Date(
      paste0(waterYear - 1, "-10-01")
    ) + ice_on_dowy - 1,
    label = paste("Ice-on:", format(ice_on_date, "%b %d"))
  )
#-----------------------------------------------
### 02 Plotting:
#-----------------------------------------------
################## One variable - all years #################
ggplot(sink_ice_on_window %>% filter(waterYear < 2025),
       aes(x = days_from_ice_on, y = swe)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  facet_wrap(~ waterYear, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Days Relative to Ice-On",
    y = "Snow Water Equivalent (SWE)",
    title = "SWE and Observed Ice-On"
  )

############### All variables all the time everywhere ################
# Making observed time series long
sink_ice_on_long <- sink_ice_on_window %>%
  select(
    waterYear, days_from_ice_on,
    swe, precip, precip_cumulative,
    airT_mean, airT_max, airT_min,
    wind_10m_mean, wind_10m_max,
    cond_uScm, water_temp_C, Flow, cumulative_dis
  ) %>%
  pivot_longer(
    cols = -c(waterYear, days_from_ice_on),
    names_to = "variable",
    values_to = "value"
  )
# Making whole time series data long
sink_ice_on_long_all_years <- sink_ice_on_window_all_years %>%
  select(
    waterYear, days_from_ice_on,
    swe, precip, precip_cumulative,
    airT_mean, airT_max, airT_min,
    wind_10m_mean, wind_10m_max,
    cond_uScm, water_temp_C, Flow, cumulative_dis
  ) %>%
  pivot_longer(
    cols = -c(waterYear, days_from_ice_on),
    names_to = "variable",
    values_to = "value"
  )
# Making the whole time series long and by Date rather than by days from ice on
sink_ice_on_long_all_years_by_date <- sink_ice_on_window_all_years %>%
  select(
    waterYear, Date,
    swe, precip, precip_cumulative,
    airT_mean, airT_max, airT_min,
    wind_10m_mean, wind_10m_max,
    cond_uScm, water_temp_C, Flow, cumulative_dis
  ) %>%
  pivot_longer(
    cols = -c(waterYear, Date),
    names_to = "variable",
    values_to = "value"
  )


ggplot(sink_ice_on_long_all_years %>% filter(waterYear < 2025),
       aes(x = days_from_ice_on, y = value)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "steelblue", linewidth = 0.6) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-On",
    y = "Value",
    title = "Environmental Variables Around Ice-On"
  )

######### one variable - all years ###########
ggplot(sink_ice_on_window %>% filter(waterYear < 2025),
       aes(x = days_from_ice_on,
           y = Flow,
           color = factor(waterYear),
           group = waterYear)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "black") +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Days Relative to Ice-On",
    y = "Streamflow",
    color = "Water Year",
    title = "Streamflow Around Ice-On Across Years"
  )
####### Met variables all together #########
met_vars <- c(
  "swe", "precip", "precip_cumulative",
  "airT_mean", "airT_max", "airT_min",
  "wind_10m_mean", "wind_10m_max"
)
# Observed years
sink_met_long_ice_on <- sink_ice_on_window %>%
  select(waterYear, days_from_ice_on, all_of(met_vars)) %>%
  pivot_longer(
    cols = all_of(met_vars),
    names_to = "variable",
    values_to = "value"
  )
# Whole time series 
sink_met_long_ice_on_all_years <- sink_ice_on_window_all_years %>%
  select(waterYear, days_from_ice_on, all_of(met_vars)) %>%
  pivot_longer(
    cols = all_of(met_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_met_long_ice_on_all_years %>% filter(waterYear < 2014 & waterYear > 2009),
       aes(x = days_from_ice_on, y = value)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  geom_line(color = "steelblue", linewidth = 0.6,
            na.rm = TRUE) +
  facet_grid(
    variable ~ waterYear,
    scales = "free_y"
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-On",
    y = "Value",
    title = "Meteorological Conditions Around Ice-On"
  )

############# Snow variables together: ###################
snow_vars <- c(
  "swe",
  "precip",
  "precip_cumulative"
)
# Observed years
sink_snow_long_ice_on <- sink_ice_on_window %>%
  select(waterYear, days_from_ice_on, all_of(snow_vars)) %>%
  pivot_longer(
    cols = all_of(snow_vars),
    names_to = "variable",
    values_to = "value"
  )
# All years
sink_snow_long_ice_on_all_years <- sink_ice_on_window_all_years %>%
  select(waterYear, days_from_ice_on, all_of(snow_vars)) %>%
  pivot_longer(
    cols = all_of(snow_vars),
    names_to = "variable",
    values_to = "value"
  )
# All years by date, not date from ice on
sink_snow_long_ice_on_all_years_by_date <- sink_data_real_full_time_series_fall %>%
  select(waterYear, wy_doy, all_of(snow_vars)) %>%
  pivot_longer(
    cols = all_of(snow_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_snow_long_ice_on_all_years_by_date %>% filter(waterYear < 1998 & waterYear > 1993),
       aes(x = wy_doy, y = value)) +
  # geom_vline(xintercept = 0, linetype = "dashed",
  #            color = "red") +
  geom_vline(
    data = ice_on_labels_all_years %>% filter(waterYear < 1998 & waterYear > 1993),
    aes(xintercept = ice_on_dowy),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "red"
  ) +
  geom_line(color = "steelblue", linewidth = 0.6,
            na.rm = TRUE) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_on_labels_all_years %>% filter(waterYear < 1998 & waterYear > 1993),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1.6,
    vjust = 1.5,
    size = 2.5
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Day of Water Year",
    y = "Value",
    title = "Snow and Precipitation Around Ice-On"
  )

############## Air temp variables: ###############
airT_vars <- c(
  "airT_mean",
  "airT_max",
  "airT_min"
)
# Observed years
sink_airT_long_ice_on <- sink_ice_on_window %>%
  select(waterYear, days_from_ice_on, all_of(airT_vars)) %>%
  pivot_longer(
    cols = all_of(airT_vars),
    names_to = "variable",
    values_to = "value"
  )
# All years:
sink_airT_long_ice_on_all_years <- sink_ice_on_window_all_years %>%
  select(waterYear, days_from_ice_on, all_of(airT_vars)) %>%
  pivot_longer(
    cols = all_of(airT_vars),
    names_to = "variable",
    values_to = "value"
  )
# All years by date, not day relative to ice on
sink_airT_long_ice_on_all_years_by_date <- sink_data_real_full_time_series_fall %>%
  select(waterYear, wy_doy, all_of(airT_vars)) %>%
  pivot_longer(
    cols = all_of(airT_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_airT_long_ice_on_all_years_by_date %>% filter(waterYear < 2014 & waterYear > 2009),
       aes(x = wy_doy, y = value)) +
  geom_vline(
    data = ice_on_labels_all_years %>% filter(waterYear < 2014 & waterYear > 2009),
    aes(xintercept = ice_on_dowy),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "red"
  ) +
  # geom_vline(xintercept = 0, linetype = "dashed",
  #            color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey") +
  geom_line(color = "darkgreen", linewidth = 0.5,
            na.rm = TRUE) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_on_labels_all_years %>% filter(waterYear < 2014 & waterYear > 2009),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 1.6,
    vjust = 1.5,
    size = 2.5
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Day of Water Year",
    y = "Temp °C",
    title = "Air Temperature Around Ice-On"
  )

####### Wind variables together ##############
wind_vars <- c(
  "wind_10m_mean",
  "wind_10m_max"
)

sink_wind_long_ice_on <- sink_ice_on_window %>%
  select(waterYear, days_from_ice_on, all_of(wind_vars)) %>%
  pivot_longer(
    cols = all_of(wind_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_wind_long_ice_on %>% filter(waterYear < 2018),
       aes(x = days_from_ice_on, y = value)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "orange", linewidth = 0.5,
            na.rm = TRUE) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_on_labels %>% filter(waterYear < 2018),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2.2,
    vjust = 2,
    size = 2.5
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-On",
    y = "Meters/Second",
    title = "10m Wind Around Ice-On"
  )

####### Hydro variables all together #########
hydro_vars <- c(
  "Flow", "cumulative_dis",
  "water_temp_C", "cond_uScm"
)

sink_hydro_long_ice_on <- sink_ice_on_window %>%
  select(waterYear, days_from_ice_on, all_of(hydro_vars)) %>%
  pivot_longer(
    cols = all_of(hydro_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_hydro_long_ice_on %>% filter(waterYear < 2018),
       aes(x = days_from_ice_on, y = value)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "red"
  ) +
  geom_line(color = "darkblue", linewidth = 0.7,
            na.rm = TRUE) +
  facet_grid(
    variable ~ waterYear,
    scales = "free_y"
  ) +
  geom_text(
    data = ice_on_labels %>% filter(waterYear < 2018),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2.2,
    vjust = 2,
    size = 2.5
  ) +
  theme_minimal(base_size = 11) +
  labs(
    x = "Days Relative to Ice-On",
    y = "Value",
    title = "Hydrological Conditions Around Ice-On"
  )
