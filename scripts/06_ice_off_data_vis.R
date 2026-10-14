# Lookin at some data for ice off:

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
met_data_spring <- filter_by_year_and_doy(met_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <=2024)

hydro_data_spring <- filter_by_year_and_doy(hydro_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <=2024)

sink_data_spring <- filter_by_year_and_doy(sink_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <= 2024)
# Trim full time series for spring
sink_data_real_full_time_series_spring <- filter_by_year_and_doy(sink_data_real_full_time_series, c(170,288))

#### Pulling out observed ice-off dates

#filtering ice_only to be spring only:
ice_only_spring <- filter_by_year_and_doy(ice_only, c(170,288))

# initializing df:
ice_off_observed <- data.frame()

# Iterate through each unique waterYear
for(year in unique(ice_only_spring$waterYear)) {
  
  # Filter the data for the current year where predicted_ice == "no ice"
  year_data <- ice_only_spring %>% 
    filter(waterYear == year, ice == 0) %>%
    arrange(wy_doy)  # Sort by wy_doy to find the first day
  
  # If there is any day where predicted_ice == "no ice"
  if (nrow(year_data) > 0) {
    # Get the first day (earliest day) in that year where predicted_ice == "no ice"
    first_no_ice_wy_doy <- year_data %>% slice(1)
    
    # Create a new row with the waterYear and first wy_doy
    result_row <- data.frame(
      waterYear = year,
      first_no_ice_wy_doy = first_no_ice_wy_doy$wy_doy
    )
    
    # Append the result_row to the result_df
    ice_off_observed <- bind_rows(ice_off_observed, result_row)
  }
}

### Merging ice-off dates with sink data for plotting:
sink_ice_off_window <- sink_data_real_full_time_series_spring %>%
  inner_join(ice_off_observed, by = "waterYear") %>%
  mutate(
    days_from_ice_off = wy_doy - first_no_ice_wy_doy
  ) %>%
  filter(
    days_from_ice_off >= -30,
    days_from_ice_off <= 15
  )

### Making labels for ice-off dates:
ice_off_labels <- ice_off_observed %>%
  mutate(
    ice_off_date = as.Date(
      paste0(waterYear - 1, "-10-01")
    ) + first_no_ice_wy_doy - 1,
    label = paste("Ice-off:", format(ice_off_date, "%b %d"))
  )

#-----------------------------------------------
### 02 Plotting:
#-----------------------------------------------

################## One variable - all years #################
ggplot(sink_ice_off_window %>% filter(waterYear < 2025),
       aes(x = days_from_ice_off, y = cond_uScm)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "steelblue", linewidth = 0.8) +
  facet_wrap(~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_off_labels %>% filter(waterYear < 2025),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2,
    vjust = 1.5,
    size = 2.5
  ) +
  theme_minimal(base_size = 13) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Outlet Conductivity (uS/cm)",
    title = "Outlet Conductivity and Observed Ice-Off"
  )

############### All variables all the time everywhere ################
sink_ice_off_long <- sink_ice_off_window %>%
  select(
    waterYear, days_from_ice_off,
    swe, precip, precip_cumulative,
    airT_mean, airT_max, airT_min,
    wind_10m_mean, wind_10m_max,
    cond_uScm, water_temp_C, Flow, cumulative_dis
  ) %>%
  pivot_longer(
    cols = -c(waterYear, days_from_ice_off),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_ice_off_long %>% filter(waterYear < 2025),
       aes(x = days_from_ice_off, y = value)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "steelblue", linewidth = 0.6) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Value",
    title = "Environmental Variables Around Ice-Off"
  )
######### one variable - all years ###########
ggplot(sink_ice_off_window %>% filter(waterYear < 2025),
       aes(x = days_from_ice_off,
           y = wind_10m_max,
           color = factor(waterYear),
           group = waterYear)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "black") +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Max 10m Wind Speed (m/s)",
    color = "Water Year",
    title = "Max Wind Speed Around Ice-Off Across Years"
  )

####### Met variables all together #########
met_vars <- c(
  "swe", "precip", "precip_cumulative",
  "airT_mean", "airT_max", "airT_min",
  "wind_10m_mean", "wind_10m_max"
)

sink_met_long_ice_off <- sink_ice_off_window %>%
  select(waterYear, days_from_ice_off, all_of(met_vars)) %>%
  pivot_longer(
    cols = all_of(met_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_met_long_ice_off %>% filter(waterYear < 2025),
       aes(x = days_from_ice_off, y = value)) +
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
    x = "Days Relative to Ice-Off",
    y = "Value",
    title = "Meteorological Conditions Around Ice-Off"
  )

############# Snow variables together: ###################
snow_vars <- c(
  "swe",
  "precip",
  "precip_cumulative"
)

sink_snow_long_ice_off <- sink_ice_off_window %>%
  select(waterYear, days_from_ice_off, all_of(snow_vars)) %>%
  pivot_longer(
    cols = all_of(snow_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_snow_long_ice_off %>% filter(waterYear >=2022 & waterYear <= 2024),
       aes(x = days_from_ice_off, y = value)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "steelblue", linewidth = 0.6,
            na.rm = TRUE) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_off_labels %>% filter(waterYear >=2022 & waterYear <= 2024),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2.2,
    vjust = 1.5,
    size = 2.5
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Value",
    title = "Snow and Precipitation Around Ice-Off"
  )

############## Air temp variables: ###############
airT_vars <- c(
  "airT_mean",
  "airT_max",
  "airT_min"
)

sink_airT_long_ice_off <- sink_ice_off_window %>%
  select(waterYear, days_from_ice_off, all_of(airT_vars)) %>%
  pivot_longer(
    cols = all_of(airT_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_airT_long_ice_off %>% filter(waterYear >=2018 & waterYear <= 2021),
       aes(x = days_from_ice_off, y = value)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey") +
  geom_line(color = "darkgreen", linewidth = 0.5,
            na.rm = TRUE) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_off_labels %>% filter(waterYear >=2018 & waterYear <= 2021),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2.5,
    vjust = 1.5,
    size = 2.5
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Temp °C",
    title = "Air Temperature Around Ice-Off"
  )

####### Wind variables together ##############
wind_vars <- c(
  "wind_10m_mean",
  "wind_10m_max"
)

sink_wind_long_ice_off <- sink_ice_off_window %>%
  select(waterYear, days_from_ice_off, all_of(wind_vars)) %>%
  pivot_longer(
    cols = all_of(wind_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_wind_long_ice_off %>% filter(waterYear >=2018 & waterYear <= 2021),
       aes(x = days_from_ice_off, y = value)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  geom_line(color = "orange", linewidth = 0.5,
            na.rm = TRUE) +
  facet_grid(variable ~ waterYear, scales = "free_y") +
  geom_text(
    data = ice_off_labels %>% filter(waterYear >=2018 & waterYear <= 2021),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2.5,
    vjust = 2,
    size = 2.5
  ) +
  theme_minimal(base_size = 10) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Meters/Second",
    title = "10m Wind Around Ice-Off"
  )

####### Hydro variables all together #########
hydro_vars <- c(
  "Flow", "cumulative_dis",
  "water_temp_C", "cond_uScm"
)

sink_hydro_long_ice_off <- sink_ice_off_window %>%
  select(waterYear, days_from_ice_off, all_of(hydro_vars)) %>%
  pivot_longer(
    cols = all_of(hydro_vars),
    names_to = "variable",
    values_to = "value"
  )

ggplot(sink_hydro_long_ice_off %>% filter(waterYear >=2018 & waterYear <= 2021),
       aes(x = days_from_ice_off, y = value)) +
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
    data = ice_off_labels %>% filter(waterYear >=2018 & waterYear <= 2021),
    aes(
      x = Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = 2.5,
    vjust = 2,
    size = 2.5
  ) +
  theme_minimal(base_size = 11) +
  labs(
    x = "Days Relative to Ice-Off",
    y = "Value",
    title = "Hydrological Conditions Around Ice-Off"
  )
