
################################################################################
#   UPPER BROOKS BUOY – 2025 CLEANING & MERGING PIPELINE
#   Author: Samantha Peña
#   Purpose:
#      - Convert all sensors to UTC + MST
#      - Clean MiniDOT DO & Temp (surface + 4m)
#      - Clean PAR sensor (4m temperature + PAR)
#      - Clean pendant temperature (2m & 3m)
#      - Clean weather station pressure (GMT → UTC → MST)
#      - Unify and average 4m temperatures across sensors
#      - Elevation-correct atmospheric pressure
#      - Compute temperature-dependent DO saturation using Garcia–Benson
#      - Export clean final dataset
###############################################################################
library(tidyverse)
library(lubridate)
library(purrr)
library(dplyr)
library(LakeMetabolizer) 

################################################################################
# Function: Adjust pressure for elevation (mbar at elev_from → mbar at elev_to)
################################################################################
adjust_pressure_elevation <- function(Px_mbar, elev_from, elev_to, T_C) {
  g <- 9.80665
  M <- 0.0289644
  R <- 8.3144598
  T <- T_C + 273.15  # convert °C to K
  
  delta_h <- elev_to - elev_from
  Py_mbar <- Px_mbar * exp(-(g * M * delta_h) / (R * T))
  return(Py_mbar)
}

################################################################################
# WEATHER STATION (pressure, PAR, air temp, RH, wind)
################################################################################

weather <- read.csv(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/BrooksWeather_2025.csv",
  skip = 1, header = TRUE
) %>%
  rename(
    datetime_raw   = Date.Time..GMT.07.00,
    pressure_inHg  = Pressure..in.Hg..LGR.S.N..22313858..SEN.S.N..22279472.,
    par_surface    = PAR..µmol.m..s..LGR.S.N..22313858..SEN.S.N..22300749.,
    temp_air_F     = Temp...F..LGR.S.N..22313858..SEN.S.N..22305821.,
    RH_raw         = RH.....LGR.S.N..22313858..SEN.S.N..22305821.,
    wind_speed_mph = Wind.Speed..mph..LGR.S.N..22313858..SEN.S.N..22309738.,
    gust_speed_mph = Gust.Speed..mph..LGR.S.N..22313858..SEN.S.N..22309738.,
    wind_dir_raw   = Wind.Direction..ø..LGR.S.N..22313858..SEN.S.N..22309738.
  ) %>%
  mutate(
    # Time zones
    datetime_GMT7 = mdy_hms(datetime_raw, tz = "Etc/GMT+7"),
    datetime_UTC  = with_tz(datetime_GMT7, "UTC"),
    datetime_MST  = with_tz(datetime_GMT7, "America/Denver"),
    
    # Unit conversions
    atm_pressure_mbar = as.numeric(pressure_inHg) * 33.8639,   # inHg → mbar
    par_surface       = as.numeric(par_surface),
    temp_air          = (as.numeric(temp_air_F) - 32) * 5/9,   # °F → °C
    RH                = as.numeric(RH_raw),
    wind_speed        = as.numeric(wind_speed_mph) * 0.44704,  # mph → m/s
    gust_speed        = as.numeric(gust_speed_mph) * 0.44704,
    wind_dir          = as.numeric(wind_dir_raw)
  ) %>%
  # Round times to 10-min
  mutate(
    datetime_UTC = round_date(datetime_UTC, "10 minutes"),
    datetime_MST = round_date(datetime_MST, "10 minutes")
  ) %>%
  # Aggregate to 10-min bins
  group_by(datetime_UTC, datetime_MST) %>%
  summarise(
    atm_pressure_mbar = mean(atm_pressure_mbar, na.rm = TRUE),
    par_surface       = mean(par_surface,       na.rm = TRUE),
    temp_air          = mean(temp_air,          na.rm = TRUE),
    RH                = mean(RH,                na.rm = TRUE),
    wind_speed        = mean(wind_speed,        na.rm = TRUE),
    gust_speed        = mean(gust_speed,        na.rm = TRUE),
    wind_dir          = mean(wind_dir,          na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# MiniDOT SURFACE SENSOR (1 m DO + temp)
################################################################################

do_surface <- read.table(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/DO/UB_Surface.TXT",
  sep = ",", header = TRUE, skip = 7
) %>%
  slice(-1) %>%
  rename(
    datetime_MST = Mountain.Standard.Time,
    datetime_UTC = `UTC_Date_._Time`,
    temp_1m      = Temperature,
    do_mgl_1m    = Dissolved.Oxygen,
    do_sat_raw_1m = Dissolved.Oxygen.Saturation,
    Battery_1m   = Battery
  ) %>%
  mutate(
    datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
    datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
    datetime_MST = round_date(datetime_MST, "10 minutes"),
    datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )

################################################################################
# MiniDOT DEPTH SENSOR (4 m DO + temp)
################################################################################

do_depth <- read.table(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/DO/UB_Depth.TXT",
  sep = ",", header = TRUE, skip = 7
) %>%
  slice(-1) %>%
  rename(
    datetime_MST = Mountain.Standard.Time,
    datetime_UTC = `UTC_Date_._Time`,
    do_temp_4m   = Temperature,
    do_mgl_4m    = Dissolved.Oxygen,
    do_sat_raw_4m = Dissolved.Oxygen.Saturation,
    Battery_4m   = Battery
  ) %>%
  mutate(
    datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
    datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
    datetime_MST = round_date(datetime_MST, "10 minutes"),
    datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )

################################################################################
# PAR SENSOR (4 m PAR + its internal temperature)
################################################################################

par <- read.table(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/PAR/UB_PAR.TXT",
  sep = ",", header = TRUE, skip = 4
) %>%
  slice(-1) %>%
  rename(
    datetime_MST = Mountain.Standard.Time,
    datetime_UTC = `UTC_Date_._Time`,
    par_temp_4m  = Temperature,
    par_4m       = PAR
  ) %>%
  mutate(
    datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
    datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
    datetime_MST = round_date(datetime_MST, "10 minutes"),
    datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )

################################################################################
# PENDANT TEMPERATURE (2 m & 3 m)
################################################################################

clean_pendant <- function(path, depth_label) {
  
  raw <- read.csv(path, skip = 1, header = TRUE)
  dt_col <- grep("Date.Time..GMT", names(raw), value = TRUE)
  
  datetime_GMT <- mdy_hms(raw[[dt_col]], tz = "GMT")
  datetime_UTC <- with_tz(datetime_GMT, "UTC")
  datetime_MST <- with_tz(datetime_GMT, "US/Mountain")
  
  df <- tibble(
    datetime_UTC = datetime_UTC,
    datetime_MST = datetime_MST,
    temp_F       = raw[[3]]
  ) %>%
    mutate(temp_C = (temp_F - 32) * 5/9) %>%
    complete(datetime_UTC = seq(min(datetime_UTC), max(datetime_UTC), by = "5 min")) %>%
    arrange(datetime_UTC) %>%
    mutate(temp_C = approx(datetime_UTC, temp_C, datetime_UTC)$y) %>%
    mutate(
      datetime_UTC = floor_date(datetime_UTC, "10 minutes"),
      datetime_MST = with_tz(datetime_UTC, "US/Mountain")
    ) %>%
    group_by(datetime_UTC, datetime_MST) %>%
    summarise(temp_C = mean(temp_C, na.rm = TRUE), .groups = "drop") %>%
    rename(!!paste0("temp_", depth_label) := temp_C)
  
  return(df)
}

temp_2m <- clean_pendant(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/Temp/UB_2m.csv",
  "2m"
)

temp_3m <- clean_pendant(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/Temp/UB_3m.csv",
  "3m"
)

################################################################################
# TRIM ALL DATASETS TO COMMON DEPLOYMENT WINDOW
################################################################################

weather_start <- min(weather$datetime_UTC, na.rm = TRUE)
deploy_end    <- ymd_hms("2025-10-14 20:00:00", tz = "UTC")

trim <- function(df) {
  df %>%
    filter(datetime_UTC >= weather_start,
           datetime_UTC <= deploy_end)
}

do_surface <- trim(do_surface)
do_depth   <- trim(do_depth)
par        <- trim(par)
temp_2m    <- trim(temp_2m)
temp_3m    <- trim(temp_3m)
weather    <- trim(weather)

################################################################################
# FULL MERGE (using datetime_UTC as key)
################################################################################

df_list <- list(do_surface, do_depth, par, temp_2m, temp_3m, weather)

upper_brooks <- reduce(df_list, full_join, by = "datetime_UTC")

################################################################################
# TYPE COERCION – explicitly make key columns numeric
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(
    temp_1m          = as.numeric(temp_1m),
    temp_2m          = as.numeric(temp_2m),
    temp_3m          = as.numeric(temp_3m),
    do_temp_4m       = as.numeric(do_temp_4m),
    par_temp_4m      = as.numeric(par_temp_4m),
    do_mgl_1m        = as.numeric(do_mgl_1m),
    do_mgl_4m        = as.numeric(do_mgl_4m),
    par_4m           = as.numeric(par_4m),
    atm_pressure_mbar = as.numeric(atm_pressure_mbar),
    Battery_1m       = as.numeric(Battery_1m),
    Battery_4m       = as.numeric(Battery_4m),
    par_surface      = as.numeric(par_surface),
    temp_air         = as.numeric(temp_air),
    RH               = as.numeric(RH),
    wind_speed       = as.numeric(wind_speed),
    gust_speed       = as.numeric(gust_speed),
    wind_dir         = as.numeric(wind_dir)
  )

# Drop the raw MiniDOT % sat so we can replace with our own
upper_brooks <- upper_brooks %>%
  select(-any_of(c("do_sat_1m", "do_sat_4m", "do_sat_raw_1m", "do_sat_raw_4m")))

################################################################################
# DEDUPLICATE BY AVERAGING NUMERIC COLUMNS AT EACH TIMESTAMP
################################################################################

upper_brooks <- upper_brooks %>%
  group_by(datetime_UTC) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

################################################################################
# RECREATE MST FROM UTC
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(datetime_MST = with_tz(datetime_UTC, "America/Denver")) %>%
  relocate(datetime_MST, .after = datetime_UTC)

################################################################################
# UNIFIED 4 m TEMPERATURE
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(
    temp_4m = rowMeans(cbind(do_temp_4m, par_temp_4m), na.rm = TRUE)
  )

################################################################################
# PRESSURE CORRECTION + DO SATURATION (Garcia–Benson)
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(
    atm_pressure_buoy = adjust_pressure_elevation(
      Px_mbar   = atm_pressure_mbar,
      elev_from = 2758,   # weather station
      elev_to   = 2775,   # buoy depth
      T_C       = temp_1m
    ),
  
    # Correct: o2.at.sat.base(temp_C, pressure_mbar)
    do_sat_eq_1m = o2.at.sat.base(temp_1m, atm_pressure_buoy, model = "garcia-benson"),
    do_sat_eq_4m = o2.at.sat.base(temp_4m, atm_pressure_buoy, model = "garcia-benson"),
    
    # Percent saturation
    do_sat_1m = (do_mgl_1m / do_sat_eq_1m) * 100,
    do_sat_4m = (do_mgl_4m / do_sat_eq_4m) * 100
  ) %>%
  select(-do_sat_eq_1m, -do_sat_eq_4m)

################################################################################
# FINAL COLUMN ORDER + CLEANUP
################################################################################

upper_brooks <- upper_brooks %>%
  # rename raw pressure column
  rename(atm_pressure_raw = atm_pressure_mbar) %>%
  # drop MiniDOT diagnostics & extraneous columns
  select(
    -matches("^Unix.Timestamp"),
    -matches("^Q\\."),
    -matches("^Acceleration"),
    -matches("^X\\.")
  ) %>%
  # reorder in the logical structure you wanted
  select(
    # Time
    datetime_UTC,
    datetime_MST,
    
    # Temperature (surface → deep)
    temp_1m,
    temp_2m,
    temp_3m,
    temp_4m,
    
    # Dissolved Oxygen (1 m)
    do_mgl_1m,
    do_sat_1m,
    
    # Dissolved Oxygen (4 m)
    do_mgl_4m,
    do_sat_4m,
    
    # Weather station variables
    wind_speed,
    gust_speed,
    wind_dir,
    par_surface,
    temp_air,
    RH,
    atm_pressure_raw,
    atm_pressure_buoy,
    
    # Sensor diagnostics + raw 4 m temp channels
    do_temp_4m,
    par_temp_4m,
    Battery_1m,
    Battery_4m,
    
    # anything else we didn't explicitly list (should be minimal)
    everything()
  )

################################################################################
# EXPORT
################################################################################


################################################################################
#   UPPER BROOKS BUOY – 2025 CLEANING & MERGING PIPELINE
#   Author: Samantha Peña
#   Purpose:
#      - Convert all sensors to UTC + MST
#      - Clean MiniDOT DO & Temp (surface + 4m)
#      - Clean PAR sensor (4m temperature + PAR)
#      - Clean pendant temperature (2m & 3m)
#      - Clean weather station pressure (GMT → UTC → MST)
#      - Unify and average 4m temperatures across sensors
#      - Elevation-correct atmospheric pressure
#      - Compute temperature-dependent DO saturation using Garcia–Benson
#      - Export clean final dataset
###############################################################################
library(tidyverse)
library(lubridate)
library(purrr)
library(dplyr)
library(LakeMetabolizer) 

################################################################################
# Function: Adjust pressure for elevation (mbar at elev_from → mbar at elev_to)
################################################################################
adjust_pressure_elevation <- function(Px_mbar, elev_from, elev_to, T_C) {
  g <- 9.80665
  M <- 0.0289644
  R <- 8.3144598
  T <- T_C + 273.15  # convert °C to K
  
  delta_h <- elev_to - elev_from
  Py_mbar <- Px_mbar * exp(-(g * M * delta_h) / (R * T))
  return(Py_mbar)
}

################################################################################
# WEATHER STATION (pressure, PAR, air temp, RH, wind)
################################################################################

weather <- read.csv(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/BrooksWeather_2025.csv",
  skip = 1, header = TRUE
) %>%
  rename(
    datetime_raw   = Date.Time..GMT.07.00,
    pressure_inHg  = Pressure..in.Hg..LGR.S.N..22313858..SEN.S.N..22279472.,
    par_surface    = PAR..µmol.m..s..LGR.S.N..22313858..SEN.S.N..22300749.,
    temp_air_F     = Temp...F..LGR.S.N..22313858..SEN.S.N..22305821.,
    RH_raw         = RH.....LGR.S.N..22313858..SEN.S.N..22305821.,
    wind_speed_mph = Wind.Speed..mph..LGR.S.N..22313858..SEN.S.N..22309738.,
    gust_speed_mph = Gust.Speed..mph..LGR.S.N..22313858..SEN.S.N..22309738.,
    wind_dir_raw   = Wind.Direction..ø..LGR.S.N..22313858..SEN.S.N..22309738.
  ) %>%
  mutate(
    # Time zones
    datetime_GMT7 = mdy_hms(datetime_raw, tz = "Etc/GMT+7"),
    datetime_UTC  = with_tz(datetime_GMT7, "UTC"),
    datetime_MST  = with_tz(datetime_GMT7, "America/Denver"),
    
    # Unit conversions
    atm_pressure_mbar = as.numeric(pressure_inHg) * 33.8639,   # inHg → mbar
    par_surface       = as.numeric(par_surface),
    temp_air          = (as.numeric(temp_air_F) - 32) * 5/9,   # °F → °C
    RH                = as.numeric(RH_raw),
    wind_speed        = as.numeric(wind_speed_mph) * 0.44704,  # mph → m/s
    gust_speed        = as.numeric(gust_speed_mph) * 0.44704,
    wind_dir          = as.numeric(wind_dir_raw)
  ) %>%
  # Round times to 10-min
  mutate(
    datetime_UTC = round_date(datetime_UTC, "10 minutes"),
    datetime_MST = round_date(datetime_MST, "10 minutes")
  ) %>%
  # Aggregate to 10-min bins
  group_by(datetime_UTC, datetime_MST) %>%
  summarise(
    atm_pressure_mbar = mean(atm_pressure_mbar, na.rm = TRUE),
    par_surface       = mean(par_surface,       na.rm = TRUE),
    temp_air          = mean(temp_air,          na.rm = TRUE),
    RH                = mean(RH,                na.rm = TRUE),
    wind_speed        = mean(wind_speed,        na.rm = TRUE),
    gust_speed        = mean(gust_speed,        na.rm = TRUE),
    wind_dir          = mean(wind_dir,          na.rm = TRUE),
    .groups = "drop"
  )

################################################################################
# MiniDOT SURFACE SENSOR (1 m DO + temp)
################################################################################

do_surface <- read.table(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/DO/UB_Surface.TXT",
  sep = ",", header = TRUE, skip = 7
) %>%
  slice(-1) %>%
  rename(
    datetime_MST = Mountain.Standard.Time,
    datetime_UTC = `UTC_Date_._Time`,
    temp_1m      = Temperature,
    do_mgl_1m    = Dissolved.Oxygen,
    do_sat_raw_1m = Dissolved.Oxygen.Saturation,
    Battery_1m   = Battery
  ) %>%
  mutate(
    datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
    datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
    datetime_MST = round_date(datetime_MST, "10 minutes"),
    datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )

################################################################################
# MiniDOT DEPTH SENSOR (4 m DO + temp)
################################################################################

do_depth <- read.table(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/DO/UB_Depth.TXT",
  sep = ",", header = TRUE, skip = 7
) %>%
  slice(-1) %>%
  rename(
    datetime_MST = Mountain.Standard.Time,
    datetime_UTC = `UTC_Date_._Time`,
    do_temp_4m   = Temperature,
    do_mgl_4m    = Dissolved.Oxygen,
    do_sat_raw_4m = Dissolved.Oxygen.Saturation,
    Battery_4m   = Battery
  ) %>%
  mutate(
    datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
    datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
    datetime_MST = round_date(datetime_MST, "10 minutes"),
    datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )

################################################################################
# PAR SENSOR (4 m PAR + its internal temperature)
################################################################################

par <- read.table(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/PAR/UB_PAR.TXT",
  sep = ",", header = TRUE, skip = 4
) %>%
  slice(-1) %>%
  rename(
    datetime_MST = Mountain.Standard.Time,
    datetime_UTC = `UTC_Date_._Time`,
    par_temp_4m  = Temperature,
    par_4m       = PAR
  ) %>%
  mutate(
    datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
    datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
    datetime_MST = round_date(datetime_MST, "10 minutes"),
    datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )

################################################################################
# PENDANT TEMPERATURE (2 m & 3 m)
################################################################################

clean_pendant <- function(path, depth_label) {
  
  raw <- read.csv(path, skip = 1, header = TRUE)
  dt_col <- grep("Date.Time..GMT", names(raw), value = TRUE)
  
  datetime_GMT <- mdy_hms(raw[[dt_col]], tz = "GMT")
  datetime_UTC <- with_tz(datetime_GMT, "UTC")
  datetime_MST <- with_tz(datetime_GMT, "US/Mountain")
  
  df <- tibble(
    datetime_UTC = datetime_UTC,
    datetime_MST = datetime_MST,
    temp_F       = raw[[3]]
  ) %>%
    mutate(temp_C = (temp_F - 32) * 5/9) %>%
    complete(datetime_UTC = seq(min(datetime_UTC), max(datetime_UTC), by = "5 min")) %>%
    arrange(datetime_UTC) %>%
    mutate(temp_C = approx(datetime_UTC, temp_C, datetime_UTC)$y) %>%
    mutate(
      datetime_UTC = floor_date(datetime_UTC, "10 minutes"),
      datetime_MST = with_tz(datetime_UTC, "US/Mountain")
    ) %>%
    group_by(datetime_UTC, datetime_MST) %>%
    summarise(temp_C = mean(temp_C, na.rm = TRUE), .groups = "drop") %>%
    rename(!!paste0("temp_", depth_label) := temp_C)
  
  return(df)
}

temp_2m <- clean_pendant(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/Temp/UB_2m.csv",
  "2m"
)

temp_3m <- clean_pendant(
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/Temp/UB_3m.csv",
  "3m"
)

################################################################################
# TRIM ALL DATASETS TO COMMON DEPLOYMENT WINDOW
################################################################################

weather_start <- min(weather$datetime_UTC, na.rm = TRUE)
deploy_end    <- ymd_hms("2025-10-14 20:00:00", tz = "UTC")

trim <- function(df) {
  df %>%
    filter(datetime_UTC >= weather_start,
           datetime_UTC <= deploy_end)
}

do_surface <- trim(do_surface)
do_depth   <- trim(do_depth)
par        <- trim(par)
temp_2m    <- trim(temp_2m)
temp_3m    <- trim(temp_3m)
weather    <- trim(weather)

################################################################################
# FULL MERGE (using datetime_UTC as key)
################################################################################

df_list <- list(do_surface, do_depth, par, temp_2m, temp_3m, weather)

upper_brooks <- reduce(df_list, full_join, by = "datetime_UTC")

################################################################################
# TYPE COERCION – explicitly make key columns numeric
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(
    temp_1m          = as.numeric(temp_1m),
    temp_2m          = as.numeric(temp_2m),
    temp_3m          = as.numeric(temp_3m),
    do_temp_4m       = as.numeric(do_temp_4m),
    par_temp_4m      = as.numeric(par_temp_4m),
    do_mgl_1m        = as.numeric(do_mgl_1m),
    do_mgl_4m        = as.numeric(do_mgl_4m),
    par_4m           = as.numeric(par_4m),
    atm_pressure_mbar = as.numeric(atm_pressure_mbar),
    Battery_1m       = as.numeric(Battery_1m),
    Battery_4m       = as.numeric(Battery_4m),
    par_surface      = as.numeric(par_surface),
    temp_air         = as.numeric(temp_air),
    RH               = as.numeric(RH),
    wind_speed       = as.numeric(wind_speed),
    gust_speed       = as.numeric(gust_speed),
    wind_dir         = as.numeric(wind_dir)
  )

# Drop the raw MiniDOT % sat so we can replace with our own
upper_brooks <- upper_brooks %>%
  select(-any_of(c("do_sat_1m", "do_sat_4m", "do_sat_raw_1m", "do_sat_raw_4m")))

################################################################################
# DEDUPLICATE BY AVERAGING NUMERIC COLUMNS AT EACH TIMESTAMP
################################################################################

upper_brooks <- upper_brooks %>%
  group_by(datetime_UTC) %>%
  summarise(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

################################################################################
# RECREATE MST FROM UTC
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(datetime_MST = with_tz(datetime_UTC, "America/Denver")) %>%
  relocate(datetime_MST, .after = datetime_UTC)

################################################################################
# UNIFIED 4 m TEMPERATURE
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(
    temp_4m = rowMeans(cbind(do_temp_4m, par_temp_4m), na.rm = TRUE)
  )

################################################################################
# PRESSURE CORRECTION + DO SATURATION (Garcia–Benson)
################################################################################

upper_brooks <- upper_brooks %>%
  mutate(
    atm_pressure_buoy = adjust_pressure_elevation(
      Px_mbar   = atm_pressure_mbar,
      elev_from = 2758,   # weather station
      elev_to   = 2775,   # buoy depth
      T_C       = temp_1m
    ),
    
    # Correct: o2.at.sat.base(temp_C, pressure_mbar)
    do_sat_eq_1m = o2.at.sat.base(temp_1m, atm_pressure_buoy, model = "garcia-benson"),
    do_sat_eq_4m = o2.at.sat.base(temp_4m, atm_pressure_buoy, model = "garcia-benson"),
    
    # Percent saturation
    do_sat_1m = (do_mgl_1m / do_sat_eq_1m) * 100,
    do_sat_4m = (do_mgl_4m / do_sat_eq_4m) * 100
  ) %>%
  select(-do_sat_eq_1m, -do_sat_eq_4m)

################################################################################
# FINAL COLUMN ORDER + CLEANUP
################################################################################

upper_brooks <- upper_brooks %>%
  # rename raw pressure column
  rename(atm_pressure_raw = atm_pressure_mbar) %>%
  # drop MiniDOT diagnostics & extraneous columns
  select(
    -matches("^Unix.Timestamp"),
    -matches("^Q\\."),
    -matches("^Acceleration"),
    -matches("^X\\.")
  ) %>%
  # reorder in the logical structure you wanted
  select(
    # Time
    datetime_UTC,
    datetime_MST,
    
    # Temperature (surface → deep)
    temp_1m,
    temp_2m,
    temp_3m,
    temp_4m,
    
    # Dissolved Oxygen (1 m)
    do_mgl_1m,
    do_sat_1m,
    
    # Dissolved Oxygen (4 m)
    do_mgl_4m,
    do_sat_4m,
    
    # Weather station variables
    wind_speed,
    gust_speed,
    wind_dir,
    par_surface,
    temp_air,
    RH,
    atm_pressure_raw,
    atm_pressure_buoy,
    
    # Sensor diagnostics + raw 4 m temp channels
    do_temp_4m,
    par_temp_4m,
    Battery_1m,
    Battery_4m,
    
    # anything else we didn't explicitly list (should be minimal)
    everything()
  )

################################################################################
# EXPORT
################################################################################

saveRDS(
  upper_brooks,
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/upper_brooks_2025_clean.rds"
)



## Overall sanity check no dups,

# 1. No duplicate timestamps
dup_times <- upper_brooks %>%
  count(datetime_UTC) %>%
  filter(n > 1)

dup_times  # should be 0 rows

# 2. Strict 10-min spacing
is.unsorted(upper_brooks$datetime_UTC)             # should be FALSE
table(diff(as.numeric(upper_brooks$datetime_UTC))) # should be all 600

# 3. No NA in key variables
colSums(is.na(upper_brooks[, c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "par_4m",
  "atm_pressure_buoy"
)]))

# 4. Sanity check ranges
summary(upper_brooks[, c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "do_sat_1m","do_sat_4m"
)])



## Overall sanity check no dups,

# 1. No duplicate timestamps
dup_times <- upper_brooks %>%
  count(datetime_UTC) %>%
  filter(n > 1)

dup_times  # should be 0 rows

# 2. Strict 10-min spacing
is.unsorted(upper_brooks$datetime_UTC)             # should be FALSE
table(diff(as.numeric(upper_brooks$datetime_UTC))) # should be all 600

# 3. No NA in key variables
colSums(is.na(upper_brooks[, c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "par_4m",
  "atm_pressure_buoy"
)]))

# 4. Sanity check ranges
summary(upper_brooks[, c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "do_sat_1m","do_sat_4m"
)])