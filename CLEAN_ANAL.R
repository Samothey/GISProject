## ============================================================
## In this script we are going to get the flagged csv sheet 
## that our shiny app created and we are going to remove the 
# flagged values (within reason), after we will visualize 
## the data and then run some analysis.
## ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

## 1. FILE PATHS -------------------------------------------------

# Original pre-QA/QC dataset (same one you used in Shiny)
path_raw_rds   <- "/Users/samanthapena/Desktop/GISProject/upper_brooks_2025_clean.rds"


# Flagged CSV exported from Shiny ("Download flagged timestamps"

path_flags_csv <- "/Users/samanthapena/Desktop/GISProject/UpperBrooks_flags_2025-12-09.csv"

# Output cleaned files
path_out_rds   <- "/Users/samanthapena/Desktop/GISProject/upper_brooks_DOtemp_clean.rds"
path_out_csv   <- "/Users/samanthapena/Desktop/GISProject/upper_brooks_DOtemp_clean.csv"


## 2. LOAD DATA --------------------------------------------------

upper_raw <- readRDS(path_raw_rds)

flags_all <- read_csv(path_flags_csv,
                      show_col_types = FALSE)

# Check columns in flags_all if you want:
# glimpse(flags_all)

## 3. VARIABLES OF INTEREST -------------------------------------

vars_interest <- c(
  "do_mgl_1m","do_mgl_4m",
  "do_sat_1m","do_sat_4m",
  "temp_1m","temp_2m","temp_3m","temp_4m"
)

# Keep only flags for those variables
flags_sub <- flags_all %>%
  filter(variable %in% vars_interest)

# Ensure datetime_UTC is POSIXct and matches type in upper_raw
if (!inherits(upper_raw$datetime_UTC, "POSIXct")) {
  stop("datetime_UTC in upper_raw is not POSIXct – check your raw file.")
}

if (!inherits(flags_sub$datetime_UTC, "POSIXct")) {
  flags_sub <- flags_sub %>%
    mutate(datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"))
}


## 4. LONG FORMAT JOIN ------------------------------------------

# Put DO + temp from raw into long format
raw_long <- upper_raw %>%
  select(datetime_UTC, all_of(vars_interest)) %>%
  pivot_longer(
    cols = -datetime_UTC,
    names_to = "variable",
    values_to = "value"
  )

# Join flags onto long data by datetime_UTC + variable
raw_long_flagged <- raw_long %>%
  left_join(
    flags_sub %>%
      select(datetime_UTC, variable,
             RANGE, NEGATIVE, SPIKE, FLAT,
             REMOVAL, DO_SPIKE, BUBBLE, PAR_NIGHT),
    by = c("datetime_UTC", "variable")
  )

## 5. DEFINE "BAD" FLAGS TO REMOVE -------------------------------

raw_long_clean <- raw_long_flagged %>%
  mutate(
    RANGE    = coalesce(RANGE,    FALSE),
    NEGATIVE = coalesce(NEGATIVE, FALSE),
    REMOVAL  = coalesce(REMOVAL,  FALSE),
    BUBBLE   = coalesce(BUBBLE,   FALSE),
    # DO_SPIKE, SPIKE, FLAT, PAR_NIGHT left as-is for info only
    
    # Rule: remove if any of these structural flags are TRUE
    bad_flag = RANGE | NEGATIVE | REMOVAL | BUBBLE,
    
    # Cleaned value: NA if bad_flag, else original
    value_clean = if_else(bad_flag, NA_real_, value)
  )

## 6. WIDE FORMAT (BACK TO ONE COLUMN PER VARIABLE) --------------

do_temp_clean_wide <- raw_long_clean %>%
  select(datetime_UTC, variable, value_clean) %>%
  pivot_wider(
    names_from  = variable,
    values_from = value_clean
  ) %>%
  arrange(datetime_UTC)

## 7. MERGE BACK WITH OTHER COLUMNS ------------------------------

# Drop old DO+temp from raw, join cleaned ones back in
upper_brooks_DOtemp_clean <- upper_raw %>%
  select(-all_of(vars_interest)) %>%
  left_join(do_temp_clean_wide, by = "datetime_UTC")

## 8. SAVE CLEANED DATA ------------------------------------------

saveRDS(upper_brooks_DOtemp_clean, path_out_rds)
write.csv(upper_brooks_DOtemp_clean, path_out_csv, row.names = FALSE)

cat("Cleaning complete.\n")
cat("Saved RDS to: ", path_out_rds, "\n", sep = "")
cat("Saved CSV to: ", path_out_csv, "\n", sep = "")

 ## now lets visualize cleaned data!
### ===============================================================
### UPPER BROOKS – RAW vs CLEANED DATA VISUALIZATION
### Allows toggling between datasets
### ===============================================================

library(dygraphs)
library(xts)
library(tidyverse)
library(lubridate)

## ---------------------------------------------------------------
## 1. Load RAW and CLEANED datasets
## ---------------------------------------------------------------

raw_path   <- "/Users/samanthapena/Desktop/GISProject/upper_brooks_2025_clean.rds"
clean_path <- "/Users/samanthapena/Desktop/GISProject/upper_brooks_DOtemp_clean.rds"

upper_raw   <- readRDS(raw_path)
upper_clean <- readRDS(clean_path)

stopifnot("datetime_UTC" %in% names(upper_clean))
stopifnot(inherits(upper_clean$datetime_UTC, "POSIXct"))

## ---------------------------------------------------------------
## 2. Add MST timestamp to both datasets
## ---------------------------------------------------------------
upper_raw <- upper_raw %>%
  mutate(dt = with_tz(datetime_UTC, "America/Denver"))

upper_clean <- upper_clean %>%
  mutate(dt = with_tz(datetime_UTC, "America/Denver"))

## ---------------------------------------------------------------
## 3. SELECT DATASET HERE
## ---------------------------------------------------------------
## change between: "raw" or "clean"

data_choice <- "clean"   # <<<<<< SET HERE

upper_brooks <- if (data_choice == "raw") upper_raw else upper_clean


## ---------------------------------------------------------------
## 4. Generic dygraph: ANY variable
## ---------------------------------------------------------------
dy_var <- function(df, var, start = "2025-06-29", end = "2025-10-14") {
  
  df2 <- df %>% 
    filter(dt >= ymd(start), dt <= ymd(end))
  
  dat <- xts(df2[[var]], order.by = df2$dt)
  
  dygraph(dat, main = paste("Upper Brooks (", data_choice, ") -", var, sep="")) %>%
    dyAxis("y", label = var) %>%
    dyRangeSelector()
}

## ---------------------------------------------------------------
## 5. Depth-specific DO viewer
## ---------------------------------------------------------------
view_DO <- function(df, depth){
  
  df2 <- df %>%
    filter(dt >= ymd("2025-06-20"), dt <= ymd("2025-10-20")) %>%
    select(dt, DO = !!sym(depth))
  
  dat <- xts(df2$DO, order.by = df2$dt)
  
  dygraph(dat, main = paste("DO mg/L at", depth, "(", data_choice, ")")) %>%
    dyAxis("y", label = "mg/L") %>%
    dyRangeSelector()
}

## ---------------------------------------------------------------
## 6. Depth-specific Temperature viewer
## ---------------------------------------------------------------
view_temp <- function(df, depth){
  
  df2 <- df %>%
    filter(dt >= "2025-06-20", dt <= "2025-10-20") %>%
    select(dt, temp = !!sym(depth))
  
  dat <- xts(df2$temp, order.by = df2$dt)
  
  dygraph(dat, main = paste("Temperature at", depth, "(", data_choice, ")")) %>%
    dyRangeSelector()
}

## ---------------------------------------------------------------
## 7. Multi-depth Temperature Panel
## ---------------------------------------------------------------
temp_xts <- xts(
  upper_brooks %>% select(temp_1m, temp_4m),
  order.by = upper_brooks$dt
)

dygraph(temp_xts, main = paste("Upper Brooks Temperature (", data_choice, ")", sep="")) %>%
  dySeries("temp_1m", label = "1 m") %>%
  dySeries("temp_4m", label = "4 m") %>%
  dyRangeSelector()

## ---------------------------------------------------------------
## 8. Multi-panel Weather Visualization
## ---------------------------------------------------------------
weather_xts <- xts(
  upper_brooks %>% select(wind_speed, gust_speed),
  order.by = upper_brooks$dt
)

dygraph(weather_xts, main = paste("Weather Variables (", data_choice, ")", sep="")) %>%
  dyRangeSelector()

## ---------------------------------------------------------------
## 9. Mixed stratification panel (Temp + Wind)
## ---------------------------------------------------------------
## plot surafce and depth temp 
strat_xts <- xts(
  upper_brooks %>% select(temp_1m, temp_4m, wind_speed),
  order.by = upper_brooks$dt
)

dygraph(strat_xts, main = paste("Stratification Drivers (", data_choice, ")", sep="")) %>%
  dyRangeSelector()

## all temp plotted 
strat_xts <- xts(
  upper_brooks %>% select(temp_1m, temp_2m, temp_3m, temp_4m, wind_speed),
  order.by = upper_brooks$dt
)

dygraph(strat_xts, main = paste("Stratification Drivers (", data_choice, ")", sep="")) %>%
  dyRangeSelector()

## plotted DO 
strat_xts <- xts(
  upper_brooks %>% select(do_mgl_1m, do_mgl_4m, wind_speed),
  order.by =upper_brooks$dt
)
dygraph(strat_xts, main = paste("Stratification Drivers (", data_choice, ")", sep="")) %>%
  dyRangeSelector()

## END






