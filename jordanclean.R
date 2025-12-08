### ===============================================================
### UPPER BROOKS – DATA VISUALIZATION FOR CLEANING
### Updated to include all weather variables
### ===============================================================
 ## make sure to run code for upper brooks so it is in evvironemtn. 

lapply(c("dygraphs", "xts", "tidyverse", "lubridate"), require, character.only = TRUE)

upper_brooks <- upper_brooks %>%
  mutate(dt = as.POSIXct(datetime_UTC, tz = "UTC"))


## -------------------------------------------------------------------
## 1. Generic dygraph function for any variable
## -------------------------------------------------------------------
dy_var <- function(df, var, start = "2025-06-29", end = "2025-10-14") {
  
  df2 <- df %>% 
    filter(dt >= start, dt <= end)
  
  dat <- xts(df2[[var]], order.by = df2$dt)
  
  dygraph(dat, main = paste("Upper Brooks -", var)) %>%
    dyAxis("y", label = var) %>%
    dyRangeSelector()
}

## Examples — you can now use ANY variable:
dy_var(upper_brooks, "temp_1m")
dy_var(upper_brooks, "do_mgl_1m")
dy_var(upper_brooks, "par_4m")
dy_var(upper_brooks, "atm_pressure_buoy")

## NEW: Weather variables
dy_var(upper_brooks, "temp_air")
dy_var(upper_brooks, "RH")
dy_var(upper_brooks, "par_surface")
dy_var(upper_brooks, "wind_speed")
dy_var(upper_brooks, "gust_speed")
dy_var(upper_brooks, "wind_dir")
dy_var(upper_brooks, "atm_pressure_raw")
dy_var(upper_brooks, "atm_pressure_buoy")


## -------------------------------------------------------------------
## 2. Depth-specific temperature views
## -------------------------------------------------------------------
view_temp <- function(df, depth){
  
  df2 <- df %>%
    filter(dt >= "2025-06-20", dt <= "2025-10-20") %>%
    select(dt, temp = !!sym(depth))
  
  dat <- xts(df2$temp, order.by = df2$dt)
  
  dygraph(dat, main = paste("Temperature at", depth)) %>%
    dyRangeSelector()
}

view_temp(upper_brooks, "temp_1m")
view_temp(upper_brooks, "temp_4m")


## -------------------------------------------------------------------
## 3. Depth-specific Dissolved Oxygen views
## -------------------------------------------------------------------
view_DO <- function(df, depth){
  
  df2 <- df %>%
    filter(dt >= "2025-06-20", dt <= "2025-10-20") %>%
    select(dt, DO = !!sym(depth))
  
  dat <- xts(df2$DO, order.by = df2$dt)
  
  dygraph(dat, main = paste("DO mg/L at", depth)) %>%
    dyRangeSelector()
}

view_DO(upper_brooks, "do_mgl_1m")
view_DO(upper_brooks, "do_mgl_4m")


## -------------------------------------------------------------------
## 4. Multi-depth temperature panel
## -------------------------------------------------------------------
temp_xts <- xts(
  upper_brooks %>% 
    select(temp_1m, temp_4m),
  order.by = upper_brooks$dt
)

dygraph(temp_xts, main = "Upper Brooks Temperature All Depths") %>%
  dySeries("temp_1m", label = "1 m") %>%
 
  dySeries("temp_4m", label = "4 m") %>%
  dyRangeSelector()


## -------------------------------------------------------------------
## 5. NEW — Multi-panel weather visualization
## -------------------------------------------------------------------
weather_xts <- xts(
  upper_brooks %>% 
    select(wind_speed, gust_speed),
  order.by = upper_brooks$dt
)

dygraph(weather_xts, main = "Upper Brooks Weather Station Variables") %>%
  dyRangeSelector()


strat_xts <- xts(
  upper_brooks %>%
    select(temp_1m, temp_4m, wind_speed),
                 order.by = upper_brooks$dt
)

dygraph(strat_xts, main = "upper brooks temp and wind") %>% 
  dyRangeSelector()