
## ================================================================
##    UPPER BROOKS BUOY CLEANING PIPELINE
##    Keeps UTC + MST, Add in pressure from weather station 
## =================================================================

library(tidyverse)
library(lubridate)
library(purrr)
library(dplyr)
library(LakeMetabolizer) 

 ## ------------------------------------------------------------
 ## Function: Fix midight timestamp that loses "00:00:00" 
 ## (removed by converting datetime to character (do this before writing to csv)
 ## ---------------------------------------------------------- 
      convert_to_character <- function(df) {
     na_test <- as.POSIXct(df$datetime_UTC, format = "%Y-%m-%d %H:%M") # Checks for NA's in UTC column (will only find them at midnight)
     na_indices <- is.na(na_test) # Gets indices of NA's 
       df$datetime[na_indices] <- paste(df$datetime[na_indices], "00:00:00") # Replaces with 00:00:00
      return(df)
        }
  
  ## ----------------------------------------------------------
  ## Function: Adjust pressure for elevation 
  ## (takes atmospheric pressure at x elevation and converts it to atmospheric pressure at y elevation (in mb)
  ## Px_mbar = input atmospheric pressure
  ## T_C = Air temp at the same time as the above pressure
  ## ----------------------------------------------------------
  adjust_pressure_elevation <- function(Px_mbar, elev_from, elev_to, T_C) {
    g <- 9.80665
    M <- 0.0289644
    R <- 8.3144598
    T <- T_C + 273.15  # convert C to K
    
    delta_h <- elev_to - elev_from
    Py_mbar <- Px_mbar * exp(-(g * M * delta_h) / (R * T))
    return(Py_mbar)
       }
  
  ## =====================================================================
  ## ATMOSPHERIC PRESSURE (ONLY PRESSURE column kept) 
  ## =====================================================================
   weather_pressure <- read.csv("/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/BrooksWeather_2025.csv", skip = 1, header = TRUE) %>%
     rename(datetime_raw = Date.Time..GMT.07.00,
  pressure_inHg = Pressure..in.Hg..LGR.S.N..22313858..SEN.S.N..22279472.) %>%
  select(datetime_raw, pressure_inHg) %>%    # Keep ONLY pressure
  mutate(
    # Convert raw timestamp to POSIXct assuming input is GMT-7
    datetime_GMT7 = mdy_hms(datetime_raw, tz = "Etc/GMT+7"),
    
    # Convert to **both UTC and MST**
    datetime_UTC = with_tz(datetime_GMT7, tzone = "UTC"),
    datetime_MST = with_tz(datetime_GMT7, tzone = "America/Denver"),
    
    # Convert inches Hg → mbar
    atm_pressure_mbar = pressure_inHg * 33.8639
  ) %>%
  select(datetime_UTC, datetime_MST, atm_pressure_mbar)
      
  ## =================================================================================
  ## MINI DOT SURFACE SENSOR ( DISSOLVED OXYGEN, TEMPERATURE, BATTERY)
  ## ==============================================================================
     do_surface <-read.table(
       "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/DO/UB_Surface.TXT", 
       sep = ",", header=TRUE, skip = 7
       ) %>%
        slice(-1) %>% 
       select(
            Mountain.Standard.Time,
            `UTC_Date_._Time`,
             Temperature,
            Dissolved.Oxygen, 
            Dissolved.Oxygen.Saturation,
            Battery
      ) %>% 
      rename(
               datetime_MST = Mountain.Standard.Time,
               datetime_UTC = `UTC_Date_._Time`,
               temp_1m = Temperature,
               do_mgl_1m = Dissolved.Oxygen,
               do_sat_1m = Dissolved.Oxygen.Saturation,
               Battery_1m = Battery
    ) %>%
       mutate(
         datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
         datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
         datetime_MST = round_date(datetime_MST, "10 minutes"),
         datetime_UTC = round_date(datetime_UTC, "10 minutes")
       )
  ## ============================================================================
  ## MINI DOT DEPTH SENSOR ( DISSOLVED OXYGEN, TEMPERATURE, BATTERY)
  ## =========================================================================== 
     
     do_depth <-read.table(
       "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/DO/UB_Depth.TXT", 
       sep = ",", header=TRUE, skip = 7
     ) %>%
       slice(-1) %>% 
       select(
         Mountain.Standard.Time,
         `UTC_Date_._Time`,
         Temperature,
         Dissolved.Oxygen, 
         Dissolved.Oxygen.Saturation,
         Battery
       ) %>% 
       rename(
         datetime_MST = Mountain.Standard.Time,
         datetime_UTC = `UTC_Date_._Time`,
         do_temp_4m = Temperature,
         do_mgl_4m = Dissolved.Oxygen,
         do_sat_4m = Dissolved.Oxygen.Saturation,
         Battery_4m = Battery
       ) %>%
       mutate(
         datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
         datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
         datetime_MST = round_date(datetime_MST, "10 minutes"),
         datetime_UTC = round_date(datetime_UTC, "10 minutes")
       )
     
   ## ============================================================================
   ##  PAR SENSOR 
   ## =========================================================================== 
     par <- read.table(
       "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/RAW/PAR/UB_PAR.TXT",  
       sep = ",", header = T, skip = 4
       ) %>% 
       slice(-1) %>% 
       select(
           Mountain.Standard.Time,
           `UTC_Date_._Time`,
           Temperature,
           PAR
       ) %>%
       rename(
           datetime_MST = Mountain.Standard.Time,
           datetime_UTC = `UTC_Date_._Time`,
           par_temp_4m = Temperature,
           par_4m = PAR
       ) %>%
       mutate(
          datetime_MST = ymd_hms(datetime_MST, tz = "US/Mountain"),
          datetime_UTC = ymd_hms(datetime_UTC, tz = "UTC"),
          datetime_MST = round_date(datetime_MST, "10 minutes"),
          datetime_UTC = round_date(datetime_UTC, "10 minutes")
  )
     
   ## ========================================================================================
   ##  PENDANT TEMPERATURE ( DEPTHS: 2M AND 3M ) 
   ##  WORTH TO NOTE WE USED TEMPERATURE ON MINIDOT SENSORS FOR 1M AND 4M (SURFACE AND DEPTH)
   ## =========================================================================================
     
     clean_pendant <- function(path, depth_label) {
       
       #-----------------------------------------------------------#
       # 1. Read raw pendant file (GMT timestamps, temp in °F)
       #-----------------------------------------------------------#
       raw <- read.csv(path, skip = 1, header = TRUE)
       
       # Identify columns automatically so this works for all pendants
       dt_col <- grep("Date.Time..GMT", names(raw), value = TRUE)
       temp_col <- grep("^Temp", names(raw), value = TRUE)
       
       # Convert original timestamp (GMT -07:00)
       datetime_GMT <- mdy_hms(raw[[dt_col]], tz = "GMT")
       
       #-----------------------------------------------------------#
       # 2. Convert GMT → UTC and GMT → MST
       #-----------------------------------------------------------#
       datetime_UTC <- with_tz(datetime_GMT, "UTC")
       datetime_MST <- with_tz(datetime_GMT, "US/Mountain")
       
       #-----------------------------------------------------------#
       # 3. Build working dataframe
       #-----------------------------------------------------------#
       df <- tibble(
         datetime_GMT = datetime_GMT,
         datetime_UTC = datetime_UTC,
         datetime_MST = datetime_MST,
         temp_F = raw[[3]]    #column 3 as temp
       )
       
       #-----------------------------------------------------------#
       # 4. Convert temperature from °F → °C
       #-----------------------------------------------------------#
       df <- df %>%
         mutate(temp_C = (temp_F - 32) * 5/9)
       
       #-----------------------------------------------------------#
       # 5. Interpolate to 5-minute resolution ( we did this because the pendants were set to every 15 minutes but the minidot senesors were set to every 10 mins)
       # (we use UTC as the "working" timeline so all sensors align)
       #-----------------------------------------------------------#
       df <- df %>%
         complete(datetime_UTC = seq(min(datetime_UTC), max(datetime_UTC), by = "5 min")) %>%
         arrange(datetime_UTC) %>%
         mutate(temp_C = approx(datetime_UTC, temp_C, datetime_UTC)$y)
       
       #-----------------------------------------------------------#
       # 6. Bin to 10-min intervals + average
       #-----------------------------------------------------------#
       df <- df %>%
         mutate(
           datetime_UTC = floor_date(datetime_UTC, "10 minutes"),
           datetime_MST = with_tz(datetime_UTC, "US/Mountain")
         ) %>%
         group_by(datetime_UTC, datetime_MST) %>%
         summarise(temp_C = mean(temp_C, na.rm = TRUE), .groups = "drop")
       
       #-----------------------------------------------------------#
       # 7. Rename temp column to depth-specific name
       #-----------------------------------------------------------#
       df <- df %>%
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
   
     ## ========================================================================================
     ##  MERGE ALL DATA (USING UTC AS THE MERGE KEY)
     ## =================================================================================  
      
     
     ##===============================================
     ## Quick check for PAR TEMP AND DO TEMP
     ## both at sensos has a 4M temp
     ##=================================================
     ##### decision: Updated Sensor Agreement Results Correlation (r) = 0.9987, This is still extraordinarily high.
     #The sensors track each other with near-perfect synchrony. No meaningful differences in patterns or timing. Bias = –0.117°C
     #This means: The PAR sensor’s temperature reads 0.12°C cooler on average than the MiniDOT. This offset is tiny and well within normal instrument variability.
     # RMSE = 0.155°C. This indicates: the average difference between sensors is 0.16°C. This is extremely small, especially in natural lake systems.
     # RMSE < 0.2°C is considered excellent agreement. FINAL TAKEAWAY: Averaging the two sensors is absolutely justified
     #With: r ≈ 0.999, bias ≈ 0.12°C, RMSE ≈ 0.15°C. the two sensors essentially provide the same physical signal, just with small measurement noise.
     # Averaging them: reduces random error, stabilizes the dataset, avoids having to choose one sensor arbitrarily, provides the most robust 4m temperature record for your analysis
     
     df_list <- list(
       do_surface, do_depth, par,
       temp_2m, temp_3m, weather_pressure
     )
     
     upper_brooks <- reduce(df_list, full_join, by = "datetime_UTC") %>%
       filter(datetime_UTC >= ymd_hms("2025-06-24 16:20:00"))
     
     upper_brooks <- upper_brooks %>%
       mutate(
         do_temp_4m  = as.numeric(trimws(do_temp_4m)),
         par_temp_4m = as.numeric(trimws(par_temp_4m))
       )
     
     
     ## plot 
     ggplot(upper_brooks, aes(x = do_temp_4m, y = par_temp_4m)) +
       geom_point(alpha = 0.3) +
       geom_smooth(method = "lm", color = "red") +
       labs(title = "Sensor Agreement at 4 m",
            x = "MiniDOT 4m Temp (°C)",
            y = "PAR Temp (°C)")
     
     cor_val <- cor(upper_brooks$do_temp_4m, upper_brooks$par_temp_4m, use = "complete.obs")
     bias_val <- mean(upper_brooks$par_temp_4m - upper_brooks$do_temp_4m, na.rm = TRUE)
     rmse_val <- sqrt(mean((upper_brooks$par_temp_4m - upper_brooks$do_temp_4m)^2, na.rm = TRUE))
     
     cor_val
     bias_val
     rmse_val
     
     ##============================================
     # Stats i used to come to this conclusion: 
     #=============================================
     #Pearson correlation (r) — This measured how similarly the two sensors changed over time. 
     #correlation (~0.999) shows the sensors track each other almost perfectly.
     #Mean bias — This quantified whether one sensor consistently reads higher or lower than the other. Your bias (~ –0.12 °C) indicates the PAR sensor reads only slightly cooler on average, a negligible systematic difference.
     #Root Mean Square Error (RMSE) — This measured the average size of the differences between the two sensors. Your RMSE (~ 0.15 °C) shows the disagreement is very small and well within normal sensor precision
  
     
     
     
     
     
     
     
     
     
     
     