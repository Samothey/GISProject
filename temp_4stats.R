## analysis for PAR TEMP AND DO TMP _4m. 
##################
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

## ------------------------------------------------------------
## Function: Fix midight timestamp that loses "00:00:00" 
## (removed by converting datetime to character (do this before writing to csv)
## ---------------------------------------------------------- 
convert_to_character <- function(df) {
  na_test <- as.POSIXct(df$datetime_UTC, format = "%Y-%m-%d %H:%M") # Checks for NA's in UTC column (will only find them at midnight)
  na_indices <- is.na(na_test) # Gets indices of NA's 
  df$datetime_UTC[na_indices] <- paste(df$datetime_UTC[na_indices], "00:00:00") # Replaces with 00:00:00
  return(df)
}

df <- read.csv("/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/UpperBrooks_2025_merged_clean.csv")

View(df)      
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















