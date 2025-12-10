####### STATS AND VISUALS ####
##########
###############################################################
## Brooks Lake – Thermal Structure, Mixing & Oxygen Gradients
## Using rLakeAnalyzer for stratification + heatmap
###############################################################

## 0. PACKAGES -------------------------------------------------
## (uncomment install lines the first time you use them)
# install.packages("rLakeAnalyzer")

library(tidyverse)
library(lubridate)
library(zoo)
library(rLakeAnalyzer)

## 1. LOAD & PREP DATA -----------------------------------------

df <- readRDS("~/Desktop/GISProject/upper_brooks_DOtemp_clean.rds") %>%
  mutate(
    dt_local = with_tz(datetime_UTC, "America/Denver")
  )

## Water-temp data frame in LakeAnalyzer format:
## datetime + columns wtr_#.# (depths in m)
wtr <- df %>%
  select(
    datetime = datetime_UTC,
    wtr_1.0  = temp_1m,
    wtr_2.0  = temp_2m,
    wtr_3.0  = temp_3m,
    wtr_4.0  = temp_4m
  )

## Make sure datetime is POSIXct (it should already be):
stopifnot(inherits(wtr$datetime, "POSIXct"))

## 2. BASIC THERMAL & OXYGEN GRADIENTS -------------------------

df <- df %>%
  mutate(
    dT  = temp_1m - temp_4m,        # thermal gradient (°C)
    dDO = do_mgl_1m - do_mgl_4m     # oxygen gradient (mg/L)
  )

## Stratification “state”
df <- df %>%
  mutate(
    strat_state = case_when(
      dT > 2   ~ "Strong",
      dT > 1   ~ "Moderate",
      dT > 0.5 ~ "Weak",
      TRUE     ~ "Mixed"
    )
  )

## 3. METADEPTHS & STRATIFICATION DYNAMICS ---------------------

## Metalimnion top/bottom via LakeAnalyzer (no bathy needed)
meta_ts <- ts.meta.depths(wtr, slope = 0.1)   # uses default density gradient

## Join back to main df
df <- df %>%
  left_join(meta_ts, by = c("datetime_UTC" = "datetime"))

## meta_ts has:
##   top    = top of metalimnion (m)
##   bottom = bottom of metalimnion (m)
## NA when no clear metalimnion

## DAILY summaries for stratification dynamics
daily_strat <- df %>%
  mutate(date = date(dt_local)) %>%
  group_by(date) %>%
  summarise(
    mean_dT      = mean(dT, na.rm = TRUE),
    max_dT       = max(dT, na.rm = TRUE),
    strat_freq   = list(table(strat_state) / sum(!is.na(strat_state))),
    meta_top_med = median(top, na.rm = TRUE),
    meta_bot_med = median(bottom, na.rm = TRUE)
  )

print(head(daily_strat, 10))

## Diel (hourly) stratification pattern
hourly_strat <- df %>%
  mutate(hour = hour(dt_local)) %>%
  group_by(hour) %>%
  summarise(
    mean_dT  = mean(dT, na.rm = TRUE),
    mean_top = mean(top, na.rm = TRUE),
    mean_bot = mean(bottom, na.rm = TRUE)
  )

print(hourly_strat)

## 4. OXYGEN DYNAMICS ACROSS WATER COLUMN ----------------------

## Seasonal ΔDO stats
dDO_stats <- summary(df$dDO)
print(dDO_stats)

## Diel oxygen pattern
hourly_DO <- df %>%
  mutate(hour = hour(dt_local)) %>%
  group_by(hour) %>%
  summarise(
    mean_DO_1m = mean(do_mgl_1m, na.rm = TRUE),
    mean_DO_4m = mean(do_mgl_4m, na.rm = TRUE),
    mean_dDO   = mean(dDO, na.rm = TRUE)
  )

print(hourly_DO)

## Simple relationship between ΔT and ΔDO (no full model, just summary)
dT_dDO_summary <- df %>%
  summarise(
    cor_dT_dDO = cor(dT, dDO, use = "complete.obs"),
    mean_dT    = mean(dT,  na.rm = TRUE),
    mean_dDO   = mean(dDO, na.rm = TRUE)
  )

print(dT_dDO_summary)

## 5. MIXING EVENT DETECTION & DRIVERS -------------------------

## ΔT change at each timestep
df <- df %>%
  arrange(datetime_UTC) %>%
  mutate(
    dT_change = c(NA, diff(dT)),
    ## “Mixing” = big collapse of ΔT
    mixing_event =
      (dT < 1 & dT_change < -0.5) |   # stratification collapses to weak/mixed
      (dT_change < -0.75)             # or just a big drop in ΔT
  )

mixing_summary <- df %>%
  summarise(
    total_points   = n(),
    mixing_points  = sum(mixing_event, na.rm = TRUE),
    pct_mixing     = round(100 * sum(mixing_event, na.rm = TRUE) / n(), 3)
  )

print(mixing_summary)

## Top 10 strongest mixing collapses
top_mixing <- df %>%
  arrange(dT_change) %>%
  slice(1:10) %>%
  select(datetime_UTC, dt_local, dT, dT_change,
         temp_1m, temp_4m, temp_air, wind_speed)

print(top_mixing)

## DAILY mean wind & ΔT to see if windier days are less stratified
daily_wind_strat <- df %>%
  mutate(date = date(dt_local)) %>%
  group_by(date) %>%
  summarise(
    mean_wind = mean(wind_speed, na.rm = TRUE),
    mean_dT   = mean(dT, na.rm = TRUE)
  )

print(head(daily_wind_strat, 15))

## Quick nonparametric comparison: “windy” vs “calm” days
wind_cutoff <- quantile(daily_wind_strat$mean_wind, 0.90, na.rm = TRUE)

windy_days <- daily_wind_strat %>% filter(mean_wind >= wind_cutoff)
calm_days  <- daily_wind_strat %>% filter(mean_wind <  wind_cutoff)

wind_strat_compare <- tibble(
  group   = c("Windy days (top 10%)", "Calm days (bottom 90%)"),
  mean_dT = c(mean(windy_days$mean_dT), mean(calm_days$mean_dT)),
  sd_dT   = c(sd(windy_days$mean_dT),   sd(calm_days$mean_dT)),
  n       = c(nrow(windy_days), nrow(calm_days))
)

print(wind_strat_compare)

windy_vs_calm_test <- wilcox.test(windy_days$mean_dT, calm_days$mean_dT)
print(windy_vs_calm_test)

## 6. LAKEANALYZER VISUALS -------------------------------------

## 6A. Temperature heatmap (main “wow” figure)
##    This opens a base-R filled contour plot.
wtr.heat.map(wtr,
             plot.title = "Brooks Lake – Temperature Heatmap",
             zlim = range(wtr[,-1], na.rm = TRUE))

## 6B. Thermocline / metalimnion tracks on top of heatmap
##     Using wtr.heatmap.layers requires extra inputs; here we just
##     overlay meta.top / meta.bottom on a simple time series.

## Convert meta depths to long format for optional ggplot overlay
meta_long <- meta_ts %>%
  pivot_longer(cols = c("top","bottom"),
               names_to = "layer",
               values_to = "depth_m")

## Example ggplot of meta depths vs time (not a heatmap, but nice)
ggplot(meta_long, aes(x = datetime, y = depth_m, colour = layer)) +
  geom_line() +
  scale_y_reverse() +
  labs(
    title = "Metalimnion Top & Bottom Depths (LakeAnalyzer)",
    x     = "Date",
    y     = "Depth (m)"
  ) +
  theme_minimal()

###############################################################
## END OF SCRIPT
###############################################################


