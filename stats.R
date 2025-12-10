###############################################################
## BROOKS LAKE PHYSICAL ANALYSIS: STRATIFICATION, WIND & DO
## Samantha Peña — 2025
##
## GOAL:
##   Use cleaned buoy data (temp, DO, wind) to:
##     1. Quantify stratification (ΔT)
##     2. Detect mixing events
##     3. Test how wind influences stratification & mixing
##     4. Test how stratification influences DO gradients
##     5. Produce figures for a class project
##
## INPUT FILE:
##   "~/Desktop/GISProject/upper_brooks_DOtemp_clean.rds"
##
###############################################################
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)

#---------------------------------------------------------------
# 1. Load Cleaned Data
#---------------------------------------------------------------

df <- readRDS("~/Desktop/GISProject/upper_brooks_DOtemp_clean.rds") %>%
  mutate(
    dt = with_tz(datetime_UTC, "America/Denver")
  )

#---------------------------------------------------------------
# 2. Compute ΔT (stratification) and ΔDO (oxygen separation)
#---------------------------------------------------------------

df <- df %>%
  mutate(
    dT  = temp_1m - temp_4m,     # thermal gradient
    dDO = do_mgl_1m - do_mgl_4m  # oxygen gradient
  )

#---------------------------------------------------------------
# 3. Detect stratification categories
#---------------------------------------------------------------

df <- df %>%
  mutate(
    strat_state = case_when(
      dT > 2   ~ "Strong",
      dT > 1   ~ "Moderate",
      dT > 0.5 ~ "Weak",
      TRUE     ~ "Mixed"
    )
  )

#---------------------------------------------------------------
# 4. Detect wind-driven mixing events
#---------------------------------------------------------------

df <- df %>%
  mutate(
    dT_change = c(NA, diff(dT)),
    mixing_event =
      (dT < 1 & dT_change < -0.5) |   # collapse into weak stratification
      (dT_change < -0.75)             # big drop alone
  )

#---------------------------------------------------------------
# 5. Smooth variables for visualization
#---------------------------------------------------------------

df <- df %>%
  mutate(
    wind_30min = rollmean(wind_speed, k = 3, fill = NA),
    dT_smooth  = rollmean(dT, k = 5, fill = NA)
  )

#---------------------------------------------------------------
# 6. DESCRIPTIVE STATISTICS (NO regression models here)
#---------------------------------------------------------------

# Percent of time strongly stratified
strat_summary <- df %>%
  count(strat_state) %>%
  mutate(pct = n / sum(n) * 100)

print(strat_summary)

# ΔT summary
summary(df$dT)

# ΔDO summary
summary(df$dDO)

# Largest mixing events by ΔT collapse
top_mixing <- df %>%
  arrange(dT_change) %>%
  slice(1:10)

print(top_mixing)

#---------------------------------------------------------------
# 7. VISUALIZATIONS
#---------------------------------------------------------------

## FIGURE 1 — Stratification strength through time
p1 <- ggplot(df, aes(dt, dT)) +
  geom_line(color="#2c7bb6") +
  geom_hline(yintercept=c(0.5,1,2), linetype="dashed") +
  labs(
    y="ΔT (°C)",
    x="Date",
    title="Stratification Strength (ΔT) Over Time"
  )
print(p1)


## FIGURE 2 — ΔDO through time (oxygen separation)
p2 <- ggplot(df, aes(dt, dDO)) +
  geom_line(color="#ab2328") +
  labs(
    y="ΔDO (mg/L)",
    x="Date",
    title="Vertical Oxygen Gradient (ΔDO) Over Time"
  )
print(p2)


## FIGURE 3 — Wind vs ΔT with rolling smoothing (descriptive only)
p3 <- ggplot(df, aes(dt)) +
  geom_line(aes(y = dT_smooth), color="#1f78b4") +
  geom_line(aes(y = wind_30min), color="#33a02c") +
  scale_y_continuous(
    name="ΔT (°C)",
    sec.axis = sec_axis(~ ., name="Wind (m/s)")
  ) +
  labs(
    title="Wind and Stratification Over Time",
    x="Date"
  )
print(p3)


## FIGURE 4 — Mixing event detection
p4 <- ggplot(df, aes(dt, dT)) +
  geom_line() +
  geom_point(
    data=df %>% filter(mixing_event),
    aes(dt, dT),
    color="red", size=2
  ) +
  labs(
    title="Detected Mixing Events",
    y="ΔT (°C)",
    x="Date"
  )
print(p4)


## FIGURE 5 — Diel cycles (zoom window)
# Filter 3 sample days
diel_days <- df %>% 
  filter(date(dt) %in% sort(unique(date(df$dt)))[20:22])

p5 <- ggplot(diel_days, aes(dt, dT)) +
  geom_line(color="#2c7bb6") +
  labs(
    title="Diel Temperature Cycle (Example Days)",
    x="Time",
    y="ΔT (°C)"
  )
print(p5)


## FIGURE 6 — Temperature pseudo-heatmap (line-fill)
p6 <- df %>%
  select(dt, temp_1m, temp_4m) %>%
  pivot_longer(cols = starts_with("temp"), names_to="depth", values_to="temp") %>%
  ggplot(aes(dt, depth, fill=temp)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title="Temperature Structure (Pseudo-Heatmap)",
    x="Date",
    y="Depth Layer"
  )
print(p6)


###############################################################
## END OF SCRIPT
###############################################################


###############################################################
## BROOKS LAKE — PHYSICAL ANALYSIS (NO MODELING, NO VISUALS)
## Variables: temp_1m, temp_4m, do_mgl_1m, do_mgl_4m, wind_speed
## Output: descriptive summaries of stratification, DO gradients,
##         mixing events, and wind patterns.
###############################################################

library(tidyverse)
library(lubridate)
library(zoo)

#---------------------------------------------------------------
# 1. Load cleaned dataset
#---------------------------------------------------------------
df <- readRDS("~/Desktop/GISProject/upper_brooks_DOtemp_clean.rds") %>%
  mutate(dt = with_tz(datetime_UTC, "America/Denver"))

#---------------------------------------------------------------
# 2. Compute ΔT (thermocline strength) and ΔDO (oxygen separation)
#---------------------------------------------------------------
df <- df %>%
  mutate(
    dT  = temp_1m - temp_4m,       # °C difference
    dDO = do_mgl_1m - do_mgl_4m    # mg/L difference
  )

#---------------------------------------------------------------
# 3. Assign stratification categories
#---------------------------------------------------------------
df <- df %>%
  mutate(
    strat_state = case_when(
      dT > 2   ~ "Strong Stratification",
      dT > 1   ~ "Moderate Stratification",
      dT > 0.5 ~ "Weak Stratification",
      TRUE     ~ "Mixed"
    )
  )

#---------------------------------------------------------------
# 4. Compute change in ΔT to identify mixing events
#---------------------------------------------------------------
df <- df %>%
  mutate(
    dT_change = c(NA, diff(dT)),
    
    mixing_event =
      (dT < 1 & dT_change < -0.5) |   # collapse into weak stratification
      (dT_change < -0.75)             # OR large drop alone
  )

#---------------------------------------------------------------
# 5. Smooth wind for summary analyses (optional)
#---------------------------------------------------------------
df <- df %>%
  mutate(
    wind_30min = rollmean(wind_speed, k = 3, fill = NA)
  )

#---------------------------------------------------------------
# 6. SUMMARY ANALYSIS OUTPUT
#---------------------------------------------------------------

### 6A — How frequently was the lake stratified?
strat_freq <- df %>%
  count(strat_state) %>%
  mutate(percent = round(n / sum(n) * 100, 1))
print(strat_freq)

### 6B — Overall ΔT statistics
dT_stats <- summary(df$dT)
print(dT_stats)

### 6C — Overall ΔDO statistics
dDO_stats <- summary(df$dDO)
print(dDO_stats)

### 6D — Summary of mixing events
mixing_summary <- df %>%
  summarise(
    total_points = n(),
    mixing_points = sum(mixing_event, na.rm=TRUE),
    pct_mixing = round(sum(mixing_event, na.rm=TRUE) / n() * 100, 2)
  )
print(mixing_summary)

### 6E — Largest 10 mixing-event collapses
top_mixing_events <- df %>%
  arrange(dT_change) %>%
  slice(1:10) %>%
  select(dt, dT, dT_change, wind_speed, wind_30min)
print(top_mixing_events)

### 6F — Compare stratification on windy vs calm periods
wind_threshold <- quantile(df$wind_speed, 0.75, na.rm=TRUE)

wind_strat_summary <- df %>%
  mutate(windy = wind_speed >= wind_threshold) %>%
  group_by(windy) %>%
  summarise(
    mean_dT = mean(dT, na.rm=TRUE),
    mean_dDO = mean(dDO, na.rm=TRUE),
    mean_wind = mean(wind_speed, na.rm=TRUE),
    n = n()
  )
print(wind_strat_summary)

### 6G — Identify diel variation (hourly patterns)
hourly_summary <- df %>%
  mutate(hour = hour(dt)) %>%
  group_by(hour) %>%
  summarise(
    mean_temp1 = mean(temp_1m, na.rm=TRUE),
    mean_temp4 = mean(temp_4m, na.rm=TRUE),
    mean_dT = mean(dT, na.rm=TRUE),
    mean_DO_surface = mean(do_mgl_1m, na.rm=TRUE),
    mean_DO_bottom = mean(do_mgl_4m, na.rm=TRUE),
    mean_dDO = mean(dDO, na.rm=TRUE)
  )
print(hourly_summary)

### 6H — Summaries for reporting
lake_summary <- list(
  "Stratification Frequency" = strat_freq,
  "ΔT Statistics" = dT_stats,
  "ΔDO Statistics" = dDO_stats,
  "Mixing Summary" = mixing_summary,
  "Wind vs Stratification" = wind_strat_summary,
  "Hourly Patterns" = hourly_summary
)

lake_summary
###############################################################
## END OF SCRIPT
###############################################################

