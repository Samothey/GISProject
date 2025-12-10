## ###############################################################################
#   UPPER BROOKS BUOY DATA – Stats analysis and visualization
#   Author: Samantha Peña
#   Purpose:
#      - We will look at the thermal structure of upper brooks lake throughout the season
#.     as well as look as dissolved oxygen dynamics 
###############################################################################============================================================

#LOAD LIBRARIES #####
library(tidyverse)
library(lubridate)
library(rLakeAnalyzer)
library(zoo)


##### READ & PREPARE DATA #####
# Convert UTC to local (MST/MDT) and select needed variables
df <- readRDS("~/Desktop/GISProject/upper_brooks_DOtemp_clean.rds") %>%
  mutate(dt_local = with_tz(datetime_UTC, "America/Denver")) %>%
  select(
    dt_local,
    temp_1m, temp_2m, temp_3m, temp_4m,
    do_mgl_1m, do_mgl_4m,
    do_sat_1m, do_sat_4m
  )


##### PREP TEMPERATURE DATA FOR STABILITY #####
# rLakeAnalyzer requires names wtr_1.0 etc.
buoy <- df %>%
  rename(
    wtr_1.0 = temp_1m,
    wtr_2.0 = temp_2m,
    wtr_3.0 = temp_3m,
    wtr_4.0 = temp_4m
  ) %>%
  select(dt_local, starts_with("wtr_"))

# Sensor depths
depths <- c(1, 2, 3, 4)

# Simple bathymetry (Brooks is small & shallow)
bthD <- c(1, 2, 3, 4)
bthA <- c(1, 0.8, 0.6, 0.4)


##### CALCULATE SCHMIDT STABILITY #####
stability_df <- buoy %>%
  drop_na() %>%
  rowwise() %>%
  mutate(
    St = schmidt.stability(
      wtr = c_across(starts_with("wtr_")),
      depths = depths,
      bthA = bthA,
      bthD = bthD
    )
  ) %>%
  ungroup()

# Smooth stability signal (18 points ≈ 3 hours for 10-min data)
stability_df <- stability_df %>%
  mutate(St_smooth = zoo::rollmean(St, k = 12, fill = NA, align = "center"))


##### MERGE WITH DO DATA #####
st_do <- stability_df %>%
  left_join(
    df %>%
      select(
        dt_local,
        do_surface = do_mgl_1m,
        do_deep = do_mgl_4m,
        do_sat_surface = do_sat_1m,
        do_sat_deep = do_sat_4m
      ),
    by = "dt_local"
  )


##### NOTE ABOUT STRATIFICATION MARKERS #####
# These are NOT used for classification 
# They are OPTIONAL visual reference lines.

strat_onset <- st_do %>% filter(St_smooth > 3) %>% slice(1) %>% pull(dt_local)
peak_strat  <- st_do %>% filter(St_smooth == max(St_smooth, na.rm = TRUE)) %>% pull(dt_local)
turnover    <- st_do %>% filter(dt_local > peak_strat, St_smooth < 1) %>% slice(1) %>% pull(dt_local)



##############################
###   GRAPHS   ###
##############################

##### FIGURE 1 — FULL-SEASON STABILITY ( with dashes lines)  #####
ggplot(st_do, aes(dt_local, St_smooth)) +
  geom_line(color = "#0077BB") +
  geom_vline(xintercept = as.numeric(strat_onset), linetype="dashed", color="green", linewidth = 1.0) +
  geom_vline(xintercept = as.numeric(peak_strat),  linetype="dashed", color="red", linewidth = 1.0) +
  geom_vline(xintercept = as.numeric(turnover),    linetype="dashed", color="purple", linewidth = 1.0
             ) +
  labs(
    title = "Seasonal Thermal Stability — Upper Brooks Lake",
    x = "Date",
    y = "Schmidt Stability (J/m²)"
  ) +
  theme_minimal()

##### FIGURE 1.2 — FULL-SEASON STABILITY ( without  dashes lines)  #####
ggplot(st_do, aes(dt_local, St_smooth)) +
  geom_line(color = "#0077BB") +
  labs(
    title = "Seasonal Thermal Stability — Upper Brooks Lake",
    x = "Date",
    y = "Schmidt Stability (J/m²)"
  ) +
  theme_minimal()


##### FIGURE 2 — ZOOMED STABILITY (JUNE → MID-AUGUST) #####
zoom_start <- as.POSIXct("2025-06-29", tz = "America/Denver")
zoom_end   <- as.POSIXct("2025-08-15", tz = "America/Denver")

ggplot(
  st_do %>% filter(dt_local >= zoom_start, dt_local <= zoom_end),
  aes(dt_local, St_smooth)
) +
  geom_line(color = "#0077BB", linewidth = 1.0) +
  labs(
    title = "Thermal Stability (Zoom: June 29–Aug 15)",
    x = "Date",
    y = "Schmidt Stability (J/m²)"
  ) +
  theme_minimal(base_size = 14)


##### FIGURE 3 — DO DYNAMICS OVER TIME #####
ggplot(st_do, aes(dt_local)) +
  geom_line(aes(y = do_surface, color = "Surface DO"), linewidth = 0.8) +
  geom_line(aes(y = do_deep,    color = "Deep DO"),    linewidth = 0.8) +
  labs(
    title = "Surface & Deep Dissolved Oxygen Through the Season",
    x = "Month",
    y = "DO (mg/L)",
    color = ""
  ) +
  theme_minimal()


##### FIGURE 4 — FUNCTIONAL RESPONSE: DEEP DO vs STABILITY #####
ggplot(st_do, aes(St_smooth, do_deep)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
              color = "blue", linewidth = 1.2, se = FALSE) +
  labs(
    title = "Deep DO Response to Thermal Stability",
    x = "Schmidt Stability (J/m²)",
    y = "Deep DO (mg/L)"
  ) +
  theme_minimal()

## END
