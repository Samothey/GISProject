library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

# ---- 1. List all datastreams for YOUR buoy ----

thing_url <- "https://wyseason.uwyo.edu/sta/v1.1/Things(321)/Datastreams"

ds_raw <- request(thing_url) |> 
  req_perform() |> 
  resp_body_string() |> 
  fromJSON()

datastreams <- ds_raw$value

# Preview names + IDs
datastreams %>% select(name, `@iot.id`, properties.parameterName)


# ---- 2. Function to download observations for one datastream ----

get_obs <- function(ds_id, param_name) {
  
  obs_url <- paste0("https://wyseason.uwyo.edu/sta/v1.1/Datastreams(", ds_id, ")/Observations")
  
  raw <- request(obs_url) |>
    req_perform() |>
    resp_body_string() |>
    fromJSON()
  
  # Convert to tibble
  df <- raw$value %>%
    transmute(
      datetime = ymd_hms(phenomenonTime),
      !! param_name := as.numeric(result)
    )
  
  return(df)
}

# ---- 3. Download all datastreams automatically ----

all_dfs <- map2(
  datastreams$`@iot.id`,
  datastreams$properties$parameterName,
  get_obs
)

# ---- 4. Merge into one large time-aligned dataframe ----

full_data <- reduce(all_dfs, full_join, by = "datetime") %>%
  arrange(datetime)

# ---- 5. Optional: filter to your deployment window ----

full_data <- full_data %>%
  filter(datetime >= ymd_hms("2025-06-29 11:30:00"),
         datetime <= ymd_hms("2025-10-14 07:00:00"))

# ---- 6. View the final dataset ----

glimpse(full_data)
head(full_data)
