library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

# ---------------------------------------------------------------------------
# 1. Retrieve all datastream metadata for the Upper Brooks buoy (Thing 321)
# ---------------------------------------------------------------------------

thing_url <- "https://wyseason.uwyo.edu/sta/v1.1/Things(321)/Datastreams"

ds_raw <- request(thing_url) |> 
  req_perform() |> 
  resp_body_string() |> 
  fromJSON(flatten = TRUE)

datastreams <- ds_raw$value

# The parameterName field is stored in the flattened column "properties.parameterName"
# Create a simpler name for it so map2() can use it later.
datastreams$param <- datastreams$`properties.parameterName`

# Quick look at what datastreams exist (names, IDs, parameter names)
datastreams %>% select(name, `@iot.id`, param)


# ---------------------------------------------------------------------------
# 2. Function to download ALL observations for one datastream
#    (Includes automatic pagination using @iot.nextLink)
# ---------------------------------------------------------------------------

get_obs <- function(ds_id, param_name) {
  
  # Base URL for this datastream
  base_url <- paste0(
    "https://wyseason.uwyo.edu/sta/v1.1/Datastreams(",
    ds_id,
    ")/Observations"
  )
  
  # Start by requesting everything ordered by time.
  # NOTE: The space before 'asc' must be URL-encoded as %20.
  next_url <- paste0(base_url, "?$orderby=phenomenonTime%20asc")
  
  pages <- list()   # store each page of data here
  page <- 1
  
  repeat {
    # Print the URL being requested (helpful for debugging)
    print(paste("Requesting:", next_url))
    
    # Perform the request and parse JSON
    raw <- request(next_url) |> 
      req_perform() |> 
      resp_body_string() |> 
      fromJSON(flatten = TRUE)
    
    # Store the results for this page
    pages[[page]] <- raw$value
    
    # Stop if there is no nextLink (meaning we reached the end)
    if (is.null(raw[["@iot.nextLink"]])) break
    
    # ---------------------------------------------------------
    # Extract $skip value from the nextLink
    # SensorThings returns something like:
    #   ...Observations?$skip=10000&$orderby=phenomenonTime...
    # We extract the 10000 (or 20000, etc.)
    # ---------------------------------------------------------
    skip_val <- sub(".*\\$skip=([0-9]+).*", "\\1", raw[["@iot.nextLink"]])
    
    # Build the next request URL cleanly
    next_url <- paste0(
      base_url,
      "?$skip=", skip_val,
      "&$orderby=phenomenonTime%20asc"
    )
    
    page <- page + 1
  }
  
  # Combine all pages and convert into a clean tibble
  df <- bind_rows(pages) %>%
    transmute(
      datetime = ymd_hms(phenomenonTime),        # parse timestamps
      !! param_name := as.numeric(result)        # name column after parameter
    )
  
  return(df)
}


# ---------------------------------------------------------------------------
# 3. Download every datastream for the buoy automatically
# ---------------------------------------------------------------------------

all_dfs <- map2(
  datastreams$`@iot.id`,   # vector of datastream IDs
  datastreams$param,       # vector of parameter names
  get_obs                  # function that downloads each dataset
)


# ---------------------------------------------------------------------------
# 4. Merge all datastreams into one time-aligned dataset
# ---------------------------------------------------------------------------

full_data <- reduce(all_dfs, full_join, by = "datetime") %>%
  arrange(datetime)


# ---------------------------------------------------------------------------
# 5. Filter to the known deployment window (optional)
# ---------------------------------------------------------------------------

full_data <- full_data %>%
  filter(datetime >= ymd_hms("2025-06-29 11:30:00"),
         datetime <= ymd_hms("2025-10-14 07:00:00"))


# ---------------------------------------------------------------------------
# 6. Inspect final results
# ---------------------------------------------------------------------------

glimpse(full_data)
head(full_data)

# Check that we retrieved all timestamps
max(full_data$datetime, na.rm = TRUE)
min(full_data$datetime, na.rm = TRUE)

all.equal(full_data$do_mgl_1m, full_data$do_mgl_1m)
all.equal(full_data$do_sat_1m, full_data$do_sat_1m)

view(full_data)
