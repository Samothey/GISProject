## ============================================================
## UPPER BROOKS – MULTI-VARIABLE + QA/QC DASHBOARD (FINAL VERSION)
## Includes DO bubble logic, DO_sat rules, temp2/3 flatline disable,
## Metadata & Help tab
## ============================================================

library(shiny)
library(dygraphs)
library(xts)
library(dplyr)
library(lubridate)
library(zoo)

#------------------------------------------------------------------
# LOAD CLEAN DATA
#------------------------------------------------------------------
upper_brooks <- readRDS(
  "/Users/samanthapena/Desktop/GISProject/upper_brooks_2025_clean.rds"
)

stopifnot("datetime_UTC" %in% names(upper_brooks))
stopifnot(inherits(upper_brooks$datetime_UTC, "POSIXct"))

upper_brooks$datetime_MST <- with_tz(upper_brooks$datetime_UTC, "America/Denver")

#------------------------------------------------------------------
# CONSTANTS & METADATA
#------------------------------------------------------------------
site_lat  <- 43.791909
site_lon  <- -110.010259
deployment_start    <- ymd_hms("2025-06-29 00:00:00", tz="America/Denver")
sensor_removal_time <- ymd_hms("2025-10-14 13:30:00", tz="America/Denver")

sensor_vars <- setdiff(names(upper_brooks), c("datetime_UTC","datetime_MST"))

y1_auto <- c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "do_sat_1m","do_sat_4m",
  "par_surface","par_4m",
  "atm_pressure_raw","atm_pressure_buoy","RH"
)

y2_auto <- c("wind_speed","gust_speed","wind_dir","temp_air")

#------------------------------------------------------------------
# COLORS
#------------------------------------------------------------------
variable_colors <- setNames(
  hcl.colors(length(sensor_vars), palette="Dark2"),
  sensor_vars
)

series_color <- function(v) variable_colors[[v]]

#------------------------------------------------------------------
# NIGHT DETECTOR
#------------------------------------------------------------------
is_night <- function(dt_mst) {
  h <- hour(dt_mst)
  h < 6 | h > 21
}

#------------------------------------------------------------------
# DO BUBBLE ARTIFACT DETECTORS
#   - Separate functions for DO mg/L and DO saturation
#------------------------------------------------------------------

# DO mg/L: strong crash behavior (bubble hits)
flag_DO_bubbles_mgl <- function(
    x,
    drop_threshold   = 2,   # sudden drop > 2 mg/L
    recovery_window  = 1,   # must recover to within ±1 mg/L of baseline
    low_DO           = 3,   # anything < 3 mg/L treated as bubble floor
    max_forward_pts  = 18   # up to ~3 hours (10-min data)
){
  n    <- length(x)
  flag <- rep(FALSE, n)
  dx   <- c(NA, diff(x))
  
  crash_idx <- which(dx < -drop_threshold & !is.na(dx))
  
  if (length(crash_idx) == 0) {
    flag[!is.na(x) & x < low_DO] <- TRUE
    return(flag)
  }
  
  for (i in crash_idx) {
    baseline_idx <- i - 1
    if (baseline_idx < 1) next
    
    baseline <- x[baseline_idx]
    if (is.na(baseline)) next
    
    # flag crash point
    flag[i] <- TRUE
    
    # Forward: remove recovery arc until near baseline
    j     <- i + 1
    steps <- 0
    while (j <= n && steps < max_forward_pts) {
      flag[j] <- TRUE
      
      if (!is.na(x[j]) && abs(x[j] - baseline) <= recovery_window) {
        break
      }
      j     <- j + 1
      steps <- steps + 1
    }
    
    # Backwards: remove any pre-crash dips
    k <- baseline_idx
    while (k >= 1 && !is.na(x[k]) && x[k] < (baseline - 0.5)) {
      flag[k] <- TRUE
      k <- k - 1
    }
  }
  
  # Anything < low_DO also flagged
  flag[!is.na(x) & x < low_DO] <- TRUE
  
  flag
}

# DO saturation: percent crash behavior, tuned separately
flag_DO_bubbles_sat <- function(
    x,
    drop_threshold   = 40,   # sudden drop > 40 %sat
    recovery_window  = 10,   # must recover to within ±10 % of baseline
    max_forward_pts  = 18    # up to ~3 hours
){
  n    <- length(x)
  flag <- rep(FALSE, n)
  dx   <- c(NA, diff(x))
  
  crash_idx <- which(dx < -drop_threshold & !is.na(dx))
  
  if (length(crash_idx) == 0) {
    return(flag)
  }
  
  for (i in crash_idx) {
    baseline_idx <- i - 1
    if (baseline_idx < 1) next
    
    baseline <- x[baseline_idx]
    if (is.na(baseline)) next
    
    flag[i] <- TRUE
    
    # Forward: remove recovery arc until near baseline
    j     <- i + 1
    steps <- 0
    while (j <= n && steps < max_forward_pts) {
      flag[j] <- TRUE
      
      if (!is.na(x[j]) && abs(x[j] - baseline) <= recovery_window) {
        break
      }
      j     <- j + 1
      steps <- steps + 1
    }
    
    # Backwards: remove any deep dips just before crash
    k <- baseline_idx
    while (k >= 1 && !is.na(x[k]) && (baseline - x[k]) > 5) {
      flag[k] <- TRUE
      k <- k - 1
    }
  }
  
  flag
}

#------------------------------------------------------------------
# MAIN QA/QC ENGINE
#------------------------------------------------------------------
run_qaqc_var <- function(df, var) {
  x <- df[[var]]
  n <- length(x)
  
  # Spike thresholds for instantaneous 10-min changes
  spike_threshold <- case_when(
    grepl("temp", var)       ~ 2,
    grepl("do_mgl", var)     ~ 2,
    grepl("do_sat", var)     ~ 250,   # big tolerance for sat spikes (bloom-driven)
    grepl("par", var)        ~ 300,
    grepl("wind", var)       ~ 5,
    TRUE ~ 9999
  )
  
  # Range checks
  range_flag <- case_when(
    grepl("temp", var)       & (x < -5 | x > 35)   ~ TRUE,
    grepl("do_mgl", var)     & (x < 0  | x > 20)   ~ TRUE,
    grepl("do_sat", var)     & (x < 0  | x > 300)  ~ TRUE,
    grepl("par", var)        & (x < 0  | x > 2500) ~ TRUE,
    grepl("wind_speed", var) & (x < 0  | x > 30)   ~ TRUE,
    grepl("wind_dir", var)   & (x < 1  | x > 360)  ~ TRUE,
    TRUE ~ FALSE
  )
  
  negative_flag <- ifelse(x < 0 & grepl("temp|do_mgl|par", var), TRUE, FALSE)
  
  # Spikes = |Δx| > threshold
  dx <- c(NA, diff(x))
  spike_flag <- abs(dx) > spike_threshold
  spike_flag[is.na(spike_flag)] <- FALSE
  
  do_spike <- if (grepl("do_", var)) spike_flag else rep(FALSE, n)
  
  #----------------------------------------------------
  # FLATLINES (ignore temp_2m & temp_3m)
  #----------------------------------------------------
  flat_flag <- rep(FALSE, n)
  
  if (var %in% c("temp_2m","temp_3m")) {
    flat_flag <- rep(FALSE, n)   # interpolated depths, ignore
    
  } else if (grepl("temp", var) && n >= 18) {
    flat_flag <- rollapply(
      x, 18,
      function(z) sd(z, na.rm = TRUE) < 0.001,
      fill = FALSE, align = "right"
    )
    
  } else if (!grepl("temp", var) && n >= 6) {
    flat_flag <- rollapply(
      x, 6,
      function(z) length(unique(z)) == 1,
      fill = FALSE, align = "right"
    )
  }
  
  flat_flag[is.na(flat_flag)] <- FALSE
  
  # After-removal flags
  removal_flag <- df$datetime_MST > sensor_removal_time
  
  # PAR at night
  par_night_flag <- rep(FALSE, n)
  if (grepl("par", var)) {
    par_night_flag <- is_night(df$datetime_MST) & !is.na(x) & x > 10
  }
  
  #----------------------------------------------------
  # DO bubble artifact detection (using the new functions)
  #----------------------------------------------------
  bubble_flag <- rep(FALSE, n)
  
  if (grepl("do_mgl", var)) {
    bubble_flag <- flag_DO_bubbles_mgl(
      x,
      drop_threshold   = 2,
      recovery_window  = 1,
      low_DO           = 3,
      max_forward_pts  = 18
    )
  } else if (grepl("do_sat", var)) {
    bubble_flag <- flag_DO_bubbles_sat(
      x,
      drop_threshold   = 40,
      recovery_window  = 10,
      max_forward_pts  = 18
    )
  }
  
  #----------------------------------------------------
  # Combine into reason strings
  #----------------------------------------------------
  reason <- rep(NA_character_, n)
  
  add_reason <- function(cond, msg){
    idx <- which(cond)
    if (length(idx) > 0) {
      reason[idx] <<- ifelse(
        is.na(reason[idx]),
        msg,
        paste(reason[idx], msg, sep = "; ")
      )
    }
  }
  
  add_reason(range_flag,     "Out of range")
  add_reason(negative_flag,  "Negative value")
  add_reason(spike_flag,     "Spike > threshold")
  add_reason(flat_flag,      "Flatline")
  add_reason(removal_flag,   "After removal")
  add_reason(do_spike,       "DO sudden change")
  add_reason(par_night_flag, "PAR at night")
  add_reason(bubble_flag,    "DO bubble artifact")
  
  ANY_FLAG <- !is.na(reason)
  
  data.frame(
    datetime_UTC   = df$datetime_UTC,
    datetime_MST   = df$datetime_MST,
    value          = x,
    RANGE          = range_flag,
    NEGATIVE       = negative_flag,
    SPIKE          = spike_flag,
    FLAT           = flat_flag,
    REMOVAL        = removal_flag,
    DO_SPIKE       = do_spike,
    BUBBLE         = bubble_flag,
    PAR_NIGHT      = par_night_flag,
    reason         = reason,
    ANY_FLAG       = ANY_FLAG
  )
}

#------------------------------------------------------------------
# PRECOMPUTE QA/QC
#------------------------------------------------------------------
qaqc_results <- lapply(sensor_vars, function(v) run_qaqc_var(upper_brooks, v))
names(qaqc_results) <- sensor_vars

global_any_flag <- Reduce("|", lapply(qaqc_results, `[[`, "ANY_FLAG"))
upper_brooks_cleaned_all <- upper_brooks[!global_any_flag, ]

#------------------------------------------------------------------
# UI
#------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Upper Brooks 2025 – Time-Series & QA/QC Dashboard"),
  
  tabsetPanel(
    
    #-----------------------------------------------------------
    # TAB 1 – TIME SERIES VIEWER
    #-----------------------------------------------------------
    tabPanel(
      "Time Series Viewer",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("vars","Variables to Plot:",
                             choices = sensor_vars, selected = "temp_1m"),
          checkboxInput("thin","Plot every 3rd point",FALSE),
          checkboxInput("hide_removed","Hide data after removal time",TRUE),
          checkboxInput("show_clean_only","Show cleaned only",FALSE),
          checkboxInput("show_xzoom","Enable x-axis zoom",TRUE),
          dateRangeInput("date_range","Date range (MST):",
                         start = as.Date(min(upper_brooks$datetime_MST)),
                         end   = as.Date(max(upper_brooks$datetime_MST))),
          verbatimTextOutput("debug")
        ),
        mainPanel(
          dygraphOutput("plot",height="550px"),
          hr(),
          h3("Zoom Viewer"),
          dygraphOutput("plot_zoom",height="250px")
        )
      )
    ),
    
    #-----------------------------------------------------------
    # TAB 2 – QA/QC DIAGNOSTICS
    #-----------------------------------------------------------
    tabPanel(
      "QA/QC Diagnostics",
      sidebarLayout(
        sidebarPanel(
          selectInput("qaqc_var","Variable:",choices = sensor_vars),
          checkboxInput("show_flags","Highlight flagged",TRUE),
          br(),
          downloadButton("download_flags","Download flagged CSV"),
          br(), br(),
          downloadButton("download_clean","Download cleaned data")
        ),
        mainPanel(
          dygraphOutput("qaqc_plot",height="400px"),
          dygraphOutput("qaqc_zoom",height="200px"),
          h3("Flagged timestamps"),
          tableOutput("flag_table"),
          hr(),
          h3("Flag summary by variable"),
          tableOutput("qc_summary")
        )
      )
    ),
    
    #-----------------------------------------------------------
    # TAB 3 – METADATA & HELP
    #-----------------------------------------------------------
    tabPanel(
      "Metadata & Help",
      fluidRow(
        column(
          width=6,
          h3("Site Metadata"),
          tableOutput("meta_table")
        ),
        column(
          width=6,
          h3("QA/QC Rules (Summary)"),
          verbatimTextOutput("qaqc_help")
        )
      )
    )
  )
)

#------------------------------------------------------------------
# SERVER
#------------------------------------------------------------------
server <- function(input, output, session) {
  
  #------------------------------
  # Filtered dataset
  #------------------------------
  filtered_data <- reactive({
    req(input$date_range)
    
    df <- upper_brooks
    
    start_dt <- as.POSIXct(input$date_range[1], tz="America/Denver")
    end_dt   <- as.POSIXct(input$date_range[2] + 1, tz="America/Denver") - 1
    
    if (isTRUE(input$show_clean_only)) df <- df[!global_any_flag, ]
    if (isTRUE(input$hide_removed)) df <- df %>% filter(datetime_MST <= sensor_removal_time)
    
    df <- df %>% filter(datetime_MST >= start_dt, datetime_MST <= end_dt)
    
    if (input$thin && nrow(df) > 0) df <- df[seq(1, nrow(df), 3),]
    
    df
  })
  
  output$debug <- renderPrint({
    df <- filtered_data()
    cat("Rows:", nrow(df), "\n")
    if (nrow(df)>0){
      cat("Range:",
          format(min(df$datetime_MST),"%Y-%m-%d %H:%M"), "to",
          format(max(df$datetime_MST),"%Y-%m-%d %H:%M"), "\n")
    }
    head(df)
  })
  
  #------------------------------
  # TIME SERIES VIEWER
  #------------------------------
  output$plot <- renderDygraph({
    df <- filtered_data()
    vars <- input$vars
    req(nrow(df)>1, length(vars)>0)
    
    xts_data <- xts(df[,vars,drop=FALSE], order.by=df$datetime_MST)
    indexTZ(xts_data) <- "America/Denver"
    
    g <- dygraph(xts_data, main="Time Series (MST)") %>% 
      dyOptions(useDataTimezone=TRUE)
    
    for (v in vars) {
      g <- g %>% dySeries(v,
                          axis  = ifelse(v %in% y2_auto,"y2","y"),
                          color = series_color(v))
    }
    
    if (isTRUE(input$show_xzoom)) g <- g %>% dyRangeSelector()
    
    dfmin <- min(df$datetime_MST, na.rm=TRUE)
    dfmax <- max(df$datetime_MST, na.rm=TRUE)
    
    if (sensor_removal_time >= dfmin & sensor_removal_time <= dfmax) {
      g <- g %>% 
        dyEvent(sensor_removal_time,"Sensor removed") %>%
        dyShading(from=sensor_removal_time, to=dfmax, color="#ffeeee")
    }
    g
  })
  
  # Zoom viewer
  output$plot_zoom <- renderDygraph({
    df <- filtered_data()
    vars <- input$vars
    req(nrow(df)>1, length(vars)>0)
    
    xts_zoom <- xts(df[,vars,drop=FALSE], order.by=df$datetime_MST)
    indexTZ(xts_zoom) <- "America/Denver"
    
    dygraph(xts_zoom, main="Zoom Window") %>% 
      dyOptions(useDataTimezone=TRUE)
  })
  
  #------------------------------
  # QA/QC PLOTS
  #------------------------------
  output$qaqc_plot <- renderDygraph({
    v <- input$qaqc_var
    df <- upper_brooks
    flags <- qaqc_results[[v]]
    
    xts_raw  <- xts(df[[v]], order.by=df$datetime_MST)
    xts_flag <- xts(ifelse(flags$ANY_FLAG, df[[v]], NA), order.by=df$datetime_MST)
    
    merged <- cbind(xts_raw, xts_flag)
    colnames(merged) <- c("raw","flagged")
    
    g <- dygraph(merged, main=paste("QA/QC –",v)) %>%
      dySeries("raw", color=series_color(v)) %>%
      dyRangeSelector()
    
    if (isTRUE(input$show_flags)) {
      g <- g %>% dySeries("flagged", drawPoints=TRUE, pointSize=3, color="red")
    }
    
    g
  })
  
  output$qaqc_zoom <- renderDygraph({
    v <- input$qaqc_var
    xts_zoom <- xts(upper_brooks[[v]], order.by=upper_brooks$datetime_MST)
    
    dygraph(xts_zoom, main="Zoom Window") %>%
      dyOptions(useDataTimezone=TRUE)
  })
  
  # Flag table
  output$flag_table <- renderTable({
    v <- input$qaqc_var
    flags <- qaqc_results[[v]]
    
    flagged <- flags %>%
      filter(ANY_FLAG) %>%
      mutate(datetime_MST=format(datetime_MST,"%Y-%m-%d %H:%M")) %>%
      select(datetime_MST, value, RANGE, NEGATIVE, SPIKE, FLAT,
             DO_SPIKE, BUBBLE, PAR_NIGHT, REMOVAL, reason)
    
    if (nrow(flagged)==0) return(data.frame(Message="No flags detected."))
    flagged
  })
  
  # Summary
  output$qc_summary <- renderTable({
    tibble(
      Variable = sensor_vars,
      Flags    = sapply(sensor_vars, function(v) sum(qaqc_results[[v]]$ANY_FLAG))
    )
  })
  
  # Downloads
  output$download_flags <- downloadHandler(
    filename=function(){ paste0("UpperBrooks_flags_",Sys.Date(),".csv") },
    content=function(file){
      flagged <- bind_rows(
        lapply(sensor_vars, function(v){
          qaqc_results[[v]] %>% filter(ANY_FLAG) %>% mutate(variable=v)
        })
      )
      write.csv(flagged, file, row.names=FALSE)
    }
  )
  
  output$download_clean <- downloadHandler(
    filename=function(){ paste0("UpperBrooks_clean_",Sys.Date(),".csv") },
    content=function(file){
      write.csv(upper_brooks_cleaned_all, file, row.names=FALSE)
    }
  )
  
  #-----------------------------------------------------------
  # METADATA TAB
  #-----------------------------------------------------------
  output$meta_table <- renderTable({
    data.frame(
      Field=c(
        "Site",
        "Latitude",
        "Longitude",
        "Weather Station Start (Analysis Start)",
        "Buoy Deployment",
        "Sensor Removal Time",
        "Temperature Depths (m)",
        "DO Depths (m)",
        "PAR Depths (m)"
      ),
      Value=c(
        "Upper Brooks Lake, WY",
        site_lat,
        site_lon,
        "2025-06-29",
        "2025-06-24 10:22",
        format(sensor_removal_time,"%Y-%m-%d %H:%M"),
        "1,2,3,4",
        "1,4",
        "1,4"
      )
    )
  })
  
  output$qaqc_help <- renderText({
    "Summary of QA/QC Rules:

1) Range checks:
   Temp:     -5 to 35 °C
   DO mg/L:  0–20 mg/L
   DO sat:   0–300 %
   PAR:      0–2500 μmol m^-2 s^-1
   Wind:     0–30 m/s
   Wind dir: 1–360 °

2) Spikes (10-min changes):
   Temp:     > 2 °C
   DO mg/L:  > 2 mg/L
   DO sat:   > 250 % change
   PAR:      > 300
   Wind:     > 5 m/s

3) Flatlines:
   Temp_2m & Temp_3m: flatlines ignored (interpolated)
   Other temps: ≥ 3 hours
   Other sensors: ≥ 1 hour

4) DO bubble artifacts:
   DO mg/L:
     - Crash > 2 mg/L
     - Recovery points removed until within ±1 mg/L of baseline
     - DO < 3 mg/L always flagged as artifact
   DO sat:
     - Crash > 40 % saturation between points
     - Recovery removed until within ±10 % of baseline

5) Nighttime PAR:
   PAR > 10 during local night (before 06:00 or after 21:00) flagged.

6) All data after 2025-10-14 13:30 MST flagged as 'After removal'.
"
  })
}

#------------------------------------------------------------------
shinyApp(ui, server)
