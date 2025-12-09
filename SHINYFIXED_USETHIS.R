## ============================================================
## UPPER BROOKS – MULTI-VARIABLE + QA/QC DASHBOARD (MST FIXED)
## Samantha Peña
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
  "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/upper_brooks_2025_clean.rds"
)

stopifnot("datetime_UTC" %in% names(upper_brooks))
stopifnot(inherits(upper_brooks$datetime_UTC, "POSIXct"))

# Add MST version
upper_brooks$datetime_MST <- with_tz(upper_brooks$datetime_UTC, "America/Denver")

#------------------------------------------------------------------
# AXIS GROUPS
#------------------------------------------------------------------
y1_auto <- c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "do_sat_1m","do_sat_4m",
  "par_surface","par_4m",
  "atm_pressure_raw","atm_pressure_buoy",
  "RH"
)

y2_auto <- c("wind_speed","gust_speed","wind_dir","temp_air")

#------------------------------------------------------------------
# QA/QC ENGINE (Option B with reasons)
#------------------------------------------------------------------
run_qaqc <- function(df, var) {
  
  x <- df[[var]]
  
  # Spike size thresholds
  spike_threshold <- case_when(
    grepl("temp", var)      ~ 2,
    grepl("do_mgl", var)    ~ 1,
    grepl("par", var)       ~ 200,
    grepl("wind", var)      ~ 5,
    TRUE ~ 9999
  )
  
  # Range flags
  range_flag <- case_when(
    grepl("temp", var)   & (x < -5 | x > 35)  ~ TRUE,
    grepl("do_mgl", var) & (x < 0 | x > 20)   ~ TRUE,
    grepl("do_sat", var) & (x < 0 | x > 200)  ~ TRUE,
    grepl("par", var)    & (x < 0 | x > 2500) ~ TRUE,
    grepl("wind", var)   & (x < 0 | x > 30)   ~ TRUE,
    TRUE ~ FALSE
  )
  
  # Negative impossible
  negative_flag <- ifelse(x < 0 & grepl("temp|do_mgl|par", var), TRUE, FALSE)
  
  # Spikes
  dx <- c(NA, abs(diff(x)))
  spike_flag <- dx > spike_threshold
  spike_flag[is.na(spike_flag)] <- FALSE
  
  # Flatline detection (6 pts ≈ 1 hr)
  flat_flag <- rollapply(
    x, width = 6,
    FUN = function(z) length(unique(z)) == 1,
    fill = FALSE, align = "right"
  )
  flat_flag[is.na(flat_flag)] <- FALSE
  
  # Reason text
  reason <- rep("", length(x))
  reason[range_flag]     <- paste(reason[range_flag], "Out of range", sep="; ")
  reason[negative_flag]  <- paste(reason[negative_flag], "Negative value", sep="; ")
  reason[spike_flag]     <- paste(reason[spike_flag], "Spike detected", sep="; ")
  reason[flat_flag]      <- paste(reason[flat_flag], "Flatline ≥1hr", sep="; ")
  
  reason <- gsub("^; ", "", reason)
  reason[reason == ""] <- NA
  
  ANY_FLAG <- !is.na(reason)
  
  data.frame(
    datetime_UTC  = df$datetime_UTC,
    datetime_MST  = df$datetime_MST,
    value         = x,
    RANGE         = range_flag,
    NEGATIVE      = negative_flag,
    SPIKE         = spike_flag,
    FLAT          = flat_flag,
    reason        = reason,
    ANY_FLAG      = ANY_FLAG
  )
}

#------------------------------------------------------------------
# USER INTERFACE
#------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Upper Brooks 2025 – Time-Series & QA/QC Dashboard"),
  
  tabsetPanel(
    
    #-----------------------------------------------------------
    # TAB 1 – TIME SERIES VIEWER
    #-----------------------------------------------------------
    tabPanel("Time Series Viewer",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "vars",
                   "Variables to Plot:",
                   choices = names(upper_brooks)[!names(upper_brooks) %in% c("datetime_UTC","datetime_MST")],
                   selected = c("temp_1m")
                 ),
                 checkboxInput("thin", "Plot every 3rd point", FALSE),
                 dateRangeInput(
                   "date_range",
                   "Date Range (MST):",
                   start = as.Date(min(upper_brooks$datetime_MST)),
                   end   = as.Date(max(upper_brooks$datetime_MST))
                 ),
                 verbatimTextOutput("debug")
               ),
               mainPanel(
                 dygraphOutput("plot", height = "650px")
               )
             )
    ),
    
    #-----------------------------------------------------------
    # TAB 2 – QA/QC
    #-----------------------------------------------------------
    tabPanel("QA/QC Diagnostics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("qaqc_var", "Select Variable:",
                             choices = names(upper_brooks)[!names(upper_brooks) %in% c("datetime_UTC","datetime_MST")]),
                 checkboxInput("show_flags", "Highlight flagged points", TRUE)
               ),
               mainPanel(
                 dygraphOutput("qaqc_plot", height = "500px"),
                 dygraphOutput("qaqc_zoom", height = "200px"),
                 h3("Flagged timestamps"),
                 tableOutput("flag_table")
               )
             )
    )
  )
)

#------------------------------------------------------------------
# SERVER
#------------------------------------------------------------------
server <- function(input, output, session) {
  
  #-------------------------------------------
  # Filter main data (use whole end day)
  #-------------------------------------------
  filtered_data <- reactive({
    req(input$date_range)
    
    start_dt <- as.POSIXct(input$date_range[1], tz = "America/Denver")
    # include the entire end date: +1 day - 1 second
    end_dt   <- as.POSIXct(input$date_range[2] + 1, tz = "America/Denver") - 1
    
    df <- upper_brooks %>%
      filter(datetime_MST >= start_dt,
             datetime_MST <= end_dt)
    
    if (input$thin) df <- df[seq(1, nrow(df), 3), ]
    df
  })
  
  output$debug <- renderPrint({
    cat("Date range input:", as.character(input$date_range), "\n")
    cat("Filtered range MST:", 
        format(min(filtered_data()$datetime_MST), "%Y-%m-%d %H:%M"),
        "to",
        format(max(filtered_data()$datetime_MST), "%Y-%m-%d %H:%M"), "\n")
    head(filtered_data())
  })
  
  
  #-------------------------------------------
  # MAIN TIME SERIES DYGRAPH (MST DISPLAY)
  #-------------------------------------------
  output$plot <- renderDygraph({
    df <- filtered_data()
    vars <- input$vars
    req(nrow(df) > 1, length(vars) > 0)
    
    # Index with MST (already correct in df$datetime_MST)
    dt_mst <- df$datetime_MST
    
    xts_data <- xts(df[, vars, drop = FALSE], order.by = dt_mst)
    indexTZ(xts_data) <- "America/Denver"
    
    g <- dygraph(xts_data, main = "Time Series (MST)") %>%
      dyOptions(useDataTimezone = TRUE)
    
    for (v in vars) {
      side <- ifelse(v %in% y2_auto, "y2", "y")
      g <- g %>% dySeries(v, axis = side)
    }
    
    g %>%
      dyAxis("y",  label = "Y1: Temp / DO / PAR / Pressure") %>%
      dyAxis("y2", label = "Y2: Wind / Air Temp / Wind Dir")
  })
  
  #-------------------------------------------
  # QA/QC MAIN PLOT
  #-------------------------------------------
  output$qaqc_plot <- renderDygraph({
    req(input$qaqc_var)
    v <- input$qaqc_var
    
    df <- upper_brooks
    flags <- run_qaqc(df, v)
    
    dt_mst <- as.POSIXct(df$datetime_MST, tz="America/Denver")
    
    raw_xts     <- xts(df[[v]], order.by = dt_mst)
    flagged_xts <- xts(ifelse(flags$ANY_FLAG, df[[v]], NA), order.by = dt_mst)
    
    indexTZ(raw_xts) <- "America/Denver"
    indexTZ(flagged_xts) <- "America/Denver"
    
    merged <- cbind(raw_xts, flagged_xts)
    colnames(merged) <- c("raw","flagged")
    
    g <- dygraph(merged, main = paste("QA/QC –", v)) %>%
      dyOptions(useDataTimezone = TRUE) %>%
      dySeries("raw", label = v, color = "blue") %>%
      dyRangeSelector()
    
    if (input$show_flags) {
      g <- g %>% dySeries("flagged",
                          label = "Flagged",
                          drawPoints = TRUE,
                          pointSize = 3,
                          color = "red")
    }
    
    g
  })
  
  #-------------------------------------------
  # QA/QC ZOOM WINDOW
  #-------------------------------------------
  output$qaqc_zoom <- renderDygraph({
    req(input$qaqc_var)
    
    df <- upper_brooks
    v  <- input$qaqc_var
    
    dt_mst <- as.POSIXct(df$datetime_MST, tz="America/Denver")
    xts_zoom <- xts(df[[v]], order.by = dt_mst)
    indexTZ(xts_zoom) <- "America/Denver"
    
    dygraph(xts_zoom, main = "Zoom Window") %>%
      dyOptions(useDataTimezone = TRUE)
  })
  
  #-------------------------------------------
  # FLAG TABLE
  #-------------------------------------------
  output$flag_table <- renderTable({
    req(input$qaqc_var)
    flags <- run_qaqc(upper_brooks, input$qaqc_var)
    
    flagged <- flags %>%
      filter(ANY_FLAG) %>%
      mutate(datetime_MST = format(datetime_MST, "%Y-%m-%d %H:%M")) %>%
      select(datetime_MST, value, RANGE, NEGATIVE, SPIKE, FLAT, reason)
    
    if (nrow(flagged) == 0)
      return(data.frame(Message = "No flags detected."))
    
    flagged
  })
}

#------------------------------------------------------------------
shinyApp(ui, server)
