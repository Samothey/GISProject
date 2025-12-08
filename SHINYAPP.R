## ============================================================
## UPPER BROOKS – QA/QC + MULTI-VARIABLE VISUALIZATION DASHBOARD
## Samantha Peña
## ============================================================

library(shiny)
library(dygraphs)
library(xts)
library(dplyr)
library(lubridate)

## -----------------------------
## 1. Load + Prepare Data
## -----------------------------

data_path <- "/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/UpperBrooks_2025_merged_clean.csv"

upper_brooks <- read.csv(data_path) %>%
  mutate(
    datetime_UTC = as.POSIXct(datetime_UTC, tz = "UTC"),
    datetime_MST = as.POSIXct(datetime_MST, tz = "America/Denver"),
    dt = datetime_UTC               # ensures dygraphs aligns cleanly (NO TEETH)
  )

## -----------------------------
## Identify numeric variables to plot
## -----------------------------
num_vars <- names(upper_brooks)[sapply(upper_brooks, is.numeric)]

## Label dictionary
var_labels <- c(
  temp_1m = "Temp 1 m (°C)",
  temp_2m = "Temp 2 m (°C)",
  temp_3m = "Temp 3 m (°C)",
  temp_4m = "Temp 4 m (°C)",
  do_mgl_1m = "DO 1 m (mg/L)",
  do_sat_1m = "DO Sat 1 m (%)",
  do_mgl_4m = "DO 4 m (mg/L)",
  do_sat_4m = "DO Sat 4 m (%)",
  temp_air = "Air Temp (°C)",
  wind_speed = "Wind Speed (m/s)",
  gust_speed = "Gust Speed (m/s)",
  wind_dir = "Wind Direction (°)",
  atm_pressure_buoy = "Atm Pressure (Buoy-Corrected)"
  
)

## Keep only labels for variables that exist
var_labels <- var_labels[names(var_labels) %in% num_vars]

## -----------------------------
## 2. UI
## -----------------------------
ui <- fluidPage(
  
  titlePanel("Upper Brooks 2025 – QA/QC Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Visualization Controls"),
      
      ## MULTI VARIABLE SELECTION
      selectizeInput(
        "vars", "Select variables to plot:",
        choices = setNames(names(var_labels), var_labels),
        selected = c("temp_1m", "temp_2m"),
        multiple = TRUE,
        options = list(maxItems = 12)
      ),
      
      ## DATE RANGE SELECTION
      dateRangeInput(
        "date_range", "Date range (UTC):",
        start = as.Date(min(upper_brooks$datetime_UTC)),
        end   = as.Date(max(upper_brooks$datetime_UTC)),
        min   = as.Date(min(upper_brooks$datetime_UTC)),
        max   = as.Date(max(upper_brooks$datetime_UTC))
      ),
      
      ## DATA THINNING
      checkboxInput(
        "thin_data", "Use every 3rd point (faster plotting)", value = TRUE
      ),
      
      hr(),
      h4("QC Options (single-variable)"),
      selectInput(
        "qc_var", "Choose variable for QC:",
        choices = setNames(names(var_labels), var_labels),
        selected = "temp_1m"
      ),
      numericInput(
        "spike_thresh", "Spike threshold (absolute change):",
        value = 2, min = 0, step = 0.25
      ),
      
      hr(),
      helpText("Use the multi-variable plot to explore patterns, and the QC tab to inspect anomalies.")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("📈 Multi-Variable Plot",
                 br(),
                 dygraphOutput("multi_plot", height = "450px")
        ),
        
        tabPanel("🔍 QC – Flag Summary",
                 br(),
                 verbatimTextOutput("flag_summary"),
                 br(),
                 dataTableOutput("flag_table")
        ),
        
        tabPanel("ℹ️ Metadata",
                 br(),
                 verbatimTextOutput("meta_info")
        )
      )
    )
  )
)

## -----------------------------
## 3. SERVER
## -----------------------------
server <- function(input, output, session) {
  
  ## ---- Filter Data Range ----
  filtered_data <- reactive({
    df <- upper_brooks %>%
      filter(datetime_UTC >= as.POSIXct(input$date_range[1], tz = "UTC"),
             datetime_UTC <= as.POSIXct(input$date_range[2] + 1, tz = "UTC"))
    
    checkboxInput(
      "thin_data",
      "Use every 3rd point (faster plotting)",
      value = FALSE   # <-- now default is “plot all data”
    )
    
    
  })
  
  ## -------------------------------------------------------
  ## MULTI-VARIABLE DYGRAPH PLOT  (Jordan-style multi-line)
  ## -------------------------------------------------------
  output$multi_plot <- renderDygraph({
    df <- filtered_data()
    vars <- input$vars
    req(length(vars) > 0)
    req(nrow(df) > 0)
    
    xts_data <- xts(df[, vars], order.by = df$dt)
    
    dygraph(xts_data, main = "Upper Brooks – Multi-Variable Time Series") %>%
      dyRangeSelector() %>%
      dyLegend(show = "follow", width = 350) %>%
      dyOptions(drawGrid = TRUE)
  })
  
  ## -------------------------------------------------------
  ## QC FLAGGING (single variable)
  ## -------------------------------------------------------
  flagged_points <- reactive({
    df <- filtered_data()
    var <- input$qc_var
    
    vals <- df[[var]]
    lag_vals <- dplyr::lag(vals)
    
    tibble(
      datetime_UTC = df$datetime_UTC,
      value        = vals,
      negative     = vals < 0,
      spike        = abs(vals - lag_vals) > input$spike_thresh,
      zero_value   = vals == 0,
      is_na        = is.na(vals)
    ) %>%
      mutate(any_flag = negative | spike | zero_value | is_na) %>%
      filter(any_flag)
  })
  
  ## Flag Summary Output
  output$flag_summary <- renderPrint({
    df <- filtered_data()
    flags <- flagged_points()
    var <- input$qc_var
    
    cat("QC on variable:", var_labels[var], "\n")
    cat("Date range:", as.character(min(df$datetime_UTC)),
        "→", as.character(max(df$datetime_UTC)), "\n\n")
    
    cat("Total points inspected:", nrow(df), "\n")
    cat("Flag rules:\n",
        "- Value < 0\n",
        "- |Δ between timestamps| >", input$spike_thresh, "\n",
        "- Value = 0\n",
        "- NA\n\n")
    
    if (nrow(flags) == 0) {
      cat("No flagged points detected.\n")
    } else {
      cat("Flagged points:", nrow(flags), "\n")
      print(colSums(flags[, c("negative", "spike", "zero_value", "is_na")]))
    }
  })
  
  ## Table of flagged points
  output$flag_table <- renderDataTable({
    flags <- flagged_points()
    if (nrow(flags) == 0) return(NULL)
    flags %>% arrange(datetime_UTC)
  })
  
  ## -------------------------------------------------------
  ## Metadata Panel
  ## -------------------------------------------------------
  output$meta_info <- renderPrint({
    cat("Upper Brooks 2025 – Metadata\n\n")
    
    cat("Datetime (UTC) range:\n")
    print(range(upper_brooks$datetime_UTC, na.rm = TRUE))
    
    cat("\nVariables available:\n")
    print(var_labels)
    
    cat("\nTotal rows:", nrow(upper_brooks), "\n")
  })
}

## -----------------------------
## 4. Run the App
## -----------------------------
shinyApp(ui, server)
