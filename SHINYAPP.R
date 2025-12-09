## ============================================================
## UPPER BROOKS – WORKING MULTI-VARIABLE DYGRAPH DASHBOARD
## Samantha Peña
## ============================================================

library(shiny)
library(dygraphs)
library(xts)
library(dplyr)
library(lubridate)

#------------------------------------------------------------------
# LOAD CLEAN DATA (RDS – preserves time and types perfectly)
#------------------------------------------------------------------
upper_brooks <- readRDS("/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/upper_brooks_2025_clean.rds")

# Ensure datetime exists and is POSIXct
stopifnot("datetime_UTC" %in% names(upper_brooks))
stopifnot(inherits(upper_brooks$datetime_UTC, "POSIXct"))

#------------------------------------------------------------------
# SMART AXIS RULES
#------------------------------------------------------------------

y1_auto <- c(
  "temp_1m","temp_2m","temp_3m","temp_4m",
  "do_mgl_1m","do_mgl_4m",
  "do_sat_1m","do_sat_4m",
  "par_surface","par_4m",
  "atm_pressure_raw","atm_pressure_buoy",
  "RH"
)

y2_auto <- c(
  "wind_speed","gust_speed","wind_dir","temp_air"
)


#------------------------------------------------------------------
# USER INTERFACE
#------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Upper Brooks 2025 – Time-Series Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select variables to plot"),
      
      checkboxGroupInput(
        "vars",
        "Variables:",
        choices = names(upper_brooks)[!names(upper_brooks) %in% c("datetime_UTC", "datetime_MST")],
        selected = c("temp_1m")
      ),
      
      checkboxInput("thin", "Plot every 3rd point (faster)", FALSE),
      
      dateRangeInput(
        "date_range",
        "Date Range:",
        start = min(upper_brooks$datetime_UTC),
        end   = max(upper_brooks$datetime_UTC)
      ),
      
      verbatimTextOutput("debug")
    ),
    
    mainPanel(
      dygraphOutput("plot", height = "650px")
    )
  )
)

#------------------------------------------------------------------
# SERVER LOGIC
#------------------------------------------------------------------
server <- function(input, output, session) {
  
  #---------------------
  # Reactive filtered data
  #---------------------
  filtered_data <- reactive({
    req(input$date_range)
    
    df <- upper_brooks %>%
      filter(
        datetime_UTC >= input$date_range[1],
        datetime_UTC <= input$date_range[2]
      )
    
    # thinning if selected
    if (input$thin) { df <- df[seq(1, nrow(df), 3), ]
    }
    
    df
  })
  
  #Debug info panel
  #-----------------------------------------
  output$debug <- renderPrint({
    cat("Selected vars:", input$vars, "\n")
    cat("Rows:", nrow(filtered_data()), "\n")
    print(head(filtered_data()))
  })
  
  #-----------------------------------------
  # Main dygraph
  #-----------------------------------------
  output$plot <- renderDygraph({
    df <- filtered_data()
    vars <- input$vars
    
    req(nrow(df) > 1)
    req(length(vars) > 0)
    
    xts_data <- xts(df[, vars, drop = FALSE], order.by = df$datetime_UTC)
    
    g <- dygraph(xts_data, main = "Upper Brooks – Multi-Variable Time Series") %>%
      dyRangeSelector() %>%
      dyLegend(show = "always")
    
    #-----------------------------------------
    # SMART AXIS ASSIGNMENT
    #-----------------------------------------
    for (v in vars) {
      
      axis_side <- ifelse(
        v %in% y2_auto, "y2",  # wind & related vars
        "y"                    # all DO, temp, PAR, pressure → Y1
      )
      
      g <- g %>% dySeries(v, axis = axis_side)
    }
    
    g <- g %>%
      dyAxis("y",  label = "Y1: Temp / DO / PAR / Pressure") %>%
      dyAxis("y2", label = "Y2: Wind / Air Temp / Wind Dir")
    
    g
  })
}

#------------------------------------------------------------------
shinyApp(ui, server)