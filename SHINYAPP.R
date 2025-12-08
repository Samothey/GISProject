## ============================================================
## UPPER BROOKS – WORKING MULTI-VARIABLE DYGRAPH DASHBOARD
## Samantha Peña
## ============================================================

library(shiny)
library(dygraphs)
library(xts)
library(dplyr)

#------------------------------------------------------------------
# LOAD CLEAN DATA (RDS – preserves time and types perfectly)
#------------------------------------------------------------------
upper_brooks <- readRDS("/Users/samanthapena/Desktop/Project/Brooks_lake_2025/DATA/Upper Brooks/upper_brooks_2025_clean.rds")

# Ensure datetime exists and is POSIXct
stopifnot("datetime_UTC" %in% names(upper_brooks))
stopifnot(inherits(upper_brooks$datetime_UTC, "POSIXct"))

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
    if (input$thin) {
      df <- df[seq(1, nrow(df), 3), ]
    }
    
    df
  })
  
  #---------------------
  # Debug printing
  #---------------------
  output$debug <- renderPrint({
    cat("Selected vars:", input$vars, "\n")
    cat("Rows in filtered data:", nrow(filtered_data()), "\n")
    cat("Datetime range:", range(filtered_data()$datetime_UTC), "\n")
  })
  
  #---------------------
  # Main dygraph
  #---------------------
  output$plot <- renderDygraph({
    df <- filtered_data()
    req(nrow(df) > 1)
    req(length(input$vars) > 0)
    
    # Build xts object for selected variables
    xts_data <- xts(df[, input$vars, drop = FALSE], order.by = df$datetime_UTC)
    
    # Initial dygraph
    dg <- dygraph(xts_data, main = "Upper Brooks – Multi-Variable Time Series") %>%
      dyRangeSelector() %>%
      dyLegend(show = "always")
    
    # Add a secondary y-axis if more than one variable selected
    if (length(input$vars) > 1) {
      for (v in input$vars[-1]) {
        dg <- dg %>% dySeries(v, axis = "y2")
      }
      dg <- dg %>% dyAxis("y2", label = "Secondary Axis")
    }
    
    dg
  })
}

#------------------------------------------------------------------
shinyApp(ui, server)
