## ============================================================
## UPPER BROOKS – MULTI-VARIABLE + QA/QC + CLEANED DATA DASHBOARD
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

upper_brooks$datetime_MST <- with_tz(upper_brooks$datetime_UTC, "America/Denver")

#------------------------------------------------------------------
# QA/QC ENGINE (with reasons)
#------------------------------------------------------------------
run_qaqc <- function(df, var) {
  x <- df[[var]]
  
  spike_threshold <- case_when(
    grepl("temp", var)      ~ 2,
    grepl("do_mgl", var)    ~ 1,
    grepl("par", var)       ~ 200,
    grepl("wind", var)      ~ 5,
    TRUE ~ 9999
  )
  
  range_flag <- case_when(
    grepl("temp", var)   & (x < -5 | x > 35)  ~ TRUE,
    grepl("do_mgl", var) & (x < 0 | x > 20)   ~ TRUE,
    grepl("do_sat", var) & (x < 0 | x > 200)  ~ TRUE,
    grepl("par", var)    & (x < 0 | x > 2500) ~ TRUE,
    grepl("wind", var)   & (x < 0 | x > 30)   ~ TRUE,
    TRUE ~ FALSE
  )
  
  negative_flag <- ifelse(x < 0 & grepl("temp|do_mgl|par", var), TRUE, FALSE)
  
  dx <- c(NA, abs(diff(x)))
  spike_flag <- dx > spike_threshold
  spike_flag[is.na(spike_flag)] <- FALSE
  
  flat_flag <- rollapply(
    x, width = 6,
    FUN = function(z) length(unique(z)) == 1,
    fill = FALSE, align = "right"
  )
  flat_flag[is.na(flat_flag)] <- FALSE
  
  reason <- rep("", length(x))
  reason[range_flag]    <- paste(reason[range_flag], "Out of range", sep="; ")
  reason[negative_flag] <- paste(reason[negative_flag], "Negative value", sep="; ")
  reason[spike_flag]    <- paste(reason[spike_flag], "Spike detected", sep="; ")
  reason[flat_flag]     <- paste(reason[flat_flag], "Flatline ≥1hr", sep="; ")
  
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
# UI
#------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Upper Brooks 2025 – Time-Series, QA/QC, Cleaned Data"),
  
  tabsetPanel(
    
    #====================================================
    # TAB 1 — TIME SERIES VIEWER
    #====================================================
    tabPanel("Time Series Viewer",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput(
                   "vars", "Variables:",
                   choices = names(upper_brooks)[!names(upper_brooks) %in% c("datetime_UTC","datetime_MST")],
                   selected = "temp_1m"
                 ),
                 checkboxInput("thin", "Plot every 3rd point", FALSE),
                 dateRangeInput("date_range", "Date Range (MST):",
                                start = min(upper_brooks$datetime_MST),
                                end   = max(upper_brooks$datetime_MST)
                 )
               ),
               mainPanel(
                 dygraphOutput("plot", height = "650px")
               )
             )
    ),
    
    #====================================================
    # TAB 2 — QA/QC
    #====================================================
    tabPanel("QA/QC Diagnostics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("qaqc_var", "Variable:",
                             choices = names(upper_brooks)[!names(upper_brooks) %in% c("datetime_UTC","datetime_MST")]
                 ),
                 checkboxInput("show_flags", "Highlight flagged points", TRUE)
               ),
               mainPanel(
                 dygraphOutput("qaqc_plot", height = "450px"),
                 dygraphOutput("qaqc_zoom", height = "200px"),
                 h3("Flagged timestamps"),
                 tableOutput("flag_table")
               )
             )
    ),
    
    #====================================================
    # ⭐ TAB 3 — CLEANED DATA (NEW)
    #====================================================
    tabPanel("Cleaned Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput("clean_var", "Variable to clean:",
                             choices = names(upper_brooks)[!names(upper_brooks) %in% c("datetime_UTC","datetime_MST")]
                 ),
                 
                 checkboxGroupInput("which_flags", "Remove flagged values for:",
                                    choices = c("RANGE","NEGATIVE","SPIKE","FLAT"),
                                    selected = c("RANGE","NEGATIVE")
                 ),
                 
                 checkboxInput("remove_after", "Remove observations after buoy retrieval (10/14 13:30 MDT)", FALSE),
                 
                 downloadButton("download_clean", "Download Cleaned CSV")
               ),
               
               mainPanel(
                 dygraphOutput("clean_plot", height = "450px"),
                 h3("Preview of cleaned data"),
                 tableOutput("clean_table")
               )
             )
    )
  )
)

#------------------------------------------------------------------
# SERVER
#------------------------------------------------------------------
server <- function(input, output, session) {
  
  #===========================================================
  # CLEANED DATA REACTIVE
  #===========================================================
  cleaned_data <- reactive({
    req(input$clean_var)
    
    df <- upper_brooks
    flags <- run_qaqc(df, input$clean_var)
    
    remove <- rep(FALSE, nrow(df))
    
    if ("RANGE"    %in% input$which_flags) remove <- remove | flags$RANGE
    if ("NEGATIVE" %in% input$which_flags) remove <- remove | flags$NEGATIVE
    if ("SPIKE"    %in% input$which_flags) remove <- remove | flags$SPIKE
    if ("FLAT"     %in% input$which_flags) remove <- remove | flags$FLAT
    
    if (input$remove_after) {
      cutoff <- as.POSIXct("2025-10-14 13:30", tz="America/Denver")
      remove <- remove | (df$datetime_MST > cutoff)
    }
    
    df_clean <- df[!remove, ]
    return(df_clean)
  })
  
  #===========================================================
  # CLEANED PLOT
  #===========================================================
  output$clean_plot <- renderDygraph({
    df <- cleaned_data()
    v <- input$clean_var
    
    xts_clean <- xts(df[[v]], order.by = df$datetime_MST)
    indexTZ(xts_clean) <- "America/Denver"
    
    dygraph(xts_clean, main = paste("Cleaned:", v)) %>%
      dyOptions(useDataTimezone = TRUE)
  })
  
  #===========================================================
  # CLEANED TABLE
  #===========================================================
  output$clean_table <- renderTable({
    head(cleaned_data()[, c("datetime_MST", input$clean_var)], 50)
  })
  
  #===========================================================
  # DOWNLOAD CLEANED DATA
  #===========================================================
  output$download_clean <- downloadHandler(
    filename = function() {
      paste0("UpperBrooks_cleaned_", input$clean_var, ".csv")
    },
    content = function(file) {
      write.csv(cleaned_data(), file, row.names = FALSE)
    }
  )
}

#------------------------------------------------------------------
shinyApp(ui, server)
