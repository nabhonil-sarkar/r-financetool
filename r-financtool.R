library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

# --- UI ---
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Financial Portfolio"),
  
  dashboardSidebar(
    fileInput("upload", "1. Upload Portfolio CSV", accept = ".csv"),
    # THEME TOGGLE
    selectInput("theme_choice", "2. Choose Theme:", 
                choices = c("Dark Mode" = "dark", "Light Mode" = "light")),
    uiOutput("filter_ui")
  ),
  
  dashboardBody(
    # Dynamic CSS Injector
    uiOutput("style_css"),
    
    fluidRow(
      valueBoxOutput("total_pos", width = 4),
      valueBoxOutput("opt_count", width = 4),
      valueBoxOutput("stk_count", width = 4)
    ),
    
    fluidRow(
      box(title = "Data View", status = "primary", width = 12, 
          DTOutput("portfolio_table"))
    ),
    
    fluidRow(
      box(title = "Visual Distribution", status = "info", width = 12, 
          plotOutput("symbol_plot"))
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  # 1. CSS Switcher Logic
  output$style_css <- renderUI({
    if (input$theme_choice == "dark") {
      tags$head(tags$style(HTML("
        .content-wrapper, .right-side { background-color: #1e1e27 !important; }
        .main-header .navbar { background-color: #1e1e27 !important; }
        .box { background: #2d2d38; border-top: 2px solid #444; color: white; }
        .box-header { color: white; }
        .main-sidebar { background-color: #181821 !important; }
        body { color: #e1e1e1; }
        .dataTables_wrapper { color: white !important; }
        table.dataTable tbody tr { background-color: #2d2d38 !important; color: white !important; }
        .dataTables_info, .dataTables_paginate { color: white !important; }
      ")))
    } else {
      # Light Mode CSS (Default Dashboard Colors)
      tags$head(tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9 !important; }
        .box { background: white; color: black; }
        body { color: #333; }
      ")))
    }
  })
  
  # 2. Data Loading
  raw_data <- reactive({
    req(input$upload)
    read_csv(input$upload$datapath)
  })
  
  output$filter_ui <- renderUI({
    data <- raw_data()
    tagList(
      selectInput("symbol_select", "3. Select Symbol:", choices = sort(unique(data$Symbol)), multiple = TRUE),
      selectInput("type_select", "4. Select Type:", choices = sort(unique(data$Type)), multiple = TRUE)
    )
  })
  
  filtered_data <- reactive({
    df <- raw_data()
    if (!is.null(input$symbol_select)) df <- df %>% filter(Symbol %in% input$symbol_select)
    if (!is.null(input$type_select)) df <- df %>% filter(Type %in% input$type_select)
    df
  })
  
  # 3. Dynamic Value Boxes
  output$total_pos <- renderValueBox({ 
    color <- if(input$theme_choice == "dark") "black" else "purple"
    valueBox(nrow(filtered_data()), "Total Positions", icon = icon("list"), color = color) 
  })
  
  output$opt_count <- renderValueBox({ 
    val <- nrow(filter(filtered_data(), Type == "OPT"))
    valueBox(val, "Options", color = "teal") 
  })
  
  output$stk_count <- renderValueBox({ 
    val <- nrow(filter(filtered_data(), Type == "STK"))
    valueBox(val, "Stocks", color = "light-blue") 
  })
  
  # 4. Data Table
  output$portfolio_table <- renderDT({
    # The 'style' argument helps the table adapt to background changes
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 5. Plot with Reactive Theme
  output$symbol_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    p <- ggplot(filtered_data(), aes(x = Symbol, fill = Type)) + 
      geom_bar() + 
      theme_minimal()
    
    if (input$theme_choice == "dark") {
      p <- p + theme(
        plot.background = element_rect(fill = "#2d2d38", color = NA),
        panel.background = element_rect(fill = "#2d2d38", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.major = element_line(color = "#444")
      ) + scale_fill_manual(values = c("OPT" = "#008080", "STK" = "#3c8dbc"))
    } else {
      p <- p + scale_fill_brewer(palette = "Set1")
    }
    
    return(p)
  })
}

shinyApp(ui, server)