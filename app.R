# This app was inspired by: https://shiny.rstudio.com/gallery/real-estate-investment.html
library(shiny)
library(tidyverse)
library(data.table)
library(plotly)
library(lubridate)
library(shinyWidgets)
library(leaflet)
library(maps)
library(DT)

source("data.R")
source("html.R")


ui <- navbarPage("SevenAgency",
  includeCSS("www/style/style.css"),
  tabPanel(
    "Know Thy Customers",
    knowThyCustomers()
  ),
  tabPanel(
    "Question Thy Products",
    questionThyProducts()
  ),
  tabPanel(
    "Jet Around The Globe",
    jetAround()
  ),
  tabPanel(
    "Analyze in Depth",
    analyzeInDepth()
  )
)
 
# Define server logic                                                                                                    
server <- function(input, output, session) {

  # First page
  output$nbr_cust <- renderText({
    format(dim(data)[1], big.mark = ",")
  })
  output$ttl_revenue <- renderText({
    rev <- data %>%
      select(ends_with("_TotalStakes"), Poker_TotalBuyingAmount) %>%
        sum(na.rm = TRUE) / 1e6
    paste0(floor(rev), "M")
  })
  output$avg_lor <- renderText({
    avg_lor()
  })
  output$drop_off <- renderPlotly({
    render_home_plot(input$gender, input$region)
  })

  # Second page
  lapply(
    1:8,
    function(i){
      observeEvent(input[[paste0("prod", i)]], {
        if (i == 3) {
          output$chart_section <- renderUI({render_poker_chart_UI()})
        } else {
          output$chart_section <- renderUI({render_bett_chart_UI()})
        }
        
        chart_data <- render_prod_chart(i, input$gender2, input$region2, input$country2, input$wealth_slider)
        output$line_chart_bl <- renderPlotly({chart_data[[1]]})
        output$line_chart_br <- renderPlotly({chart_data[[2]]})
        output$pie_chart_tl <- renderPlotly({chart_data[[3]]})
        output$bar_chart_tr <- renderPlotly({chart_data[[4]]})
        output$pct_player <- renderText({chart_data[[5]]})
      })
    }
  )

  # Third page
  output$map <- renderLeaflet({render_map()})
  # Ref: https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
  observe({
    click <- input$map_marker_click
    if (is.null(click)) return()
    output$map_chart <- renderUI(render_map_chart_UI())
    output$country_name <- renderText(render_country_name(click$lat, click$lng))
    output$table_top <- renderTable(render_map_table(click$lat, click$lng), width = "100%")
    output$bar_chart_btm <- renderPlotly(render_map_chart(click$lat, click$lng))
  })

  # Fourth page
  observeEvent(input$var_prod, {
    updatePickerInput(
      session,
      "var_reg",
      choices = get_regions(input$var_prod),
      selected = get_regions(input$var_prod)
    )
    updatePickerInput(
      session,
      "var_ctry",
      choices = get_countries(input$var_prod, input$var_reg),
      selected = get_countries(input$var_prod, input$var_reg)
    )
    updatePickerInput(
      session,
      "var_var",
      choices = get_variables(input$var_prod, input$var_reg, input$var_ctry)[-c(1:3)]
    )
  })
  observeEvent(input$var_reg, {
    updatePickerInput(
      session,
      "var_ctry",
      choices = get_countries(input$var_prod, input$var_reg),
      selected = get_countries(input$var_prod, input$var_reg)
    )
    updatePickerInput(
      session,
      "var_var",
      choices = get_variables(input$var_prod, input$var_reg, input$var_ctry)[-c(1:3)]
    )
  })
  observeEvent(input$gen_table, {
    output$gen_chart_table <- renderUI(render_table_UI())
    output$var_table <- DT::renderDataTable({
      DT::datatable(get_chart_table_data(input$var_prod, input$var_reg, input$var_ctry),
      options = list(
        searching = FALSE,
        pageLength = 10,
        scrollX = TRUE
        )
      )})
  })
  observeEvent(input$gen_chart, {
    output$gen_chart_table <- renderUI(render_chart_UI())
    output$sum_n <- renderText(get_summary_data(input$var_prod, input$var_reg, input$var_ctry, input$var_var)[[1]])
    output$sum_min <- renderText(get_summary_data(input$var_prod, input$var_reg, input$var_ctry, input$var_var)[[2]])
    output$sum_med <- renderText(get_summary_data(input$var_prod, input$var_reg, input$var_ctry, input$var_var)[[3]])
    output$sum_avg <- renderText(get_summary_data(input$var_prod, input$var_reg, input$var_ctry, input$var_var)[[4]])
    output$sum_max <- renderText(get_summary_data(input$var_prod, input$var_reg, input$var_ctry, input$var_var)[[5]])

    output$var_chart <- renderPlotly(render_var_chart(input$var_prod, input$var_reg, input$var_ctry, input$var_var))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
