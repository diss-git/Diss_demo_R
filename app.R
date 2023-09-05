# Load the required libraries and dependencies
source("dependencies.R")

# Perform data extraction, transformation, and loading (ETL)
source("etl.R")

# Define UI
ui <- fluidPage(
    
    # Include your custom CSS file
    includeCSS("www/styles.css"),
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "https://www150.statcan.gc.ca/wet-boew4b/css/theme.min.css")),
    
    # Add line under title for readability
    div(style = "border-bottom: 1px solid #333;",
        titlePanel("Population Analysis")),
    
    # Add line break between title and Select menu
    tags$br(),  
    
    fluidRow(
        # Add Select Geography dropdown
        column(width = 6,
               selectInput(inputId = "geo_filter", label = "Geography", choices = unique(cleaned_data$Geography))),
        
        # Add Chart Type dropdown (Population or Growth Rate)
        column(width = 6,
               selectInput(inputId = "chart_type_toggle", label = "Value",
                           choices = c("Population", "Growth rate"),
                           selected = "Population", multiple = FALSE))
    ),
    
    # Skip link for accessibility
    tags$div(
            tags$a(href = "#formatted_table", class = "wb-inv wb-show-onfocus wb-sl",
                    "Skip the interactive chart and go directly to the data table")
    ),
  
  # Render Highchart
  highchartOutput("line_chart"),
  
  # Add line break between graph and table
  tags$br(), 
  
  # Render formatted DataTable
    DTOutput("formatted_table")
  
)

# Define server
server <- function(input, output) {
    
    # Filter data based on selected Geography
    filtered_data <- reactive({
        subset(cleaned_data, Geography == input$geo_filter)
    })
    
    # Render Highchart for Population and Growth Rate
    output$line_chart <- renderHighchart({
        if (input$chart_type_toggle == "Population") {
            # Render Highchart for Population
            highchart() %>%
                hc_chart(type = "line") %>%
                hc_title(text = "<b>Population over time</b>",
                         align = "left") %>%
                hc_xAxis(categories = filtered_data()$"Reference period") %>%
                hc_yAxis(title = list(text = "population"),
                         labels = list(
                             formatter = JS("function() {
                                 return Highcharts.numberFormat(this.value, 0, '.', ',');
                             }")
                         )) %>%
                hc_add_series(name = "Population", data = filtered_data()$Population, color = "navy",showInLegend = F) %>%
                hc_tooltip(pointFormatter = JS("
                    function() {
                        var formattedValue = Highcharts.numberFormat(this.y, 0, '.', ',');
                        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + formattedValue + '</b><br/>';
                    }
                "))
        } else if (input$chart_type_toggle == "Growth rate") {
            # Render Highchart for Growth Rate
            highchart() %>%
                hc_chart(type = "line") %>%
                hc_title(text = "<b>Growth rate over time percentage</b>",
                         align = "left") %>%
                hc_xAxis(categories = filtered_data()$"Reference period") %>%
                hc_yAxis(title = list(text = "percentage"),
                         labels = list(
                             formatter = JS("function() {
                                 return Highcharts.numberFormat(this.value, 0, '.', ',') + '%';
                             }")
                         )) %>%
                hc_add_series(name = "Growth rate", data = filtered_data()$"Growth rate", color = "green", showInLegend = F) %>%
                hc_tooltip(pointFormatter = JS("
                    function() {
                        var formattedValue = Highcharts.numberFormat(this.y, 2, '.', ',') + '%';
                        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + formattedValue + '</b><br/>';
                    }
                "))
        }
    })
    
    # Render formatted DataTable
    output$formatted_table <- renderDT({
        datatable(filtered_data(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE) %>%
            formatStyle(columns = c("Reference period", "Geography", "Population", "Growth rate"),
                        backgroundColor = 'white',
                        textAlign = 'left') %>%
            formatCurrency("Population", currency = "", interval = 3, mark = ",") %>%
            formatRound("Growth rate", digits = 2) %>%
            formatRound("Population", digits = 0) %>%
            formatStyle("Population", textAlign = "right") %>%
            formatStyle("Growth rate", textAlign = "left") %>%
            # Align column name to the right
            formatStyle(names(filtered_data())[names(filtered_data()) == "Growth rate"],
                        textAlign = "right")  
    })
    
}

# Run the Shiny app by combining the UI and server
shinyApp(ui, server)
