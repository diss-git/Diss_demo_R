# Load the required libraries and dependencies
source("dependencies.R")

# Perform data extraction, transformation, and loading (ETL)
source("etl.R")

# Define UI
ui <- fluidPage(
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
                           choices = c("Population", "Growth Rate"),
                           selected = "Population", multiple = FALSE))
    ),
    
    # Add CSS style for rounded borders
    tags$style(HTML("
    .rounded-border {
      border-radius: 5px;
    }
  ")),
  
  # Render Highchart
  highchartOutput("line_chart"),
  
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
                hc_title(text = "Population over time") %>%
                hc_xAxis(categories = filtered_data()$Year) %>%
                hc_yAxis(title = list(text = "Population"),
                         labels = list(
                             formatter = JS("function() {
                                 return Highcharts.numberFormat(this.value, 0, '.', ',');
                             }")
                         )) %>%
                hc_add_series(name = "Population", data = filtered_data()$Population, color = "navy") %>%
                hc_tooltip(pointFormatter = JS("
                    function() {
                        var formattedValue = Highcharts.numberFormat(this.y, 0, '.', ',');
                        return '<span style=\"color:' + this.series.color + '\">' + this.series.name + '</span>: <b>' + formattedValue + '</b><br/>';
                    }
                "))
        } else if (input$chart_type_toggle == "Growth Rate") {
            # Render Highchart for Growth Rate
            highchart() %>%
                hc_chart(type = "line") %>%
                hc_title(text = "Growth rate over time") %>%
                hc_xAxis(categories = filtered_data()$Year) %>%
                hc_yAxis(title = list(text = "Growth rate (per 1000)"),
                         labels = list(
                             formatter = JS("function() {
                                 return Highcharts.numberFormat(this.value, 0, '.', ',') + '%';
                             }")
                         )) %>%
                hc_add_series(name = "Growth Rate", data = filtered_data()$"Growth Rate", color = "green") %>%
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
                  class = 'cell-border stripe rounded-border',
                  rownames = FALSE) %>%
            formatStyle(columns = c("Year", "Geography", "Population", "Growth Rate"),
                        backgroundColor = 'white',
                        textAlign = 'left') %>%
            formatCurrency("Population", currency = "", interval = 3, mark = ",") %>%
            formatRound("Growth Rate", digits = 2) %>%
            formatRound("Population", digits = 0) %>%
            formatStyle("Population", textAlign = "right") %>%
            formatStyle("Growth Rate", textAlign = "left") %>%
            # Align column name to the right
            formatStyle(names(filtered_data())[names(filtered_data()) == "Growth Rate"],
                        textAlign = "right")  
    })
    
}

# Run the Shiny app by combining the UI and server
shinyApp(ui, server)
