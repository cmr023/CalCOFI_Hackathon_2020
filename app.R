library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

###UI###
ui <- fluidPage(
    headerPanel('CalCOFI Phytoplankton Bloom Data'),
   
    sidebarPanel(
        selectInput('ycol','Species', names(NE_line[,8:length(NE_line)]))
        ),
    
    mainPanel(
        fluidRow(
            leafletOutput("mymap")
        ),
        br(),
        fluidRow(
            splitLayout(cellWidths = c("50%","50%"), 
                    plotlyOutput("plotly1", height = 300), 
                    plotlyOutput("plotly2", height = 300))
        ),
        br(),
        fluidRow(
            splitLayout(cellWidths = c("50%","50%"),
                        plotlyOutput("plotly3", height = 300),
                        plotlyOutput("plotly4", height = 300))
        ),
        br(),
        fluidRow(
            plotlyOutput("plotly5")
        ),
        br()
    )
)

server <- function(input, output) {
    
    output$mymap <- renderLeaflet({n})
    
    output$plotly1 <- renderPlotly(
        ne1 <- ggplot(data = NE_line, 
                      aes(x = Date, 
                          y = get(input$ycol))) +
            geom_line() +
            geom_point(size = .5) +
            ggtitle("Northeast") +
            ylab("cells/l"))
    
    output$plotly2 <- renderPlotly(
        se1 <- ggplot(data = SE_line, 
               aes(x = Date, 
                   y = get(input$ycol))) +
            geom_line() +
            geom_point(size = .5) +
            ggtitle("Southeast") +
            ylab("cells/l")
    )
    
    output$plotly3 <- renderPlotly(
        al1 <- ggplot(data = Alley_line, 
                      aes(x = Date, 
                          y = get(input$ycol))) +
            geom_line() +
            geom_point(size = .5) +
            ggtitle("Alley") +
            ylab("cells/l")
    )
    
    output$plotly4 <- renderPlotly(
        os1 <- ggplot(data = Offshore_line, 
                      aes(x = Date, 
                          y = get(input$ycol))) +
            geom_line() +
            geom_point(size = 1) +
            ggtitle("Offshore") +
            ylab("cells/l")
    )
    
    output$plotly5 <- renderPlotly(
        df <- ggplot(data = Sum_line, 
                   aes(x = Date,  
                       y = get(input$ycol))) +
            geom_point(size = 1) +
            geom_line() +
            ggtitle("Combined Abundance") +
            ylab("cells/l")
    )
    
}

shinyApp(ui,server)