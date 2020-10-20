library(shiny)
library(ggplot2)

#input data
data1<- read.csv("unfoodr_1.csv")

ui <- fluidPage(
  #title
  title =  "615 project1",
  
  
  
  #select State & Year & Chemical
  fluidRow(
    column(4,
           selectInput("state",
                       "State:",
                       c("All",
                         unique(as.character(data1$State))))
    ),
    column(4,
           selectInput("year",
                       "Year:",
                       c("All",
                         unique(data1$Year)))
    )
  ),
  DT::dataTableOutput("table1"),
  verbatimTextOutput("summary"),
  plotOutput("plot1", click = "plot_click"),
  plotOutput("plot2", click = "plot_click")
) 




server <- function(input, output) {
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- data1
    if (input$state != "All") {
      data <- data[data$State == input$state,]
    }
    if (input$year != "All") {
      data <- data[data$Year == input$year,]
    }
   
    data
  }))
  output$summary<-renderPrint({
    dataset<-data1
    summary(dataset)
  })
  
  output$plot1 <- renderPlot({
    h1 <- ggplot(data1, aes(data1$Value))
    h1 <- h1 + geom_histogram(breaks = seq(0, 50, by = 2), col = "black", fill = "light blue")
    h1
  })
  output$plot2 <- renderPlot({
  bp1 <- ggplot(data1, aes(x = Chemical, y = Value))
  bp1 <- bp1 + geom_boxplot() +
    labs(x = "Chemical type")
  bp1
  })
}

shinyApp(ui, server)
