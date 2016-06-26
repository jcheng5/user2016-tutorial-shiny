library(shiny)

datasets <- c("pressure", "cars", "mtcars")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Dataset", c("", datasets)),
      checkboxGroupInput("vars", "Variables", c())
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    # Stop silently if input$dataset isn't set
    req(input$dataset)
    
    get(input$dataset, "package:datasets", inherits = FALSE)
  })
  
  selectedData <- reactive({
    # Stop silently if input$vars isn't set
    req(input$vars)
    
    dataset()[, input$vars]
  })
  
  observe({
    updateCheckboxGroupInput(session, "vars", choices = c(names(dataset())))
    # updateCheckboxGroupInput modifies input$vars asynchronously.
    # Let no other reactive make use of input$vars until the value of input$vars
    # has had a chance to be updated from the client.
    invalidateReactiveValue(input, "vars")
  }, priority = 1)
  
  output$plot <- renderPlot({
    plot(selectedData())
  })
}

shinyApp(ui, server)
