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
    if (input$dataset == "")
      return(NULL)
    
    get(input$dataset, "package:datasets", inherits = FALSE)
  })
  
  selectedData <- reactive({
    if (is.null(dataset()))
      return(NULL)
    if (length(input$vars) == 0)
      return(NULL)
    if (!all(input$vars %in% names(dataset())))
      return(NULL)
    
    dataset()[, input$vars]
  })
  
  observe({
    if (is.null(dataset()))
      return(NULL)
    
    updateCheckboxGroupInput(session, "vars", choices = c(names(dataset())))
  })
  
  output$plot <- renderPlot({
    if (is.null(selectedData()))
      return(NULL)
    
    plot(selectedData())
  })
}

shinyApp(ui, server)
