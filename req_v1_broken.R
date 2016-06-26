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
    get(input$dataset, "package:datasets", inherits = FALSE)
  })
  
  selectedData <- reactive({
    dataset()[, input$vars]
  })
  
  observe({
    updateCheckboxGroupInput(session, "vars", choices = c(names(dataset())))
  })
  
  output$plot <- renderPlot({
    plot(selectedData())
  })
}

shinyApp(ui, server)
