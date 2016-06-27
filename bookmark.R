library(shiny)
library(ggplot2)
library(htmltools)

ui <- function(req) {
  fluidPage(
    h2("Bookmarkable state demo"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload data"),
        # This will hold column dropdowns and "Add plot" button
        uiOutput("column_ui"),
        hr(),
        saveStateButton("bookmark")
      ),
      mainPanel(
        # This <div> will hold all of the plots we're going to
        # dynamically add. It's going to be super fun!
        div(id = "plot_container")
      )
    ),
    # Disable fading effect when processing
    tags$style(".recalculating { opacity: 1; }")
  )
}

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    plot_count = 0,
    plots = list(),
    brush = NULL
  )
  
  observe({
    rv$brush <- input$brush
  }, priority = 10)
  
  dataset <- reactive({
    # dataset() can't be fulfilled if no file was uploaded
    req(input$file)
    
    # read.csv the uploaded file. If it fails, squelch the
    # error by using req(FALSE), and show the error using a
    # notification instead. Not sure this is good or evil.
    tryCatch(
      read.csv(input$file$datapath[1]),
      error = function(e) {
        showNotification(
          ui = tagList(strong("Error:"), conditionMessage(e)),
          type = "error"
        )
        req(FALSE)
      }
    )
  })
  
  # Let user choose columns, and add plot.
  output$column_ui <- renderUI({
    choices <- c("Choose one" = "", names(dataset()))
    tagList(
      selectInput("xvar", "X variable", choices),
      selectInput("yvar", "Y variable", choices),
      conditionalPanel("input.xvar && input.yvar",
        actionButton("addplot", "Add plot")
      )
    )
  })
  
  # Add a plot when addplot is clicked
  observeEvent(input$addplot, {
    rv$plot_count <- rv$plot_count + 1
    
    id <- paste0("plot", rv$plot_count)
    # Take a static snapshot of xvar/yvar; the renderPlot we're
    # creating here cares only what their values are now, not in
    # the future.
    xvar <- input$xvar
    yvar <- input$yvar
    
    plot_spec <- list(id = id, xvar = xvar, yvar = yvar)
    rv$plots <- c(rv$plots, list(plot_spec))
    
    add_plot(plot_spec)
  })
  
  add_plot <- function(plot_spec) {
    id <- plot_spec$id
    xvar <- plot_spec$xvar
    yvar <- plot_spec$yvar
    message("Add plot ", id)
    
    output[[id]] <- renderPlot({
      df <- brushedPoints(dataset(), rv$brush, allRows = TRUE)
      
      ggplot(df, aes_string(xvar, yvar, color = "selected_")) +
        geom_point(alpha = 0.6) +
        scale_color_manual(values = c("black", "green")) +
        guides(color = FALSE) +
        xlab(xvar) + ylab(yvar)
    })
    insertUI("#plot_container", where = "beforeEnd",
      ui = div(style = css(display = "inline-block"),
        plotOutput(id, brush = "brush", width = 275, height = 275)
      )
    )
  }
  
  # Whenever the dataset changes, clear all plots
  observeEvent(dataset(), {
    removeUI("#plot_container *")
    rv$plots <- list()
  })
  
  configureBookmarking(input$bookmark, type = "persist",
    onBookmark = function(restoreContext) {
      restoreContext$values <- list(
        plot_count = rv$plot_count,
        plots = rv$plots,
        brush = rv$brush
      )
    },
    onRestore = function(restoreContext) {
      if (length(restoreContext$values) == 0)
        return()
      session$onFlushed(function() {
        rv$plot_count <- restoreContext$values$plot_count
        rv$plots <- restoreContext$values$plots
        rv$brush <- restoreContext$values$brush
        lapply(restoreContext$values$plots, add_plot)
      })
    }
  )
}

shinyApp(ui, server)
