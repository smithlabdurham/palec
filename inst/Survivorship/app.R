library("shiny")

# Define UI for app that draws a histogram ----
ui <- fluidPage(title = 'Survivorship curves', theme = "Ternary.css",
  sidebarLayout(
    sidebarPanel(
     tags$div("Upload a csv or spreadsheet in which each row or column ",
              "represents a measurement."),
     fileInput("datafile", "Data", placeholder = "No data file selected",
               accept = c('.csv', '.txt', '.xls', '.xlsx')),
     textOutput(outputId = "dataStatus"),
     checkboxInput('log', 'Log transform', FALSE),
     textInput("xlab", "X Label", "Value / unit"),
    ),

  # Sidebar layout with input and output definitions ----

    mainPanel(
      tabsetPanel(
        tabPanel('Plot',
                 fluidRow(plotOutput(outputId = "plot")),
                 fluidRow(id = 'saveButtons',
                   tags$span("Save as: "),
                   downloadButton('saveR', 'R script'),
                   downloadButton('savePdf', 'PDF'),
                   downloadButton('savePng', 'PNG'),
                   tags$span("PNG size: ", id = 'pngSizeLabel'),
                   numericInput('pngSize', NULL, 800, 100,
                               width = "70px", step = 10),
                   tags$span("pixels"),
                 ),
        ),
        tabPanel("R code",
                 fluidRow(verbatimTextOutput('code')),
        )
      )
    )
  )


  # References and notes
)

server <- function(input, output, session) {

  r <- reactiveValues()
  displaySetting <- function(id) id %in% input$display

  filePath <- reactive({
    fileInput <- input$datafile
    exampleFile <- system.file('linear.txt', package = 'palec')
    if (is.null(fileInput)) {
      output$dataStatus <- renderText(paste(
        "Data file not found; using example from", exampleFile))
      candidate <- exampleFile
    } else {
      candidate <- fileInput$datapath
      if (is.null(candidate)) {
        output$dataStatus <- renderText("Data file not found; using example.")
        candidate <- exampleFile
      } else {
        r$fileName <- fileInput$name
        output$dataStatus <- renderText(paste0("Loaded data from ", fileInput$name))
      }
    }

    # Return:
    candidate
  })

  fileExt <- reactive({
    fp <- filePath()
    if (nchar(fp) < 2) "<none>" else substr(fp, nchar(fp) - 3, nchar(fp))
  })

  myData <- reactive({
    fp <- filePath()
    ret <- switch(fileExt(),
                  '.csv' = read.csv(fp),
                  '.txt' = read.table(fp),
                  '.xls' = readxl::read_excel(fp),
                  'xlsx' = readxl::read_excel(fp),
                  {
                    output$dataStatus <- renderText({
                      paste0("Unsupported file extension: ", fileExt())})
                    matrix(0, 0, 3)
                  }
    )

    as.numeric(unlist(ret))
  })

  xlab <- reactive(input$xlab)

  makePlot <- function () {
    dat <- myData()

    x <- unique(dat)
    y <- vapply(x, function (x) sum(dat >= x), 0L) * 100 / length(dat)

    plot(y ~ x,
         log = if(input$log) 'xy' else 'y',
         xlab = xlab(), ylab = "Percentage of individuals surviving",
         pch = 3, frame = FALSE)

  }

  rScript <- function() {
    paste0(
      '# Include the full path to your data file here if necessary:\n',
      'myData <- ', switch(fileExt(), '.csv' = 'read.csv',
                           '.txt' = 'read.table',
                           '.xls' = 'readxl::read_excel',
                           'xlsx' = 'readxl::read_excel', 'read.csv'),
      '("', r$fileName, '")\n\n',

      'x <- c(0, unique(myData))\n',
      'y <- vapply(x, function (x) sum(myData >= x), 0L) * 100 / length(myData)\n',
      'plot(y ~ x,\n',
      '     log = "', if(input$log) 'xy' else 'y', '",\n',
      '     xlab = "', xlab(), '",\n',
      '     ylab = "Percentage of individuals surviving",\n',
      '     pch = 3, frame = FALSE)\n\n'
    )
  }

  output$plot <- renderPlot(makePlot())
  output$code <- renderText(rScript())
  output$savePng <- downloadHandler(
    filename = 'Survivorship.png',
    content = function (file) {
      png(file, width = input$pngSize, height = input$pngSize)
      makePlot()
      dev.off()
    })
  output$savePdf <- downloadHandler(
    filename = 'Survivorship.pdf',
    content = function (file) {
      pdf(file,
          title = paste0('Ternary plot',
                         if(filePath() != '') paste0('  from ', filePath())))
      makePlot()
      dev.off()
    })
  output$saveR <- downloadHandler(
    filename = 'Survivorship.R',
    content = function (file) {
      writeLines(rScript(), file)
    })

}

shinyApp(ui = ui, server = server)
