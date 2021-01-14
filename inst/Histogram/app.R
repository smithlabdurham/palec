library("shiny", quietly = TRUE)

# Define UI for app that draws a histogram ----
ui <- fluidPage(title = 'Histogram plotter', theme = "Ternary.css",


                sidebarLayout(
                  sidebarPanel(
                    tags$div("Upload a csv or spreadsheet in which each row or column ",
                             "represents a measurement."),
                    fileInput("datafile", "Data", placeholder = "No data file selected",
                              accept = c('.csv', '.txt', '.xls', '.xlsx')),
                    textOutput(outputId = "dataStatus"),
                    textOutput(outputId = "skewness"),
                    checkboxInput('norm', 'Fit normal', FALSE),
                    checkboxInput('log', 'Log transform', FALSE),
                    sliderInput('breaks', "Number of bins", 0, 6, 0, pre = '2^', step = 0.01),
                    tags$div("Set bins to 2^0 for automatic bin size."),
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

  logData <- reactive(if(input$log) log(myData()) else myData())
  xlab <- reactive(if(input$log) paste0('log(', input$xlab, ')') else input$xlab)

  makePlot <- function () {
    dat <- logData()

    myHist <- hist(dat,
                   main = "",
                   xlab = xlab(),
                   breaks = if(input$breaks == 0) 'Sturges' else 2 ^ input$breaks
    )

    if (input$norm) {
      x <- seq(min(dat), max(dat), length.out = 128)
      multiplier <- myHist$counts[1] / myHist$density[1]
      curve(dnorm(x, mean(dat), sd(dat)) * multiplier, add = TRUE)
    }

  }

  rScript <- function() {
    paste0(
      '# Read the data\n',
      '# Include the full path to your data file here if necessary:\n',
      'myData <- ', switch(fileExt(), '.csv' = 'read.csv',
                           '.txt' = 'read.table',
                           '.xls' = 'readxl::read_excel',
                           'xlsx' = 'readxl::read_excel', 'read.csv'),
      '("', r$fileName, '")\n',

      if (input$log) "\n# Log transform the data\nmyData <- log(myData)\n",

      '\n# Plot the histogram\n',
      'myHist <- hist(myData, main = "",\n',
      '               breaks = ',
      if(input$breaks == 0) '"Sturges"' else input$breaks, ',\n',
      '               xlab = "', xlab(), '")\n\n',
      if (input$norm) {
        paste0('# Overlay the best-fitting normal curve\n',
               'x <- seq(min(myData), max(myData), length.out = 128)\n',
               'multiplier <- myHist$counts[1] / myHist$density[1]\n',
               'curve(dnorm(x, mean(myData), sd(myData)) * multiplier, add = TRUE)\n\n')
      }
    )
  }

  output$skewness <- renderText(paste0("Skewness: ",
                                       signif(moments::skewness(logData()), 4)))
  output$plot <- renderPlot(makePlot())
  output$code <- renderText(rScript())
  output$savePng <- downloadHandler(
    filename = 'Histogram.png',
    content = function (file) {
      png(file, width = input$pngSize, height = input$pngSize)
      makePlot()
      dev.off()
    })
  output$savePdf <- downloadHandler(
    filename = 'Histogram.pdf',
    content = function (file) {
      pdf(file,
          title = paste0('Ternary plot',
                         if(filePath() != '') paste0('  from ', filePath())))
      makePlot()
      dev.off()
    })
  output$saveR <- downloadHandler(
    filename = 'Histogram.R',
    content = function (file) {
      writeLines(rScript(), file)
    })

}

shinyApp(ui = ui, server = server)
