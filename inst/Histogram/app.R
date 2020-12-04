library("shiny")

palettes <- list("#91aaa7",
                 c("#969660", "#c3dfca"),
                 c("#be83ae", "#2ea7af", "#fbcdcf"),
                 c("#72c5a9", "#b7c5ff", "#dbdabb", "#a28bac"),
                 c("#59c4c0", "#ea9a9a", "#7998a6", "#e9d7a9", "#9c9379"),
                 c("#e8b889", "#67c6eb", "#e5d5c2", "#938fba", "#64b69a", "#779c7b"),
                 c("#c4808f", "#5ba08f", "#f0a693", "#ccd7fe", "#cdb87e", "#c6aae2", "#d2dad8"),
                 c("#d0847f", "#63a5d7", "#d7b981", "#5a9bbb", "#9bb67e", "#dea6d5", "#91967e", "#ca7f96"),
                 c("#8b93a8", "#ccb97e", "#8e9dd7", "#57a384", "#dbb1e7", "#2da7af", "#d68986", "#75d2f9", "#e4d1f0"),
                 c("#dfcf92", "#40b3cb", "#b88a61", "#ecb2e0", "#d6dbbc", "#a28bae", "#edcfeb", "#7498ab", "#b187a0", "#8f939c"),
                 c("#a98f70", "#7be5e2", "#d295c0", "#9ae2bd", "#d3b7f1", "#eca88d", "#8993cd", "#ffc7bb", "#8695a8", "#b3e1df", "#b6878a"),
                 c("#eaa9d3", "#7ac09b", "#fdaca8", "#8ce7e4", "#eed69b", "#70a4d9", "#e8d6ba", "#589bbb", "#959672", "#d0dbd1", "#9b9282", "#d9d9c6"),
                 c("#7498ab", "#e5bd8a", "#7ed8ff", "#de8f8e", "#46bac6", "#ffc0d3", "#57ae96", "#f7cddd", "#50a098", "#b58a6d", "#add49d", "#a18da1", "#cedad9"),
                 c("#8097a4", "#d0dea9", "#a78cc3", "#aee4bf", "#bb82a8", "#5dc9c6", "#b88690", "#26a3b9", "#ad8e6f", "#a4e2ef", "#869a65", "#efcfdd", "#60a089", "#9e927b"),
                 c("#b9aae5", "#bbd69c", "#e2adde", "#77a777", "#f8abc8", "#8ee7ce", "#f2a1a5", "#81bdf1", "#f2bb91", "#b8dcfe", "#aeb276", "#f2cdef", "#e8d6b2", "#8d92b0", "#b7878d"),
                 c("#c3d79b", "#b28cc0", "#64a985", "#e3a7d4", "#2ea2aa", "#e69892", "#85c6f9", "#fbd1a0", "#7696be", "#89996c", "#ddcdff", "#719d89", "#f5cde6", "#b6e0da", "#e8d4cd", "#b5ddfa"),
                 c("#a98d83", "#84e1ff", "#bb8964", "#46b1d1", "#ffbfa5", "#6199c0", "#bbcb8f", "#bf82ab", "#85ddc4", "#eea0ba", "#c1d8ff", "#c3818b", "#c5c6ff", "#999388", "#e8cbff", "#ffb5b6", "#d2dad7"),
                 c("#faccde", "#60a987", "#c6abe4", "#6f9e77", "#c48093", "#a5e5d3", "#cc8f6f", "#499fae", "#d9dca6", "#7796b8", "#bee1ba", "#b4daff", "#919583", "#e2d3e9", "#47a19b", "#ebd4bc", "#7c9993", "#a9e3e0"),
                 c("#739e6e", "#ffbfd9", "#43b6bb", "#e8ad88", "#5e9bce", "#c2af75", "#a8e0fe", "#fad0a8", "#679e8d", "#ffc7b1", "#abe5c0", "#ac8d78", "#c5dddc", "#a48f84", "#cadfb0", "#899694", "#fdcdc1", "#d1dad5", "#dfd8c4"),
                 c("#6e9c93", "#ffb4b3", "#7ec6a2", "#eeccfe", "#cddb9d", "#8a90c5", "#dcb983", "#77bff0", "#f0ab92", "#90ddff", "#f1d3a9", "#b5c2fe", "#c1e1b7", "#7596ba", "#bce1c4", "#a88c96", "#5a9daf", "#b18b80", "#d4d6f3", "#949577"),
                 c("#e7d6bb", "#749ed5", "#f9d29d", "#67b3e2", "#d09773", "#65ccec", "#d38585", "#7fe8ef", "#cf8190", "#94e8cd", "#ae8cc1", "#b3cf95", "#cbc0fc", "#94a66c", "#eeccff", "#ada368", "#e9a6ce", "#48a297", "#ffc1df", "#799c7a", "#facbe0", "#5d9e9a", "#ffc6c1", "#619bb0", "#fccdcb", "#7197bb", "#b1e4c3", "#9390b1", "#c3e0c0", "#a98c90", "#ade3ce", "#9c927d", "#c2dafe", "#869881", "#e6d3dc", "#6e9ba4", "#bde0d0", "#8196a4", "#b2e1df", "#b9deea")
)

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
    exampleFile <- system.file('terebratulid.txt', package = 'palec')
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
