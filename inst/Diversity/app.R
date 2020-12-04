library("shiny")

# Define UI for app that draws a histogram ----
ui <- fluidPage(title = 'Diversity analysis', theme = "Ternary.css",
        sidebarLayout(
          sidebarPanel(
            tags$div("Upload a csv or spreadsheet in which each column ",
                     "represents the number of occurrences in an ",
                     "assemblage."),
            fileInput("datafile", "Data", placeholder = "No data file selected",
                      accept = c('.csv', '.txt', '.xls', '.xlsx')),
            textOutput(outputId = "dataStatus"),
            radioButtons('col', 'Assemblage',
                        setNames(as.list(1:10), paste0('CD', 1:10))),
            sliderInput('xlim', 'Axis size (0 = auto)', 0, 420, 0, step = 1),
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

                       fluidRow(
                         withTags(
                           table(
                             tr(
                               td('Total count (n): '),
                               textOutput('n', container = td)
                             ),
                             tr(
                               td('Count of most abundant (n_t_): '),
                               textOutput('nMax', td)
                             ),
                             tr(
                               td('Species richness (S): '),
                               textOutput('richness', td)
                             ),
                             tr(
                               td("Menhinick's richness: "),
                               textOutput('menhinick', td)
                             ),
                             tr(
                               td("Margalef's richness: "),
                               textOutput('margalef', td)
                             ),
                             tr(
                               td('Berger–Parker index: '),
                               textOutput('bpi', td)
                             ),
                             tr(
                               td('Simpson index: '),
                               textOutput('simpson', td)
                             ),
                             tr(
                               td('Shannon entropy: '),
                               textOutput('shannon', td)
                             ),
                             tr(
                               td('Equitibility (J): '),
                               textOutput('equit', td)
                             ),
                           )
                         ),
                       ),
              ),
              tabPanel("R code",
                       fluidRow(verbatimTextOutput('code')),
              )
            )
          )
        )
)

server <- function(input, output, session) {

  r <- reactiveValues()

  filePath <- reactive({
    fileInput <- input$datafile
    exampleFile <- system.file('Llanvirn.txt', package = 'palec')
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

  UpdateAssemblages <- function (dat) {
    updateRadioButtons(session, 'col',
                      choices = as.list(dat[1, ] * 0L + seq_along(dat[1, ])))
    updateSliderInput(session, 'xlim', max = max(dat))
  }

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

    UpdateAssemblages(ret)
    ret
  })

  assemblage <- reactive(myData()[, as.integer(input$col)])

  makePlot <- function () {
    dat <- myData()
    lab <- rownames(dat)

    par(las = 1, cex = 0.8, mar = c(3, max(nchar(lab)) * 0.6, 0, 1))
    barplot(assemblage(),
            main = "",
            horiz = TRUE,
            names.arg = rownames(dat),
            xlab = "Count",
            xlim = if (input$xlim > 0) c(0, input$xlim) else NULL
    )

  }

  rScript <- function() {
    paste0(
      '# Read the data\n',
      '# Include the full path to your data file here if necessary:\n',
      'myData <- ', switch(fileExt(), '.csv' = 'read.csv',
                           '.txt' = 'read.table',
                           '.xls' = 'readxl::read_excel',
                           'xlsx' = 'readxl::read_excel', 'read.csv'),
      '("', r$fileName, '")\n'
    )
  }

  output$plot <- renderPlot(makePlot())
  output$code <- renderText(rScript())

  S <- reactive(sum(assemblage() > 0))
  n <- reactive(sum(assemblage()))
  pi <- reactive(assemblage() / n())
  log0 <- function (x) ifelse(x == 0, 0, log(x))
  Entropy <- function (p) -sum(p * log0(p))
  shannon <- reactive(Entropy(pi()))

  output$n <- renderText(n())
  output$nMax <- renderText(max(assemblage()))
  output$richness <- renderText(S())
  output$menhinick <- renderText(signif(S() / sqrt(n()), 4))
  output$margalef <- renderText(signif((S() - 1) / log(n()), 4))
  output$bpi <- renderText(signif(max(assemblage()) / n(), 4))
  output$simpson <- renderText(signif(sum(pi() ^ 2), 4))
  output$shannon <- renderText(signif(shannon(), 4))
  output$equit <- renderText({
    mx <- Entropy(rep(1 / S(), S()))
    mn <- Entropy(c(rep(1, S() - 1), n() - S() + 1) / n())
    eq <- (shannon() - mn) / (mx - mn)
    signif(eq, 4)
    })

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
