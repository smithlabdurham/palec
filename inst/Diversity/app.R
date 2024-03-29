suppressPackageStartupMessages(library("shiny"))
library("shinyjs", exclude = "runExample", warn.conflicts = FALSE,
        quietly = TRUE)

# Define UI for app that draws a histogram ----
ui <- fluidPage(title = "Diversity analysis", theme = "Ternary.css",
        useShinyjs(),
        sidebarLayout(
          sidebarPanel(
            tags$div("Upload a csv or spreadsheet in which each column ",
                     "represents the number of occurrences in an ",
                     "assemblage."),
            fileInput("datafile", "Data", placeholder = "No data file selected",
                      accept = c(".csv", ".txt", ".xls", ".xlsx")),
            textOutput(outputId = "dataStatus"),
            radioButtons("col", "Assemblage",
                        setNames(as.list(1:4), paste0("Assemblage_", 1:4))),
            checkboxInput("rank", "Order by rank abundance", FALSE),
            checkboxInput("log", "Log transform abundance", FALSE),
            radioButtons("plotType", "Plot type",
                         list("Bar plot" = "bar", "Scatter plot" = "scatter",
                              "Octave plot" = "octave")),
            hidden(checkboxInput("norm", "Fit log-normal", FALSE)),
            hidden(checkboxInput("geom", "Fit (log)linear", FALSE)),
            sliderInput("xlim", "Axis size (0 = auto)", 0, 420, 0, step = 1),
          ),

          # Sidebar layout with input and output definitions ----

          mainPanel(
            tabsetPanel(
              tabPanel("Plot",
                       fluidRow(plotOutput(outputId = "plot")),
                       fluidRow(id = "saveButtons",
                                tags$span("Save as: "),
                                downloadButton("saveR", "R script"),
                                downloadButton("savePdf", "PDF"),
                                downloadButton("savePng", "PNG"),
                                tags$span("PNG size: ", id = "pngSizeLabel"),
                                numericInput("pngSize", NULL, 800, 100,
                                             width = "70px", step = 10),
                                tags$span("pixels"),
                       ),
                       inlineCSS(setNames(as.list(paste0("border-right: ",
                                                         hcl.colors(129, "inferno"),
                                                         " solid ",
                                                         signif(0:128 / 14, 3), "em;")),
                                          paste0(".scale", 0:128))),
                       fluidRow(
                         withTags(
                           table(
                             tr(th("Sample size")),
                             tr(
                               td("Total count (n): "),
                               textOutput("n", container = td),
                               td(id = "nSwatch", class = "swatch")
                             ),

                             tr(th("Richness")),
                             tr(
                               td("Species richness (S): "),
                               textOutput("richness", td),
                               td(id = "richnessSwatch", class = "swatch")
                             ),
                             tr(
                               td("Menhinick's richness: "),
                               textOutput("menhinick", td),
                               td(id = "menhinickSwatch", class = "swatch")
                             ),
                             tr(
                               td("Margalef's richness: "),
                               textOutput("margalef", td),
                               td(id = "margalefSwatch", class = "swatch")
                             ),

                             tr(th("Dominance")),
                             tr(
                               td(HTML(paste0("Count of most abundant (n", tags$sub("t"), "): "))),
                               textOutput("nMax", td),
                               td(id = "nMaxSwatch", class = "swatch")
                             ),
                             tr(
                               td("Berger–Parker index: "),
                               textOutput("bpi", td),
                               td(id = "bpiSwatch", class = "swatch")
                             ),
                             tr(
                               td("Simpson index: "),
                               textOutput("simpson", td),
                               td(id = "simpsonSwatch", class = "swatch")
                             ),
                             tr(
                               td("Shannon entropy: "),
                               textOutput("shannon", td),
                               td(id = "shannonSwatch", class = "swatch")
                             ),
                             tr(
                               td("Equitability (J): "),
                               textOutput("equit", td),
                               td(id = "equitSwatch", class = "swatch")
                             ),

                             tr(td(), td(),
                                td(class = "swatch",
                                   style = "border-right: solid #000A 9.14em; height: 1px")
                              )
                           )
                         ),
                         checkboxInput("correct", "Correct range for sample size", FALSE),
                       ),

              ),
              tabPanel("R code",
                       fluidRow(verbatimTextOutput("code")),
              )
            )
          )
        )
)

server <- function(input, output, session) {
  r <- reactiveValues()
  myPalette <- palette.colors(4)[-1]

  filePath <- reactive({
    fileInput <- input$datafile
    exampleFile <- system.file("diversity.txt", package = "palec")
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
    updateRadioButtons(session, "col",
                      choices = setNames(seq_along(dat[1, ]), colnames(dat)))
    updateSliderInput(session, "xlim", max = max(dat, na.rm = TRUE))
  }

  myData <- reactive({
    fp <- filePath()
    ret <- switch(fileExt(),
                  ".csv" = read.csv(fp),
                  ".txt" = read.table(fp),
                  ".xls" = as.data.frame(readxl::read_excel(fp)),
                  "xlsx" = as.data.frame(readxl::read_excel(fp)),
                  {
                    output$dataStatus <- renderText({
                      paste0("Unsupported file extension: ", fileExt())})
                    matrix(0, 0, 3)
                  }
    )

    if (is.character(ret[, 1])) {
      rn <- ret[, 1]
      rn[is.na(rn)] <- "BLANK"
      dups <- duplicated(rn)
      rn[dups] <- paste0(rn[dups], "_", seq_along(rn[dups]))
      rownames(ret) <- rn
      ret <- ret[, -1]
    }

    UpdateAssemblages(ret)
    ret
  })

  assemblage <- reactive(myData()[, as.integer(input$col)])
  counts <- reactive({
    ass <- assemblage()
    ass[!is.na(ass)]
  })

  Swatch <- function (id, val, mx = 1, mn = 0) {
    scalePoint <- as.integer(128 * (val - mn) / (mx - mn))
    addCssClass(paste0(id, "Swatch"),
                paste0("scale", scalePoint),
                asis = TRUE)
  }

  makePlot <- function () {
    dat <- assemblage()
    dat[dat == 0] <- NA
    obs <- dat[!is.na(dat)]

    order <- if (input$rank) order(assemblage()) else seq_along(assemblage())
    obsOrder <- if (input$rank) order(obs) else seq_along(obs)

    switch(input$plotType,
           "bar" = {
             showElement("rank", TRUE)
             showElement("log", TRUE)
             hideElement("norm", TRUE)
             hideElement("geom", TRUE)
             showElement("xlim", TRUE)
             lab <- if (input$rank) {
               rownames(myData())[!is.na(dat)][obsOrder]
             } else {
               rownames(myData())[order]
             }
             par(las = 1, cex = 0.8, mar = c(3, max(nchar(lab)) * 0.6, 0, 1))
             barplot(if(input$rank) obs[obsOrder] else dat,
                     main = "",
                     log = if(input$log) "x" else "",
                     horiz = TRUE,
                     names.arg = lab,
                     xlab = "Count",
                     xlim = if (input$xlim > 0) c(0, input$xlim) else NULL
             )
           },
           "scatter" = {
             showElement("rank", TRUE)
             showElement("log", TRUE)
             hideElement("norm", TRUE)
             showElement("geom", TRUE)
             showElement("xlim", TRUE)
             par(las = 1, cex = 0.8, mar = c(4, 4, 0, 1))
             plot(obs[obsOrder] ~ rev(seq_along(obs)),
                  main = "",
                  log = if(input$log) "y" else "",
                  xlab = "Rank order",
                  ylab = "Count",
                  ylim = if (input$xlim > 0) c(if(input$log) 1 else 0,
                                               as.integer(input$xlim)) else NULL,
                  frame = FALSE,
                  pch = 3
             )
             if (input$geom) {
               y <- if(input$log) log10(obs[obsOrder]) else obs[obsOrder]
               abline(lm(y ~ rev(seq_along(obs))),
                      lty = "dashed",
                      col = myPalette[1])
             }
           },
           "octave" = {
             hideElement("rank", TRUE)
             hideElement("log", TRUE)
             showElement("geom", TRUE)
             showElement("norm", TRUE)
             hideElement("xlim", TRUE)
             octaves <- palec::Octaves(obs)
             maxFreq <- max(table(octaves))
             breaks <- seq_len(max(octaves) + 1) - 1L

             myHist <- hist(octaves,
                  breaks = breaks,
                  main = "",
                  xlab = "Octave",
                  axes = FALSE
                  )
             axis(1, at = breaks, labels = 2L ^ (breaks))
             axis(2, at = if(maxFreq < 8) seq_len(1 + maxFreq) - 1L, NULL)
             if (input$norm) {
               x <- seq(0, max(breaks), length.out = 128L)
               fullBin <- which.max(myHist$counts)
               multiplier <- (myHist$counts / myHist$density)[fullBin]

               curve(dnorm(x, mean(octaves) - 0.5, sd(octaves)) * multiplier,
                     add = TRUE, col = myPalette[2])

               if (length(obs) > 2) {
                 isLogNormal <- shapiro.test(log2(obs))
                 text(par("usr")[2] * 1, par("usr")[4] * 0.9,
                      paste0("Shapiro test for normality\n p = ",
                             round(isLogNormal$p.value, 3),
                             "\n(normal unlikely if p < 0.05)"
                             ), pos = 2,
                      col = myPalette[2])
               } else {
                 text(par("usr")[2] * 1, par("usr")[4] * 0.9,
                      "n too small to check for fit", pos = 2,
                      col = myPalette[2])
               }
             }
             if (input$geom && length(obs) > 2) {
                freq <- table(octaves)
                reg <- lm(freq ~ I(as.integer(names(freq)) - 0.5))
                abline(reg, lty = "dashed",
                       col = myPalette[1])
                if (length(freq) > 2L) {
                  text(par("usr")[2] * 1, par("usr")[4] * 0.7,
                       paste0("Adjusted R² = ", round(summary(reg)$adj, 3),
                              "\n(perfect fit if r² = 1)"
                       ), pos = 2, col = myPalette[1])
                }
             }


           }
    )
    for (class in paste0("scale", 0:128)) {
      removeCssClass(class = class, selector = "td.swatch")
    }
    Swatch("n", n(), max(colSums(myData())), 1)
    Swatch("richness", S(), nrow(myData()), 1)
    Swatch("menhinick", menh(), sqrt(n()), 1 / sqrt(n()))
    Swatch("margalef", marg(), (n() - 1) / log(n()))
    Swatch("shannon", equit())
    Swatch("equit", equit())
    Swatch("nMax", nMax() - evens(), n() - S() - evens())
    if (input$correct) {
      Swatch("bpi", bpi(), (n() + 1 - S()) / n(), 1 / S())
      Swatch("simpson", simpson(),
             mx = sum((c(rep(1, S() - 1), n() + 1 - S()) / n()) ^ 2),
             mn = sum(rep(1 / S(), S()) ^ 2))
    } else {
      Swatch("bpi", bpi())
      Swatch("simpson", simpson())
    }
  }

  rScript <- function() {
    paste0(
      "# Read the data\n",
      "# Include the full path to your data file here if necessary:\n",
      "myData <- ", switch(fileExt(), ".csv" = "read.csv",
                           ".txt" = "read.table",
                           ".xls" = "readxl::read_excel",
                           "xlsx" = "readxl::read_excel", "read.csv"),
      "(\"", r$fileName, "\")\n\n# Code not available. Check the source code..."
    )
  }

  output$plot <- renderPlot(makePlot())
  output$code <- renderText(rScript())

  S <- reactive(sum(counts() > 0))
  n <- reactive(sum(counts()))
  evens <- reactive(n() / S())
  nMax <- reactive(max(counts()))
  menh <- reactive(S() / sqrt(n()))
  marg <- reactive((S() - 1) / log(n()))
  bpi <- reactive(nMax() / n())
  pi <- reactive(counts() / n())
  simpson <- reactive(sum(pi() ^ 2))
  log0 <- function (x) ifelse(x == 0, 0, log(x))
  Entropy <- function (p) -sum(p * log0(p))
  shannon <- reactive(Entropy(pi()))
  equit <- reactive({
    mx <- Entropy(rep(1 / S(), S()))
    mn <- Entropy(c(rep(1, S() - 1), n() - S() + 1) / n())
    (shannon() - mn) / (mx - mn)
  })

  output$n <- renderText(n())
  output$nMax <- renderText(nMax())
  output$richness <- renderText(S())
  output$menhinick <- renderText(signif(menh(), 4))
  output$margalef <- renderText(signif(marg(), 4))
  output$bpi <- renderText(signif(bpi(), 4))
  output$simpson <- renderText(signif(simpson(), 4))
  output$shannon <- renderText(signif(shannon(), 4))
  output$equit <- renderText(signif(equit(), 4))

  output$savePng <- downloadHandler(
    filename = "Histogram.png",
    content = function (file) {
      png(file, width = input$pngSize, height = input$pngSize)
      makePlot()
      dev.off()
    })
  output$savePdf <- downloadHandler(
    filename = "Histogram.pdf",
    content = function (file) {
      pdf(file,
          title = paste0("Ternary plot",
                         if(filePath() != "") paste0("  from ", filePath())))
      makePlot()
      dev.off()
    })
  output$saveR <- downloadHandler(
    filename = "Histogram.R",
    content = function (file) {
      writeLines(rScript(), file)
    })

}

shinyApp(ui = ui, server = server)
