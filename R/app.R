# LOAD LIBRARIES ------------------
library(tidyverse)
library(shiny)
library(shinydashboard)
library(pdftools)
library(png)
library(svglite)
library(ggExtra)

#LOAD FUNCTIONS -------------------
source("script/PdfToDat.R")
source("script/PdfToDatSmpl.R")
source("script/PlotFitDiag.R")
source("script/ExtractRSq.R")
source("script/PlotStdCurve.R")
source("script/PlotControls.R")
source("script/ControlStats.R")
source("script/PlotAll.R")
source("script/ExtractQualityData.R")

# UI ------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "HPLC Data Extractor"),
  dashboardSidebar(
    fileInput("file", label = h3("Control File input")),
    fileInput("file2", label = h3("Sample File input")),
    sidebarMenu(
      menuItem("Main", tabName = "Main", icon = icon("dashboard")),
      menuItem("PNG vs SVG", tabName = "Grandstand", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Main",
        box(title = "Extracted Data",
            status = "success",
            solidHeader = TRUE,
            h4("Standard Curve",align = "center"),
            tableOutput("tbl"),
            downloadButton('dl_std', label = "Download Data"),
            h4("Sample Results",align = "center"),
            tableOutput("tbl_smps"),
            downloadButton('dl_smp', label = "Download Data")
        ),
        tabBox(title="Standard Curve",id="tabset",
               tabPanel("Main",
                          plotOutput("plot")
                        ),
               tabPanel("Fit Diagnostics",
                        plotOutput("plot2")
               ),
               tabPanel("Assay Control",
                          plotOutput("plot_ac")
                        ),
               tabPanel("Quality Data",
                          tableOutput("tblq")
                        )
        ),
        box(title = "Data on Standard Curve", 
            status = "success", 
            solidHeader = TRUE,
            plotOutput("plot_all"),
            downloadButton('dl_plot', label = "Download PNG"),
            downloadButton('dl_plot2', label = "Download SVG")
        ),
        downloadButton("report", "Generate report")
      ),
      tabItem(tabName = "Grandstand",
              fluidRow(
                box(title = "Example PNG", 
                    status = "primary", 
                    solidHeader = FALSE,
                    img(src = "pngex.png")
                )
                ),
              
              fluidRow(
                box(title = "Example SVG", 
                    status = "primary", 
                    solidHeader = FALSE,
                    img(src = "svgex.png",height=750,width=1630)
              )
              )
      )
  )
)
)


# SERVER -------------------------------------
server <- function(input, output, session) {
  
  output$tbl <- renderTable(PdfToDat(
    input$file
    ))
  output$plot <- renderPlot(
    PlotStdCurve(
        PdfToDat(input$file)
    )
  )
  output$plot2 <- renderPlot(
    PlotFitDiag(
      PdfToDat(input$file)
    )
  )
  output$rsq <- renderText(ExtractRSq(
    filter(PdfToDat(input$file),Group == "STD")
    ))
  output$plot_ac <- renderPlot(PlotControls(
    PdfToDat(input$file)
  ))
  output$rel_er <- renderText(
    ControlStats(
      filter(PdfToDat(input$file),Group == "CNTR")
    )[[1]]
  )
  output$rel_sd <- renderText(
    ControlStats(
      filter(PdfToDat(input$file),Group == "CNTR")
    )[[2]]
  )
  output$tbl_smps <- renderTable(
    PdfToDatSmpl(input$file2)
  )
  
  output$plot_all <- renderPlot(
        PlotAll(
          PdfToDat(input$file),
          PdfToDatSmpl(input$file2)
        )
      )
  output$dl_std <- downloadHandler(
      filename = function() {
        paste('std_data','.csv', sep='')
      },
      content = function(file) {
        write.csv(filter(PdfToDat(input$file),Group == "STD"), file)
      }
    )
  output$dl_cnt <- downloadHandler(
    filename = function() {
      paste('cntr_data', '.csv', sep='')
    },
    content = function(file) {
      write.csv(filter(PdfToDat(input$file), file))
    }
  )
  output$dl_smp <- downloadHandler(
    filename = function() {
      paste('smp_data', '.csv', sep='')
    },
    content = function(file) {
      write.csv(filter(PdfToDat(input$file),Group == "STD"), file)
    }
  )
  output$dl_plot <- downloadHandler(
    filename = function() {
      paste("plot.png")
    },
    content = function(file) {
      ggsave(file, device = "png", PlotAllSamples(
               filter(PdfToDat(input$file),Group == "STD"),
               filter(PdfToDat(input$file),Group == "CNTR"),
               PdfToDatSmpl(input$file2)
             )
             )
    }
  )
  output$dl_plot2 <- downloadHandler(
    filename = function() {
      paste("plot.png")
    },
    content = function(file) {
      ggsave(file, device = "svg", PlotAllSamples(
        filter(PdfToDat(input$file),Group == "STD")(input$file),
        filter(PdfToDat(input$file),Group == "CNTR"),
        PdfToDatSmpl(input$file2)
      )
      )
    }
  )
  output$tblq <- renderTable(digits=3,
    ExtractQualityData(
      PdfToDat(input$file)
    )
  )
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      params <- list(n = input$file, p = input$file2)
      rmarkdown::render("report_template.Rmd", 
                        output_file = file, 
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
    }
  )
}

shinyApp(ui, server)