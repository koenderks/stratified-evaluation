#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Stratified evaluation of audit samples"),
    sidebarLayout(
        sidebarPanel(
            
            p("This is an application exploring stratification in an audit setting. Upload some data and inspect your sample. Your file must be in .csv format to upload, have ';' as the cell separator and use ',' as a decimal separator."),
            
            fileInput(inputId = "datafile", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
            
            selectInput(inputId = "var1", label = "Ist variable", choices = ""),
            selectInput(inputId = "var2", label = "Taint variable", choices = ""),
            selectInput(inputId = "stratum", label = "Stratum variable", choices = ""),
            
            sliderInput("confidence", "Confidence for upper bound:",
                        min = 0.8, max = 0.999,
                        value = 0.95, step = 0.01),
            
            actionButton(inputId = "update", label = "Go!", icon = icon("cat"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = "All", 
                         icon = icon("boxes"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "maintable"), color = "darkred", type = 4, size = 1)
                ),
                tabPanel(title = "Method of moments",
                         icon = icon("ruler-combined"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "momentMainTable"), color = "darkred", type = 4, size = 1)
                ),
                tabPanel(title = "Weighting",
                         icon = icon("balance-scale-right"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "weightingMainTable"), color = "darkred", type = 4, size = 1)
                ),
                tabPanel(title = "Multilevel regression with poststratification",
                         icon = icon("layer-group"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "postMainTable"), color = "darkred", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "postPlot"), color = "darkred", type = 4, size = 1),
                         shinycssloaders::withSpinner(tableOutput(outputId = "postStratumTable"), color = "darkred", type = 4, size = 1)
                )
            )
        )
    )
)

shinyUI(ui)
