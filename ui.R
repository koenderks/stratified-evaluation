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
                         h3("Results"),
                         tableOutput(outputId = "maintable")
                ),
                tabPanel(title = "Method of moments",
                         h3("Results"),
                         tableOutput("momentTable")
                ),
                tabPanel(title = "Weighting",
                         h3("Results"),
                         tableOutput("weightingTable")
                ),
                tabPanel(title = "Multilevel regression with poststratification",
                         h3("Results"),
                         tableOutput(outputId = "postMainTable"),
                         plotOutput(outputId = "postPlot"),
                         tableOutput(outputId = "postStratumTable")
                )
            )
        )
    )
)

shinyUI(ui)
