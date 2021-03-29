if(!"shiny" %in% installed.packages()) 
{ 
    install.packages("shiny") 
}
library(shiny)

if(!"shinyjs" %in% installed.packages()) 
{ 
    install.packages("shinyjs") 
}
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    useShinyjs(),
    
    tags$head(tags$style(
        HTML('
                    #title{
                      color: black;
                      font-size: 30px;
                      font-style: italic;
                    }
                    #text1{
                      color: white;
                    }
                    #datafile{
                      font-color: white;
                    }
                                 
                   #sidebar {
                   background-color: #1e90ff;
                   }
                   
.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: black}
.js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: black}
                   
                   body, label, input, button, select { 
                   font-family: "Arial";
                   }')
    )),
    
    titlePanel(title = "", windowTitle = "Stratification"),
    p(id = "title", "Stratified evaluation of audit samples"),
    
    sidebarLayout(
        
        sidebarPanel(
            id="sidebar",
            
            p(id = "text1", "This is an application exploring stratification in an audit setting. Upload some data and inspect your sample. Your file must be in .csv format to upload, have ';' as the cell separator and use ',' as a decimal separator."),
            
            fileInput(inputId = "datafile", label = "Upload a data file", multiple = FALSE, placeholder = "No file selected", accept = "csv", buttonLabel = "Step 1: Data"),
            
            #selectInput(inputId = "var1", label = "Ist variable", choices = ""),
            selectInput(inputId = "var2", label = "Taint variable", choices = ""),
            selectInput(inputId = "stratum", label = "Stratum variable", choices = ""),
            
            sliderInput(inputId = "confidence", label = "Confidence for upper bound:",
                        min = 0.8, 
                        max = 0.999,
                        value = 0.95, 
                        step = 0.01),
            sliderInput(inputId = "iter", label = "Number of iterations:", 
                        value = 2000, 
                        min = 500, 
                        max = 5000, 
                        step = 100),
            
            numericInput(inputId = "num", label = "Number of strata", value = 2, min = 2),
            
            uiOutput(outputId = "out"),
            
            actionButton(inputId = "refresh", label = "Step 2: Stratum sizes", icon = icon("cat")),
            actionButton(inputId = "update", label = "Step 3: Analyze", icon = icon("cat"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = "All methods", 
                         icon = icon("boxes"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "maintable"), color = "#FF4500", type = 4, size = 1)
                ),
                tabPanel(title = "Method of moments",
                         icon = icon("ruler-combined"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "momentMainTable"), color = "#FF4500", type = 4, size = 1)
                ),
                tabPanel(title = "Weighting",
                         icon = icon("balance-scale-right"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "weightingMainTable"), color = "#FF4500", type = 4, size = 1)
                ),
                tabPanel(title = "MRP",
                         icon = icon("layer-group"),
                         h3("Inference on the population"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpMainTable"), color = "#FF4500", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorPredictive"), color = "#FF4500", type = 4, size = 1),
                         h3("Inference on individual strata"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpStratumTable"), color = "#FF4500", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpComparison"), color = "#FF4500", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorPredictives"), color = "#FF4500", type = 4, size = 1)
                )
            )
        )
    )
)

shinyUI(ui)
