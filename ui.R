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
                    
                    #datafile{
                      font-color: white;
                    }
                                 
                   #sidebar {
                   background-color: #82CAFF;
                   }
                   
.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FFB682}
.js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #FFB682}
                   
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
            
            fileInput(inputId = "datafile", label = "", multiple = FALSE, placeholder = "No file selected", accept = "csv", buttonLabel = "Step 1: Upload data"),
   
            sliderInput(inputId = "confidence", label = "Confidence for upper bound:",
                        min = 0.8, 
                        max = 0.999,
                        value = 0.95, 
                        step = 0.01),
                     
            selectInput(inputId = "var2", label = "Taint variable", choices = ""),
            selectInput(inputId = "stratum", label = "Stratum variable", choices = ""),
            
            sliderInput(inputId = "iter", label = "Number of iterations:", 
                        value = 5000, 
                        min = 500, 
                        max = 10000, 
                        step = 100),
            
            numericInput(inputId = "num", label = "Number of strata", value = 2, min = 2, max = 10),
            
            uiOutput(outputId = "out"),
            
            actionButton(inputId = "refresh", label = "Step 2: Stratum sizes", icon = icon("shapes")),
            actionButton(inputId = "update", label = "Step 3: Run!", icon = icon("running"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = "All methods", 
                         icon = icon("boxes"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "descriptivesTable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(tableOutput(outputId = "maintable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "comparison"), color = "#FFB682", type = 4, size = 1)
                ),
                tabPanel(title = "Method of moments",
                         icon = icon("ruler-combined"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "momentMainTable"), color = "#FFB682", type = 4, size = 1)
                ),
                tabPanel(title = "Weighting",
                         icon = icon("balance-scale-right"),
                         h3("Results"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "weightingMainTable"), color = "#FFB682", type = 4, size = 1)
                ),
                tabPanel(title = "MRP",
                         icon = icon("layer-group"),
                         h3("Inference on the population"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpMainTable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorPredictive"), color = "#FFB682", type = 4, size = 1),
                         h3("Inference on individual strata"),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpStratumTable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorPredictives"), color = "#FFB682", type = 4, size = 1)
                )
            )
        )
    )
)

shinyUI(ui)
