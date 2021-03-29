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
    
    tags$head(
        tags$style(type = "text/css", "a{color: #008b27;}")
    ),
    
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
            
            p(id = "text1", "This is an application exploring stratification in an audit setting. Upload your data via the Browse button and compare the different methods. Your data must be in .csv format, have ',' as the cell separator, and use '.' as a decimal separator."),
            
            fileInput(inputId = "datafile", label = "Data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
                     
            selectInput(inputId = "var2", label = "Taint variable", choices = ""),
            selectInput(inputId = "stratum", label = "Stratum variable", choices = ""),
            
            sliderInput(inputId = "confidence", label = "Confidence for upper bound:",
                        min = 0.8, 
                        max = 0.999,
                        value = 0.95, 
                        step = 0.01),
            
            sliderInput(inputId = "iter", label = "Number of iterations", 
                        value = 5000, 
                        min = 500, 
                        max = 10000, 
                        step = 100),
            
            actionButton(inputId = "update", label = "Run!", icon = icon("running")),
            br(),
            br(),
            uiOutput(outputId = "out")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = "Compare", 
                         icon = icon("cloudsmith"),
                         h3("All Methods"),
                         p("The table below displays descriptive statistics of the population and sample."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "descriptivesTable"), color = "#FFB682", type = 4, size = 1),
                         p("The table and figure below displays a comparison of all methods with respect to their most likely error and upper bound."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "maintable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "comparison"), color = "#FFB682", type = 4, size = 1)
                ),
                tabPanel(title = "Method of Moments",
                         icon = icon("confluence"),
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
                         h3("Multilevel Regression with Poststratification (MRP)"),
                         p("Poststratification is a technique for adjusting a non-representative sample (i.e., a convenience sample or other observational data) for which there are demographic predictors characterizing the strata. It is carried out after a model is fit to the observed data, hence the name poststratification (Little 1993). Poststratification can be fruitfully combined with regression modeling (or more general parametric modeling), which provides estimates based on combinations of predictors (or general parameters) rather than raw counts in each stratum. Multilevel modeling is useful in determining how much partial pooling to apply in the regressions, leading to the popularity of the combination of multilevel regression and poststratification (MRP) (Park, Gelman, and Bafumi 2004)."),
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
