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
        tags$style(type = "text/css", "a{color: #000000;}")
    ),
    
    tags$head(tags$style(
        HTML('
                    #title{
                      color: black;
                      font-size: 30px;
                      font-style: italic;
                    }
                    
                    #text1{
                      font-size: 12px;
                    }
                    #text2{
                      font-size: 12px;
                    }
                    #text3{
                      font-size: 12px;
                    }
                    #text4{
                      font-size: 12px;
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
            
            p(id = "text1", "Step 1: Upload your data file via the Browse button. Your data must be in CSV (.csv) format, have a comma (,) as the cell separator, and have a dot (.) as a decimal separator."),
            p(id = "text2", "Step 2: Select the taint variable and the stratum variable."),
            p(id = "text3", "Step 3: Adjust the population sizes of the strata at the bottom."),
            p(id = "text4", "Step 4: Click run and compare the various evaluation methods."),
            br(),
            fileInput(inputId = "datafile", label = "Data file", multiple = FALSE, placeholder = "No file selected", accept = "csv"),
            
            selectInput(inputId = "var2", label = "Taint variable (0 or 1)", choices = ""),
            selectInput(inputId = "stratum", label = "Stratum variable (1, 2, 3, ...)", choices = ""),
            
            sliderInput(inputId = "confidence", label = "Confidence for upper bound",
                        min = 0.8, 
                        max = 0.999,
                        value = 0.95, 
                        step = 0.05),
            
            sliderInput(inputId = "iter", label = "Number of MCMC samples", 
                        value = 5000, 
                        min = 1000, 
                        max = 10000, 
                        step = 500),
            
            div(style = "display:inline-block; float:right", actionButton(inputId = "update", label = "Run!", icon = icon("running"))),
            br(),
            br(),
            uiOutput(outputId = "out")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(title = "Overview", 
                         icon = icon("cloudsmith"),
                         h3("Descriptive Statistics"),
                         p("The tables below displays the input and some descriptive statistics for the population."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "descriptivesTable"), color = "black", type = 4, size = 1),
                         p("The table below displays some descriptive statistics of the sample split by stratum."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "stratumDescriptivesTable"), color = "black", type = 4, size = 1),
                         h3("Compare Evaluations"),
                         p("The table and figure below displays a comparison of all evaluation methods with respect to their expected relative error and upper bound."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "maintable"), color = "black", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "comparison"), color = "black", type = 4, size = 1)
                ),
                tabPanel(title = "1. No Stratification",
                         icon = icon("circle"),
                         h3("1. No Stratification"),
                         p("In the no stratification approach all information about the available strata is discarded and the inference is solely made on the basis of the observed taints."),
                         h3("1.1 Inference on the Population"),
                         p("Inference on the population can be performed using the posterior distribution on the taintings."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "noMainTable"), color = "#4682b4", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "noMainFigure"), color = "#4682b4", type = 4, size = 1)
                ),
                tabPanel(title = "2. Method of Moments",
                         icon = icon("confluence"),
                         h3("2. Method of Moments"),
                         p("The method of moments procedure aggregates the mean and variance of the beta distributions of the individual strata and computes the parameters of the (beta) group distribution using the moment estimator (Stewart 2013)."),
                         h3("2.1 Inference on the Population"),
                         p("Inference on the population can be performed using the moment-aggregated posterior distribution."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "momentMainTable"), color = "darkred", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "momentMainFigure"), color = "darkred", type = 4, size = 1),
                         h3("2.2 Inference on Individual Strata"),
                         p("Inference on the individual strata can be performed using the posterior distributions for each stratum."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "momentStratumTable"), color = "darkred", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "momentStratumFigure"), color = "darkred", type = 4, size = 1),
                         h3("2.3 Predictions for Individual Strata"),
                         p("Predictions for the individual strata can be made using the posterior predictive distributions for each stratum."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "momentStratumPredictions"), color = "darkred", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "momentPredictionsFigure"), color = "darkred", type = 4, size = 1)
                ),
                tabPanel(title = "3. Weighting",
                         icon = icon("balance-scale-right"),
                         h3("3. Weighting"),
                         p("The weighting method assigns a weight to each stratum according to how much of the stratum is seen in the sample. The taints in each stratum are counted according to these weights to form the weighted posterior distribution."),
                         h3("3.1 Inference on the Population"),
                         p("Inference on the population can be performed using the weighted posterior distribution."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "weightingMainTable"), color = "#008000", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "weightingMainFigure"), color = "#008000", type = 4, size = 1),
                         h3("3.2 Inference on Individual Strata"),
                         p("Inference on the population can be performed using the weighted posterior distribution for each stratum."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "weightingStratumTable"), color = "#008000", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "weightingStratumFigure"), color = "#008000", type = 4, size = 1),
                         h3("3.3 Predictions for Individual Strata"),
                         p("Predictions for the individual strata can be made using the weighted posterior predictive distributions for each stratum."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "weightingStratumPredictions"), color = "#008000", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "weightingPredictionsFigure"), color = "#008000", type = 4, size = 1)
                ),
                tabPanel(title = "4. MRP",
                         icon = icon("layer-group"),
                         h3("4. Multilevel Regression with Poststratification (MRP)"),
                         p("Poststratification is a technique for adjusting a non-representative sample (i.e., a convenience sample or other observational data) for which there are certain predictors characterizing the various strata. It is carried out after a model is fit to the observed data, hence the name poststratification. Poststratification can be fruitfully combined with regression modeling (or more general parametric modeling), which provides estimates based on combinations of predictors (or general parameters) rather than raw counts in each stratum. Multilevel modeling is useful in determining how much partial pooling to apply in the regressions, leading to the popularity of the combination of multilevel regression and poststratification (MRP)."),
                         h3("4.1 Inference on the Population"),
                         p("Inference on the population can be performed using the samples of the posterior distributions for the strata."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpMainTable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorPredictive"), color = "#FFB682", type = 4, size = 1),
                         h3("4.2 Inference on Individual Strata"),
                         p("Inference on the probability of misstatement in each stratum can be performed using the posterior distributions for the linear parameter."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpStratumTable"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorDistributions"), color = "#FFB682", type = 4, size = 1),
                         h3("4.3 Predictions for Individual Strata"),
                         p("Predictions for the errors in each stratum can be made using the posterior predictive distributions for each stratum."),
                         shinycssloaders::withSpinner(tableOutput(outputId = "mrpStratumPredictions"), color = "#FFB682", type = 4, size = 1),
                         shinycssloaders::withSpinner(plotOutput(outputId = "mrpPosteriorPredictives"), color = "#FFB682", type = 4, size = 1)
                )
            )
        )
    )
)

shinyUI(ui)
