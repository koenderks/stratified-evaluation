#################################################################################################
############################ Packages ###########################################################
#################################################################################################

if(!"shiny" %in% installed.packages()) 
{ 
    install.packages("shiny") 
}
library(shiny)

if(!"rstanarm" %in% installed.packages()) 
{ 
    install.packages("rstanarm") 
}
library(rstanarm)

if(!"ggplot2" %in% installed.packages()) 
{ 
    install.packages("ggplot2") 
}
library(ggplot2)

if(!"bayesplot" %in% installed.packages()) 
{ 
    install.packages("bayesplot") 
}
library(bayesplot)
theme_set(bayesplot::theme_default())

# options(mc.cores = 4) 

if(!"dplyr" %in% installed.packages()) 
{ 
    install.packages("dplyr") 
}
library(dplyr)

if(!"tidyr" %in% installed.packages()) 
{ 
    install.packages("tidyr") 
}
library(tidyr)

if(!"shinycssloaders" %in% installed.packages()) 
{ 
    install.packages("shinycssloaders") 
}
library(shinycssloaders)

if(!"gridExtra" %in% installed.packages()) 
{ 
    install.packages("gridExtra") 
}
library(gridExtra)

if(!"shinyjs" %in% installed.packages()) 
{ 
    install.packages("shinyjs") 
}
library(shinyjs)

#################################################################################################
############################ Server function ####################################################
#################################################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Define a reactive input field
    contentsrea <- reactive({
        inFile <- input$datafile
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath, header = TRUE)
    })
    
    # Create an observer for the inputs
    observe({
        #updateSelectInput(session, "var1", choices = names(contentsrea()), selected = names(contentsrea())[3])
        updateSelectInput(session, "var2", choices = names(contentsrea()), selected = names(contentsrea())[4])
        updateSelectInput(session, "stratum", choices = names(contentsrea()), selected = names(contentsrea())[2])
    })
    
    output$out <- renderUI({
        numinputs <- lapply(1:10, function(i){
            numericInput(inputId = paste0("n", i), label = paste0("Size stratum ", i), value = 100, min = 1)
        })
        shinyjs::hidden(numinputs)
    })
    
    observeEvent(eventExpr = input$refresh, handlerExpr = {
        n <- seq(length.out = as.numeric(input$num))
        lapply(seq(10), function(i) {
            if(i %in% n) {
                shinyjs::show(id = paste0("n", i))
            } else{
                shinyjs::hide(id = paste0("n", i))
            }
        })
    })
    
    # Initialize main output
    output$maintable <- renderTable({
        x <- data.frame(method = c("No stratification", "Method of moments", "Weighting", "Multilevel regression with poststratification"), x1 = rep(NA, 4), x2 = rep(NA, 4), x3 = rep(NA, 4))
        colnames(x) <- c("", "Expected relative taint", "Std. Deviation taint", "Upper bound relative taint")
        x
    }, striped = TRUE, na = ".")
    
    # Initialize main moment output
    output$momentMainTable <- renderTable(NULL)
    
    # Initialize main weighting output
    output$weightingMainTable <- renderTable(NULL)
    
    # Initialize main post stratification output
    output$postMainTable <- renderTable(NULL)
    output$postPlot <- renderPlot(NULL)
    output$postStratumTable <- renderTable(NULL)
    
    observeEvent(input$update, {
        if(!is.null(input$datafile)){
            
            withProgress(message = 'Running analysis', value = 0, {
                
                steps <- 12
                
                #################################################################################################
                ############################ Read data ##########################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Reading data [1/12]")
                
                df <- read.csv(input$datafile$datapath, header = TRUE)
                #ist <- df[, which(colnames(df) == input$var1)]
                taint <- df[, which(colnames(df) == input$var2)]
                stratum <- df[, which(colnames(df) == input$stratum)]
                #sample <- data.frame(ist = ist, taint = taint, stratum = stratum)
                sample <- data.frame(taint = taint, stratum = stratum)
                
                #################################################################################################
                ############################ Data manipulation ##################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Preparing input [2/12]")
                
                # Filtering on the basis of user-input
                sample <- sample[sample$stratum <= input$num, ]
                
                sizes <- c(input$n1, input$n2, input$n3, input$n4, input$n5)
                sizes <- sizes[1:input$num]
                poststrat <- data.frame(stratum = 1:input$num, N = sizes, N_errors = sum(taint))
                
                #################################################################################################
                ############################ Model fitting ######################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Fitting Method of moments model [3/12]")
                
                momentAlpha <- mean(sample$taint) * ( ( (mean(sample$taint) * (1 - mean(sample$taint))) / var(sample$taint)) - 1)
                momentBeta <- (momentAlpha * (1 - mean(sample$taint))) / mean(sample$taint)
                momentMean <- momentAlpha / (momentAlpha + momentBeta)
                momentBound <- qbeta(p = input$confidence, shape1 = momentAlpha, shape2 = momentBeta)
                
                incProgress(1/steps, detail = "Fitting Weighting model [4/12]")
                
                incProgress(1/steps, detail = "Fitting MRP model [5/12]")
                
                sample_alt <- sample %>% group_by(stratum) %>% summarise(N_errors = sum(taint), N = n()) %>% ungroup()
                fit <- stan_glmer(cbind(N_errors, N - N_errors) ~ (1 | stratum), family = binomial("logit"), data = sample_alt)
                pp <- posterior_predict(fit, newdata = poststrat)
                
                posterior_prob <- posterior_epred(fit, newdata = poststrat)
                poststrat_prob <- posterior_prob %*% poststrat$N / sum(poststrat$N)
                
                postMean <- round(mean(poststrat_prob), 3)
                postBound <- as.numeric(quantile(poststrat_prob, probs = input$confidence))
                
                #################################################################################################
                ############################ All methods table ##################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Rendering main results table [6/12]")
                
                output$maintable <- renderTable({
                    
                    table <- data.frame(method = "No stratification", mle = round(mean(sample$taint), 3), ub = qbeta(input$confidence, 1 + sum(sample$taint), 1 + length(sample$taint) - sum(sample$taint)))
                    table <- rbind(table, data.frame(method = "Method of moments", mle = momentMean, ub = momentBound))
                    table <- rbind(table, data.frame(method = "Weighting", mle = -1, ub = -1))
                    table <- rbind(table, data.frame(method = "Multilevel regression with poststratification", mle = postMean, ub = postBound))
                    colnames(table) <- c("", "Expected relative taint", "Upper bound relative taint")
                    table
                    
                }, striped = T, na = ".")
                
                #################################################################################################
                ############################ MRP tables and figures #############################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Rendering MRP main table [7/12]")
                
                output$mrpMainTable <- renderTable({
                    
                    df <- data.frame(name = "Multilevel regression with poststratification", mean = round(mean(poststrat_prob), 3), sd = round(sd(poststrat_prob), 3), bound = as.numeric(quantile(poststrat_prob, probs = input$confidence)))
                    colnames(df) <- c("", "Expected relative taint", "Std. Deviation taint", "Upper bound relative taint")
                    df

                }, striped = T, na = ".")
                
                incProgress(1/steps, detail = "Rendering MRP main figure [8/12]")
                
                output$mrpPosteriorPredictive <- renderPlot({
                    
                    dat <- as.data.frame(table(as.numeric(unlist(pp))), stringsAsFactors = F)
                    dat$Var1 <- as.numeric(dat$Var1)
                    yBreaks <- pretty(c(0, max(dat$Freq)), min.n = 4)
                    p1 <- ggplot(data = dat, mapping = aes(x = Var1, y = Freq)) +
                        geom_bar(fill = "darkred", colour = "black", stat = "identity") +
                        labs(title = "Posterior predictive distribution of the error (beta-binomial)") +
                        scale_y_continuous(name = "", limits = range(yBreaks), breaks = yBreaks) +
                        scale_x_continuous(name = "Predicted errors in population") +
                        theme_bw() +
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank())
                    
                    dat <- data.frame(x = as.numeric(unlist(poststrat_prob)))
                    p2 <- ggplot(data = dat, mapping = aes(x = x)) +
                        geom_density(fill = "darkred", colour = "black", alpha = 0.8) +
                        labs(title = "Posterior distribution of the error (linear predictor)") +
                        scale_y_continuous(name = "") +
                        scale_x_continuous(name = "Error probability in population") +
                        theme_bw() +
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank())
                    
                    p <- grid.arrange(p2, p1, nrow = 1)
                })    
                
                incProgress(1/steps, detail = "Creating MRP sub table [9/12]")
                
                stratumtable <- data.frame(
                    stratum = 1:length(unique(sample$stratum)),
                    N = rep(-1, length(unique(sample$stratum)),
                            stratum_sample = rep(-1, length(unique(sample$stratum))),
                            stratum_sample_sd = rep(-1, length(unique(sample$stratum))),
                            stratum_population_n = rep(-1, length(unique(sample$stratum))),
                            stratum_est = rep(-1, length(unique(sample$stratum))),
                            stratum_sd = rep(-1, length(unique(sample$stratum))),
                            stratum_ub = rep(-1, length(unique(sample$stratum))))
                )
                
                for(i in 1:length(levels(as.factor(poststrat$stratum)))) {
                    poststrat_stratum <- poststrat[poststrat$stratum == i, ]
                    posterior_prob_stratum <- posterior_epred(fit,draws = 1000, newdata = as.data.frame(poststrat_stratum))
                    poststrat_prob_stratum <- (posterior_prob_stratum %*% poststrat_stratum$N) / sum(poststrat_stratum$N)
                    
                    stratumtable$N[i] <- as.integer(length(sample$taint[sample$stratum == i]))
                    stratumtable$stratum_sample[i] <- round(mean(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_sample_sd[i] <- round(sd(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_population_n <- sizes
                    stratumtable$stratum_est[i] <- round(mean(poststrat_prob_stratum), 3)
                    stratumtable$stratum_sd[i] <- round(sd(poststrat_prob_stratum), 3)
                    stratumtable$stratum_ub[i] <- round(quantile(posterior_prob_stratum, probs = input$confidence), 3) # Upper bound
                }
                
                incProgress(1/steps, detail = "Rendering MRP sub figure [10/12]")
                
                output$mrpComparison <- renderPlot({
                    
                    taint_by_stratum <- sample %>%
                        group_by(stratum) %>%
                        summarise(y_mean = mean(taint), y_sd = sqrt(mean(taint) * (1 - mean(taint)) / n())) %>%
                        ungroup()
                    
                    yBreaks <- pretty(c(0, 0.5), min.n = 4)
                    
                    compare <- ggplot(data=taint_by_stratum, aes(x=stratum, y=y_mean, group=1)) +
                        geom_ribbon(aes(ymin=y_mean-y_sd,ymax=y_mean+y_sd,x=stratum),fill='lightgrey',alpha=.7)+
                        geom_line(aes(x=stratum, y=y_mean))+
                        geom_point()+
                        scale_y_continuous(breaks = yBreaks, limits= range(yBreaks))+
                        scale_colour_manual(values=c('#1f78b4','#33a02c','#e31a1c','#ff7f00',
                                                     '#8856a7')) + 
                        geom_point(data=stratumtable, mapping=aes(x=stratum, y=stratum_est),
                                   inherit.aes=TRUE,colour='darkred')+
                        geom_line(data=stratumtable, mapping=aes(x=stratum, y=stratum_est,group=1),
                                  inherit.aes=TRUE,colour='darkred')+
                        geom_ribbon(data=stratumtable,mapping=aes(x=stratum,ymin=stratum_est-stratum_sd,
                                                                  ymax=stratum_est+stratum_sd,group=1),
                                    inherit.aes=FALSE,fill='darkred',alpha=.3) +
                        theme_bw()+
                        labs(x="Stratum",y="Taint")+
                        theme(legend.position="none",
                              axis.title=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.text.x=element_text(size=10),
                              legend.title=element_text(size=10),
                              legend.text=element_text(size=10))
                    
                    compare2 <- ggplot()+
                        geom_hline(yintercept = mean(sample$taint),size=.8)+
                        geom_text(aes(x = 5.2, y = mean(sample$taint)+.025, label = "No stratification"), size = 2.5)+
                        scale_x_continuous(name = "Population") +
                        scale_y_continuous(name = "", breaks = yBreaks, limits= range(yBreaks))+
                        geom_hline(yintercept = mean(poststrat_prob), colour = 'darkred', size = 1) +
                        geom_text(aes(x = 5.2, y = mean(poststrat_prob) + .025), label = "MRP", colour = 'darkred') +
                        theme_bw()+
                        theme(legend.position="none",
                              axis.text.y=element_text(size=10),
                              axis.text.x=element_text(colour="white"),
                              legend.title=element_text(size=10),
                              legend.text=element_text(size=10),
                              axis.ticks.x = element_blank())
                    
                    p <- bayesplot_grid(compare,compare2, grid_args = list(nrow=1, widths = c(8,2)))
                    p
                })
                
                incProgress(1/steps, detail = "Rendering MRP sub table [11/12]")
                
                output$mrpStratumTable <- renderTable({
                    colnames(stratumtable) <- c("Stratum", "Sample size", "Mean taint", "SD", "Population", "Predicted mean", "Predicted sd", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                    stratumtable
                }, striped = T, na = ".")
                
                incProgress(1/steps, detail = "Rendering MRP additional plots [12/12]")
                
                output$mrpPosteriorPredictives <- renderPlot({
                    plotList <- list()
                    for(i in 1:length(unique(sample$stratum))){
                        dat <- as.data.frame(table(pp[, i]), stringsAsFactors = F)
                        dat$Var1 <- as.numeric(dat$Var1)
                        yBreaks <- pretty(c(0, max(dat$Freq)), min.n = 4)
                        plotList[[i]] <- ggplot(data = dat, mapping = aes(x = Var1, y = Freq)) +
                            geom_bar(fill = "darkred", colour = "black", stat = "identity") +
                            labs(title = paste0("Posterior predictive distribution for stratum: ", i, " (beta-binomial)")) +
                            scale_y_continuous(name = "", limits = range(yBreaks), breaks = yBreaks) +
                            scale_x_continuous(name = "Predicted errors in stratum") +
                            theme_bw() +
                            theme(axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank())
                    }
                    n <- length(plotList)
                    nCol <- floor(sqrt(n))
                    do.call("grid.arrange", c(plotList, ncol=nCol))
                })
            })
        }
    })
}

shinyServer(server)
