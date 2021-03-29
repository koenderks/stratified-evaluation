#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Custom packages
library(rstanarm)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())
# options(mc.cores = 4) 
library(dplyr)
library(tidyr)
library(gridExtra)
library(shinycssloaders)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    contentsrea <- reactive({
        inFile <- input$datafile
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath, header = TRUE)
    })
    observe({
        updateSelectInput(session, "var1", choices = names(contentsrea()), selected = names(contentsrea())[3])
        updateSelectInput(session, "var2", choices = names(contentsrea()), selected = names(contentsrea())[4])
        updateSelectInput(session, "stratum", choices = names(contentsrea()), selected = names(contentsrea())[2])
    })
    
    output$maintable <- momentMainTable <- renderTable(NULL)
    output$momentMainTable <- renderTable(NULL)
    output$weightingMainTable <- renderTable(NULL)
    output$postMainTable <- renderTable(NULL)
    output$postPlot <- renderPlot(NULL)
    output$postStratumTable <- renderTable(NULL)
    
    observeEvent(input$update, {
        if(!is.null(input$datafile)){
            
            withProgress(message = 'Analyzing', value = 0, {
                
                steps <- 5
                
                #################################################################################################
                ############################ Read data ##########################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Reading data [1/5]")
                
                df <- read.csv(input$datafile$datapath, header = TRUE)
                ist <- df[, which(colnames(df) == input$var1)]
                taint <- df[, which(colnames(df) == input$var2)]
                stratum <- df[, which(colnames(df) == input$stratum)]
                sample <- data.frame(ist = ist, taint = taint, stratum = stratum)
                
                #################################################################################################
                ############################ Model fitting ######################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Fitting model [2/5]")
                
                fit <- stan_glmer(formula = taint ~ (1 | stratum), family = binomial(link = "logit"), data = sample)
                
                poststrat <- data.frame(stratum = 1:length(unique(sample$stratum)), N = rep(1000, length(unique(sample$stratum)) ))
                
                posterior_prob <- posterior_linpred(fit, transform = TRUE, newdata = poststrat)
                poststrat_prob <- posterior_prob %*% poststrat$N / sum(poststrat$N)
                
                #################################################################################################
                ############################ Visualization ######################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Visualizing data [3/5]")
                
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
                    scale_x_continuous(name = "", labels = NULL) + 
                    scale_y_continuous(breaks = yBreaks, limits= range(yBreaks))+
                    theme_bw()+
                    labs(x="Population",y="")+
                    theme(legend.position="none",
                          axis.text.y=element_text(size=10),
                          axis.text.x=element_text(size = 10),
                          legend.title=element_text(size=10),
                          legend.text=element_text(size=10))
                
                #################################################################################################
                ############################ Main table #########################################################
                #################################################################################################
                
                output$maintable <- renderTable({
                    
                    incProgress(1/steps, detail = "Rendering main table [4/5]")
                    
                    table <- data.frame(method = "No stratification", mle = round(mean(sample$taint), 3), sd = round(sd(sample$taint), 3), ub = qbeta(input$confidence, 1 + sum(sample$taint), 1 + length(sample$taint) - sum(sample$taint)))
                    table <- rbind(table, data.frame(method = "Method of moments", mle = -1, sd = -1, ub = -1))
                    table <- rbind(table, data.frame(method = "Weighting", mle = -1, sd = -1, ub = -1))
                    table <- rbind(table, data.frame(method = "Multilevel regression with poststratification", mle = round(mean(poststrat_prob), 3), sd = round(sd(poststrat_prob), 3), ub = -1))
                    colnames(table) <- c("", "Expected relative taint", "Std. Deviation taint", "Upper bound relative taint")    
                    table
                    
                })
                
                #################################################################################################
                ############################ Individual strata table ############################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Rendering additional tables [5/5]")
                
                stratumtable <- data.frame(
                    stratum = 1:length(unique(sample$stratum)),
                    N = rep(-1, length(unique(sample$stratum)),
                            stratum_sample = rep(-1, length(unique(sample$stratum))),
                            stratum_sample_sd = rep(-1, length(unique(sample$stratum))),
                            stratum_est = rep(-1, length(unique(sample$stratum))),
                            stratum_sd = rep(-1, length(unique(sample$stratum))))
                )
                
                for(i in 1:length(levels(as.factor(poststrat$stratum)))) {
                    poststrat_stratum <- poststrat[poststrat$stratum == i, ]
                    posterior_prob_stratum <- posterior_linpred(
                        fit,
                        transform = TRUE,
                        draws = 1000,
                        newdata = as.data.frame(poststrat_stratum)
                    )
                    poststrat_prob_stratum <- (posterior_prob_stratum %*% poststrat_stratum$N) / sum(poststrat_stratum$N)
                    stratumtable$N[i] <- as.integer(length(sample$taint[sample$stratum == i]))
                    stratumtable$stratum_sample[i] <- round(mean(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_sample_sd[i] <- round(sd(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_est[i] <- round(mean(poststrat_prob_stratum), 3)
                    stratumtable$stratum_sd[i] <- round(sd(poststrat_prob_stratum), 3)
                }
                
                output$postStratumTable <- renderTable({
                    colnames(stratumtable) <- c("Stratum", "Size", "Sample mu", "Sample sigma", "Model mu", "Model sigma")
                    stratumtable
                })
                
                #################################################################################################
                ############################ Additions to figure ################################################
                #################################################################################################
                
                output$postPlot <- renderPlot({
                    compare <- compare +
                        geom_point(data=stratumResults, mapping=aes(x=stratum, y=stratum_est),
                                   inherit.aes=TRUE,colour='#238b45')+
                        geom_line(data=stratumResults, mapping=aes(x=stratum, y=stratum_est,group=1),
                                  inherit.aes=TRUE,colour='#238b45')+
                        geom_ribbon(data=stratumResults,mapping=aes(x=stratum,ymin=stratum_est-stratum_sd,
                                                                    ymax=stratum_est+stratum_sd,group=1), 
                                    inherit.aes=FALSE,fill='#2ca25f',alpha=.3)
                    
                    compare2 <- compare2 +
                        geom_hline(yintercept = mean(poststrat_prob), colour = '#2ca25f', size = 1) +
                        geom_text(aes(x = 5.2, y = mean(poststrat_prob) - .025), label = "MRP", colour = '#2ca25f')
                    
                    p <- bayesplot_grid(compare,compare2, grid_args = list(nrow=1, widths = c(8,2)))
                    p
                })
                
            })
        }
    })
}

shinyServer(server)
