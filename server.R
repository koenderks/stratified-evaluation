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

                steps <- 7

                #################################################################################################
                ############################ Read data ##########################################################
                #################################################################################################

                incProgress(1/steps, detail = "Reading data [1/7]")

                df <- read.csv(input$datafile$datapath, header = TRUE)
                #ist <- df[, which(colnames(df) == input$var1)]
                taint <- df[, which(colnames(df) == input$var2)]
                stratum <- df[, which(colnames(df) == input$stratum)]
                #sample <- data.frame(ist = ist, taint = taint, stratum = stratum)
                sample <- data.frame(taint = taint, stratum = stratum)

                #################################################################################################
                ############################ Model fitting ######################################################
                #################################################################################################

                incProgress(1/steps, detail = "Fitting model [2/7]")

                fit <- stan_glmer(formula = taint ~ (1 | stratum), family = binomial(link = "logit"), data = sample, iter = input$iter)

                sizes <- c(input$n1, input$n2, input$n3, input$n4, input$n5)
                sizes <- sizes[1:length(unique(sample$stratum))]
                poststrat <- data.frame(stratum = 1:length(sizes), N = sizes)

                posterior_prob <- posterior_epred(fit, newdata = poststrat)
                poststrat_prob <- posterior_prob %*% poststrat$N / sum(poststrat$N)

                # For posterior predictives

                incProgress(1/steps, detail = "Computing posterior predictive [3/7]")

                sample_alt <- sample %>%
                    group_by(stratum) %>%
                    summarise(N_errors = sum(taint), N = n()) %>%
                    ungroup()

                fit_alt <- stan_glmer(cbind(N_errors, N - N_errors) ~ (1 | stratum), family = binomial("logit"), data = sample_alt)
                pp <- posterior_predict(fit_alt)

                #################################################################################################
                ############################ Visualization ######################################################
                #################################################################################################

                incProgress(1/steps, detail = "Visualizing data [4/7]")

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
                    scale_x_continuous(name = "Population") +
                    scale_y_continuous(name = "", breaks = yBreaks, limits= range(yBreaks))+
                    theme_bw()+
                    theme(legend.position="none",
                          axis.text.y=element_text(size=10),
                          axis.text.x=element_text(colour="white"),
                          legend.title=element_text(size=10),
                          legend.text=element_text(size=10),
                          axis.ticks.x = element_blank())

                #################################################################################################
                ############################ Main table #########################################################
                #################################################################################################

                incProgress(1/steps, detail = "Rendering main table [5/7]")

                output$maintable <- renderTable({

                    momentAlpha <- mean(sample$taint) * ( ( (mean(sample$taint) * (1 - mean(sample$taint))) / var(sample$taint)) - 1)
                    momentBeta <- (momentAlpha * (1 - mean(sample$taint))) / mean(sample$taint)
                    momentMean <- momentAlpha / (momentAlpha + momentBeta)
                    #browser()
                    momentBound <- qbeta(p = input$confidence, shape1 = momentAlpha, shape2 = momentBeta)

                    table <- data.frame(method = "No stratification", mle = round(mean(sample$taint), 3), sd = round(sd(sample$taint), 3), ub = qbeta(input$confidence, 1 + sum(sample$taint), 1 + length(sample$taint) - sum(sample$taint)))
                    table <- rbind(table, data.frame(method = "Method of moments", mle = momentMean, sd = NA, ub = momentBound))
                    table <- rbind(table, data.frame(method = "Weighting", mle = -1, sd = -1, ub = -1))
                    table <- rbind(table, data.frame(method = "Multilevel regression with poststratification", mle = round(mean(poststrat_prob), 3), sd = round(sd(poststrat_prob), 3), ub = -1))
                    colnames(table) <- c("", "Expected relative taint", "Std. Deviation taint", "Upper bound relative taint")
                    table

                }, striped = T, na = ".")

                #################################################################################################
                ############################ Individual strata table ############################################
                #################################################################################################

                incProgress(1/steps, detail = "Rendering additional tables [6/7]")

                stratumtable <- data.frame(
                    stratum = 1:length(unique(sample$stratum)),
                    N = rep(-1, length(unique(sample$stratum)),
                            stratum_sample = rep(-1, length(unique(sample$stratum))),
                            stratum_sample_sd = rep(-1, length(unique(sample$stratum))),
                            stratum_population_n = rep(-1, length(unique(sample$stratum))),
                            stratum_est = rep(-1, length(unique(sample$stratum))),
                            stratum_sd = rep(-1, length(unique(sample$stratum))))
                )

                for(i in 1:length(levels(as.factor(poststrat$stratum)))) {
                    poststrat_stratum <- poststrat[poststrat$stratum == i, ]
                    posterior_prob_stratum <- posterior_epred(
                        fit,
                        draws = 1000,
                        newdata = as.data.frame(poststrat_stratum)
                    )
                    poststrat_prob_stratum <- (posterior_prob_stratum %*% poststrat_stratum$N) / sum(poststrat_stratum$N)
                    stratumtable$N[i] <- as.integer(length(sample$taint[sample$stratum == i]))
                    stratumtable$stratum_sample[i] <- round(mean(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_sample_sd[i] <- round(sd(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_population_n <- sizes
                    stratumtable$stratum_est[i] <- round(mean(poststrat_prob_stratum), 3)
                    stratumtable$stratum_sd[i] <- round(sd(poststrat_prob_stratum), 3)
                }

                output$postStratumTable <- renderTable({
                    colnames(stratumtable) <- c("Stratum", "Sample size", "Sample mu", "Sample sigma", "Population size", "Population mu", "Population sigma")
                    stratumtable
                })

                #################################################################################################
                ############################ Additions to figure ################################################
                #################################################################################################

                incProgress(1/steps, detail = "Rendering additional plots [7/7]")

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

                output$stratumPlot <- renderPlot({
                    plotList <- list()
                    for(i in 1:length(unique(sample$stratum))){
                        dat <- as.data.frame(table(pp[, i]), stringsAsFactors = F)
                        dat$Var1 <- as.numeric(dat$Var1)
                        yBreaks <- pretty(c(0, max(dat$Freq)), min.n = 4)
                        plotList[[i]] <- ggplot(data = dat, mapping = aes(x = Var1, y = Freq)) +
                            geom_bar(fill = "darkred", colour = "black", stat = "identity") +
                            ggtitle(paste0("Stratum: ", i)) +
                            scale_y_continuous(name = "", limits = range(yBreaks), breaks = yBreaks) +
                            scale_x_continuous(name = "Predicted errors") +
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
