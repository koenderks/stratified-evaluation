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

if(!"jfa" %in% installed.packages()) 
{ 
  install.packages("jfa") 
}
library(jfa)

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
    
    # Observer with high priority
    observe(priority = 1, {
      if(!is.null(input$datafile)){
        df <- read.csv(input$datafile$datapath, header = TRUE)
        stratum <- df[, which(colnames(df) == input$stratum)]
        noStrata <- length(unique(stratum))
        output$out <- renderUI({
          numinputs <- lapply(1:noStrata, function(i){
            numericInput(inputId = paste0("n", i), label = paste0("Population size stratum ", i), value = 100, min = 1)
          })
        })   
      }
    })
    
    # Observer with lower priority
    observe(priority = 0, {
        updateSelectInput(session, "var2", choices = names(contentsrea()), selected = names(contentsrea())[4])
        updateSelectInput(session, "stratum", choices = names(contentsrea()), selected = names(contentsrea())[2])
    })
    
    # Initialize main output
    output$maintable <- renderTable({
        x <- data.frame(method = c("No stratification", "Method of moments", "Weighting", "Multilevel regression with poststratification"), x1 = rep(NA, 4), x2 = rep(NA, 4))
        colnames(x) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
        x
    }, striped = TRUE, na = ".")
    
    output$descriptivesTable <- renderTable ({
        tab <- data.frame(pop = NA, strata = NA, sample = NA, errors = NA)
        colnames(tab) <- c("Population size", "Number of strata", "Sample size", "Sum of taints")
        tab
    }, na = ".")
    
    output$stratumDescriptivesTable <- renderTable ({
      tab <- data.frame(pop = NA, sizes = NA, size = NA, errors = NA, mean = NA, sd = NA)
      colnames(tab) <- c("Stratum", "Population size", "Sample Size", "Sum of taints", "Mean", "SD")
      tab
    }, striped = T, na = ".")
    
    output$comparison <- renderPlot(NULL)
    
    # Initialize no stratification output
    output$noMainTable <- renderTable({
      
      df <- data.frame(name = "No stratification", mean = NA, bound = NA)
      colnames(df) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
      df
      
    }, striped = T, na = ".")
    output$noMainFigure <- renderPlot(NULL)
    
    # Initialize moment output
    output$momentMainTable <- renderTable({
        
        df <- data.frame(name = "Method of moments", mean = NA, bound = NA)
        colnames(df) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
        df
        
    }, striped = T, na = ".")
    output$momentMainFigure <- renderPlot(NULL)
    output$momentStratumFigure <- renderPlot(NULL)
    output$momentPredictionsFigure <- renderPlot(NULL)
    
    # Initialize main weighting output
    output$weightingMainTable <- renderTable({
        
        df <- data.frame(name = "Weighting", mean = NA, bound = NA)
        colnames(df) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
        df
        
    }, striped = T, na = ".")
    
    # Initialize main post stratification output
    output$mrpMainTable <- renderTable({
        
        df <- data.frame(name = "Multilevel regression with poststratification", mean = NA, sd = NA, bound = NA)
        colnames(df) <- c("", "Most likely error", "Std. Deviation taint", paste0(round(input$confidence * 100, 2), "% Upper bound"))
        df
        
    }, striped = T, na = ".")
    
    output$mrpStratumTable <- renderTable(NULL)
    output$mrpPosteriorPredictive <- renderPlot(NULL)
    output$mrpPosteriorPredictives <- renderPlot(NULL)
    output$mrpStratumPredictions <- renderTable(NULL)
    output$mrpPosteriorDistributions <- renderPlot(NULL)

    observeEvent(input$update, {
        if(!is.null(input$datafile)){
            
            withProgress(message = 'Running analysis', value = 0, {
                
                steps <- 12
                
                #################################################################################################
                ############################ Read data ##########################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Reading data [1/12]")
                
                df <- read.csv(input$datafile$datapath, header = TRUE)
                taint <- df[, which(colnames(df) == input$var2)]
                stratum <- df[, which(colnames(df) == input$stratum)]
                sample <- data.frame(taint = taint, stratum = stratum)
                
                noStrata <- length(unique(stratum))
                
                #################################################################################################
                ############################ Data manipulation ##################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Preparing input [2/12]")
                
                sizes <- c(input$n1, input$n2, input$n3, input$n4, input$n5, input$n6, input$n7, input$n8)
                sizes <- sizes[1:noStrata]
                poststrat <- data.frame(stratum = 1:noStrata, N = sizes, N_errors = sum(taint))
                
                #################################################################################################
                ############################ Model fitting ######################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Calculating non-stratified outcomes [3/12]")
                
                # 1. Calculate the parameters of the beta distribution
                meanTaint <- mean(sample$taint)
                sdTaint <- sd(sample$taint)
                n <- length(sample$taint)
                k <- sum(sample$taint)
                # 2. Calculate the upper bound for the beta distribution
                ubTaint <- qbeta(input$confidence, 1 + k, 1 + n - k)
                
                incProgress(1/steps, detail = "Calculating Method of moment parameters [4/12]")
                
                # 1. Placeholders
                alpha_s <- numeric()
                beta_s <- numeric()
                mean_s <- numeric()
                sd_s <- numeric()
                mean_d <- numeric()
                var_d <- numeric()
                sum_s <- numeric()
                s_n <- numeric()
                
                # 2. Calculate parameters of beta distribution in each stratum
                for (i in 1:length(levels(as.factor(sample$stratum)))) {
                    level <- levels(as.factor(sample$stratum))[i]
                    s_i <- sample[sample$stratum == i, ]
                    s_n[i] <- length(s_i$taint)
                    sum_s[i] <- sum(s_i$taint)
                    mean_s[i] <- mean(s_i$taint)
                    sd_s[i] <- sd(s_i$taint)
                    alpha_s[i] <- 1 + sum(s_i$taint)
                    beta_s[i] <- 1 + length(s_i$taint) - sum(s_i$taint)
                    mean_d[i] <- alpha_s[i] / (alpha_s[i] + beta_s[i])
                    var_d[i] <- (alpha_s[i] * beta_s[i]) / ((alpha_s[i] * beta_s[i])^2 * (alpha_s[i] * beta_s[i] + 1))
                }

                # 3. Calculate the parameters of the moment-aggregated beta distribution
                # This is still wrong.... ??
                momentAlpha <- mean(mean_d) * ( ( (mean(mean_d) * (1 - mean(mean_d))) / var(mean_d)) - 1)
                momentBeta <- (momentAlpha * (1 - mean(mean_d))) / mean(mean_d)
                
                # 4. Calculate the mean and upper bound of the beta distribution
                momentMean <- momentAlpha / (momentAlpha + momentBeta)
                momentBound <- qbeta(p = input$confidence, shape1 = momentAlpha, shape2 = momentBeta)
                
                incProgress(1/steps, detail = "Calculating Weighted parameters [5/12]")
                
                # 1. Skip the calculation of parameters for the strata (already done)
                
                incProgress(1/steps, detail = "Fitting MRP model [6/12]")
                
                # 1. Recode the data into n and k per stratum
                sample_alt <- sample %>% group_by(stratum) %>% summarise(N_errors = sum(taint), N = n()) %>% ungroup()
                
                # 2. Fit the stan model to the sample data
                fit <- stan_glmer(cbind(N_errors, N - N_errors) ~ (1 | stratum), family = binomial("logit"), data = sample_alt, iter = input$iter)
                
                # 3. Predict the post-stratified data
                posterior_prob <- posterior_epred(fit, draws = 1000, newdata = poststrat) # Posterior estimates for error probability given the proportion of items in the population in each level of the factors included in the model.
                poststrat_prob <- posterior_prob %*% poststrat$N / sum(poststrat$N)       # Adjust these (according to https://cran.r-project.org/web/packages/rstanarm/vignettes/mrp.html)
                pp <- posterior_predict(fit, newdata = poststrat)                         # Posterior predictive data given the proportion of items in the population in each level of the factors included in the model.
                
                # 4. Calculate the mean and upper bound using the posterior estimates of all strata
                postMean <- round(mean(poststrat_prob), 3)
                postBound <- as.numeric(quantile(poststrat_prob, probs = input$confidence))
                
                #################################################################################################
                ############################ All methods table ##################################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Rendering main results table [6/12]")
                
                output$descriptivesTable <- renderTable ({
                    tab <- data.frame(pop = sum(sizes), strata = noStrata, sample = nrow(sample), errors = sum(sample$taint))
                    colnames(tab) <- c("Population size", "Number of strata", "Sample size", "Sum of taints")
                    tab
                }, na = "")
                
                output$stratumDescriptivesTable <- renderTable ({
                  tab <- data.frame(pop = 1:noStrata, sizes = sizes, size = s_n, errors = round(sum_s, 3), mean = round(mean_s, 3), sd = round(sd_s, 3))
                  colnames(tab) <- c("Stratum", "Population size", "Sample Size", "Sum of taints", "Mean", "SD")
                  tab
                }, na = "")
                
                output$maintable <- renderTable({
                    
                    table <- data.frame(method = "No stratification", mle = round(meanTaint, 4), ub = round(ubTaint, 4))
                    table <- rbind(table, data.frame(method = "Method of moments", mle = round(momentMean, 4), ub = round(momentBound, 4)))
                    table <- rbind(table, data.frame(method = "Weighting", mle = NA, ub = NA))
                    table <- rbind(table, data.frame(method = "Multilevel regression with poststratification", mle = round(postMean, 4), ub = round(postBound, 4)))
                    colnames(table) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                    table
                    
                }, striped = T, na = ".")

                #################################################################################################
                ############################ No stratification tables and figures ###############################
                #################################################################################################
                
                output$noMainTable <- renderTable({
                  
                  table <- data.frame(method = "No stratification", mle = round(meanTaint, 4), ub = round(ubTaint, 4))
                  colnames(table) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                  table
                }, striped = T, na = ".")
                
                output$noMainFigure <- renderPlot({
                  p1 <- ggplot() +
                    stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 1), xlim = c(0, 1), color = "black", linetype = "dashed") +
                    stat_function(fun = dbeta, args = list(shape1 = 1 + k, shape2 = 1 + n - k), xlim = c(0, 1), geom = "area", fill = "#4682b4", alpha = 0.8, color = "black") +
                    labs(title = paste0("Prior and posterior distribution (\u03B1 = ", round(1 + k, 2), ", \u03B2 = ", round(1 + n - k, 2), ")")) +
                    scale_y_continuous(name = "Density") +
                    scale_x_continuous(name = "Error probability in population (beta)", limits = c(0,1), breaks = seq(0, 1, 0.2)) +
                    theme_bw() +
                    theme(axis.ticks.y = element_blank(),
                          panel.grid = element_blank())
                  
                  dat <- jfa:::.dBetaBinom(0:1000, N = sum(sizes), shape1 = 1 + k, shape2 = 1 + n - k)
                  dat <- dat[dat > 0.0001]
                  dat <- data.frame(x = 1:length(dat), y = dat)
                  yBreaks <- pretty(c(0, dat$y), min.n = 4)
                  p2 <- ggplot(dat, aes(x = x, y = y)) + 
                          geom_bar(fill = "#4682b4", colour = "black", stat = "identity") +
                          labs(title = paste0("Posterior predictive distribution (N = ", sum(sizes), ", \u03B1 = ", round(1 + k, 2), ", \u03B2 = ", round(1 + n - k, 2), ")")) +
                          scale_y_continuous(name = "Probability", limits = c(0, max(yBreaks)), breaks = yBreaks, labels = round(yBreaks, 3)) +
                          scale_x_continuous(name = "Predicted errors in population (beta-binomial)") +
                          theme_bw()
                    
                  p <- grid.arrange(p1, p2, nrow = 1)
                  p
                })
                                
                #################################################################################################
                ############################ Method of Moments tables and figures ###############################
                #################################################################################################
                
                output$momentMainTable <- renderTable({
                  
                  table <- data.frame(method = "Method of moments", mle = round(momentMean, 4), ub = round(momentBound, 4))
                  colnames(table) <- c("", "Most likely error", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                  table
                }, striped = T, na = ".")

                output$momentMainFigure <- renderPlot({
                    p1 <- ggplot() +
                      stat_function(fun = dbeta, args = list(shape1 = momentAlpha, shape2 = momentBeta), xlim = c(0, 1), geom = "area", fill = "darkred", alpha = 0.8, color = "black") +
                      labs(title = paste0("Moment-aggregated posterior distribution (\u03B1 = ", round(momentAlpha, 2), ", \u03B2 = ", round(momentBeta, 2), ")")) +
                      scale_y_continuous(name = "Density") +
                      scale_x_continuous(name = "Error probability in population (beta)", limits = c(0,1), breaks = seq(0, 1, 0.2)) +
                      theme_bw() +
                      theme(axis.ticks.y = element_blank(),
                            panel.grid = element_blank(),
                            axis.text.y = element_blank())
                  
                    dat <- jfa:::.dBetaBinom(0:1000, N = sum(sizes), shape1 = momentAlpha, shape2 = momentBeta)
                    dat <- dat[dat > 0.0001]
                    dat <- data.frame(x = 1:length(dat), y = dat)
                    yBreaks <- pretty(c(0, dat$y), min.n = 4)
                    p2 <- ggplot(dat, aes(x = x, y = y)) + 
                      geom_bar(fill = "darkred", colour = "black", stat = "identity") +
                      labs(title = paste0("Posterior predictive distribution (N = ", sum(sizes), ", \u03B1 = ", round(momentAlpha, 2), ", \u03B2 = ", round(momentBeta, 2), ")")) +
                      scale_y_continuous(name = "Probability", limits = c(0, max(yBreaks)), breaks = yBreaks, labels = round(yBreaks, 3)) +
                      scale_x_continuous(name = "Predicted errors in population (beta-binomial)") +
                      theme_bw()
                    
                    p <- grid.arrange(p1, p2, nrow = 1)
                })
                                
                output$momentStratumFigure <- renderPlot({
                  plotList <- list()
                  for (i in 1:length(levels(as.factor(sample$stratum)))) {
                    strat <- sample[sample$stratum == levels(as.factor(sample$stratum))[i], ]
                    s_alpha <- 1 + sum(strat$taint) 
                    s_beta <- 1 + length(strat$taint) - sum(strat$taint)

                    plotList[[i]] <- ggplot() +
                      stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 1), xlim = c(0, 1), color = "black", linetype = "dashed") +
                      stat_function(fun = dbeta, args = list(shape1 = s_alpha, shape2 = s_beta), xlim = c(0, 1), geom = "area", fill = "darkred", alpha = 0.8, color = "black") +
                      labs(title = paste0("Prior and posterior distribution for stratum ", i, " (\u03B1 = ", round(s_alpha, 2), ", \u03B2 = ", round(s_beta, 2), ")")) +
                      scale_y_continuous(name = "Density") +
                      scale_x_continuous(name = "Error probability in stratum (beta)", limits = c(0,1), breaks = seq(0, 1, 0.2)) +
                      theme_bw() +
                      theme(axis.ticks.y = element_blank(),
                            panel.grid = element_blank(),
                            axis.text.y = element_blank())
                  }
                  n <- length(plotList)
                  nCol <- floor(sqrt(n))
                  do.call("grid.arrange", c(plotList, ncol=nCol))
                })
                
                output$momentPredictionsFigure <- renderPlot({
                  plotList <- list()
                  for (i in 1:length(levels(as.factor(sample$stratum)))) {
                    strat <- sample[sample$stratum == levels(as.factor(sample$stratum))[i], ]
                    s_alpha <- 1 + sum(strat$taint) 
                    s_beta <- 1 + length(strat$taint) - sum(strat$taint)
                    dat <- jfa:::.dBetaBinom(0:1000, N = sizes[i], shape1 = s_alpha, shape2 = s_beta)
                    dat <- dat[dat > 0.0001]
                    dat <- data.frame(x = 1:length(dat), y = dat)
                    yBreaks <- pretty(c(0, dat$y), min.n = 4)
                    plotList[[i]] <- ggplot(dat, aes(x = x, y = y)) + 
                      geom_bar(fill = "darkred", colour = "black", stat = "identity") +
                      labs(title = paste0("Posterior predictive distribution for stratum ", i," (N = ", sizes[i], ", \u03B1 = ", round(s_alpha, 2), ", \u03B2 = ", round(s_beta, 2), ")")) +
                      scale_y_continuous(name = "Probability", limits = c(0, max(yBreaks)), breaks = yBreaks, labels = round(yBreaks, 3)) +
                      scale_x_continuous(name = "Predicted errors in population (beta-binomial)") +
                      theme_bw()
                  }
                  n <- length(plotList)
                  nCol <- floor(sqrt(n))
                  do.call("grid.arrange", c(plotList, ncol=nCol))
                })
                
                #################################################################################################
                ############################ MRP tables and figures #############################################
                #################################################################################################
                
                incProgress(1/steps, detail = "Rendering MRP main table [7/12]")
                
                output$mrpMainTable <- renderTable({
                    
                    df <- data.frame(name = "Multilevel regression with poststratification", mean = round(mean(poststrat_prob), 3), sd = round(sd(poststrat_prob), 3), bound = as.numeric(quantile(poststrat_prob, probs = input$confidence)))
                    colnames(df) <- c("", "Most likely error", "Std. Deviation taint", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                    df

                }, striped = T, na = ".")
                
                incProgress(1/steps, detail = "Rendering MRP main figure [8/12]")
                
                output$mrpPosteriorPredictive <- renderPlot({
                    dat <- as.data.frame(table(as.numeric(unlist(pp))), stringsAsFactors = F)
                    dat$Var1 <- as.numeric(dat$Var1)
                    dat$Prob <- dat$Freq / sum(dat$Freq)
                    yBreaks <- pretty(c(0, max(dat$Prob)), min.n = 4)
                    p1 <- ggplot(data = dat, mapping = aes(x = Var1, y = Prob)) +
                        geom_bar(fill = "#FFB682", colour = "black", stat = "identity") +
                        labs(title = paste0("Posterior predictive distribution (N = ", sum(sizes), ")")) +
                        scale_y_continuous(name = "Probability", limits = c(0, max(yBreaks)), breaks = yBreaks, labels = round(yBreaks, 3)) +
                        scale_x_continuous(name = "Predicted errors in population (beta-binomial)") +
                        theme_bw()
                    
                    dat <- data.frame(x = as.numeric(unlist(poststrat_prob)))
                    p2 <- ggplot(data = dat, mapping = aes(x = x)) +
                        geom_density(fill = "#FFB682", colour = "black", alpha = 0.8) +
                        labs(title = "Posterior distribution (of the linear predictor)") +
                        scale_y_continuous(name = "Density") +
                        scale_x_continuous(name = "Error probability in population", limits = c(0,1), breaks = seq(0, 1, 0.2)) +
                        theme_bw() +
                        theme(axis.ticks.y = element_blank(),
                              panel.grid = element_blank())
                    
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
                            stratum_ub = rep(-1, length(unique(sample$stratum))),
                            stratum_pred_mean = rep(-1, length(unique(sample$stratum))),
                            stratum_pred_sd = rep(-1, length(unique(sample$stratum))),
                            stratum_pred_ub = rep(-1, length(unique(sample$stratum))))
                )
                
                for(i in 1:length(levels(as.factor(poststrat$stratum)))) {
                    poststrat_stratum <- poststrat[poststrat$stratum == i, ]
                    posterior_prob_stratum <- posterior_epred(fit, draws = 1000, newdata = as.data.frame(poststrat_stratum))
                    poststrat_prob_stratum <- (posterior_prob_stratum %*% poststrat_stratum$N) / sum(poststrat_stratum$N)
                    
                    posterior_prob_pred_stratum <- posterior_predict(fit, newdata = as.data.frame(poststrat_stratum))
                    
                    stratumtable$N[i] <- length(sample$taint[sample$stratum == i])
                    stratumtable$stratum_sample[i] <- round(mean(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_sample_sd[i] <- round(sd(sample$taint[sample$stratum == i]), 3)
                    stratumtable$stratum_population_n <- sizes
                    stratumtable$stratum_est[i] <- round(mean(poststrat_prob_stratum), 3)
                    stratumtable$stratum_sd[i] <- round(sd(poststrat_prob_stratum), 3)
                    stratumtable$stratum_ub[i] <- round(quantile(posterior_prob_stratum, probs = input$confidence), 3) # Upper bound
                    stratumtable$stratum_pred_mean[i] <- round(mean(posterior_prob_pred_stratum), 3)
                    stratumtable$stratum_pred_sd[i] <- round(sd(posterior_prob_pred_stratum), 3)
                    stratumtable$stratum_pred_ub[i] <- round(quantile(posterior_prob_pred_stratum, probs = input$confidence), 3)
                }
                
                incProgress(1/steps, detail = "Rendering MRP sub table [10/12]")
                
                output$mrpStratumTable <- renderTable({
                    table <- stratumtable[, -(9:11)]
                    colnames(table) <- c("Stratum", "Sample size", "Mean taint", "SD", "Population", "Estimated \u03BC", "Estimated \u03C3", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                    table
                }, striped = T, na = ".")
                
                output$mrpPosteriorDistributions <- renderPlot({
                  plotList <- list()
                  for (i in 1:length(levels(as.factor(sample$stratum)))) {

                    dat <- data.frame(x = as.numeric(unlist(posterior_prob[, i])))
                    plotList[[i]] <- ggplot(data = dat, mapping = aes(x = x)) +
                      geom_density(fill = "#FFB682", colour = "black", alpha = 0.8) +
                      labs(title = paste0("Posterior distribution for stratum ", i)) +
                      scale_y_continuous(name = "Density") +
                      scale_x_continuous(name = "Error probability in stratum", limits = c(0,1), breaks = seq(0, 1, 0.2)) +
                      theme_bw() +
                      theme(axis.ticks.y = element_blank(),
                            panel.grid = element_blank())
                  }
                  n <- length(plotList)
                  nCol <- floor(sqrt(n))
                  do.call("grid.arrange", c(plotList, ncol=nCol))
                })
                
                incProgress(1/steps, detail = "Rendering MRP additional plots [11/12]")
                
                output$mrpStratumPredictions <- renderTable({
                  table <- stratumtable[, -(6:8)]
                  colnames(table) <- c("Stratum", "Sample size", "Mean taint", "SD", "Population", "Predicted \u03BC", "Predicted \u03C3", paste0(round(input$confidence * 100, 2), "% Upper bound"))
                  table
                }, striped = T, na = ".")
                
                output$mrpPosteriorPredictives <- renderPlot({
                    plotList <- list()
                    for(i in 1:length(unique(sample$stratum))){
                        dat <- as.data.frame(table(pp[, i]), stringsAsFactors = F)
                        dat$Var1 <- as.numeric(dat$Var1)
                        dat$Prob <- dat$Freq / sum(dat$Freq)
                        yBreaks <- pretty(c(0, max(dat$Prob)), min.n = 4)
                        plotList[[i]] <- ggplot(data = dat, mapping = aes(x = Var1, y = Prob)) +
                            geom_bar(fill = "#FFB682", colour = "black", stat = "identity") +
                            labs(title = paste0("Posterior predictive distribution for stratum ", i, " (N = ", sizes[i], ")")) +
                            scale_y_continuous(name = "Probability", limits = c(0, max(yBreaks)), breaks = yBreaks, labels = round(yBreaks, 3)) +
                            scale_x_continuous(name = "Predicted errors in stratum (beta-binomial)") +
                            theme_bw()
                    }
                    n <- length(plotList)
                    nCol <- floor(sqrt(n))
                    do.call("grid.arrange", c(plotList, ncol=nCol))
                })
                
                incProgress(1/steps, detail = "Rendering main plot [12/12]")
                
                output$comparison <- renderPlot({
                    
                    taint_by_stratum <- sample %>%
                        group_by(stratum) %>%
                        summarise(y_mean = mean(taint), y_sd = sqrt(mean(taint) * (1 - mean(taint)) / n())) %>%
                        ungroup()
                    
                    yBreaks <- pretty(c(0, 0.5), min.n = 4)
                    
                    compare <- ggplot(data=taint_by_stratum, aes(x=stratum, y=y_mean, group=1)) +
                        geom_ribbon(aes(ymin=y_mean-y_sd,ymax=y_mean+y_sd,x=stratum),fill='#4682b4',alpha=.7)+
                        geom_line(aes(x=stratum, y=y_mean), col = "#4682b4")+
                        geom_point(col = "#4682b4", size = 1.5)+
                        scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))+
                        geom_point(data=stratumtable, mapping=aes(x=stratum, y=stratum_est), inherit.aes=TRUE,colour='#FFB682', size = 1.5)+
                        geom_line(data=stratumtable, mapping=aes(x=stratum, y=stratum_est,group=1), inherit.aes=TRUE,colour='#FFB682')+
                        geom_ribbon(data=stratumtable,mapping=aes(x=stratum,ymin=stratum_est-stratum_sd, ymax=stratum_est+stratum_sd), inherit.aes=FALSE,fill='#FFB682',alpha=.3) +
                      geom_point(mapping=aes(x=1:noStrata, y=mean_d), inherit.aes=TRUE,colour='darkred', size = 1.5)+
                      geom_line(mapping=aes(x=1:noStrata, y=mean_d), inherit.aes=TRUE,colour='darkred')+
                      geom_ribbon(mapping=aes(x=1:noStrata,ymin=mean_d-sqrt(var_d), ymax=mean_d+sqrt(var_d)), inherit.aes=FALSE,fill='darkred',alpha=.3) +
                        theme_bw()+
                        labs(x="Stratum",y="Average taint")+
                        theme(legend.position="none",
                              axis.title=element_text(size=10),
                              axis.text.y=element_text(size=10),
                              axis.text.x=element_text(size=10),
                              legend.title=element_text(size=10),
                              legend.text=element_text(size=10),
                              panel.grid = element_blank())
                    
                    compare2 <- ggplot()+
                        geom_hline(yintercept = meanTaint, size = 1, col = "#4682b4")+
                        geom_text(aes(x = 5.2, y = meanTaint +.025, label = "No stratification"), size = 3, col = "#4682b4")+
                        scale_x_continuous(name = "Most likely error") +
                        scale_y_continuous(name = "", breaks = yBreaks, limits= range(yBreaks))+
                        geom_hline(yintercept = postMean, colour = '#FFB682', size = 1) +
                        geom_text(aes(x = 5.2, y = postMean + .025), label = "MRP", colour = '#FFB682') +
                        geom_hline(yintercept = momentMean, colour = 'darkred', size = 1) +
                        geom_text(aes(x = 5.2, y = momentMean + .025), label = "Moment", colour = 'darkred') +
                        theme_bw()+
                        theme(legend.position="none",
                              axis.text.y=element_text(size=10),
                              axis.text.x=element_text(colour="white"),
                              legend.title=element_text(size=10),
                              legend.text=element_text(size=10),
                              axis.ticks.x = element_blank(),
                              panel.grid = element_blank())
                    
                    p <- bayesplot_grid(compare,compare2, grid_args = list(nrow=1, widths = c(8,2)))
                    p
                })
            })
        }
    })
}

shinyServer(server)
