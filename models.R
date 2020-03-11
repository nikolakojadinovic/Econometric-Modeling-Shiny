library(shiny)
library(shinydashboard)
library(DT)
library(reshape2)
library(urca)



#This script contains functions for estimating statistical models (unit root testing, evaluation loop, SUR, OLS, Monte Carlo)

phillips_perron_test <- function(data, kind){
  
  #Performing PP Unit Root test for types drift and trend
  data = data.frame(data)
  if (kind == "drift") {
    pp_test_results <- c()
    for (i in 1:ncol(data)) {
      var <- data[,i]
      current_test <- ur.pp(as.numeric(unlist(var)), type = "Z-tau", model = "constant")
      pp_test_results <- c(pp_test_results, current_test)
      
    }
    
    return(pp_test_results)
    
  } else if (kind == "trend") {
    pp_test_results <- c()
    for (i in 1:ncol(data)) {
      var <- data[,i]
      current_test <- ur.pp(as.numeric(unlist(var)), type = "Z-tau", model = "trend")
      pp_test_results <- c(pp_test_results, current_test)
    }
    return(pp_test_results) 
  } 
  
}



adf_test <- function(data, kind){
  
  #Performing ADF Unit Root test for types none drift and trend
  data = data.frame(data)
  
  if (kind == "none") {
    adf_test_results <-c()
    for (i in 1:ncol(data)) {
      var <- na.omit(data[,i])
      current_test <- ur.df(as.numeric(unlist(var)),type = "none", selectlags = "AIC")
      adf_test_results <- c(adf_test_results, current_test)
    }
    return(adf_test_results)
  } else if (kind == "drift") {
    adf_test_results <- c()
    for (i in 1:ncol(data)) {
      var <- na.omit(data[,i])
      current_test <- ur.df(as.numeric(unlist(var)), type = "drift", selectlags = "AIC")
      adf_test_results <- c(adf_test_results, current_test)
    }
    return(adf_test_results)
    
    
  } else if (kind == "trend") {
    adf_test_results <- c()
    for (i in 1:ncol(data)) {
      var <- na.omit(data[,i])
      current_test <- ur.df(as.numeric(unlist(var)), type = "trend", selectlags = "AIC")
      adf_test_results <- c(adf_test_results, current_test)
      
    }
    return(adf_test_results)
  }
}



generate_output_table <- function(data, test){

  #Formating results of PP and ADF test  
  
  if (test=="pp") {
    output_table <- data.frame(Variables = character(),
                               No_drift_nor_trend = character(),
                               Drift_no_trend = double(),
                               Drift_and_trend = double())
    
    
    no_drift_nor_trend <- rep("-", ncol(data))
    variables <- colnames(data)
    
    drift_no_trend <- c()
    drift_and_trend <- c()
    
    pp_results_d <- phillips_perron_test(data, "drift")
    pp_results_dt <- phillips_perron_test(data, "trend")
    
    
    for (i in 1:ncol(data)) {
      drift_no_trend <- c(drift_no_trend, pp_results_d[[i]]@teststat)
      drift_and_trend <- c(drift_and_trend, pp_results_dt[[i]]@teststat)
    }
    
    
    
    cvals <- c("1%", "5%", "10%")
    for (i in 1:3) {
      no_drift_nor_trend[length(no_drift_nor_trend) + i] <- cvals[i]
      drift_no_trend[length(drift_no_trend) + i] <- pp_results_d[[1]]@cval[i]
      drift_and_trend[length(drift_and_trend) + i] <- pp_results_dt[[1]]@cval[i]
      variables[length(variables) + i] <- "-"
    }
    
    table <- na.omit(data.frame(variables, no_drift_nor_trend, drift_no_trend, drift_and_trend))
    colnames(table) <- colnames(output_table)
    return(rbind(output_table, table))
    
  } else if (test == "adf") {
    
    output_table <- data.frame(Variables = character(),
                               No_drift_nor_trend = character(),
                               Drift_no_trend = double(),
                               Drift_and_trend = double())
   
    variables <- colnames(data)
    
    no_drift_nor_trend <- c()
    drift_no_trend <- c()
    drift_and_trend <- c()
    
    adf_results_n <- adf_test(data, "none")
    adf_results_d <- adf_test(data, "drift")
    adf_results_dt <- adf_test(data, "trend")
    
    for (i in 1:ncol(data)) {
      no_drift_nor_trend <- c(no_drift_nor_trend, adf_results_n[[i]]@teststat)
      drift_no_trend <- c(drift_no_trend, adf_results_d[[i]]@teststat[1])
      drift_and_trend <- c(drift_and_trend, adf_results_dt[[i]]@teststat[1])
    }
    cvals <- c("1%", "5%", "10%")
    for (i in 1:3) {
      variables[length(variables) + i] <- cvals[i]
      no_drift_nor_trend[length(no_drift_nor_trend) + i] <- adf_results_n[[1]]@cval[i]
      drift_no_trend[length(drift_no_trend) + i] <- adf_results_d[[1]]@cval[1,][i]
      drift_and_trend[length(drift_and_trend) + i] <- adf_results_dt[[1]]@cval[1,][i]
      
    }
    table <- na.omit(data.frame(variables, no_drift_nor_trend, drift_no_trend, drift_and_trend))
    colnames(table) <- colnames(output_table)
    return(rbind(output_table, table))
    
  }
}



