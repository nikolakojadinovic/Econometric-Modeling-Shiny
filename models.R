library(shiny)
library(shinydashboard)
library(DT)
library(reshape2)
library(urca)
library(systemfit)



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


constructLaggedDf <- function(dataset){
  
  #Constructing a dataframe of independent variables for SUR evaluation loop. 'final' consists of a dataframe with all independent
  #variables followed by first and second lag of independent variables
  
  df <- data.frame(dataset)
  if (ncol(df) == 1) {
    return(cbind(dataset, lagSeries(dataset,1), lagSeries(dataset,2)))
  }
  
  nCols <- ncol(dataset)
  
  final <- cbind()
  for (i in 1:nCols) {
     final <- cbind(final, dataset[,i], lagSeries(dataset[,i],1), lagSeries(dataset[,i],2)) 
  }
  return(final)
}


defineArEquation <- function(var){
  
  # final_ar_list <- c()
  # for (j in ar_list) {
  #   final_ar_list <- c(final_ar_list, j=j)
  # }
  return(unlist(var) ~ unlist(lagSeries(var,1)))
}

defineMainEquation <- function(var, indeps){
  return(unlist(var) ~ cbind(indeps))
}


getSystem <- function(Y,Xset){
  
  
  nCols <- ncol(Xset)
  newX <- cbind()
  for (k in 1:nCols) {
    newX <- cbind(newX, unlist(Xset[,k]))
  }
  
  
  main_eq <- unlist(Y) ~ newX
  system <- c(main_eq)
  for (j in 1:nCols) {
    Xt <- unlist(newX[,j])
    Xt1 <- lagSeries(unlist(newX[,j]),1)
    ar1 <- Xt ~ Xt1
    system <- c(system, ar1)
  }
  return(lapply(system, as.formula))
}

getSurCoefs <- function(sur, param){
  eqs <- sur[[1]]
  no_eqs <- length(eqs)
  coefs_list <- c()
  
  for (i in param:no_eqs) {
    coefs <- list(as.double(eqs[[i]]$coefficients))
    coefs_list <- c(coefs_list, coefs)
  }
  
  return(coefs_list)
  
}

rep2 <- function(x, times){
  if (times ==0) {
    return(numeric(0))
  }
  else {
    out <- c()
    for (i in 1:times) {
      out <- c(out, x)
    }
  }
  
  return (out)
}

out_X <- function(sur){
  
  
  out <- getSurCoefs(sur,2)
  
  
  
  
  
  N <- length(out)
  out1 <- list()
  for (i in 1:N) {
    
    rb <- i - 1
    between <- rep2("-",rb)
    
    re <- N-i
    end <- rep2("-", re)
    first <- out[[i]][1]
    last <- out[[i]][2]
    new <- c(first, between,last,end)
    print(new)
    out1[[i]] <- new
    
    
  
  }
  
  
  return(out1)
}

getRSquared <- function(){
  
}



# system <- getSystem(testdata[,15], testdata[,c(6,9)])
# systemfit(system, method = "SUR")
# 
# me <- unlist(testdata[,2]) ~ cbind(unlist(testdata[,3]), unlist(testdata[,4]))
# 
# arI <- unlist(testdata[,3]) ~lagSeries(unlist(testdata[,3]),1)
# arII <- unlist(testdata[,4]) ~ lagSeries(unlist(testdata[,4]),1)
# 
# system1 <- list(me, arI, arII)
# systemfit(system, method = "SUR")



#RAZBITI INDEPS DF NA TACNO ONOLIKO KOLIKO IMA PREDIKTORA
# constructSystemList <- function(ar_formula_list){
#   system <- c()
#   for (formula in c(ar_formula_list)) {
#     system <- c(system, formula)
#   }
#   return(system)
# }



# evaluationLoopDeps <- function(y, X){
#   for (i in 1:ncol(y)) {
#     main_eq <- y[,i] ~ cbind(X)
#     ar <- defineArEquations(X)
#     
#   }
# }
# 
# evaluationLoopIndeps <- function(){
#   
# }



