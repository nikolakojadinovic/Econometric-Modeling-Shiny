library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)
library(reshape2)
library(urca)
library(systemfit)
library(nlme)
library(rhandsontable)
library(excelR)



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

ers_test <- function(data, kind){
  
  data = data.frame(data)
  
  if (kind == "drift") {
    adf_test_results <-c()
    for (i in 1:ncol(data)) {
      var <- na.omit(data[,i])
      current_test <- ur.ers(as.numeric(unlist(var)),type = "DF-GLS", model = "constant")
      adf_test_results <- c(adf_test_results, current_test)
    }
    return(adf_test_results)
  } else if (kind == "trend") {
    adf_test_results <- c()
    for (i in 1:ncol(data)) {
      var <- na.omit(data[,i])
      current_test <- ur.ers(as.numeric(unlist(var)), type = "DF-GLS", model = "trend")
      adf_test_results <- c(adf_test_results, current_test)
    }
    return(adf_test_results)
    
    
  } 
    
}

generate_output_table <- function(data, test){

  #Formating results of PP and ADF test  
  
  if (test=="pp" || test == "ers") {
    output_table <- data.frame(Variables = character(),
                               No_drift_nor_trend = character(),
                               Drift_no_trend = double(),
                               Drift_and_trend = double())
    
    
    no_drift_nor_trend <- rep("-", ncol(data))
    variables <- colnames(data)
    
    drift_no_trend <- c()
    drift_and_trend <- c()
    
    if(test == "pp") {
      pp_results_d <- phillips_perron_test(data, "drift")
      pp_results_dt <- phillips_perron_test(data, "trend")
      
    } else if(test == "ers") {
      pp_results_d <- ers_test(data,"drift")
      pp_results_dt <- ers_test(data, "trend")
    }
    
    
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


# defineArEquation <- function(var){
#   
#   # final_ar_list <- c()
#   # for (j in ar_list) {
#   #   final_ar_list <- c(final_ar_list, j=j)
#   # }
#   return(unlist(var) ~ unlist(lagSeries(var,1)))
# }

defineMainEquation <- function(var, indeps){
  return(unlist(var) ~ cbind(indeps))
}

getSystem2 <- function(Y, Xset){
  Xset <- data.frame(Xset)
  nCols <- length(Xset)
  newX <- cbind()
  for (k in 1:nCols) {
    newX <- cbind(newX, unlist(Xset[,k]))
  }
  #function that returns a list that should be a 'system' argument for systemfit
  main <- unlist(Y) ~ newX
  system <- c(main)
  for (i in 1:nCols) {

    system <- c(system, unlist(newX[,i]) ~ lagSeries(unlist(newX[,i]),1))
    
  }
  return(system)
}




getSystem <- function(Y,Xset){
  
  # if (ncol(Xset) == 0 || ncol(Xset) == 1 || is.null(ncol(Xset)) || is.na(ncol(Xset))) {
  #   return (Xset)
  # }
  Xset <- data.frame(Xset)
  nCols <- length(Xset)
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


#BREAK OVDE! - PRIKAZIVANJE JEBE #summary eqs[[1]] ukloni
getSurCoefs <- function(sur, param){
  eqs <- sur[[1]]
  no_eqs <- length(eqs)
  coefs_list <- c()
  
  for (i in param:no_eqs) {
    coefs <- list(as.double(summary(eqs[[i]])$coefficients[,1]))
    coefs_list <- c(coefs_list, coefs)
  }
  
  return(coefs_list)
  
}

getPValues <- function(mat_out){
  p_column <- mat_out[,4]
  p_vals <-c()
  for (p in p_column) {
    p_vals <- c(p_vals, p)
  }
  return(sapply(p_vals,round,4))
  
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







getDWstat <- function(resid){
  sum_up <- 0
  for (t in 2:length(resid)) {
    sum_up <- sum_up + (resid[t] - resid[t-1])**2
    
  }
  sum_down <- 0
  for (t in 1:length(resid)) {
    sum_down <- sum_down + (resid[t])**2
  }
  res <- sum_up/sum_down
  return(res)
}

getArCoefs <- function(Xset){
  if (ncol(Xset) == 0) {
    return()
  } else { 
  out <- list()
  for (i in 1:ncol(Xset)) {
    Xt <- Xset[,i]
    Xt1 <- lagSeries(Xt,1)
    intercept <- summary(lm(Xt ~ Xt1))$coefficients[1,1]
    phi <- summary(lm(Xt ~ Xt1))$coefficients[2,1]
    lst <- c(intercept, phi)
    out[[i]] <- lst
  }
  return(out)
  
  }
}


generate_X_out <- function(coefs, var_names, model){
  if (length(coefs) != length(var_names)) {
    stop("SJEEEEEEEB!!!!")
  } else {
  
    # coefs <- sapply(coefs, as.double)
    # coefs <- sapply(coefs, round, digits = 4)
    N <- length(coefs)
    first_row_num <- c()
    rest_rbind <- list()
    
    r2_list <-c("R Squared")
    dw_list <-c("Durbin Watson")
    
    for (i in 1:N) {
      first_row_num <- c(first_row_num, coefs[[i]][1])
      between <- rep2("-",i-1)
      end <- rep2("-",N-i)
      rest_rbind[[i]] <- c(between, coefs[[i]][2], end)
      
      r2_list <- c(r2_list, round(summary(model[[1]][[i+1]])$adj.r.squared, digits = 2))
      d2_list <- c(dw_list, round(model[[1]][[i+1]]$residuals, digits = 3))
    }
    
    # r2_list <- sapply(r2_list, round, digits = 2)
    # d2_list <- sapply(d2_list, round, digits = 3)
    
  
    out <- rbind(first_row_num)
    for (i in rest_rbind) {
      out <- rbind(out,i)
    }
   
    
    first_column <- c("Intercept")
    to_paste <- "_t-1"
    for (name in var_names) {
      val <- paste(name, to_paste)
      first_column <- c(first_column, val)
    }
    out <- cbind(first_column, out)
    out <- rbind(out,r2_list,d2_list)
    colnames(out) <- c("Variables",var_names)
    
    
    # for (r in 1:(N+1)) {
    #   for(c in 2:(N+1)){
    #     out[r,c] <- round(as.double(out[r,c]), digits = 4)
    #   }
    # }
    
    
    
    
    return(out)
  # }
  }

}

getArCoefsModel <- function(Xset){
  out <- list()
  for (i in 1:ncol(Xset)) {
    Xt <- as.numeric(unlist(Xset[,i]))
    Xt1 <- lagSeries(as.numeric(unlist(Xt)),1)
    model <- lm(Xt ~ Xt1)
    intercept <- round(summary(lm(Xt ~ Xt1))$coefficients[1,1],digits = 4)
    phi <- round(summary(lm(Xt ~ Xt1))$coefficients[2,1],digits = 4)
    lst <- c(intercept, phi)
    out[[i]] <- list(lst, model)
  }
  return(out)
}


getXRs <- function(Xset){
  #takes the ouptut of getArCoefs and returns a list of R2 values for each model
  lst <- getArCoefsModel(Xset)
  r2s <- c()
  for (i in 1:length(lst)) {
    r2 <- summary(lst[[i]][[2]])$r.squared
    r2s <- c(r2s, r2)
  }
  return (sapply(r2s,round, digits = 3))
}

getXDWs <- function(Xset){
  lst <- getArCoefsModel(Xset)
  dws <- c()
  for(i in 1:length(lst)){
    dw <- getDWstat(summary(lst[[i]][[2]]$residuals))
    dws <- c(dws, dw)
  }
  return(sapply(dws,round, digits = 3))
}

generate_X_out2 <- function(Xset){
  
  #getting varnames
  var_names <- colnames(Xset)
  
  #getting the coefficients and r2
  coefs <- list()
  for (i in 1:ncol(Xset)) {
    coef12 <- getArCoefsModel(Xset)[[i]][[1]] 
    coefs[[i]] <- coef12
  }
  
  r2s <- getXRs(Xset)
  dws <- getXDWs(Xset)
  
  N <- length(coefs)
  
  r2_list <-c("R Squared",r2s)
  dw_list <-c("Durbin Watson", dws)
  
  first_row_num <-c()
  rest_rbind <- list()
  for (i in 1:N) {
    first_row_num <- c(first_row_num, coefs[[i]][1])
    between <- rep2("-",i-1)
    end <- rep2("-",N-i)
    rest_rbind[[i]] <- c(between, coefs[[i]][2], end)
    
    
  }
  
  out <- rbind(first_row_num)
  for (i in rest_rbind) {
    out <- rbind(out,i)
  }
  
  
  first_column <- c("Intercept")
  to_paste <- "_t-1"
  for (name in var_names) {
    val <- paste(name, to_paste)
    first_column <- c(first_column, val)
  }
  out <- cbind(first_column, out)
  out <- rbind(out,r2_list, dw_list)
  colnames(out) <- c("Variables",var_names)
  
  return(out)
  
  
  
  
}

getR2 <- function(y, yhat){
  if (length(y) != length(yhat)) {
    stop("SJEEEB")
  } else {
    N <- length(y)
    ybar <- mean(y)
    sum_up <-0
    sum_down <-0
    for (i in 1:N) {
      sum_up <- sum_up + (y[i] - yhat[i]) **2
      sum_down <- sum_down + (y[i] - ybar)**2
  
    }
   
    return(round(1-(sum_up/sum_down), digits = 3))
  }
}

getPVals <- function(model_obj){
  tTable <- summary(model_obj)$tTable
  N <- nrow(tTable)
  coefs <- c()
  for(i in 1:N){
    coefs <- c(coefs, tTable[i,4])
  }
  return(coefs)
  
}

modelFitPrepare <-function(Xset){
  if(is.null(ncol(Xset)) || is.na(ncol(Xset))){
    return(unlist(Xset))
  } else {
  
  Xset <- data.frame(Xset)
  nCols <- length(Xset)
  newX <- cbind()
  for (k in 1:nCols) {
    newX <- cbind(newX, unlist(Xset[,k]))
  }
  return(newX)
  }
}
