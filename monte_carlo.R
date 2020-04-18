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


#change fetching
values <- list()
setHot <- function(x){
  values[["hot"]] <<-x
}
#change fetching


getStressedEstimates <- function(stress_df, coefs_list){
  intercept <- unlist(coefs_list)[1]
  coefs_list <- unlist(coefs_list)[-1]
  out <- list()
  vals <- c()
  for(i in 1:ncol(stress_df)){
    curr_col <- stress_df[,i] * coefs_list[i]
    out[[i]] <- curr_col
  }
  out <- data.frame(out)
  
  
  for(j in 1:nrow(out)){
    curr_row <- sum(out[j,]) + intercept
    vals <- c(vals, curr_row)
    
  }
  
  return(vals)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

getXresidDF <- function(coef_model_out){
  resid <- list()
  for(i in 1:length(coef_model_out)){
    resid[[i]] <- coef_model_out[[i]][[2]]$residuals
  }
  return(resid)
}

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

revertTransformations <- function(){
  
}

inverse_logit <-function(col){
  res_list <-c()
  for (i in 1:length(col)) {
    transformed_value <- 1/(exp(col[i]) + 1)
    res_list <- c(res_list, transformed_value)
    
  }
  return(res_list)
}



runMonteCarlo <- function(inner, outer, dim, start_index, chol, model, stress_estim = NULL, revert){
  
  baseline <- c()
  for(i in 1:outer){
    start = start_index
    for(j in 1:inner){
      z_rand <- rnorm(dim)
      curr_estimate <- chol %% z_rand
      curr_single <- curr_estimate[1,1]
      if(!is.null(stress_estim)){
        simulated <- stress_estim[start] + curr_single
      } 
      else {
        simulated <- model$fitted[start] + curr_single
      }
      
      
      
      baseline <- c(baseline, simulated)
      start = start + 1
    }
  }
  
  if(revert == 1){
    baseline <- sapply(baseline, inverse_logit)
    return(as.double(baseline))
  } else if(revert == 2){
    baseline <- sapply(baseline, diffinv)
    return(as.double(baseline))
  } else if(revert == 3){
    baseline <- sapply(baseline, diffinv)
    baseline <- sapply(baseline, inverse_logit)
    return(as.double(baseline))
  } else if(revert == 9){
    return (as.double(baseline))
  }
  
  
}


