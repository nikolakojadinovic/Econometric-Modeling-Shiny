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
library(jsonlite)



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

revertTransformations <- function(){
  
}

# getStressedVars <- function(base, stress, indeps){
#   
#   stressed_vars <- list()
#   
#   cols_base <- colnames(base)
#   cols_stress <- cols_base
#   cols_indeps <- colnames(indeps)
#   
#   comparison <- base == stress
#   for (j in cols_stress) {
#     if(!all(comparison[[j]]) && cols_stress[j] %in% cols_indeps){
#       # if(cols_stress[j] %in% cols_indeps ){
#         stressed_vars[[j]] <- stress[[cols_stress[j]]]
#         print(stressed_vars[[j]])
#       }
#       else {
#         next
#       }
#       
#     
#   }
#   
#   return(stressed_vars)
#   
# }

run_baseline <- function(inner, outer, dim, start_index, chol, model){
  
  baseline <- c()
  for(i in 1:outer){
    start = start_index
    for(j in 1:inner){
      z_rand <- rnorm(dim)
      curr_estimate <- chol %% z_rand
      curr_single <- curr_estimate[1,1]
      simulated <- model$fitted[start] + curr_single
      baseline <- c(baseline, simulated)
      start = start + 1
    }
  }
  return(as.double(baseline))
}

