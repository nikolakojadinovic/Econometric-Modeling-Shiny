library(shiny)
library(shinydashboard)
library(DT)




#extracting the mode from a list or a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Used for testing the function locally
# test_NA_df <- data.frame(
#   prva = c(1,6,3,7,NA,9),
#   druga = c("kat1", "kat2", "kat2", NA, "kat3","kat2"),
#   treca = c(1.67,8.90, NA, NA, 2.34, 8.76),
#   allNAS = c(NA, NA, NA, NA, NA, NA)
#   
#   
# )



#Function to handle the missing values in uploaded dataset.

handleMissing <- function(dataset, method){
  nCols <- ncol(dataset)
  nRows <- nrow(dataset)
  
  #Deleting NA columns, if any
  j <- 1
  while(j <= nCols){
    if (all(is.na(dataset[,j]))) {
      dataset <- dataset[,-j]
      nCols <- nCols - 1
    } else{
      j <- j+1
    }
  }
  #Implementing "omit_rows" option, which simply deletes all rows that contain at least one NA value
  if (method =="omit_rows") {

  
    return(na.omit(dataset))
  
  #implementing mean imputation. All NAs in one column are replaced with mean of that column. #
  #Mean is only computed once!  
  } else if (method == "rep_mean_mode") {
    for (j in 1:nCols) {
      mean_mode  <- 0
      if (is.numeric(dataset[,j])) {
        mean_mode = mean(na.omit(dataset[,j]))
        
      } else {
        mean_mode = getmode(na.omit(dataset[,j]))
      } 
      for (i in 1:nRows) {
        if (is.na(dataset[i,j])) {
          dataset[i,j] <- mean_mode
        }
      }
    }
    return(dataset)
    
  #implementing median computation - similar as mean, only computer once!
  } else if (method == "rep_median_mode") {
    for (j in 1:nCols) {
      median_mode  <- 0
      if (is.numeric(dataset[,j])) {
        median_mode = median(na.omit(dataset[,j]))
        
      } else {
        median_mode = getmode(na.omit(dataset[,j]))
      } 

      for (i in 1:nRows) {
        if (is.na(dataset[i,j])) {
          dataset[i,j] <- median_mode
        }
      }
    }
    return(dataset)
  } 
  
  
}


getNumeric <- function(dataset){
  #Function to extract numerical columns of an uploaded dataset
  nums <- unlist(lapply(dataset, is.numeric))
  return(dataset[,nums])
  
  
}

getCharacter <- function(dataset){
  nums <- unlist(lapply(dataset, is.numeric))
  return(dataset[,!nums])
}

plotlyLine <- function(dataset){
  
}


