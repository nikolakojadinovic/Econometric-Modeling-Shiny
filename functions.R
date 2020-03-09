library(shiny)
library(shinydashboard)
library(DT)
library(reshape2)




#extracting the mode from a list or a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#Used for testing the function locally
test_NA_df <- data.frame(
  prva = c(1,6,3,7,NA,9),
  druga = c("kat1", "kat2", "kat2", NA, "kat3","kat2"),
  treca = c(1.67,8.90, NA, NA, 2.34, 8.76),
  allNAS = c(NA, NA, NA, NA, NA, NA)


)



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
  #Function to extract string variables from a dataset
  nums <- unlist(lapply(dataset, is.numeric))
  return(dataset[,!nums])
}


setIDCol <- function(dataset){
  #Function that sets ID column as first column of a dataset
  nRows <- seq(1:nrow(dataset))
  to_return <- (cbind(nRows,dataset))
  colnames(to_return) <- c("ID", colnames(dataset))
  return(to_return)
  
}

plotLineDfFormatter <- function(dataset, str_cols_arr){
  #Returning tall format for a dataset subset, used in ggplots
  tmp1 <- dataset[,str_cols_arr]
  tmp2 <- setIDCol(tmp1)
  to_plot <- reshape2::melt(tmp2, id = "ID")
  
  
  return(to_plot)
}

transformData <- function(var, transformation){
  #Function for logit transformation, zscore standardization and natural logarithm
  if (transformation == "logit") {
    return(as.double(lapply(var, function(x) (1-x)/x)))
  } else if (transformation == "zscore") {
    return(as.double(lapply(var, function(x) (x - mean(var))/sd(var))))
  } else if (transformation == "ln"){
    return(as.double(lapply(var, function(x) log(x))))
  }

}

transformDataset  <- function(dataset, transformation){


    final <- cbind()

    dataset1 <- data.frame(dataset)

    for (j in 1: ncol(dataset1)) {


      current_col <- transformData(dataset1[,j], transformation)
      final <- cbind(final, current_col)

    }
    return(final)
}

differenceData <- function(var, l, order){
  #function to perform data differencing for a given lag and order
  zeros <- c(rep(0, l))

  if (order == 0) {
    return (var)
  } else if (order == 1) {

    return(c(zeros,diff(var,lag = l, differences = order)))
  } else if (order == 2) {
    return(c(zeros,diff(var,lag = l, differences = order)))
  }
}

differenceDataset <- function(dataset, l, order){

  final <- cbind()
  dataset1 <- data.frame(dataset)


  for (j in 1:ncol(dataset1)) {
    current_col <- differenceData(dataset1[,j],l, order = order)
    final <- cbind(final, current_col)
  }
  return(final)

}

test1col <- data.frame(col1 = c(seq(1:10)))


