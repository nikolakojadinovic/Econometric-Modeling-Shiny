library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)
library(reshape2)
library(urca)
library(systemfit)








shinyServer(function(input, output, session){
  
  #####################################################################REACTIVE FUNCTIONS FILE HANDLING
  
  
  # file1 <- reactive({
  #   input$file
  #   
  # })
  
  
  data <- reactive({
    
    file1 <- req(input$file)
    if(is.null(file1)){
      return()
      } else if(tools::file_ext(file1) == "csv") {
        read.csv(file = file1$datapath, 
                   sep = input$sep, 
                   header = input$header, 
                   skipNul = TRUE)
      } else if (tools::file_ext(file1) == "xlsx"){
        readxl::read_excel(path = file1$datapath, sheet = 1)
      }
  })
  
  
  
  data1 <- reactive({
    handleMissing(data(), input$nas)
  })
  
  columns_all <- reactive({
    names(data1())
  })
  
  columns_numerical <- reactive({
    names(getNumeric(data1()))
  })
  
  columns_character <- reactive({
    names(getCharacter(data1()))
  })
  
  output$contents <- renderTable({
    data1()
  })
  
  output$nObs <- reactive({
    nrow(data1())
  })
  output$nCol <- reactive({
    ncol(data1())
  })
  
  ##################################################################### REACTIVE FUNCTIONS FILE HANDLING
  
 
  
  
  
  ##################################################################### DATA MENU OUTPUT - summary, spreadsheet, viz
  
  output$table <- renderDataTable({
    if(is.null(data1())){return()}
    data1()
  })
  
  
  
  output$struct <- renderPrint({
    if(is.null(data1())){return()}
    summary(data1())
  })
  
  ################################################# VISUALIZATION PLOTS
  
  
  output$inputwidget_hist <- renderUI({
    selectInput("col", "Select the column", choices = columns_numerical())
  })
  
  output$inputwidget1_line <- renderUI({
    selectizeInput("cols1", "Select multiple variables", choices = columns_numerical(), multiple = TRUE)
  })
  
  output$inputwidget2_scatter <- renderUI({
    selectizeInput("cols2", "Select up to 2 variables", choices = columns_numerical(), multiple = TRUE, options = list(maxItems = 2))
  })
    
  
  output$histogram <- renderPlot({
    ggplot(data1(), aes(x=unlist(data1()[,input$col]))) + geom_histogram(color = "green", 
                                                                         alpha = 0.5,
                                                                         bins = input$bins) + xlab(input$col)
  })
  
  data_line_plot <- reactive({
    plotLineDfFormatter(data1(), req(input$cols1))
  })
  
  
  
  output$linechart <- renderPlot({
    ggplot(data_line_plot(), aes_string(x = unlist(data_line_plot()[,1]),
                       y = unlist(data_line_plot()[,3]),
                       colour = unlist(data_line_plot()[,2]))) + geom_line() + xlab(names(data_line_plot()[,1]))
  })
  
  

  
  
  output$scatterplot <- renderPlot({
    ggplot(data1(), aes(x = unlist(data1()[,req(input$cols2[1])]),
                        y = unlist(data1()[,req(input$cols2[2])]))) + geom_point() + xlab(input$cols2[1]) + ylab(input$cols2[2])
  })
  
################################################# VISUALIZATION PLOTS


    
  
######################################################################DATA MENU OUTPUT - summary, spreadsheet, viz
  
  
  ##################################################################### DATA PREPROCESSING OUTPUT 
# 
output$input_logit <- renderUI({
  selectInput("cols_logit", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})
# 
output$input_1diff <- renderUI({
  selectInput("cols_1diff", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})

output$input_2diff <- renderUI({
  selectInput("cols_2diff", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})

output$input_zscore <- renderUI({
  selectInput("cols_zscore", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})

output$input_log <- renderUI({
  selectInput("cols_ln", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})

output$input_1lag <- renderUI({
  selectInput("cols_1lag", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE) 
})

output$input_2lag <- renderUI({
  selectInput("cols_2lag", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE )
})

logit_cols <- reactive({
  
  if ("None" %in% input$cols_logit) {
    return(cbind())
  } else {

  to_paste <- "_logit"
  
 
  df_for_logit <- data1()[, req(input$cols_logit)]
  df_logit <-transformDataset(df_for_logit, "logit")
  cols <- as.character(input$cols_logit)
  colnames(df_logit) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_logit)
  }
})


z_cols <- reactive({
  
  if ("None" %in% input$cols_zscore) {
    return(cbind())
  } else {
  to_paste <- "_Z_score"
  
  df_for_z <- data1()[, req(input$cols_zscore)]
  df_z <-transformDataset(df_for_z, "zscore")
  cols <- as.character(input$cols_zscore)
  colnames(df_z) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_z)
  }
})



ln_cols <- reactive({
  
  if ("None" %in% input$cols_ln) {
    return(cbind())
  } else {
  to_paste <- "_ln"

  df_for_ln <- data1()[, req(input$cols_ln)]
  df_ln <-transformDataset(df_for_ln, "ln")
  cols <- as.character(input$cols_ln)
  colnames(df_ln) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_ln)
  }

})


diff1_cols <- reactive({
  if ("None" %in% input$cols_1diff) {
    return(cbind())
  } else {
  to_paste <- "_1st_diff"
  df_for_1diff <- data1()[,req(input$cols_1diff)]
  df_1diff <- differenceDataset(df_for_1diff,1,1)
  cols <- as.character(input$cols_1diff)
  colnames(df_1diff) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_1diff)
  }
})

diff2_cols <- reactive({
  if ("None" %in% input$cols_2diff) {
    return(cbind())
  } else {
  
  to_paste <- "_2nd_diff"
  df_for_2diff <- data1()[,req(input$cols_2diff)]
  df_2diff <- differenceDataset(df_for_2diff,2,1)
  cols <- as.character(input$cols_2diff)
  colnames(df_2diff) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_2diff)
  }

})

lag1_cols <- reactive({
  if ("None" %in% input$cols_1lag) {
    return(cbind())
  } else {
    to_paste <- "_t-1"
    df_for_1lag <- data1()[, req(input$cols_1lag)]
    df_1lag <- transformDataset(df_for_1lag, "1lag")
    cols <- as.character(input$cols_1lag)
    colnames(df_1lag) <- lapply(cols, function (x) paste(x, to_paste))
    return(df_1lag)
  }
})

lag2_cols <- reactive({
  if ("None" %in% input$cols_2lag) {
    return(cbind())
  } else{
    to_paste <- "_t-2"
    df_for_2lag <- data1()[, req(input$cols_2lag)]
    df_2lag <- transformDataset(df_for_2lag, "2lag")
    cols <- as.character(input$cols_2lag)
    colnames(df_2lag) <- lapply(cols, function (x) paste(x, to_paste))
    return(df_2lag)
  }
})


transformed_data <- reactive({
  cbind(logit_cols(), z_cols(), ln_cols(), diff1_cols(), diff2_cols(), lag1_cols(), lag2_cols())
})

old_and_transformed <- reactive({
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag))  {
    data1()
  } else {
    cbind(data1(), transformed_data())
  }
  
})

columns_final <- reactive({
  colnames(old_and_transformed())
})

output$transformed <- renderTable({
  cbind(logit_cols(), z_cols(), ln_cols(), diff1_cols(), diff2_cols(), lag1_cols(), lag2_cols())
})


##################################################################### DATA PREPROCESSING OUTPUT 

  
##################################################################### DATA MODELING OUTPUT

################################################# UNIT ROOT TESTS

output$unit_vars <- renderUI({
  
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag)) {
    selectInput("unit_vars", "Select variables for unit root tests", choices = columns_numerical(), multiple = TRUE)
  } else {
    selectInput("unit_vars", "Select variables for unit root tests", choices = columns_final(), multiple = TRUE)
  }
  
  
})


unit_vars_df <- reactive({
  old_and_transformed()[, req(input$unit_vars)]
})

output$unit_table <- renderTable({
  generate_output_table(unit_vars_df(),test = req(input$test))
})


################################################# UNIT ROOT TESTS

#################################################  SUR MODELS 


####################### EVALUATION LOOP

output$y <- renderUI({
  
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag)){
  selectInput("dep_var", "Select dependent variable", choices = columns_numerical())
  } else {
    selectInput("dep_var", "Select dependent variable", choices = columns_final())
  }
})

output$x <- renderUI({
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag)){
  selectInput("ind_vars", "Select independent variables", choices = columns_numerical(), multiple = TRUE)
  } else {
    selectInput("ind_vars", "Select independent variables", choices = columns_final(), multiple = TRUE)
  }
})




#construction of variables - dependent
dep <- reactive({
  old_and_transformed()[,req(input$dep_var)]
})

l1_dep <- reactive({
  lagSeries(unlist(dep()),1)
})

l2_dep <- reactive({
  lagSeries(unlist(dep()),2)
})

dep_df <- reactive({
  cbind(dep(), l1_dep(), l2_dep())
})

dep_final_df <- reactive({
  constructLaggedDf(data.frame(deps_df()))  
})


#construction of variables  - independents
indeps_df <- reactive({
  old_and_transformed()[,req(input$ind_vars)]
})

indeps_final_df <- reactive({
  constructLaggedDf(data.frame(indeps_df()))
})


#construtcion of equations and model estimation

mainEquation <- reactive({
  dep() ~ indeps_df()
})

system <- reactive({
  getSystem(Y = dep(), Xset = indeps_df())
})

model <- reactive({
 systemfit(system(), method = "SUR")
})

coefs_all <- reactive({
  getSurCoefs(model(),1)
})

# coefs_X <- reactive({
#   getSurCoefs(model(),2)
# })



coefs_dep <- reactive({
  coefs_all()[[1]]
})



first_column_out <- reactive({
  out <- c("Intercept")
  for (i in as.character(input$ind_vars)) {
    
    out <- c(out, i)
  }
  out <- list(out)
  df <- do.call(cbind.data.frame, out)
  
  return(df)
})

y_column <- reactive({
  out <- c()
  for (i in coefs_dep()) {
    out <- c(out, i)
  }
  out <- list(out)
  df <- do.call(cbind.data.frame, out)
  
  
  
  return(out)
})


x_columns <- reactive({
  df <- do.call(cbind.data.frame, out_X(model()))
  
  df
})

sur_table_out <- reactive({
  df <- cbind(first_column_out(), y_column(), x_columns())
  colnames(df) <- c("Variables", input$dep_var, input$ind_vars)
  df
})


output$sur_out <- renderTable({
  input$action
  
  isolate(sur_table_out())
})



# system <- reactive({
#   list(mainEquation() = mainEquation())
# })
# output$model <- renderText({
#   systemfit(system(), method = "OLS")
# })


####################### EVALUATION LOOP



#################################################  SUR MODELS 




##################################################################### DATA MODELING OUTPUT

  
  
  

  
  
  
})  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


