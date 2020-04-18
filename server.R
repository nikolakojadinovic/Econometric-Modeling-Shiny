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
  
  data_mid <- reactive({
    handleMissing(data(), input$nas) 
  })
  
  
  
  len_data <- reactive({
    nrow(data_mid())
  })
  
  
  date_col <- reactive({
    date_range <- data.frame(seq.Date(from = req(input$startdate),
                                        by = req(input$ts),
                                        length.out = len_data()))
    
    
    
    colnames(date_range) <- "Date Index"
    return(date_range) 
  
    
  })
  
  date_coll <- reactive({
    date_range <- cbind(sapply(seq.Date(from = req(input$startdate),
                                        by = req(input$ts),
                                        length.out = len_data()),as.character.Date))
    
    colnames(date_range) <- "Date_Index"
    return(date_range) 
  })
  
  first_year <- reactive({
    date <- strsplit(date_col()[[1]],"-")
    return(date[[1]][1])
  })
  
  last_year <- reactive({
    date <- strsplit(date_col()[[length(date_col)]], "-")[[1]]
    return(date[[1]][1])
  })
  

  data1 <- reactive({
    if (is.null(date_col()) || is.na(date_col())) {
      return(data_mid())
    } else {
      return(cbind(date_col(), data_mid()))
    }
  })
  
  # output$datum <- renderPrint({
  #   df <- data.frame(date_col()) #data.frame
  #   colnames(df) <- "Date_Index"
  #   return(df)
  # })
  

  
  
  
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
    selectInput("col", "Select the column", choices = columns_plot_final())
  })
  
  output$inputwidget1_line <- renderUI({
    selectizeInput("cols1", "Select multiple variables", choices = columns_plot_final(), multiple = TRUE)
  })
  
  output$inputwidget2_scatter <- renderUI({
    selectizeInput("cols2", "Select 2 variables", choices = columns_numerical(), multiple = TRUE, options = list(maxItems = 2))
  })
    
  hist <- reactive({
    ggplot(old_and_transformed(), aes(x=unlist(old_and_transformed()[,input$col]))) + geom_histogram(color = "blue", 
                                                                         alpha = 0.8,
                                                                         bins = input$bins) + xlab(input$col) +
      theme(panel.grid.major = element_line(size = 0.1, linetype = "solid"), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill ="lightblue")) 
  })
  
  output$histogram <- renderPlot({
    hist()
  })
  
 output$down_hist <- downloadHandler(
    filename = function(){
      paste("Histogram", as.character(Sys.Date()), ".png", sep = "_")
    },
    content = function(file){
      png(file)
      print(hist())
      dev.off()
      
    }
  )
  
  data_line_plot <- reactive({
    
    cbind(date_col(),plotLineDfFormatter(old_and_transformed(), req(input$cols1)))
    
  })
  
  output$melted <- renderTable({
    data_line_plot()
  })
  
  
    linech <- reactive({
      ggplot(data_line_plot(), aes_string(x =  unlist(data_line_plot()[,1]),
                                          y = unlist(data_line_plot()[,4]),
                                          col = unlist(data_line_plot()[,3]))) + 
        geom_line(size = 1) + 
        theme(panel.grid.major = element_line(size = 0.1, linetype = "solid"), 
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill ="lightblue")) 
      
    })
      
  
  
  output$linechart <- renderPlot({
    # ggplot(data_line_plot(), aes_string(x = unlist(data_line_plot()[,1]),
    #                    y = unlist(data_line_plot()[,3]),
    #                    colour = unlist(data_line_plot()[,2]))) + geom_line() + xlab(names(data_line_plot()[,1])) 
    linech()
  })
  
  
  output$down_line <- downloadHandler(
    filename = function(){
      paste("Linechart", as.character(Sys.Date()), ".png", sep = "_")
    },
    content = function(file){
      png(file)
      print(linech())
      dev.off()
    }
  )
  
  scatt <- reactive({
    ggplot(data1(), aes(x = unlist(data1()[,req(input$cols2[1])]),
                        y = unlist(data1()[,req(input$cols2[2])]))) + geom_point() + xlab(input$cols2[1]) + ylab(input$cols2[2]) + 
      theme(panel.grid.major = element_line(size = 0.1, linetype = "solid"), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill ="lightblue")) 
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data1(), aes(x = unlist(data1()[,req(input$cols2[1])]),
                        y = unlist(data1()[,req(input$cols2[2])]))) + geom_point() + xlab(input$cols2[1]) + ylab(input$cols2[2]) + 
      theme(panel.grid.major = element_line(size = 0.1, linetype = "solid"), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill ="lightblue")) 
  })
  
  output$down_scatter <- downloadHandler(
    filename =  function(){
      paste("Scatterplot", as.character(Sys.Date()), ".png", sep = "_")
    },
    content = function(file){
      png(file)
      print(scatt())
      dev.off()
    }
  )
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
  selectInput("cols_2lag", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})

output$input_ld1 <- renderUI({
  selectInput("cols_ld1", "Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
})

output$input_ld2 <- renderUI({
  selectInput("cols_ld2","Select variables for this transformation", choices = c("None", columns_numerical()), multiple = TRUE)
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

ld1_cols <- reactive({
  if ("None" %in% input$cols_ld1) {
    return(cbind())
  } else {
    to_paste <- "_l1.1st_diff"
    df_for_ld1 <- data1()[,req(input$cols_ld1)]
    df_ld1 <- transformDataset(df_for_ld1, "ld1")
    cols <- as.character(input$cols_ld1)
    colnames(df_ld1) <- lapply(cols, function (x) paste(x, to_paste))
    return(df_ld1)
  }
})

ld2_cols <- reactive({
  if ("None" %in% input$cols_ld2) {
    return (cbind())
  } else {
    to_paste <- "_l1.2nd_diff"
    df_for_ld2 <- data1()[, req(input$cols_ld2)]
    df_ld2 <- transformDataset(df_for_ld2, "ld2")
    cols <- as.character(input$cols_ld2)
    colnames(df_ld2) <- lapply(cols, function(x) paste(x, to_paste))
    return(df_ld2)
  }
})

transformed_data <- reactive({
  data.frame(cbind(logit_cols(), z_cols(), ln_cols(), diff1_cols(), diff2_cols(), lag1_cols(), lag2_cols(), ld1_cols(), ld2_cols()))
})



old_and_transformed <- reactive({
  
  
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) &&
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag) && is.null(input$cols_ld1)
       && is.null(input$cols_ld2)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag && "None" %in% input$cols_ld1 && "None" %in% input$cols_ld2))  {
    
    data1()
  } else {
    
    
    cbind(data1(), transformed_data())
  }

})

columns_final <- reactive({
  names(old_and_transformed())
})


columns_plot_final <- reactive({
  names(getNumeric(old_and_transformed()))
})
output$transformed <- renderDataTable({
  cbind(logit_cols(), z_cols(), ln_cols(), diff1_cols(), diff2_cols(), lag1_cols(), lag2_cols(), ld1_cols(), ld2_cols())
  
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

unitt <- function(){
  
  data.frame(generate_output_table(unit_vars_df(),test = req(input$test)))
}
output$unit_table <- renderTable({
  input$unit_action
  isolate(unitt())
})

output$down_unit <- downloadHandler(
  filename = function(){
    paste("UnitRootTest", as.character(Sys.Date()), ".csv", sep = "_")
  },
  content = function(file){
    write.csv(unitt(), file)
    
    
  }
)

################################################# UNIT ROOT TESTS

#################################################  SUR MODELS 


####################### GETTING USER INPUT

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

output$ar1 <- renderUI({
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag)){
    selectInput("ar1_vars", "Select variables for AR(1) equations", choices = columns_numerical(), multiple = TRUE)
  } else {
    selectInput("ar1_vars", "Select variables for AR(1) equations", choices = columns_final(), multiple = TRUE)
  }
})
####################### GETTING USER INPUT

####################### SUR MODEL ESTIMATION

#construction of variables - dependent
dep <- reactive({
 old_and_transformed()[,req(input$dep_var)]

})

dep2 <<- reactive({
  return(as.numeric(unlist(dep())))
})



#construction of variables  - independents
indeps_df <- reactive({
  old_and_transformed()[as.character(req(input$ind_vars))]
 
})


ar1_df <- reactive({
  old_and_transformed()[,req(input$ar1_vars)]
})

total_indeps <- reactive({
  cbind(indeps_df(), ar1_df())
})

total_ar_coefs <- reactive({
  getArCoefs(total_indeps())
})


ncol_indeps <- reactive({
  ncol(indeps_df())
})

#estimation itself


model2 <<- reactive({
  nlme :: gls(dep2() ~ modelFitPrepare(total_indeps()), na.action = na.omit)
 
})

actual_coefs <- reactive({
  as.double(model2()$coefficients)
})



coefs_ar <- reactive({
  getArCoefs(indeps_df())
})

indeps_models <- reactive({
  getArCoefsModel(total_indeps())
})

####################### SUR MODEL ESTIMATION

####################### PRINTING RESULTS ON CLIENT


### MAIN EQUATION OUTPUT
r2_main <- reactive({
  getR2(dep2(), model2()$fitted)
})

first_row_out <- reactive({
 c(as.character(input$dep_var), "Intercept", as.character(input$ind_vars), as.character(input$ar1_vars),  "R Squared", "Durbin Watson")
})

second_row_out <- reactive({
  c("coefs: ", sapply(actual_coefs(), round, digits = 4), r2_main(), getDWstat(model2()$residuals))
})

third_row_out <- reactive({
  c("p-value:", sapply(getPVals(model2()), round, digits = 4), "-", "-")
})
### MAIN EQUATION OUPUT


###AR(1) OUTPUT

df_out <- reactive({
  df <- rbind(second_row_out(), third_row_out())
  colnames(df) <- first_row_out()
  return(df)
})

x_out <- reactive({
  generate_X_out2(total_indeps())
})

output$sur_y_out <- renderTable({
  input$action

  isolate(df_out())
})

output$sur_x_out <- renderTable({
  input$action
  isolate(x_out())
})

###AR(1) OUTPUT

####################### PRINTING RESULTS ON CLIENT


#################################################  SUR MODELS 

################################################# OLS MODELS


output$linreg_y <- renderUI({
  
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag)){
    selectInput("y_lin", "Select dependent variable", choices = columns_numerical())
  } else {
    selectInput("y_lin", "Select dependent variable", choices = columns_final())
  }
  
})

output$linreg_x <- renderUI({
  
  if ((is.null(input$cols_logit) && is.null(input$cols_zscore) && is.null(input$cols_ln) && 
       is.null(input$cols_1diff) && is.null(input$cols_2diff) && is.null(input$cols_1lag) && is.null(input$cols_2lag)) ||
      ("None" %in% input$cols_logit && "None" %in% input$cols_zscore && "None" %in% input$cols_ln 
       && "None" %in% input$cols_1diff && "None" %in% input$cols_2diff 
       && "None" %in% input$cols_1lag && "None" %in% input$cols_2lag)){
    selectInput("x_lin", "Select independent variables", choices = columns_numerical(), multiple = TRUE)
  } else {
    selectInput("x_lin", "Select independent variables", choices = columns_final(), multiple = TRUE)
  }
})



dep_lin <- reactive({
  old_and_transformed()[,req(input$y_lin)]
})

indeps_lin <- reactive({
  old_and_transformed()[as.character(req(input$x_lin))]
})

reg_model <- reactive({
  lm(dep_lin() ~ modelFitPrepare(indeps_lin()))
})

coefs_lin <- reactive({
  as.double(reg_model()$coefficients)
})

r2_main_lin <- reactive({
  getR2(dep_lin(), reg_model()$fitted)
})

first_row_out_lin <- reactive({
  c(as.character(input$y_lin), "Intercept", as.character(input$x_lin),  "R Squared")
})

second_row_out_lin <- reactive({
  c("coefs: ", sapply(coefs_lin(), round, digits = 4), r2_main_lin())
})

third_row_out_lin <- reactive({
  c("p-value:", sapply(getPValsLin(reg_model()), round, digits = 4), "-")
})

df_out_lin <- reactive({
  df <- rbind(second_row_out_lin(), third_row_out_lin())
  colnames(df) <- first_row_out_lin()
  return(df)
})


output$linreg_results <- renderTable({
  input$lin
  
  isolate(df_out_lin())
})

plot_lin <- reactive({
  df <- data.frame(date_col(), dep_lin(), reg_model()$fitted)
  colnames(df) <- c("Date","Actual", "Fitted")
  return(df)
})

output$lele <- renderText({
  class(date_col()[5])
})



fit_plot <- reactive({
  ggplot(plot_lin()) + geom_line(aes(x = Date, y = Actual, colour = "Actual")) + geom_line(aes(x=Date, y = Fitted, color = "Fitted")) + 
    theme(panel.grid.major = element_line(size = 0.1, linetype = "solid"), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill ="lightblue")) + 
    scale_color_manual(values = c(Actual = "blue",
                                 Fitted = "red")) 
})

output$fit_viz <- renderPlot({
  input$lin
  
  isolate(fit_plot())
})
################################################# OLS MODELS

################################################# MONTE CARLO 



###################### CREATING STRESSED SCENARIO - CHANGE FETCHING
previous_data <- reactive({
  if(is.null(total_indeps()) || is.na(total_indeps())){
    return()
  }
  else {
    round_df(cbind(date_col(),total_indeps()), digits = 2)
  }
})




rows_arg <- reactive({
  nrow(previous_data())
})

cols_arg <- reactive({
  ncol(previous_data())
})

for_edit <- reactive({
  rhandsontable(previous_data())
})

observe({
  input$changeBtn
  if(!is.null(values[["hot"]])){
    for_edit <- reactive({
      values$hot
    })
  }
})

observe({
  if(!is.null(input$hot)){
    for_edit <- reactive({
      hot_to_r(input$hot)
    })
    setHot(for_edit())
  }
})

output$hot <- renderRHandsontable({
  for_edit()
})

stressed <- reactive({
  hot_to_r(input$hot)
})

actual_stressed <- reactive({
  getNumeric(stressed())
})

output$promena <- renderTable({
  stressed()
})


###################### CREATING STRESSED SCENARIO - CHANGE FETCHING

###################### ACTUAL MONTE CARLO SIMULATION

output$startY <- renderUI({
  selectInput("starty", "Select start date for the simulation horizon",
              choices = date_coll())
})

remain_index <- reactive({
  which(date_coll() == as.character(req(input$starty)))
})

endy_choices <- reactive({
  date_coll()[(remain_index()+1):length(date_coll())]
})

output$endY <- renderUI({
  selectInput("endy", "Select end date for the simulation horizon",
              choices = endy_choices())
})

endy_index <- reactive({
  which(date_coll() == as.character(req(input$endy)))
})



output$huhu <- renderPrint({
  ncol(residual_df_base())
})



data_baseline_simulation <- reactive({
  previous_data()[remain_index():endy_index(),]
})

dep_resid <- reactive({
  model2()$residuals
})

indeps_resid <- reactive({
  getXresidDF(indeps_models())
})




residual_df_base <- reactive({
  inds <- matrix(unlist(indeps_resid()), nrow = length(indeps_resid()[[1]]),
                 ncol = length(indeps_resid()), byrow = T)
  df <- cbind(dep_resid(), inds)
  return(df[remain_index():endy_index(),])
})

cholesky_base <- reactive({
  chol(cov(residual_df_base()))
})

inner_loop_end <- reactive({
  endy_index() - remain_index()
})

rnorm_len <- reactive({
  ncol(residual_df_base())
})

outer_loop_end <- reactive({
  as.integer(as.integer(req(input$nruns)) / inner_loop_end())
})


stressed_estimates <- reactive({
  getStressedEstimates(actual_stressed(), actual_coefs())
})

mc_results <- reactive({
  
  values_baseline <- runMonteCarlo(inner = inner_loop_end(),
                         outer = outer_loop_end(),
                         dim =  rnorm_len(),
                         start_index = remain_index(),
                         chol = cholesky_base(),
                         model = model2(),
                         revert = as.integer(req(input$revert)))
  
  values_stressed <- runMonteCarlo(inner = inner_loop_end(),
                                    outer = outer_loop_end(),
                                    dim =  rnorm_len(),
                                    start_index = remain_index(),
                                    chol = cholesky_base(),
                                    model = model2(),
                                    stress_estim = stressed_estimates(), 
                                    revert = as.integer(req(input$revert)))
  
  
  df <- data.frame(cbind(values_baseline, values_stressed))
  colnames(df) <- c("baseline", "stressed")
  return(df)
})


output$melt_mc <- renderTable({
  mc_results()
})


mc_hist <- reactive({
  ggplot(mc_results()) + geom_histogram(aes(x = baseline, fill = "baseline"), alpha = 0.3, color = "blue", bins = 100) + 
    geom_histogram(aes(x = stressed, fill = "stressed"), alpha = 0.6, bins = 100, color = "blue") +
    scale_y_continuous(limits = c(0, max(mc_results()[,1]) + 500)) +
    scale_fill_manual(values = c(baseline = "green",
                                 stressed = "red")) + 
    theme(panel.grid.major = element_line(size = 0.1, linetype = "solid"), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill ="lightblue")) + xlab("Baseline and stressed simulated values")
})


output$monte_carlo_histogram <- renderPlot({
  
  input$runmc
  
  isolate(mc_hist())
  
})

output$down_mchist <- downloadHandler(
  filename = function(){
    paste("Monte_Carlo_NPL_Simulation", as.character(Sys.Date()), ".png", sep = "_")
  },
  content = function(file){
    png(file)
    print(mchist())
    dev.off()
  }
)

output$proba <- renderPrint({
  typeof(input$rev_diff)
})

###################### ACTUAL MONTE CARLO SIMULATION

################################################# MONTE CARLO SIMULATIONS


##################################################################### DATA MODELING OUTPUT

})  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


