library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)
library(reshape2)








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

output$input_logit <- renderUI({
  selectInput("cols_logit", "Select variables for this transformation", choices = columns_numerical(), multiple = TRUE)
})

output$input_1diff <- renderUI({
  selectInput("cols_1diff", "Select variables for this transformation", choices = columns_numerical(), multiple = TRUE)
})

output$input_2diff <- renderUI({
  selectInput("cols_2diff", "Select variables for this transformation", choices = columns_numerical(), multiple = TRUE)
})

output$input_zscore <- renderUI({
  selectInput("cols_zscore", "Select variables for this transformation", choices = columns_numerical(), multiple = TRUE)
})

output$input_log <- renderUI({
  selectInput("cols_ln", "Select variables for this transformation", choices = columns_numerical(), multiple = TRUE)
})



logit_cols <- reactive({
  
  to_paste <- "_logit"
  df_for_logit <- data1()[, req(input$cols_logit)]
  df_logit <-transformDataset(df_for_logit, "logit")
  cols <- as.character(input$cols_logit)
  colnames(df_logit) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_logit)
})


z_cols <- reactive({
  to_paste <- "_Z_score"
  df_for_z <- data1()[, req(input$cols_zscore)]
  df_z <-transformDataset(df_for_z, "zscore")
  cols <- as.character(input$cols_zscore)
  colnames(df_z) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_z)
  
})



ln_cols <- reactive({
  to_paste <- "_ln"
  df_for_ln <- data1()[, req(input$cols_ln)]
  df_ln <-transformDataset(df_for_ln, "ln")
  cols <- as.character(input$cols_ln)
  colnames(df_ln) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_ln)
  
})


diff1_cols <- reactive({
  to_paste <- "_1st_diff"
  df_for_1diff <- data1()[,req(input$cols_1diff)]
  df_1diff <- differenceDataset(df_for_1diff,1,1)
  cols <- as.character(input$cols_1diff)
  colnames(df_1diff) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_1diff)

})

diff2_cols <- reactive({
  to_paste <- "_2nd_diff"
  df_for_2diff <- data1()[,req(input$cols_2diff)]
  df_2diff <- differenceDataset(df_for_2diff,2,1)
  cols <- as.character(input$cols_2diff)
  colnames(df_2diff) <- lapply(cols, function(x) paste(x, to_paste))
  return(df_2diff)

})

transformed_data <- reactive({
  if (is.null(input$cols_logit) & is.null(input$cols_zscore) & is.null(input$cols_ln) & is.null(input$cols_1diff) & is.null(input$cols_2diff)) {
    return()
  }
  df <- cbind(logit_cols(), z_cols(), ln_cols(), diff1_cols(), diff2_cols())
  return(df)
})



output$transformed <- renderTable({
  transformed_data()
})










##################################################################### DATA PREPROCESSING OUTPUT 

  
  
  
  
  
  

  
  
  
})  
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


