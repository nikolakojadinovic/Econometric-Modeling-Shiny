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
        readxl::read_xlsx(path = file1$datapath)
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
                       colour = unlist(data_line_plot()[,2]))) + geom_line()
  })
  
  

  
  
  output$scatterplot <- renderPlot({
    ggplot(data1(), aes(x = unlist(data1()[,req(input$cols2[1])]),
                        y = unlist(data1()[,req(input$cols2[2])]))) + geom_point()
  })
  # output$scatterplot <- renderPlot({
  #   ggplot(data_plot(), aes_string(x = unlist(data_plot()[,1]),
  #                                  y = unlist(data_plot()[,3]),
  #                                  colour = unlist(data_plot()[,2]))) + geom_point()
  # })
    
  })
  
  
  

  
  ################################################# VISUALIZATION PLOTS
    
  
  ##################################################################### DATA MENU OUTPUT - summary, spreadsheet, viz

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


