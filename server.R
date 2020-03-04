library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)







shinyServer(function(input, output, session){
  
  #####################################################################REACTIVE FUNCTIONS FILE HANDLING
  
  
  # file1 <- reactive({
  #   input$file
  #   
  # })

  
  data <- reactive({
    
    file1 <- input$file
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
  
  columns <- reactive({
    names(data())
  })
  
  output$contents <- renderTable({
    data()
  })
  
  ##################################################################### REACTIVE FUNCTIONS FILE HANDLING
  
 
  
  
  
  ##################################################################### DATA MENU OUTPUT - summary, spreadsheet, viz
  
  output$table <- renderDataTable({
    if(is.null(data())){return()}
    data()
  })
  
  
  output$struct <- renderPrint({
    if(is.null(data())){return()}
    summary(data())
  })
  
  ################################################# VISUALIZATION PLOTS
  
  
  output$inputwidget <- renderUI({
    selectInput("col", "Select the column", choices = columns())
  })
  
  output$inputwidget1 <- renderUI({
    selectInput("cols1", "Select up to 3 variables", choices = columns(), multiple = TRUE)
  })
  
  output$inputwidget2 <- renderUI({
    selectInput("cols2", "Select multiple variables", choices = columns(), multiple = TRUE)
  })
    
  
  output$histogram <- renderPlot({
    ggplot(data(), aes(x=unlist(data()[,input$col]))) + geom_histogram(bins = input$bins)
  })
  
  
  
  
  

  
  ################################################# VISUALIZATION PLOTS
    
  
  ##################################################################### DATA MENU OUTPUT - summary, spreadsheet, viz
})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


