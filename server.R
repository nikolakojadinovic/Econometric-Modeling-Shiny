library(shiny)
library(shinydashboard)
library(ggplot2)
library(rsconnect)
library(DT)






shinyServer(function(input, output, session){
  
  #REACTIVE FUNCTIONS FILE HANDLING
  
  
  file1 <- reactive({
    input$file
    
  })
  
  
  
  
  data <- reactive({
    
    if(is.null(file1())){
      return()
      } else if(tools::file_ext(file1()$datapath) == "csv") {
        read.table(file = file1()$datapath, 
                   sep = input$sep, 
                   header = input$header, 
                   skipNul = TRUE)
      } else if (tools::file_ext(file1()$datapath) == "xlsx"){
        readxl::read_excel(path = file1()$datapath)
      }
  })
  
  
  
  
 
  output$table <- renderDataTable({
    if(is.null(data())){return()}
    data()
  })
  
 
})
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  


