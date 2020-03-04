library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)




shinyUI(
  dashboardPage(skin = "green",
    dashboardHeader(title = "Econometric modeling", titleWidth = 500),
    
    dashboardSidebar(
      
      
      sidebarMenu(
        
        fileInput("file", "Upload your file"),
        menuItem("Upload options", tabName = "options", icon = icon("upload"),
                 menuSubItem(radioButtons("sep", "Separator",
                                          choices = c("Comma" = ",", "Tab" = "\t", "Space" = " ", "Semicolon" = ";"))),
                 menuSubItem(radioButtons("nas", "Handle missing values", 
                                          choices = c("Omit rows with NAs", "Replace with mean", "Replace with median"))),
                 menuSubItem(checkboxInput("header", "Header", value = TRUE))),
        
        menuItem("Data", tabName = "data", icon = icon("database"),
                 menuSubItem("Summary", tabName = "summary"),
                 menuSubItem("Visualization", tabName = "viz"),
                 menuSubItem("View spreadsheet", tabName = "spread")),
        
        menuItem("Data preprocessing", tabName = "preprocess", icon = icon("desktop")),
        
        menuItem("About", tabName = "about", icon = icon("clipboard"))
        
        

      )
      
      
      
      
    ),
    
    
    
    
    dashboardBody(
      tabItems(

        tabItem(tabName = "summary", verbatimTextOutput("struct")),
        
      
      
        
        tabItem(tabName = "spread", dataTableOutput("table")),
        
        tabItem(tabName = "viz",
                fluidRow(
                  
                  box(title = "Histogram", uiOutput("inputwidget"), 
                      sliderInput("bins", "Select number of bins", min = 10, max = 60, value = 20),
                            plotOutput("histogram", height = 250)),
                  
                  box(title = "Line chart",uiOutput("inputwidget1")),
                  
                  box(title = "Scatterplot",uiOutput("inputwidget2"))
                 
                  
                  
                )),
        
        tabItem(tabName = "about", tableOutput("contents"))
        
        
        
        
        
        
        
      )
      
      
      
      
    )
    
    
    
    
  ) #dashboardPage
) #shinyUI