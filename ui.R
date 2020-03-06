library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)
library(reshape2)





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
                                          choices = c("Omit rows with NAs" = "omit_rows","Replace with mean" = "rep_mean_mode", "Replace with median" = "rep_median_mode"))),
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
                  
                  box(title = "Histogram", uiOutput("inputwidget_hist"), 
                      sliderInput("bins", "Select number of bins", min = 5, max = 20, value = 8),
                            plotOutput("histogram", height = 150, width = 200)),
                  
                  box(title = "Line chart",uiOutput("inputwidget1_line"), plotOutput("linechart", height = 350, width = 450)),
                  
                  box(title = "Scatterplot",uiOutput("inputwidget2_scatter"), plotOutput("scatterplot", height = 350, width = 450))
                 
                  
                  
                )),
        
        tabItem(tabName = "about", tableOutput("test_plot"))
    )
    
    
    
    
  ) #dashboardPage
 )
)#shinyUI