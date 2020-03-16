library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(rsconnect)
library(DT)
library(reshape2)
library(urca)
library(systemfit)





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
        
        menuItem("Data Modeling", tabName = "models", icon = icon("sitemap"),
                 menuSubItem("Unit Root Test", tabName = "unit"),
                 menuSubItem("OLS Linear Regression", tabName = "linreg"),
                 menuSubItem("SUR Models", tabName = "sur" ),
                 menuSubItem("Monte Carlo Simulation", tabName = "mc" )),
        
        menuItem("About", tabName = "about",icon = icon("clipboard"))
        
        
        
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
                            plotOutput("histogram", height = 350, width = 500)),
                  
                  box(title = "Line chart",uiOutput("inputwidget1_line"), plotOutput("linechart", height = 350, width = 450)),
                  
                  box(title = "Scatterplot",uiOutput("inputwidget2_scatter"), plotOutput("scatterplot", height = 350, width = 450))
                 
                  
                  
                )),
        tabItem(tabName = "preprocess",
                fluidRow(
                  tabBox(id = "trans",
                      tabPanel("LOGIT", uiOutput("input_logit")),
                      tabPanel("1st difference", uiOutput("input_1diff")),
                      tabPanel("2nd difference", uiOutput("input_2diff")),
                      tabPanel("Standardize (z-score)", uiOutput("input_zscore")),
                      tabPanel("Natural logarithm", uiOutput("input_log")),
                      tabPanel("1st order lag", uiOutput("input_1lag")),
                      tabPanel("2nd order lag", uiOutput("input_2lag"))
                      
                      
                      ),
                
                  
         
         box(title = "Results", tableOutput("transformed"))
         )),
        
        
        tabItem(tabName = "unit",
                fluidRow(
                  box(title = h3("Unit Root Test Options"), uiOutput("unit_vars"), 
                      radioButtons("test", "Select test", 
                      choices = c("ADF" = "adf", "Phillips Perron" = "pp")),
                      uiOutput("test_type")),
                  box(title = h3("Test Results"), tableOutput("unit_table"))
                )),
        
        tabItem(tabName = "sur",
                fluidRow(
                  
                    box(h3("Evaluation Loop Options"), uiOutput("y"), uiOutput("x")),
                    box(h3("SUR model summary"))
                )),
        
        tabItem(tabName = "about", textOutput("sur"))
        
        

        
    )
    
    
    
    
  ) #dashboardPage
 )

)#shinyUI
