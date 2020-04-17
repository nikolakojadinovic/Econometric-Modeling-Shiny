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






shinyUI(
  dashboardPage(skin = "blue",
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
        menuItem("Time series upload options", tabName = "ts", icon = icon("upload"),
                 menuSubItem(radioButtons("ts", "Time series frequency",
                                          choices = c("daily" = "day",
                                                      "monthly" = "month",
                                                      "yearly" = "year"),
                                          selected = NULL)),
                 menuSubItem(dateInput(
                   inputId =  "startdate", 
                   label = "Select starting date:", 
                   value = Sys.Date() - 10,
                   min = as.Date("1970/01/01")
                 )),
                 helpText("Choose Jan 1st if your data is"),
                 helpText("monthly or yearly.")),
        
        menuItem("Data", tabName = "data", icon = icon("database"),
                 menuSubItem("Summary", tabName = "summary"),
                 menuSubItem("Visualization", tabName = "viz"),
                 menuSubItem("View spreadsheet", tabName = "spread")),
        
        menuItem("Data preprocessing", tabName = "preprocess", icon = icon("desktop")),
        
        menuItem("Data Modeling", tabName = "models", icon = icon("sitemap"),
                 menuSubItem("Unit Root Test", tabName = "unit"),
                 menuSubItem("OLS Linear Regression", tabName = "linreg"),
                 menuSubItem("SUR Models", tabName = "sur" )),
        menuItem("Monte Carlo Simulation", tabName = "mc", icon = icon("infinity")),
        
        menuItem("About", tabName = "about",icon = icon("clipboard")),
        menuItem("TEST", tabName = "TEST")
        
        
        
      )
    ),
    dashboardBody(
      tabItems(

        tabItem(tabName = "summary", verbatimTextOutput("struct")),
        
      
      
        
        tabItem(tabName = "spread", dataTableOutput("table")),
        
        tabItem(tabName = "viz",
                fluidRow(
                  
                  box(title = "Histogram", uiOutput("inputwidget_hist"), 
                      sliderInput("bins", "Select number of bins", min = 5, max = 60, value = 8),
                            plotOutput("histogram", height = 350, width = 450),
                            downloadButton("down_hist", "Download the plot"),
                      solidHeader = T, collapsible = T, status = "primary"),
                  
                  box(title = "Line chart",uiOutput("inputwidget1_line"), plotOutput("linechart", height = "350px"),
                      downloadButton("down_line", "Download the plot"), solidHeader = T, collapsible = T, status = "primary"),
                  
                  box(title = "Scatterplot",uiOutput("inputwidget2_scatter"), plotOutput("scatterplot", height = 350, width = 450),
                      downloadButton("down_scatter", "Download the plot"), solidHeader = T, collapsible = T, status = "primary")
                 
                  
                  
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
                      tabPanel("2nd order lag", uiOutput("input_2lag")),
                      tabPanel("Diff 1st lag", uiOutput("input_ld1")),
                      tabPanel("Diff 2nd lag", uiOutput("input_ld2"))

                      
                      ),
                
                  
         
         box(title = "Results", style = 'overflow-x: scroll', tableOutput("transformed"), solidHeader = T, collapsible = T, status = "primary")
         )),
        
        
        tabItem(tabName = "unit",
                fluidRow(
                  box(title = h3("Unit Root Test Options"), uiOutput("unit_vars"), 
                      radioButtons("test", "Select test", 
                      choices = c("ADF" = "adf", "Phillips Perron" = "pp", "Elliot Rothenberg Stock" = "ers")),
                      uiOutput("test_type"),
                      solidHeader = T, collapsible = T, status = "primary"),
                  box(title = h3("Test Results"), tableOutput("unit_table"), downloadButton("down_unit", "Download the report"),
                      solidHeader = T, collapsible = T, status = "primary")
                )),
        
        tabItem(tabName = "sur",
                fluidRow(
                  
                    box(h3("Model Specification"), uiOutput("y"), uiOutput("x"), uiOutput("ar1"), actionButton("action", "Estimate the model!"),
                        solidHeader = T, collapsible = T, status = "primary"),
          

                    box(h3("SUR model summary"), 
                        helpText("Main equation summary"),
                        div(style = 'overflow-x: scroll', tableOutput('sur_y_out'), 
                            helpText("AR (1) equations summary"), 
                            tableOutput("sur_x_out")),
                        solidHeader = T, collapsible = T, status = "primary")
                )),
        
        tabItem(tabName = "mc",
                fluidRow(
                  tabBox(
                      tabPanel(h3("Creating stressed scenarios"), 
                               helpText("Overwrite values in cells to represent stressed scenario. Make sure you change the dependent variables from SUR model to see the impact of stress testing!"),
                               actionButton("changeBtn", "Apply changes"),
                                rHandsontableOutput("hot")),
                      tabPanel(h3("Run Monte Carlo"),
                               radioButtons("nruns", "Select number of runs",
                                            choices = c("1000" = 1000,
                                                        "5000" = 5000,
                                                        "10000" = 10000
                                                        )),
                          
                               checkboxInput("reverse", "Revert transformations"),
                               uiOutput("startY"),
                               uiOutput("endY"),
                               helpText("Revert transformation if your dependent variable in SUR model was previously transformed.
                                        If that's not the case (or if you want to see the simulation result on a transformed scale), leave it unchecked."),
                               actionButton("runmc", "Run Monte Carlo!"))
                      
                  
                  
                ),
                box(h3("Monte Carlo Simulation Results"), plotOutput("monte_carlo_histogram"),solidHeader = T, collapsible = T, status = "primary", background = "navy"))),
        
        
        
        
        tabItem(tabName = "about", tableOutput("melt_mc")),
        tabItem(tabName = "TEST", tableOutput("proba"))
        
        

        
    )
    
    
    
    
  ) #dashboardPage
 )

)#shinyUI
