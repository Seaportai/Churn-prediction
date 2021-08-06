

library(shiny)
library(DT)
library(shinydashboard)
shinyUI(dashboardPage(skin = 'blue',
  dashboardHeader(title="EMPLOYEE CHURN "),
  
  dashboardSidebar(
    
      fileInput("file","Choose a CSV file",accept=c('csv','comma-seperated-values','.csv')),
      downloadButton("downloadData", "Download")
    ),
    dashboardBody(
      fluidRow(
        
        fluidPage(theme="bootstrap.min.css",
                  
                  
                  tags$style(HTML("
                                  .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
                                  .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
                                  .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
                                  .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
                                  .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
                                  ")),
        
        
        
        
        tabBox( width = 400, height = 5500 ,
     
      
      
                  tabPanel("Output", p("The aim of this application is to predict the Employee Churn for the given data.",
                                       style="font-family:'Euphemia';font-size:11pt"), 
                           box( width = 8, height = 600 ,
                    title = "Output of Churn and Non-Churn ",status = "info", solidHeader = TRUE,collapsible = TRUE,
                    dataTableOutput("y_cap", height = 250, width = NULL))), 
                
                
                tabPanel("Not Leaving",# p("The aim of this application is to predict the Employee Churn for the given data.",
                                     #style="font-family:'Euphemia';font-size:11pt"), 
                         box( width = 7, height = 28000,
                              title = "Output of Employee Not Leaving ",status = "success", solidHeader = TRUE,collapsible = TRUE,
                              tableOutput("Not_Churn" ))), 
                                            
                
                tabPanel("Leaving",# p("The aim of this application is to predict the Employee Churn for the given data.",
                                        #style="font-family:'Euphemia';font-size:11pt"), 
                         box( width = 7, height = 5500,
                              title = " Output of Employee Leaving",status = "danger", solidHeader = TRUE,collapsible = TRUE,
                              tableOutput("Churn"))) 
      
                
               
                  ))))))