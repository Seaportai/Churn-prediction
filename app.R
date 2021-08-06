
library(shiny)
library(shinydashboard)
library(neuralnet)
library(nnet)
library(DT)

set.seed(03)
load("meraki.rda")

Logged = FALSE
my_username <- "test1"
my_password <- "password1"

ui <- dashboardPage(skin='blue',
                    
  ###################################################################################################################       
                    
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
      
                
               
                  ))),verbatimTextOutput("dataInfo")
                    )
  #####################################################################################################################################################       
)


server = function(input, output,session) {
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        # modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    } 
  })
  
  
  
  obs3 <- observe({
    if(Logged <<- TRUE)
    req(input$myuser)
    showModal(dataModal())
  })
  
  output$dataInfo <- renderPrint({
    

 ####################################################################################################       
        
    pred<-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      file2 <- read.csv(file1$datapath,header=TRUE)
      withProgress(message='Loading table',value=30,
                   {
                     
                     n<-10
                     
                     for(i in 1:n){
                       incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
                       Sys.sleep(0.1)
                     }
                   })
      file2$Gender<-ifelse(file2$Gender=="Male",1,0)
      file2$OverTime<-ifelse(file2$OverTime=="Yes",1,0)
      
      a=data.frame(class.ind(file2$BusinessTravel))
      b=data.frame(class.ind(file2$Department))
      c=data.frame(class.ind(file2$EducationField))
      d=data.frame(class.ind(file2$JobRole))
      e=data.frame(class.ind(file2$MaritalStatus))
      file2<-data.frame(file2,a,b,c,d,e)
      file3<-file2[,-c(2,4,6,9,15,17)]
      scaled=data.frame(scale(file3,center=mins,scale=maxs-mins))
      scaled
      test_p<-compute(model,scaled[,-1])
      Churning_Probability<-unlist(test_p$net.result)
      Prediction<-ifelse(Churning_Probability>=0.5,"Churn","Not_Churn")
      final_table<-data.frame(file2[,c(1,2)],Prediction)
      final_table
    })
    
    output$y_cap <- renderDataTable({
      pred()
    }) 
    output$Not_Churn <- renderTable({
      subset(pred(),Prediction=="Not_Churn")
    }) 
    output$Churn <- renderTable({
      subset(pred() ,Prediction=="Churn")
    }) 
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Employee Churn Prediction", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(pred(), file, row.names = FALSE)
      }
    )
    
    
 ####################################################################################################   
    
  })
  
}

shinyApp(ui,server)
  
  