
library(shiny)
library(neuralnet)
library(nnet)
library(DT)

set.seed(03)
load("meraki.rda")
shinyServer(function(input,output){
  
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

})





