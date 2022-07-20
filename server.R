#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
set.seed(122)
histdata <- rnorm(500)

server<-function(input,session,output) {
  output$select<-renderUI({
    selectInput("degisken",h3(" Choose Variable"), choices = names(iris), multiple = TRUE)
  })
  output$table<-renderTable({
    head(iris,15)
  })
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data,main = "Graph",xlab="data", ylab="frequency")
  })
  output$plot<-renderPlot({
    teta<-seq(-3,3,0.1)
    P<- 1 /(1 + exp(-input$parama *(teta- input$paramb)))
    plot(teta, P,type="l",col="blue",lwd=4,    
         main="ITEM CHARACTERISTIC CURVE",xlab="Ability",
         ylab="Correct Response Probability"    )
  })
  
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    t_test(x1, x2)
    
  })
  output$plot2 <- renderPlot({
    plot(dataset)
  }, res = 96)
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
  
}



