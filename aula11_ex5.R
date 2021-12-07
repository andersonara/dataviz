library(shiny)
library(shinythemes)


ui<-fluidPage(
  theme = shinytheme("darkly"),

  titlePanel("DISTRIBUIÇÃO  NORMAL"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("mi","Selecione o Valor da média:",-2,2,0),
      sliderInput("sig","Selecione o Valor do desvio padrão:",0,3,1)
    ),
    mainPanel("",
              plotOutput("densidade")
              )
  )
)


server<-function(input,output){
  
  output$densidade<-renderPlot({
    par(bg='black')
    x=seq(-3,3,0.01)
    plot(x,dnorm(x,input$mi,input$sig),type='l',
         lwd=2, xlab='',ylab='', col='navyblue')
    axis(1,col.axis='navyblue',col='navyblue')
    axis(2,col.axis='navyblue',col='navyblue')
  })
  
}

shinyApp(ui,server)