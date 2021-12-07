#INSTALANDO PACOTES
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")

#CHAMANDO PACOTES
require(shiny)
require(shinydashboard)
require(ggplot2)



#DEFININDO INTERFACE DE USUÁRIO

ui <- dashboardPage(
  #PAINEL DE CABEÇALHO DA PÁGINA
  dashboardHeader(title = "RShiny"),
  #PAINEL LATERAL
  dashboardSidebar(h1("MENU LATERAL"),
                   h2("TEXTO 2"), 
                   h3("TEXTO 3")),
  #PAINEL PRINCIPAL
  dashboardBody(
    fluidRow(
    box(plotOutput("graf1", height = 250)),
    box(title = "Escolha:",
      sliderInput("slider", "Número de Observações:", 1, 500, 50))
    
    )
  )
)


#DEFININDO FUNÇÕES DO R
server<-function(input,output){
  
  output$graf1 <- renderPlot({
    #Gerando dados
    gen <- rnorm(500)[seq_len(input$slider)]
    dadosg <- data.frame(X=gen)
    
    #Gráfico
    graf1 <- ggplot(dadosg, aes(x=X)) + 
      geom_histogram(color="blue", fill="gold")+
      xlab("dados")+
      ylab("Densidade")+
      theme_bw()
    graf1
  })
}

shinyApp(ui,server)
