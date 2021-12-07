library(shiny)

#DEFININDO INTERFACE DE USUÁRIO
ui<- fluidPage(
  
  #PAINEL DE TÍTULO DA PÁGINA
  titlePanel(""),
  
  #TIPO DE LAYOUT
  sidebarLayout(
    
    #PAINEL LATERAL
    sidebarPanel("Insira seus dados Pessoais",
                 textInput(inputId="nome",label="Nome:",value="João"),
                 sliderInput(inputId="peso",label="Peso:", min = 40, max = 130,
                             value = 85, step = 1),
                 sliderInput(inputId="altura",label="Altura:", min = 0.90, max = 1.95,
                             value = 1.70, step = 0.01)
    ),
    
    #PAINEL PRINCIPAL
    mainPanel("RESULTADO:",
              br(),
              textOutput("onome"),
              textOutput("opeso"),
              textOutput("aaltura"),
              h4(textOutput("imc"))
              )
    
  )
  
  
)


#DEFININDO FUNÇÕES DO R
server<-function(input,output){
  output$onome<-renderText(paste("O nome é:",input$nome))
  output$opeso<-renderText(paste("O peso é:",input$peso, "quilos."))
  output$aaltura<-renderText(paste("A altura é:",input$altura, "metros."))
  output$imc<-renderText({
    aux=round(as.numeric(input$peso)/(as.numeric(input$altura)^2),2)
    paste("O IMC é", aux, ".")
    })
}

shinyApp(ui,server)