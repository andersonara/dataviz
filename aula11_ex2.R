library(shiny)
library(ggplot2)
dados=data.frame(diamonds)

var.names=c("Peso"=1,"Corte"=2,"Cor"=3,"Claridade"=4,"Profundidade"=5,
            "Topo"=6,"Preço"=7,"Comprimento"=8,"Largura"=9,"Espessura"=10)

ui<-fluidPage(
  titlePanel("GRÁFICO DE BARRAS"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel","Selecione a Variável",choices=var.names)
    ),
    mainPanel(
      textOutput("variavele"),
      plotOutput("grafico")
    )
    
  )
  
)

server<-function(input,output){
  output$variavele=renderText(names(var.names)[as.numeric(input$variavel)])
  
  output$grafico=renderPlot({
    var.e=as.numeric(input$variavel)
    if (is.numeric(dados[,var.e])) {
    ggplot(dados)+
      aes(x=dados[,var.e])+
      geom_histogram(fill="orange")+
      labs(x=names(var.names)[var.e],y="Frequência")
    }
    else{
      ggplot(dados)+
        aes(x=dados[,var.e])+
        geom_bar(fill="blue")+
        labs(x=names(var.names)[var.e],y="Frequência")
    }
  })
  
}

shinyApp(ui,server)