library(shiny)
library(ggplot2)
dados=data.frame(diamonds)
str(dados)
var.names=c("Peso"=1,"Corte"=2,"Cor"=3,"Claridade"=4,"Profundidade"=5,
            "Topo"=6,"Preço"=7,"Comprimento"=8,"Largura"=9,"Espessura"=10)

ui<-fluidPage(
  h1("RESUMO DA VARIÁVEL:", textOutput("variavele")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel","Selecione a Variável",choices=var.names,selected ="")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumo",verbatimTextOutput("summ")),
        tabPanel("Gráfico",plotOutput("grafico")),
        tabPanel("Dados",tableOutput("tdados"))
      )
      
      
    )
    
  )
  
)

server<-function(input,output){
  output$variavele=renderText(names(var.names)[as.numeric(input$variavel)])
  output$tdados=renderTable({
    var.e=as.numeric(input$variavel)
    head(dados[var.e],n=30)
    })
  output$summ=renderPrint({
    var.e=as.numeric(input$variavel)
    summary(dados[,var.e])
  })
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