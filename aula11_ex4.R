require(ggplot2)

#CONJUNTO DE DADOS
dados=data.frame(diamonds)
var.names=c("Peso"=1,"Corte"=2,"Cor"=3,"Claridade"=4,"Profundidade"=5,
            "Topo"=6,"Preço"=7,"Comprimento"=8,"Largura"=9,"Espessura"=10)

#USER INTERFACE
ui<-fluidPage(
  titlePanel("Análise Bivariada"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "var1",
                  label = "Selecione a Variável 1",
                  choices=var.names, selected = ""),
      
      selectInput(inputId = "var2",
                  label = "Selecione a Variável 2",
                  choices=var.names, selected = "")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Resumo",verbatimTextOutput("sum")),
        tabPanel("Gráfico",plotOutput("grafico"),
                 downloadButton(outputId="down",label="Download")),
        tabPanel("Dados",tableOutput("tab"),
                 downloadButton(outputId="down2",label="Download"))
      )
      
    )
    
  )
  
)


#SERVER
server<-function(output,input){
  var1=reactive(as.numeric(input$var1))
  var2=reactive(as.numeric(input$var2))
  output$grafico=renderPlot({
    plot(dados[,var1()],dados[,var2()])
  })
  output$tab=renderTable({
    head(dados[,c(var1(),var2())],n=30)
  })
  output$sum=renderPrint({
    summary(dados[,c(var1(),var2())])
  })
  
  output$down=downloadHandler(
    filename = "grafico.png",
    content=function(file) {
      png(file)
      plot(dados[,var1()],dados[,var2()])
      dev.off()
      }
    )
  output$down2=downloadHandler(
    filename = "dados.csv",
    content = function(file) {
      write.csv(dados, file)
    }
  )
}


#CONSTRUINDO SHINYAPP
shinyApp(ui, server)
