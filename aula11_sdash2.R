#INSTALANDO PACOTES
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")

#CHAMANDO PACOTES
require(shiny)
require(shinydashboard)
require(ggplot2)



logotipo <- tags$a(href='https://www.est.ufba.br',
                   img(src='https://upload.wikimedia.org/wikipedia/commons/c/c1/Rlogo.png',heigth='40',width='40'))

#DEFININDO INTERFACE DE USUÁRIO

ui <- dashboardPage(
#PAINEL DE CABEÇALHO DA PÁGINA
dashboardHeader(title = logotipo),

#PAINEL LATERAL
dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Informações", tabName = "info", icon = icon("address-book"))
  )
),
#PAINEL PRINCIPAL
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",width='15',
            fluidRow(
              box(plotOutput("graf1", height = 250)),
              
              box(title = "Escolha:",
                  sliderInput("slider", "Número de Observações:", 1, 500, 100)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "info",
            h2("Minhas informações", align = "center"),
            br(),
            h4("Prof. Dr. Anderson Ara",align = "center"),
            HTML('<center> <a href="http://bit.ly/andersonara"> bit.ly/andersonara </a> </center>'),
            
    )
  ),
  tags$head(tags$style(HTML('
      /* Área Logotipo */
      .skin-blue .main-header .logo {
                            background-color: #ffb300;
                            }

      /* Cursos sobre o Logo*/
      .skin-blue .main-header .logo:hover {
                            background-color: #ffb300;
                            }

      /* Cabeçalho */
      .skin-blue .main-header .navbar {
                            background-color: #ffb300;
                            }        

      /* menu background*/
      .skin-blue .main-sidebar {
                            background-color: #ffb300;
                            }

      /* menuItem Ativo */
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #8d6e63;
                            }

      /*  itens do Sidebarmenu */
      .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #ffb300;
                            color: #FFFFFF;
                            }

      /* Cursor sobre itens do Sidebarmenu */
       .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #5d4037;
                            }
                            
      /* Cursor sobre o botão do menu de cabeçalho */                    
       .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #8d6e63;
                            } 
      /*Slider*/                      
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                            background: #8d6e63;
                            }
                            ')
                       )
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
    geom_histogram(color="#8d6e63", fill="gold")+
    xlab("Dados")+
    xlim(-4, 4)+
    ylab("Freq.")+
    theme_bw()
  
  graf1
})
}

shinyApp(ui,server)
