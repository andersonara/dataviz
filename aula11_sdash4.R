#INSTALANDO PACOTES
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("ggplot2")

#CHAMANDO PACOTES
require(shiny)
require(shinydashboard)
require(ggplot2)
require(leaflet)
require(dplyr)
require(sf)


#CONJUNTO DE DADOS
#DEFINDO PASTA DE TRABALHO
setwd("C:\\Users\\Anderson\\Dropbox\\---  2021\\DISCIPLINAS\\MATE56\\AULA10\\dash_final\\")

#CIDADES DA REGIÃO METROPOLITANA
reg.me=c("SALVADOR","CAMACARI","SAO FRANCISCO DO CONDE",
         "LAURO DE FREITAS","SIMOES FILHO","CANDEIAS",
         "DIAS D'AVILA","MATA DE SAO JOAO","POJUCA",
         "SÃO SEBASTIAO DO PASSE","VERA CRUZ",
         "MADRE DE DEUS","ITAPARICA")

#IMPORTANDO ARQUIVO SHAPEFILE
mapa=st_read("dados\\shapes\\ba_municipios\\29MUE250GC_SIR.shp",
             stringsAsFactors = FALSE)

#CONVERTENDO ENCONDING
mapa$NM_MUNICIP=iconv(mapa$NM_MUNICIP, from = "", to = "ASCII//TRANSLIT")
reg.me=iconv(reg.me, from = "", to = "ASCII//TRANSLIT")


#CRIANDO SUBCONJUNTO APENAS COM AS CIDADES DA REGIÃO METROPOLITANA
reg_me= subset(mapa, mapa$NM_MUNICIP %in% reg.me)


dados <- read.csv("dados//METROPOLITANA.csv",
                  sep=";",dec=",", encoding  = "UTF-8",
                  header = T)
names(dados)[1]="NM_MUNICIP"
dados$NM_MUNICIP=iconv(dados$NM_MUNICIP, from = "UTF-8", to = "ASCII//TRANSLIT")



#TRANSFORMANDO PARA UPPERCASE
dados$NM_MUNICIP=toupper(dados$NM_MUNICI)

#JUNTANDO DADOS COM A BASE
mapa_dados <- inner_join(reg_me,dados, by="NM_MUNICIP")




#DEFININDO INTERFACE DE USUÁRIO

ui <- dashboardPage(skin="green",
                    #PAINEL DE CABEÇALHO DA PÁGINA
                    dashboardHeader(title = "RShiny"),
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
                        # Primeira tab
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  infoBox("População", 3899533, icon = icon("users"),
                                           width = 3,color="green"),
                                  infoBox("IDHM", 0.688, icon = icon("globe-americas"),
                                          width = 3,color="red"),
                                  infoBox("RPC", 542.857, icon = icon("money-bill-alt"),
                                          width = 3,color="red"),
                                  infoBox("TX.DES", 20.934, icon = icon("briefcase"),
                                          width = 3,color="red"),
                                  
                                ),
                                fluidRow(
                                  box(title = "Escolha:",
                                      selectInput("variavel", "Selecione a Variávels:", 
                                                  names(dados)[-c(1:3)]),width = 3),
                                      
                                  leafletOutput("graf1", height = 400,width = 600),

                              
                                  ),
                               ),
                        
                        # Segunda tab
                        tabItem(tabName = "info",
                                h2("Informações", align = "center"),
                                br(),
                                h4("Prof. Dr. Anderson Ara",align = "center"),
                                HTML('<center> <a href="http://bit.ly/andersonara"> bit.ly/andersonara </a> </center>'),
                                
                        )
                      )
                    )
)



#DEFININDO FUNÇÕES DO R
server<-function(input,output){
  
  output$graf1 <- renderLeaflet({
    Variavel <- dados[,input$variavel]
    
    #PALETA
    qpaleta<- colorNumeric(
      palette = "YlGnBu",
      domain = Variavel
    )
    
    #Gráfico
     leaflet(reg_me) %>%
      addProviderTiles(providers$OpenStreetMap.DE) %>% 
      addTiles() %>% 
      addPolygons(label=~paste(NM_MUNICIP,":",Variavel),
                  smoothFactor = 0.5,
                  fillOpacity = 0.8,stroke = T,
                  fillColor = qpaleta(Variavel),
                  color = "", group = "Variavel") %>% 
      addLegend(pal = qpaleta, values = Variavel, 
                opacity = 0.9, title = NULL,
                position = "bottomright") %>% 
       addLayersControl(
         overlayGroups = c("Variavel"),
         options = layersControlOptions(collapsed = FALSE)
       )
  })
}

shinyApp(ui,server)
