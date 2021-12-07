
############################################################

# Carregando o pacote para dados geoespaciais
library(rgdal)
# Nome do arquivo shapefile
arquivo="29MUE250GC_SIR.shp"
# Examinando a fonte dos dados
ogrListLayers(arquivo)

# Lendo arquivo geoespacial
mapa_bahia=readOGR(arquivo)

# Mapa da Bahia R-base
plot(mapa_bahia)

############################################################

#CHAMANDO PACOTES
require(ggplot2)
require(dplyr)
require(sf)

#DEFINDO PASTA DE TRABALHO
setwd("....")

#CIDADES DA REGIÃO METROPOLITANA
reg.me=c("SALVADOR","CAMAÇARI","SÃO FRANCISCO DO CONDE",
         "LAURO DE FREITAS","SIMÕES FILHO","CANDEIAS",
         "DIAS D'ÁVILA","MATA DE SÃO JOÃO","POJUCA",
         "SÃO SEBASTIÃO DO PASSÉ","VERA CRUZ",
         "MADRE DE DEUS","ITAPARICA")

#IMPORTANDO ARQUIVO SHAPEFILE
mapa=st_read("shapes\\ba_municipios\\29MUE250GC_SIR.shp",
             stringsAsFactors = FALSE)

#CONVERTENDO ENCONDING
mapa$NM_MUNICIP=iconv(mapa$NM_MUNICIP, from = "", to = "UTF-8")

############################################################


# Criando base com apenas as cidades da região metropolittana
reg_me <- subset(mapa, mapa$NM_MUNICIP %in% reg.me)
# Carregando base com infos dos municípios da região metropolitana
dados <- read.csv("METROPOLITANA3.csv",
                  sep=";",dec=",", encoding = "UTF-8",
                  header = T)
# Renomeando coluna
names(dados)[1] = "NM_MUNICIP"
# Convertendo os nomes dos municípios para leitra maiúscula (caixa alta)
dados$NM_MUNICIP = toupper(dados$NM_MUNICI)
##Etapa chave
# Juntando as bases: dados geoespaciais + dados info
mapa_dados <- inner_join(reg_me, dados, by="NM_MUNICIP")

############################################################

# Plotando mapa em ggplot2
options(scipen = 999)
m1=ggplot(mapa_dados)+geom_sf(aes(fill=idh))

m1

############################################################

# Gráfico dinâmico
require(plotly)
ggplotly(m1)


############################################################

#install.packages("tmap")
require(tmap)
# Base de dados com info dos países
data("World")
# Mapa do mundo
tm_shape(World) + tm_polygons("HPI")

############################################################

# interatividade
tmap_mode("view"); tm_shape(World) + tm_polygons("HPI")

############################################################

tm_shape(mapa_dados)+ tm_polygons("idh",id="NM_MUNICIP",palette="Purples")

############################################################

tmap_mode("view"); mapas<-tmap_last()
# Salvando o mapa gerado
tmap_save(mapas,"mapa.html"); mapas

############################################################
require(cartography)
mapa <- st_read("29MUE250GC_SIR.shp",
                stringsAsFactors = FALSE)
mapa$NM_MUNICIP=iconv(mapa$NM_MUNICIP, from = "", to = "UTF-8")

reg.me=c("SALVADOR","CAMAÇARI","SÃO FRANCISCO DO CONDE",
         "LAURO DE FREITAS","SIMÕES FILHO","CANDEIAS",
         "DIAS D'ÁVILA","MATA DE SÃO JOÃO","POJUCA",
         "SÃO SEBASTIÃO DO PASSÉ","VERA CRUZ",
         "MADRE DE DEUS","ITAPARICA")
reg_me= subset(mapa, mapa$NM_MUNICIP %in% reg.me)

reg_me= subset(mapa, mapa$NM_MUNICIP %in% reg.me)
dados <- read.csv("METROPOLITANA3.csv",
                  sep=";",dec=",", encoding  = "UTF-8",
                  header = T)
names(dados)[1]="NM_MUNICIP"

############################################################

# Mapa
plot(st_geometry(mapa_dados),main="População")

# Círculos de acordo com dado
propSymbolsLayer(x=mapa_dados,var = "pop",
                 inches=0.4,
                 legend.pos="n")

# Legenda dos círculos
legendCirclesSymbols(var=c(min(mapa_dados$pop),
                           max(mapa_dados$pop)),
                     inches=0.15,
                     pos="bottomright",
                     title.txt="")
############################################################


choroLayer(spdf = mapa_dados, var = "pop", method = "quantile", border="white", nclass=4, col=carto.pal(pal1 = "wine.pal", n1 = 4))
labelLayer(x=mapa_dados,txt="NM_MUNICIP")

############################################################

# Mapa coroplético com hexágono
mapa_hex=getGridLayer(x=mapa_dados,cellsize = 0.01, type="hexagonal",var="pop")
choroLayer(x=mapa_hex, var="pop",method = "quantile", border="white", nclass=4, col=carto.pal(pal1 = "wine.pal", n1 = 4))

############################################################

#CRIANDO MAPA COM TILES
# Fundo dark
tiles=getTiles(x=mapa_dados,type="cartodark", zoom = 10); tilesLayer(tiles)
# Limites
plot(st_geometry(mapa_dados),add=T,border="white")
# Título
mtext(text = "Região Metropolitana de Salvador",
      side = 1, adj = 0.5, cex = 1.3, font = 3)

############################################################

#IMPORTANDO PACOTE
library(googleVis)

#CRIANDO GRÁFICO
Graf <- gvisGeoChart(Exports, locationvar='Country', colorvar='Profit')

#PLOTANDO GRÁFICO
plot(Graf) 


############################################################

#IMPORTANDO PACOTES
require(googleVis)
require(dplyr)

#IMPORTANDO DADOS
dados.voto=read.csv("votacao_candidato_2018.csv",sep=";")

#RETIRANDO ACENTUAÇÃO
dados.voto$UF=iconv(dados.voto$UF, 
                    to = "ASCII//TRANSLIT")

#MANIPULANDO DADOS
votos.estado <- 
  dados.voto %>%
  group_by(UF) %>% 
  summarise(VOTOS=sum(QT_VOTOS_NOMINAIS))

votos.estado.cands <- 
  dados.voto %>%
  group_by(UF, NR_CANDIDATO) %>% 
  summarise(VOTOS=sum(QT_VOTOS_NOMINAIS))

#VOTOS 13
votos13 <- votos.estado.cands %>% filter(NR_CANDIDATO==13)
votos13$VOTOS <- votos13$VOTOS/votos.estado$VOTOS 

############################################################

require(leaflet)
# Para iniciar o mapa
leaflet()

############################################################

map <- leaflet() %>% addTiles(); map

############################################################
require(leaflet)
# Definindo qual área do mapa exibir
map <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = -39, lat = -13, zoom = 7 ); map

############################################################

#Modificando tiles
map <- leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner); map

############################################################

# Outro tiles
map <- leaflet() %>% addTiles() %>% 
  setView(lng = -38.5, lat = -13, zoom = 8 ) %>% 
  addProviderTiles("Esri.WorldTopoMap"); map

############################################################

# Add marcador
map <- leaflet() %>% addTiles() %>%  
  addMarkers(lat=-13.000694,lng=-38.5078128, popup="IME - UFBA")

map

############################################################

# Add marcadoreS
marcs=data.frame(lat=runif(10,min=-13.1,max=-12.9),
                 lng=runif(10,min=-38.6,max=-38.5))
marcs %>% leaflet() %>% addTiles() %>% addMarkers()

############################################################

# Add circles
marcs %>% leaflet() %>% addTiles() %>% 
  addCircleMarkers(weight = 2,color="green")


############################################################

marcs %>% leaflet() %>% 
  addTiles() %>% 
  addRectangles(lat1=min(marcs$lat),lat2=max(marcs$lat),lng1=min(marcs$lng),lng2=max(marcs$lng),color = "green")


############################################################

# Add marcadores
Ricone=makeIcon(iconUrl ="https://www.rstudio.com/wp-content/uploads/2014/06/RStudio-Ball.png", iconWidth = 25, iconHeight = 25)
Sites=rep("<a href='http://www.led.ufba.br/ebest/'> EBEST </a>")
marcs %>% leaflet() %>% addTiles() %>% 
  addMarkers(icon=Ricone, popup = Sites)

############################################################

# Agrupando marcadores
marcs %>% leaflet() %>% addTiles() %>% 
  addMarkers(icon=Ricone, popup = Sites, clusterOptions = markerClusterOptions())

############################################################

#dados
dados=read.csv("METROPOLITANA3.csv",
               sep=";",dec=",", encoding  = "UTF-8", header = T)
#CRIANDO MAPA
dados %>% leaflet() %>% addTiles() %>% 
  addMarkers(label = ~X.U.FEFF.cidade, icon = Ricone) %>%
  addCircles(weight = 1,radius = sqrt(dados$pib)*3,
             color = "orange")

############################################################


#CRIANDO VARIÁVEL PIB_C (COR)
dados <- dados %>% 
  mutate(pib_c=ifelse(pib>mean(pib),"blue","red"))

############################################################

#CRIANDO VARIÁVEL PIB_C (COR)
dados <- dados %>% 
  mutate(pib_c=ifelse(pib>mean(pib),"blue","red"))
#CRIANDO MAPA
dados %>% leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(col=dados$pib_c) %>% 
  addLegend(label=c("ALTO","BAIXO"),col=c("blue","red"))

############################################################

#rótulos
labs <- paste("<p> Cidade:", dados$X.U.FEFF.cidade,"</p>",
              "<p> População:", dados$pop,"</p>",
              "<p> IDH:", dados$idh,"</p>",
              "<p> PIB:", dados$pib,"</p>")
#ícone
icons <- awesomeIcons(
  icon = 'star',
  iconColor = 'black',
  library = 'fa',
  markerColor="red")

############################################################


#plot
dados %>% leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>% 
  addAwesomeMarkers(icon=icons,popup = labs)


############################################################

#CRIANDO GRÁFICOS
require(rgdal) 
require(leaflet)
require(dplyr)

#CIDADES DA REGIÃO METROPOLITANA
reg.me=c("SALVADOR","CAMAÇARI","SÃO FRANCISCO DO CONDE",
         "LAURO DE FREITAS","SIMÕES FILHO","CANDEIAS",
         "DIAS D'ÁVILA","MATA DE SÃO JOÃO","POJUCA",
         "SÃO SEBASTIÃO DO PASSÉ","VERA CRUZ",
         "MADRE DE DEUS","ITAPARICA")

#EXBINDO SHAPEFILE
mapa_dis <- readOGR("29MUE250GC_SIR.shp")
reg_me= subset(mapa_dis, mapa_dis$NM_MUNICIP %in% reg.me)

############################################################

#ENTRANDO COM OS DADOS
dados <- read.csv("METROPOLITANA3.csv",
                  sep=";",dec=",", encoding  = "UTF-8",
                  header = T)

#ORGANIZANDO POR ORDEM ALFABÉTICA
dados <- dados %>% arrange(X.U.FEFF.cidade)

#PALETA DE CORES POR IDH
qpaleta<- colorQuantile("Greens", dados$idh, n = 5)


############################################################


leaflet(reg_me) %>% 
  addTiles() %>% 
  addPolygons(label=~paste(NM_MUNICIP,":",dados$idh),
              smoothFactor = 0.1,
              fillOpacity = 0.8,stroke = FALSE,
              color = qpaleta(dados$idh))

############################################################

require(rgdal) 
require(leaflet)
require(dplyr)

mapa_dis <- readOGR("29MUE250GC_SIR.shp")
mun=mapa_dis@data$NM_MUNICIP
votacao=read.csv("votacao_candidato_2018.csv",sep=";")
votacao=subset(votacao,votacao$SG_UF=="BA")
vssa_dis <- subset(votacao, votacao$NM_MUNICIPIO %in% mun)

v=mun %in% unique(votacao$NM_MUNICIPIO)
mun[v==F]

############################################################

votacao13=vssa_dis %>% 
  group_by(NM_MUNICIPIO, NR_CANDIDATO) %>% 
  summarise(votos = sum(QT_VOTOS_NOMINAIS)) %>% 
  filter(NR_CANDIDATO==13)

votacao17= vssa_dis %>% 
  group_by(NM_MUNICIPIO, NR_CANDIDATO) %>% 
  summarise(votos = sum(QT_VOTOS_NOMINAIS)) %>% 
  filter(NR_CANDIDATO==17)

mapa_dis$votos=votacao17$votos>votacao13$votos

############################################################

cores=rep("red",length(mapa_dis))
cores[mapa_dis$votos]="blue"

leaflet(mapa_dis) %>% 
  addTiles() %>% 
  setView(lat=-13,lng=-41.4,zoom=6) %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>% 
  addPolygons(color =cores, fillOpacity  = 0.6,
              label=~paste(NM_MUNICIP),
              weight = 2,
              highlight=highlightOptions(weight=3,
                                         color = "black")) 

############################################################

#exemplo
require(mapview)
mapview(breweries)

############################################################

library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
mapview(mapa_dados, zcol = "idh", map.types = "CartoDB.DarkMatter",
        col.regions = pal)