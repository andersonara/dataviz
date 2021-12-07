############################################################

#importar dados3 via dados3.csv em leg.ufpr.br/~ara
#PRIMEIROS PASSOS
require(ggplot2)

head(dados3)

ggplot(data = dados3, aes(y = ESPVIDA, x = 1:nrow(dados3))) + geom_line()


############################################################

g1 <-ggplot(data = dados3, aes(y = ESPVIDA, x = 1:nrow(dados3))) + geom_line()
g1 <- g1 + geom_line(size=1.1,color="orange")
g1

###########################################################

g2 <-ggplot(data = dados3, 
            aes(y = ESPVIDA, x = 1:nrow(dados3),
                colour = factor(ANO))) 
g2<-g2 + geom_line(size=1.5)
g2<-g2 + scale_x_continuous(name = "ORDEM")
g2<-g2 + scale_y_continuous(name = "EXPECT. DE VIDA")
g2<-g2 + scale_colour_discrete(name="ANO")
g2

############################################################

cores <- c("#5F9EA0", "#E1B378")
g3 <-ggplot(data = dados3, 
            aes(y = ESPVIDA, x = 1:nrow(dados3),
                colour = factor(ANO))) 
g3 <-g3 + geom_line(size=1.5)
g3 <-g3 + scale_x_continuous(name = "ORDEM")
g3 <-g3 + scale_y_continuous(name = "EXPECT. DE VIDA")
g3 <-g3 +theme(legend.position="bottom", 
               legend.direction="horizontal", 
               legend.title = element_blank())
g3 <- g3 + scale_colour_manual(values=cores)
g3


############################################################

#SCATTERPLOT
g4 <- ggplot(data = dados3, aes(y = IDH, x = MORT1))
g4 + geom_point(size = 5)
g4 + geom_point(aes(color = factor(ANO)), size = 5)
g4 + geom_point(aes(color = factor(ANO), size =HOMEMTOT)) 


############################################################

##DENSIDADE
cor.preen <- "gold1"
cor.linha <- "goldenrod2"

g2<- ggplot(dados3,aes(x = IDH)) + geom_density()
g2<-g2 + scale_x_continuous(name = "IDH", limits=c(0,1))
g2<-g2 + scale_y_continuous(name = "")
g2<-g2 + ggtitle("GRÁFICO DE DENSIDADE")
g2<-g2 + geom_density(fill = cor.preen, colour = cor.linha,alpha = 0.8,size=1.2)
g2

############################################################


#TEMA BÁSICO

g2 <- g2+ theme_bw()
g2


############################################################

##BOXPLOT
gg<-ggplot(dados3,aes(x=factor(ANO),y=IDH,fill=factor(ANO)))+geom_boxplot()
gg<- gg + scale_x_discrete(name = "")
gg<- gg + theme(axis.text.x = element_blank())
gg<- gg + scale_fill_brewer(palette="Greens")
gg<- gg + theme(legend.title = element_blank())
gg


############################################################

#PREPARANDO OS DADOS

require(ggplot2)
require(HSAUR)

data(Forbes2000)

sel=Forbes2000[,"country"]=="Germany" |
  Forbes2000[,"country"]=="United Kingdom" |
  Forbes2000[,"country"]=="United States" |
  Forbes2000[,"country"]=="Japan" 

pontos=c(min(Forbes2000$marketvalue)-0.01,median(Forbes2000$marketvalue),max(Forbes2000$marketvalue))
Forbes2000$mk=cut(Forbes2000$marketvalue,pontos)


dados=Forbes2000[sel,]


############################################################

#GRÁFICO DE BARRAS
ggplot(dados)+
  aes(x=country)+
  geom_bar()


############################################################

#GRÁFICO FINAL
ggplot(dados)+
  aes(x=country,y=(..count..)/sum(..count..))+
  geom_bar(fill="darkorange")+
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..)), stat= "count", vjust = -.5) +
  scale_y_continuous(labels =  scales::percent)+
  xlab(" ") +
  ylab("Prop. de Empresas")+
  scale_x_discrete(labels=c("Alemanha","Japão","Reino Unido","EUA"))


############################################################

#GRÁFICO DE BARRAS CONJUNTAS
ggplot(dados)+
  aes(x=country,fill=mk) +
  geom_bar(position="dodge")


############################################################

#GRÁFICO EDITADO
cores <- c("#6000C5", "#C5C500")
ggplot(dados)+
  aes(x=country,fill=mk) +
  geom_bar(position="dodge") +
  xlab(" ") +
  ylab("Freq. de Empresas")+
  scale_x_discrete(labels=c("Alemanha","Japão","Reino Unido","EUA")) +
  scale_fill_manual("Valor de Mercado", values = cores)+
  theme_bw()+
  theme(legend.position="bottom", legend.direction="horizontal")

############################################################

#GRÁFICO DE BARRAS ACUMULADAS
cores <- c("#6000C5", "#C5C500")
ggplot(dados)+
  aes(x=country,fill=mk) +
  geom_bar(position="fill")

############################################################

#GRÁFICO EDITADO
cores <- c("#6000C5", "#C5C500")
ggplot(dados)+
  aes(x=country,fill=mk) +
  geom_bar(position="fill") +
  xlab(" ") +
  ylab("Prop. de Empresas")+
  scale_x_discrete(labels=c("Alemanha","Japão","Reino Unido","EUA")) +
  scale_fill_manual("Valor de Mercado", values = cores)+
  theme_bw()+
  theme(legend.position="bottom", legend.direction="horizontal")


############################################################

#PREPARANDO O CONJUNTO DE DADOS
dados.porc=data.frame(table(dados$country),
                      as.numeric(table(dados$country)/sum(table(dados$country))))
names(dados.porc)=c("pais","freq","porc")

head(dados.porc)

#GRÁFICO BÁSICO
ggplot(dados.porc[dados.porc$freq>0,]) +
  aes(x="", y = freq,fill=pais)+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y") 


############################################################

#GRÁFICO EDITADO
ggplot(dados.porc[dados.porc$freq>0,]) +
  aes(x="", y = freq,fill=pais)+
  geom_bar(stat="identity", width=1)+
  coord_polar(theta="y")+ 
  geom_text(aes(label = paste0(round(porc*100,1), "%")), 
            position = position_stack(vjust = 0.5),size=5)+
  scale_fill_brewer(palette="Purples")+
  labs(x = NULL, y = NULL, fill = NULL)+
  theme_classic()+
  theme(axis.text = element_blank(),axis.line = element_blank())


############################################################

#SCATTERPLOT SIMPLES
ggplot(data = dados)+
  aes(x = sales, y = profits,colour=mk) +
  xlab("Vendas")+
  ylab("Ativos")+
  geom_point(size = 5,alpha=0.5)+
  theme_bw()+
  theme(legend.title = element_blank())

############################################################

#TRELIÇA SIMPLES PARA SCATTERPLOT
ggplot(data = dados)+
  aes(x = sales, y = profits,colour=mk) +
  xlab("Vendas")+
  ylab("Ativos")+
  geom_point(size = 5,alpha=0.5)+
  facet_grid(facets=. ~ country)+
  theme_bw()+
  theme(legend.title = element_blank())

############################################################

#TRELIÇA DUPLA PARA SCATTERPLOT
ggplot(data = dados)+
  aes(x = sales, y = profits) +
  xlab("Vendas")+
  ylab("Ativos")+
  geom_point(size = 5,alpha=0.5)+
  facet_grid(facets=mk~ country)+
  theme_bw() 


############################################################

#TRELIÇA DUPLA PARA DENSIDADE
ggplot(data = dados)+
  aes(x = sales) +
  geom_density(fill="orange")+
  xlab("log10(Vendas)")+
  ylab("Densidade")+
  scale_x_log10()+
  facet_grid(facets=mk~ country)+
  theme_bw()



############################################################

#IMPORTANDO A IMAGEM
library(jpeg)
imagem=readJPEG("http://leg.ufpr.br/~ara/dados/foto-casarao.jpg")
par(bg="seashell")
x=2012:2016 
y=c(2278,2233,2275,2233,2416)

#PLOT
plot(x,y,type='n', main="", xlab="ANO", ylab="N. PROFESSORES")

lim <- par()
rasterImage(imagem, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid(lwd=2)

lines(x,y, type="b", lwd=6, col="brown")


############################################################

ggplot(dados2)+
  aes(x=x,y=y)+
  xlab("ANO")+
  ylab("N. PROFESSOR")+
  annotation_custom(rasterGrob(imagem, width=unit(1,"npc"), 
                               height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  geom_line(size=3)+
  theme(panel.ontop=TRUE,
        panel.background = element_rect(colour = NA,fill="transparent"))

############################################################

require(cowplot)


g1<- ggplot(dados2)+
  aes(x = y) + 
  xlab("N. PROFESSORES")+
  ylab("DENSIDADE")+
  geom_density(colour = NA, fill = "darkolivegreen",size=1)+
  theme(plot.background=element_rect(fill="seashell"),
        panel.background = element_rect(colour = NA,fill="transparent"))

g2<-ggplot(dados2)+
  aes(x=x,y=y)+
  xlab("ANO")+
  ylab("N. PROFESSORES")+
  annotation_custom(rasterGrob(imagem, width=unit(1,"npc"), 
                               height=unit(1,"npc")))+
  geom_line(size=3,color="brown")+
  theme(plot.background=element_rect(fill="seashell"),
        panel.ontop=TRUE,
        panel.background = element_rect(colour = NA,fill="transparent"),
        panel.grid.major = element_line(linetype = 'dashed',colour = "white"),
        panel.grid.minor = element_line(linetype = 'dashed',colour = "white"))


plot_grid(g1, g2, labels = c('A', 'B'))


############################################################

plot_grid(g2, g1, g1, g2,rel_widths=c(2,1),
          rel_heights = c(2,1))

############################################################
require(extrafont)

##DOWNLOAD DA FONTE
download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")

system("cmd.exe", input = "mkdir C:\\fonts")
system("cmd.exe", input = "copy xkcd.ttf  C:\\fonts")

font_import(paths = "C:\\fonts", pattern="[X/x]kcd")
fonts()
loadfonts()

############################################################

##CRIANDO NOVO GRÁFICO COM A FONTE
gg2 <- ggplot(dados3, aes(x = ESPVIDA)) +
  geom_density(colour = "black", fill = "lightblue",size=1) +
  scale_x_continuous(name = "EXPECTATIVA DE VIDA",
                     limits=c(50, 90)) +
  scale_y_continuous(name = "Densidade") +
  ggtitle("GRAFICO") +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title=element_text(size = 20, family="xkcd", hjust = 0.5),
        text=element_text(size = 16, family="xkcd"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))
gg2

############################################################

require(datasauRus)
require(ggplot2)
require(dplyr)
require(gganimate)

datasaurus_dozen %>%  
  ggplot(aes(x=x, y=y))+
  geom_point(size=3,col="tomato")+
  theme_bw()+
  theme(legend.position = "none")+
  transition_states(dataset)+
  labs(title = "Conjunto: {closest_state}")
