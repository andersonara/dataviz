
############################################################


#CONJUNTO DE DADOS

v2016=c(900,973,1100,1205,830,950)
v2017=c(940,960,990,1300,870,1100)
m <- c('Bi1','Bi2','Bi3','Bi4','Bi5','Bi6')
dados=data.frame(m,v2016,v2017)
dados$cores <- ifelse((dados$v2017 - dados$v2016) < 0, "red", "green")

theme_set(theme_classic())

#GRÁFICO INICIAL
p<-ggplot(dados) + 
  geom_segment(aes(x=1, xend=2, y=dados$v2016, yend=v2017, col=cores), size=2, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=0.5) + 
  geom_vline(xintercept=2, linetype="dashed", size=0.5) +
  scale_color_manual(values = c("#00ba38", "#f8766d"))           


p

############################################################

# Adicionando Texto
texto_2016 <- paste(dados$m, round(dados$v2016,2),sep=", ")
texto_2017 <- paste(dados$m, round(dados$v2017,2),sep=", ")

p <- p + geom_text(label=texto_2016, y=dados$v2016, 
                   x=rep(1, nrow(dados)), hjust=1.1, size=4)+
  geom_text(label=texto_2017, y=dados$v2017, 
            x=rep(2, nrow(dados)), hjust=-0.1, size=4)+
  scale_x_continuous(limits=c(0,3),breaks = seq(0, 3, 1),
                     labels = c("",2016,2017,""))+
  ylab("VENDAS") +
  xlab("") +
  theme(axis.text.x=element_text(size=12))

p

############################################################


data(Forbes2000)

#PREPARANDO OS DADOS
valor.medio=aggregate(Forbes2000$marketvalue, by=list(Forbes2000$country), FUN=mean) 
colnames(valor.medio)=c("pais", "valor")
valor.medio=valor.medio[order(valor.medio$valor), ]
valor.medio$pais=factor(valor.medio$pais, levels = valor.medio$pais)

#GRÁFICO
ggplot(valor.medio, aes(x=pais, y=valor)) + 
  geom_point(col="tomato2", size=3) +
  geom_segment(aes(x=pais, xend=pais, y=min(valor),
                   yend=max(valor)),linetype="dashed", size=0.1) +
  scale_y_log10() +
  labs(title="Dot Plot", 
       subtitle="País Vs Valor de Mercado Médio", 
       caption="source: UFBA") +  
  coord_flip()

############################################################

theme_set(theme_bw()) 

#PACOTE ggExtra
library(ggExtra)

#GRÁFICO
g<-ggplot(Forbes2000, aes(sales,profits)) + 
  geom_count() + 
  scale_x_log10()+
  scale_y_log10()+
  geom_smooth(method="lm", se=F,col="orange")

#PLOT DAS MARGINAIS
ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")



############################################################

theme_set(theme_bw())  

#PREPARAÇÃO DOS DADOS
data(Forbes2000)
valor.medio=aggregate(Forbes2000$marketvalue, 
                      by=list(Forbes2000$country), FUN=mean) 
colnames(valor.medio)=c("pais", "valor")
valor.medio$valor_z=(valor.medio$valor-mean(valor.medio$valor))/sd(valor.medio$valor)
valor.medio$tipo=ifelse(valor.medio$valor_z < 0, "abaixo", "acima")
valor.medio=valor.medio[order(valor.medio$valor_z), ]
valor.medio$pais=factor(valor.medio$pais, levels = valor.medio$pais)

#GRÁFICO
ggplot(valor.medio) + 
  aes(x=pais, y=valor_z, label=valor_z,fill=tipo) +
  geom_bar(stat='identity', width=.5)+
  scale_fill_manual(name="Valor Médio", 
                    labels = c("Acima da média", "Abaixo da Média"), 
                    values = c("#32CD32", "#B22222"))+
  coord_flip()


############################################################


#PREPARANDO OS DADOS
data(Forbes2000)

sel=Forbes2000[,"country"]=="Germany" |
  Forbes2000[,"country"]=="United Kingdom" |
  Forbes2000[,"country"]=="United States" |
  Forbes2000[,"country"]=="Japan" 

dados=Forbes2000[sel,]

theme_set(theme_bw())

#GRÁFICO
ggplot(dados, aes(x=country,y=marketvalue)) + 
  geom_violin(fill="darkorange2") + 
  scale_y_log10() +
  labs(title="Gráfico Violino", 
       subtitle="Valor de Mercado x País",
       caption="Source: UFBA",
       x="Valor de Mercado",
       y="País")


############################################################


#PREPARANDO OS DADOS
data(Forbes2000)

sel=Forbes2000[,"country"]=="Germany" |
  Forbes2000[,"country"]=="United Kingdom" |
  Forbes2000[,"country"]=="United States" |
  Forbes2000[,"country"]=="Japan" 

sel2=Forbes2000[,"category"]=="Utilities" |
  Forbes2000[,"category"]=="Diversified financials" |
  Forbes2000[,"category"]=="Insurance" |
  Forbes2000[,"category"]=="Banking" 

dados=Forbes2000[sel & sel2,]
dados$log.sales=log(dados$sales)

#GRÁFICO
ggplot(dados) +
  aes(x=country, y=category) +
  geom_tile(aes(fill = log.sales), colour = "white")+
  scale_fill_gradient(low = "gray", high = "darkgreen")


############################################################

ggplot(data = dat)+
  aes(x = y1) +
  geom_density(fill=cor.3)+
  xlab("EIXO X")+
  ylab("EIXO Y")+
  ggtitle("GRAFICO 1")+
  facet_grid(facets=.~ grp)+
  coord_flip()

############################################################


#LISTAGEM DE FONTES
library(extrafont)
font_import()
fonts()

cor.1=NA
cor.2="#1a237e"
cor.3="tomato"
cor.4="#29b6f6"


############################################################

tema <- function() {
  theme(
    plot.background = element_rect(fill = cor.1, colour = cor.1),
    panel.background = element_rect(fill = cor.1),
    axis.text = element_text(colour = cor.2, family = "xkcd",size=15),
    plot.title = element_text(colour = cor.2, face = "bold", size = 30, 
                              vjust = 0.5, family = "xkcd"),
    axis.title = element_text(colour = cor.2, 
                              face = "bold", size = 20, family = "xkcd"),
    panel.grid.major.x = element_line(colour = cor.2),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "xkcd", colour = "white"),
    strip.background = element_rect(fill = cor.2),
    axis.ticks = element_line(colour = cor.2),
    legend.title = element_text(family =  "xkcd", colour = cor.2, 
                                size = 20),
    legend.background = element_rect(fill = cor.1),
    legend.key = element_rect(fill = cor.1, colour = cor.1),
    legend.text = element_text(family = "xkcd", colour = cor.2, size = 20))
}


############################################################

g4 <- ggplot(data = dat, aes(x = x, y = y2, group = factor(grp))) +
  geom_line(aes(linetype = factor(grp)), 
            size = 3, 
            colour = cor.3) +
  ylab("EIXO X") + xlab("EIXO Y") + ggtitle("GRAFICO 4")+
  theme(legend.position="bottom", legend.direction="horizontal")+
  tema()+ scale_linetype_discrete("GRUPO")

g4


############################################################


#ENTRADA DE DADOS
y1 <- round(rnorm(n = 36, mean = 7, sd = 2)) 
y2 <- round(rnorm(n = 36, mean = 21, sd = 6))
y3 <- round(rnorm(n = 36, mean = 50, sd = 8))
x <- sample(c(LETTERS[1:12],LETTERS[1:3]), 36,rep=T)
grp <- rep(c("G1", "G2", "G3"), each = 12)
dat <- data.frame(grp, x, y1, y2, y3)
tab <- data.frame(Freq=sort(table(x)))

#INICIANDO DIRETÓRIO DE TRABALHO
setwd("...")

#importar as imagens bk.png e RStudioLogo.png
#em leg.ufpr.br/~ara/dados/

#IMPORTANDO IMAGENS
require(png)
require(jpeg)
imag1 <- readPNG("bk.png")
im1<- rasterGrob(imag1,width = unit(297,"mm"), height = unit(420,"mm"))

imag2 <- readPNG("RStudioLogo.png")
im2 <- rasterGrob(imag2,width = unit(90,"mm"), height =unit(90,"mm"))

#CRIANDO IMAGEM
png("painel.png", width = 297 , height = 420, units = "mm", res = 500)

#CONSTRUIR UM NOVO GRID
grid.newpage() 



############################################################

#CABEÇALHO E TEXTOS
pushViewport(viewport(layout = grid.layout(1, 1)))
print(grid.draw(im1))



grid.text("MATE56 2021.1", x = unit(297/2+35, "mm"),  y = unit(390, "mm"), 
          gp = gpar(fontfamily = "VertigoFLF", col = cor.3, cex = 7))

grid.text("PROF. ANDERSON ARA", x = unit(297/2+35, "mm"),  y = unit(360, "mm"), 
          gp = gpar(fontfamily = "VertigoFLF", col = cor.2, cex = 5))

grid.text("DESt - UFPR", x = unit(297/2+20, "mm"),  y = unit(335, "mm"), hjust=-0.1, 
          gp = gpar(fontfamily = "VertigoFLF", col = cor.3, cex = 5))

grid.text("Curitiba, 16 de novembro de 2021", x = unit(297/2+65, "mm"),  y = unit(10, "mm"), hjust=0.1, 
          gp = gpar(fontfamily = "VertigoFLF", col = cor.3, cex = 3))



############################################################

#INCLUINDO GRÁFICOS
pushViewport(viewport(layout = grid.layout(4, 14)))
print(g1, vp = viewport(layout.pos.row=2:3, layout.pos.col = 2:7))
print(g2, vp = viewport(layout.pos.row=2, layout.pos.col = 8:13))
print(g3, vp = viewport(layout.pos.row=3, layout.pos.col = 8:13))
print(g4, vp = viewport(layout.pos.row=4, layout.pos.col = 2:13))


#INCLUINDO RLOGO
pushViewport(viewport(layout.pos.row=1, layout.pos.col = 2:7))
print(grid.draw(im2))

dev.off()
