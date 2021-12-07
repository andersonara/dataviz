
############################################################

set.seed(100)
#gerando variáveis
v1 <- rbinom(100,10,0.5)
v2 <- rbinom(100,10,0.25)

sunflowerplot(v1,v2)

############################################################

#install.packages("hexbin")
require(hexbin)
hex=hexbin(iris$Petal.Length,iris$Sepal.Length)
plot(hex)

############################################################


x<-c(1,10,30,22,5,20)
y<-c(4,17,4,33,20,15)
z<-c(20,15,15,34,33,30)
t<-c("T1","T2","T1","T2","T2","T2")

symbols(x,y, circles = z, inches = 0.3, bg = "orange")
text(x, y, t, cex = 1)

legend("topright",c("15", "20", "25", "30"), pch = 21, 
       box.col = NA,pt.bg = "orange", pt.cex = c(1.5,2,2.5,3)) 

############################################################


ano=c(rep(2016,12),rep(2017,12))
mes=c(rep(1:12,2))
vendas=rpois(24,15)
m <- c('Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez')
cores=c("orange","orangered1")

barplot(rbind(vendas[1:12],vendas[13:24]),beside=T,names.arg = m,col=cores,border=NA,ylab="Vendas")
legend('topleft',c('2016','2017'),pch=15,col=cores,box.col=NA)

############################################################

barplot(rbind(vendas[1:12],vendas[13:24]),names.arg = m,col=cores,border=NA,ylab="Vendas")

legend('topleft',c('2016','2017'),pch=15,col=cores,box.col=NA)

############################################################

v=rbind(vendas[1:12],vendas[13:24])
soma=apply(v,2,sum)
p=v %*% diag(1 / soma)
barplot(p,names.arg = m,col=cores,border=NA,ylab="%Vendas")

############################################################

speedo<-function(xi,vertices=5000,col=heat.colors(100),...){   
  plot(c(-1,1), c(0,1), type="n", axes=FALSE, xlab="",
       ylab="",...)
  y=c(0,pi*rep(1:100)/100)
  A=seq(0,pi,length=vertices)
  for (ii in 1:(length(y)-1)) {
    poly=A[ y[ii]<=A & A<=y[ii+1] ]
    polygon(c(cos(poly),0)*0.85,
            c(sin(poly),0)*0.85,col=col[ii],border=col[ii])
  } 
  polygon(c(cos(A),0)*0.2,c(sin(A),0)*0.2,col='white',border='white')
  arrows(0, 0, x1 = cos((1-xi)*pi)*0.87, y1 = sin((1-xi)*pi)*0.87,lwd = 4)
  text(cos((1-xi)*pi),sin((1-xi)*pi),paste0(xi*100,"%"),cex=2.5)
}

speedo(0.76)

############################################################

install.packages("tabplot")
require("tabplot")

data(iris)

tableplot(iris,sortCol=1)

############################################################

data(HairEyeColor)
head(HairEyeColor)
mosaicplot(HairEyeColor,col=c("blue","red"),main="")

############################################################

falha <- factor(c(2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1,1, 1, 1, 2, 1, 1, 1, 1, 1), levels = c(1, 2), labels = c("não", "sim"))

temperatura <- c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70, 70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81)

spineplot(falha ~ temperatura, breaks = quantile(temperatura),
          col=c('blue','orange'))


############################################################

data(mtcars)
head(mtcars)
a=as.matrix(mtcars)
heatmap(a,col=terrain.colors(256))

############################################################

data(longley)
a=as.matrix(longley[,-6])
heatmap(a,col=terrain.colors(256),
        margins=c(9,5))

############################################################

data(longley)

require("aplpack")

faces(longley[,-6],face.type=0)
faces(longley[,-6],face.type=1)
faces(longley[,-6],face.type=2)


############################################################

require(wordcloud)
require(tm)
file = readLines("http://leg.ufpr.br/~ara/dados/dilma.txt")
doc = Corpus(VectorSource(file))
doc= tm_map(doc, tolower)
doc= tm_map(doc, removePunctuation)
doc= tm_map(doc, removeNumbers)
doc= tm_map(doc, removeWords,stopwords("portuguese"))
wordcloud(as.character(doc),colors = c("gray","orange","red"),max.words=30,random.order=F)

############################################################

require(plotrix)
radial.plot(1:10,rp.type="p",lwd =3,line.col="red",
            labels=LETTERS[1:10], clockwise = TRUE, start = 0)
