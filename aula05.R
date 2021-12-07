
############################################################

#Definindo valores
x <-  c(1,2,3,4,5,6,7)

y1 <- c(1,4,9,16,25,36,49)
y2 <- c(1,5,12,21,34,51,72)
y3 <- c(1,6,14,28,47,73,106)

#Criando gráfico
plot(x, y1, type="o", col="blue", pch="o", lty=1, 
     ylim=c(0,100), ylab="y")

#Adicionando linhas e pontos
points(x, y2, col="purple", pch="*")
lines(x, y2, col="purple",lty=2)

points(x, y3, col="darkred",pch="+")
lines(x, y3, col="darkred", lty=3)

############################################################

#Adicionando legenda
legend(1,100,c("y1","y2","y3"),col=c("blue","purple", "darkred"),pch=c("o","*","+"),lty=c(1,2,3), ncol=1)


############################################################

plot(sin,0,2*pi)


############################################################


fx = function(x){ x^3 - a*x^2 + 4*x + 12}

a=6
plot(fx, from=-2, to=6, n=300, xlab="x", ylab=expression(f(x)), col="blue",lwd=2,main=expression(paste("Função  ",f(x)," = ",x^3-alpha*x^2+4*x+12)))

a=8
plot(fx, from=-2, to=6, n=300, xlab="x", ylab=expression(f(x)), col="orange",lwd=2,add=T)

abline(0,0,0,0,lty=2)

text(1,30,expression(paste(alpha," = 6")),cex=1.2,col="blue")  
text(1,25,expression(paste(alpha," = 8")),cex=1.2,col="orange") 

############################################################

set.seed(100)
#gerando variáveis
v1 <- rbinom(100,5,0.5)
v2 <- rbinom(100,5,0.25)

#plot tradicional
plot(v1,v2,cex=2,pch=19)


############################################################

#smoothScatter

Lab.palette <- colorRampPalette(c("white", "blue", "red"), space = "rgb")

smoothScatter(v1,v2,xlab="v1",ylab="v2",main="",
              colramp=Lab.palette)


############################################################

##GERAÇÃO DE VALORES ##
set.seed(1)
x <- rnorm(100,mean=10,sd=5)
y <- 2*x + rnorm(100,mean=5,sd=8)

x1 <- runif(10,0,15)
y1 <- 2*x1 + runif(10,-5,15)

x2 <- runif(10,15,25)
y2 <- 2.5*x2 - 1.0 + runif(10,-5,15)

#Gráfico
plot(x,y,xlab="Independente",ylab="Dependente",main="Geração")
points(x1,y1,col=2,pch=3,cex=1.5)
points(x2,y2,col=4,pch=5,cex=1.5)
legend(18,8,c("G1","G2","G3"),col=c(1,2,4),pch=c(1,3,5))


############################################################

plot(dnorm,from=-3,to=3,ylab="densidade",lwd=2)

############################################################

x=rnorm(1000)

y <- hist(x,plot = FALSE)

plot(c(y$breaks, max(y$breaks)), c(0, y$density, 0), col = "orange", type = "S",lwd = 2, lty = 1, xlab = " ",ylab = "Densidade", main = " ")

############################################################

x=rnorm(1000)

hist(x,prob=T,ylab="Densidade",main="",col="gray",border=NA)

points(density(x),type='l',lwd=2,col="red")

############################################################

z=seq(-3,3,length.out=1000)

dz=dnorm(z)

plot(z,dz,type='l',lwd=0,ylab=expression(phi(z)))

for (i in 1:length(z)){
  if (abs(z[i])>1) points(z[i],dz[i],type="h",col="blue")
  else  points(z[i],dz[i],type="h",col="gray")
}

############################################################

x = c(-1,1,1,-1,-1)
y = c(-1,-1,1,1,-1)

plot(x,y)

#preencimento da área
polygon(x,y,col='blue')

############################################################

x=seq(from=0,to=2*pi,length.out=100)

plot(sin(x),cos(x))

polygon(sin(x),cos(x),col='orange',border="red",
        lwd=2,lty=2)

############################################################

desvio <- 1;
x <- seq(-5,5,by=0.01)
y <- dnorm(x,sd=desvio)
z <- qnorm(0.95,sd=desvio)
plot(x,y,type="l",xaxt="n",ylab="densidade", xlab=expression(paste('Distribuição de ',bar(X))), axes=FALSE,ylim=c(0,max(y)*1.05),xlim=c(min(x),max(x)))
axis(1,at=c(-5,z,0,5), pos = c(0,0),
     labels=c(expression(' '),expression(bar(x)[critico]),
              expression(mu),expression(' ')))

axis(2)
xR <- seq(z,5,by=0.01)
yR <- dnorm(xR,sd=desvio)
polygon(c(xR,xR[length(xR)],xR[1]),c(yR,0, 0), col='blue')


############################################################

x=seq(-5,5,by=0.1)

y1=dnorm(x)
y2=dnorm(x,2,2)

plot(x,y1,type='n',ylab='densidade')
polygon(x,y1,col = rgb(red = 190, green = 190, blue = 190, alpha = 100, maxColorValue = 300), border = NA)

polygon(c(x,5),c(y2,0),col = rgb(red = 190, green = 190, blue = 190, alpha = 140, maxColorValue = 300), border = NA)


############################################################


x=rnorm(1000)

xi=seq(-3,3,by=0.01)

hist(x,prob=T,ylim=c(0,0.5),xlim=c(-3,3))
par(new=T)
plot(xi,dnorm(xi),ylim=c(0,0.5),xlim=
       c(-3,3),xlab="",ylab="",type="l",col="blue")



plot(1:10,1:10,type="n",axes=F,xlab="",ylab="")
windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Symbol")
)
text(3,3,"Visualização")
text(4,4,family="A","Visualização Arial Black")
text(5,5,family="B","Visualização Bookman Old Style")
text(6,6,family="C","Visualização Comic Sans MS")
text(7,7,family="D", "Visualização Symbol")

############################################################

par(mfrow=c(2,3)); for(i in 1:6) { plot(1:10) }

############################################################

jg <- layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE), 
             c(5,5), c(3,3),respect=T)

layout.show(jg)
for(i in 1:3) { barplot(1:10) }


############################################################

#setwd("C:\\...\\")

svg("test.svg"); plot(1:10, 1:10); dev.off()

pdf("test.pdf"); plot(1:10, 1:10); dev.off()
