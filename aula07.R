
############################################################

require(circlize)
require(vcd)
m = data.frame(table(Arthritis$Sex,Arthritis$Improved))
m
chordDiagram(m,col = rainbow(6))


############################################################

sequencia<- read.table(text = '
A-aa-aaa-end
A-aa-aaa-end
A-aa-vvv-end
A-aa-vvv-end
A-cc-vvv-end
A-cc-vvv-end
B-aa-vvv-end
B-aa-vvv-end
B-bb-rr-end
B-bb-rr-end
C-aa-rr-end
C-aa-rr-end
C-bb-rr-end
C-bb-rr-end
C-cc-rr-end
')


sequencia$V2 <- seq_along(sequencia$V1)
sequencia
require(sunburstR)
sunburst(sequencia)



############################################################

library(scatterplot3d)

set.seed(1)
x=rnorm(100)
y=rnorm(100)
z=rnorm(100)

scatterplot3d(x,y,z,pch=19,cex.symbols=2,color="orange",angle=45)


############################################################

require('rgl')

set.seed(1)
x=rnorm(100)
y=rnorm(100)
z=rnorm(100)

plot3d(x, y, z, col = 'orange',size=10)


############################################################

x = seq(-3, 3,length.out=50)
y = seq(-3, 3, length.out=50)
E = mesh(x, y)

require(plot3D) 
surf3D(x = E$x, y = E$y, z = dnorm(E$x)*dnorm(E$y), colkey=FALSE,bty="b2", main=expression(paste("Normal Bivariada Padrão ",rho,"=0")),col=terrain.colors(100))


############################################################

require(lattice)
x=seq(-3,3,length.out=50)
y=seq(-3,3,length.out=50)
Z=matrix(0,nrow=50,ncol=50)
for (i in 1:50){
  for (j in 1:50){
    Z[i,j]=dnorm(x[i])*dnorm(y[j])
  }
}
wireframe(Z,shade=T,xlab="x",ylab="z",zlab="densidade")
