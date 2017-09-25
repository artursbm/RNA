rm(list=ls())
source('~/trainperceptron.R')
source('~/yperceptron.R')
library('plot3D')

s1 <- 0.4
s2 <- 0.4
nc <- 200

xc1 <- matrix(rnorm(nc*2),ncol = 2)*s1 +t(matrix((c(2,2)),ncol=nc,nrow=2))
xc2 <- matrix(rnorm(nc*2),ncol = 2)*s2 +t(matrix((c(4,4)),ncol=nc,nrow=2))
plot(xc1[,1],xc1[,2],col='red',xlim = c(0,6),ylim=c(0,6),xlab='x_1',ylab='x_2')
par(new=T)
plot(xc2[,1],xc2[,2],col='blue',xlim = c(0,6),ylim=c(0,6),xlab='',ylab='')

x1_reta <- seq(6/100,6,6/100)
x2_reta <- -x1_reta+6
par(new=T)
plot(x1_reta,x2_reta,type='l',col='orange',xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')

yd1 <- matrix(0,nrow=nc, ncol=1)
yd2 <- matrix(1,nrow=nc, ncol=1)
yd <- rbind(yd1,yd2)

xin <- rbind(xc1,xc2)

wlist <- trainperceptron(xin,yd,0.1,0.01,2000,1)
w <- as.matrix(unlist(wlist[1]))

seqi <- seq(0,6,0.1)
seqj <- seq(0,6,0.1)
M <- matrix(0,nrow=length(seqi),ncol=length(seqj))

ci <-0
for (i in seqi){
  ci <- ci+1
  cj <- 0
  for (j in seqj){
    cj <- cj + 1
    x <- c(i,j)
    xt <- t(x)
    M[ci,cj] <- yperceptron(xt,w,1)
  }
}

plot(xc1[,1],xc1[,2],col='red',xlim=c(0,6),ylim=c(0,6),xlab='x_1',ylab='x_2')
par(new=T)
plot(xc2[,1],xc2[,2],col='blue',xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')
par(new=T)
contour(seqi,seqj,M,xlim=c(0,6),ylim=c(0,6),xlab='',ylab='')

persp3D(seqi,seqj,M,counter=T,theta=55,phi=30,r=40,d=0.1,expand=0.5,ltheta=90,lphi=180,shade=0.4,ticktype='detailed',nticks=5) 

