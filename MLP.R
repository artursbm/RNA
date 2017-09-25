# XOR é um problema de 2 camadas. Não existe uma porta lógica Básica XOR (as básicas sao AND OR e NOT)
# y = not(x2)*x1 + x2*not(X1)
# x2  x1 |  y |
# 0   0  |  0 |
# 0   1  |  1 | 
# 1   0  |  1 |
# 1   1  |  0 |

# Problema nao pode ser resolvido em 1 camada, apenas. Precisa de duas. 
# 
rm(list=ls())

library("plot3D")

x<-matrix(c(0,0,0,1,1,0,1,1), byrow=T, ncol=2)
y<-matrix(c(0,1,1,0), byrow=T, ncol=1)
xaug<-cbind(x,1)


# x2 = -x1 + 1.5 reta q separa em cima 
# x2 = -x1 + 0.5 reta q separa embaixo
# w1x1 + w2x2 = theta (w0) -> theta = w0 = 1
# x2 = -(w1/w2)x1 + (w0/w2) < w1 = 1, w2 = 1, w0 = 1.5

w_1<-matrix(c(1, 1, -1.5),ncol=1)
w_2<-matrix(c(1, 1, -0.5),ncol=1)

 plot(x[1,1], x[1,2], type='p', col='red', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
 par(new=T)
 plot(x[2,1], x[2,2], type='p', col='blue', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
 par(new=T)
 plot(x[3,1], x[3,2], type='p', col='blue', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
 par(new=T)
 plot(x[4,1], x[4,2], type='p', col='red', xlim=c(0,1), ylim=c(0,1), xlab='x1', ylab='x2')

seqi<-seq(0, 1, 0.1)
seqj<-seq(0, 1, 0.1)
#para w1=1, w2=1, theta=-1.5
x1<-seqi
x2<-(-(w_1[1]/w_1[2])*x1) + (-w_1[3]/w_1[2])

#para w1=1, w2=1, theta=-0.5
x3<-(-(w_2[1]/w_2[2])*x1) + (-w_2[3]/w_2[2])
# 
 par(new=T)
 plot(x1, x2, type='l', col='black', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
 par(new=T)
 plot(x1, x3, type='l', col='black', xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')


h1<-1*((xaug%*%w_1)>=0)
h2<-1*((xaug%*%w_2)>=0)

H<-cbind(h1,h2)


plot(H[1,1],H[1,2],col='red',type='p',xlim=c(0,1),ylim = c(0,1), xlab='', ylab='')
par(new=T)
plot(H[2,1],H[2,2],col='blue',type='p',xlim=c(0,1),ylim = c(0,1), xlab='', ylab='')
par(new=T)
plot(H[3,1],H[3,2],col='blue',type='p',xlim=c(0,1),ylim = c(0,1), xlab='', ylab='')
par(new=T)
plot(H[4,1],H[4,2],col='red',type='p',xlim=c(0,1),ylim = c(0,1), xlab='', ylab='')

w_3<-matrix(c(1,-1,0.5),ncol = 1)
x4<-(-(w_3[1]/w_3[2])*x1) + (-w_3[3]/w_3[2])
par(new=T)
plot(x1, x4, type='l', xlim=c(0,1),ylim = c(0,1), xlab='y1', ylab='y2')

H1<-matrix(1, nrow=length(seqi), ncol=length(seqj))
H2<-matrix(1, nrow=length(seqi), ncol=length(seqj))
Y<-matrix(1, nrow=length(seqi), ncol=length(seqj))
Ho<-matrix(1, nrow=length(seqi), ncol=length(seqj))

n<-0
for(i in seqi)
{
  n<-n+1
  m<-0
  for(j in seqj)
  {
    m<-m+1
    xt<-c(i, j, 1)
    Y[n, m] <- 1*((t(w_3)%*%xt)<=0)
    H1[n, m] <- 1*((t(w_2)%*%xt)>=0)
    H2[n, m] <- 1*((t(w_1)%*%xt)>=0)
    Ho[n, m] <- (1*((t(w_2)%*%xt)>=0)) && (1*((t(w_1)%*%xt)<=0))
  }
}

persp3D(seqi, seqj, Y, contour=T)
persp3D(seqi, seqj, H1, contour=T, phi = 30, theta = 30)
persp3D(seqi, seqj, H2, contour=T, phi = 30, theta = 30)
persp3D(seqi, seqj, Ho, contour=T, phi = 30, theta = 45)




