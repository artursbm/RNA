# Treinamento de um Perceptron simples (2 entradas, um neurônio MCP) feito a partir do 
# estudo do livro de Redes Neurais Artificiais

rm(list=ls())
library('plot3D')
#library('rgl')

c1<-matrix(rnorm(100, mean=2, sd=0.5), nrow=50, ncol=2)
c2<-matrix(rnorm(100, mean=4, sd=0.5), nrow=50, ncol=2)
# defini sd para 0.5, apenas para ter uma separação visual mais notável entre as distribuições
# dos dados amostrados nos espaços classificatórios (Acima e abaixo da porta de limiar)

plot(c1[,1], c1[,2], type='p', col='red', xlim=c(0,6), ylim=c(0,6), xlab='', ylab='')
par(new=T)
plot(c2[,1], c2[,2], type='p', col='blue', xlim=c(0,6), ylim=c(0,6), xlab='', ylab='')

seqi<-seq(0, 6, 0.1)
seqj<-seq(0, 6, 0.1)
w<-c(1, 1, -6) #vetor de referencia ao qual pretende-se aproximar wl
x1<-seqi #x1 de referencia para um perceptron definido para w[1 1 -6]
x2<-(-(w[1]/w[2])*x1) + (-w[3]/w[2]) #w[3] = -theta | x2 de referencia para o w[1 1 -6]

par(new=T)
plot(x1, x2, type = 'l', col = 'orange', xlim=c(0,6), ylim=c(0,6),  xlab='', ylab='')
# grid()
# wl<-c(0.1, 0.1, -0.1)
# ni<-0.4
# dc1<-0 #classe 1 recebe classificaçao 0
# dc2<-1 #classe 2 recebe classificaçao 1

# for(i in 1:1e+5)
# {
#   for(j in 1:50)  
#   {
#     yc1 <- 1*((t(wl)%*%c(c1[j,1], c1[j,2], 1))>=0)
#      wl[1] <- wl[1] + (ni*(dc1-yc1)*c1[j,1])
#      wl[2] <- wl[2] + (ni*(dc1-yc1)*c1[j,2])
#      wl[3] <- wl[3] + (ni*(dc1-yc1)*1)
#     
#      
#     yc2 <- 1*((t(wl)%*%c(c2[j,1], c2[j,2], 1)>=0))
#      wl[1] <- wl[1] + (ni*(dc2-yc2)*c2[j,1])
#      wl[2] <- wl[2] + (ni*(dc2-yc2)*c2[j,2])
#      wl[3] <- wl[3] + (ni*(dc2-yc2)*1)
#   }
# }
# 
#  X1<-seqi
#  X2<-(-(wl[1]/wl[2])*X1) + (-wl[3]/wl[2]) #w[3] = -theta
#  par(new=T)
#  plot(X1, X2, col='green', type='l', xlim=c(0,6), ylim=c(0,6),  xlab='x1', ylab='x2')

 
 
# w2*x2 = -w1*x1 + 6 é uma reta q possivelmente vai separar bem os dois pontos a serem classificados
# w1 = 1, w2 = 1, theta = -6 
# => w1*x1 + w2*x2 = -6 é a reta característica que classifica o espaço em duas regiões, 
# compondo um perceptron de porta de limiar linear

#Y <- matrix(1, nrow=length(seqi), ncol=length(seqj)) #matriz de saída encontrada
D <- matrix(1, nrow=length(seqi), ncol=length(seqj)) #matriz do resultado desejado
  n<-0
  for(i in seqi)
  {
    n<-n+1
    m<-0
    for(j in seqj)
    {
      m<-m+1
      xt<-c(i, j, 1)
      # Y[n, m] <- 1*((t(wl)%*%xt)>=0)
      D[n, m] <- 1*((t(w)%*%xt)>=0)
      # xt é uma variavel de teste em que (i,j) representam o ponto no espaço,
      # e 1 é a entrada fixa, que auxilia na determinação de theta
    }

  }
 persp3D(seqi, seqj, D, theta = 30, phi = 45)
 # persp3D(seqi, seqj, Y, theta = 30, phi = 45)

