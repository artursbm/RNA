rm(list=ls())


sech2<-function(u) {
  return(((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))
}
#XOR de duas saídas: uma é inversão da outra. Pq fazer isso? pq senão não tem como fazer somatório no final, pra 
#calcular a propagaçao do erro

#dados de entrada e saída
x<-matrix(c(-1,-1,-1,+1,+1,-1,+1,+1), byrow=T, ncol=2)
y<-matrix(c(-1,+1,+1,-1, +1,-1,-1,+1), byrow=T, ncol=2)

#entradas fixas
i1<-+1
i4<-+1
i5<-+1
i8<-+1

#inicialização dos pesos
##pesos de entrada
###neuronio 1
w61<-runif(1)-0.5 #tem q ser entre -0.5 e +0.5 pra ter chance de ser negativo e conseguir treinar direito
w62<-runif(1)-0.5
w63<-runif(1)-0.5
###neuronio 2
w72<-runif(1)-0.5
w73<-runif(1)-0.5
w74<-runif(1)-0.5

##pesos de saída
###neuronio 1
w95<-runif(1)-0.5
w96<-runif(1)-0.5
w97<-runif(1)-0.5
###neuronio 2
w106<-runif(1)-0.5
w107<-runif(1)-0.5
w108<-runif(1)-0.5

#inicializaçao de parametros
tol<-0.1
eta<-0.1
maxepocas<-1000
N<-dim(x)[1]
nepocas <- 0
eepoca <- tol+1
evec <- matrix(nrow=1, ncol=maxepocas)

while((nepocas < maxepocas) && (eepoca > tol)) {
  ei2 <- 0 #erro quadrático
  xseq<-sample(N)
  for(i in 1:N) {
    irand <- xseq[i]
    
    i2<-x[irand,2] #x1 é coluna 2
    i3<-x[irand,1] #x2 é coluna 1
    
    u6<-(i1*w61)+(i2*w62)+(i3*w63)
    u7<-(i2*w72)+(i3*w73)+(i4*w74)
    
    i6<-tanh(u6)
    i7<-tanh(u7) 
    
    u9<-(i5*w95)+(i6*w96)+(i7*w97)
    u10<-(i6*w106)+(i7*w107)+(i8*w108)
    
    i9<-tanh(u9)
    i10<-tanh(u10)
    
    e9<-(y[irand,2] - i9)
    e10<-(y[irand,1] - i10)
    
    d9<-e9*sech2(u9)
    d10<-e10*sech2(u10)
    
    dw95<-eta*d9*i5
    dw96<-eta*d9*i6
    dw97<-eta*d9*i7
    
    dw106<-eta*d10*i6
    dw107<-eta*d10*i7
    dw108<-eta*d10*i8
    
    d6<-sech2(u6)*((d9*w96) + (d10*w106))
    d7<-sech2(u7)*((d9*w97) + (d10*w107))
    
    dw61<-eta*d6*i1
    dw62<-eta*d6*i2
    dw63<-eta*d6*i3
    
    dw72<-eta*d7*i2
    dw73<-eta*d7*i3
    dw74<-eta*d7*i4
    
    #entrada
    w61<-w61+dw61
    w62<-w62+dw62
    w63<-w63+dw63
    
    w72<-w72+dw72
    w73<-w73+dw73
    w74<-w74+dw74
    
    #saída
    w95<-w95+dw95
    w96<-w96+dw96
    w97<-w97+dw97
    
    w106<-w106+dw106
    w107<-w107+dw107
    w108<-w108+dw108
    
    #erro quadrático
    ei2 <- ei2 + (e9^2) + (e10^2)
  }
  nepocas <- nepocas + 1
  evec[nepocas] <- ei2/N
  eepoca <- evec[nepocas]
}

plot(evec[1,], type='l')





