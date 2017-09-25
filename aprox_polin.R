plotaproxim <- function(ordem, amostras){
  library('corpcor')
  #Geração dos Dados
  N <- amostras
  ord <- ordem #Ordem do polinômio
  x <- runif(n=N,min=-15,max=10)
  xgrid <- seq(-15,10,0.1)
  yr <- (0.5*x**2 + 3*x+10) + 10*rnorm(length(x),0,4)
  ygrid <- (0.5*xgrid**2 + 3*xgrid+10)
  
  
  H<- c(1)
  Hgrid <-c(1)
  
  for (i in seq(1:ord)){
    H <- cbind(x**i,H)
    Hgrid <- cbind(xgrid**i,Hgrid)
  }
  w <- pseudoinverse(H) %*% yr
  yhatgrid <- cbind(Hgrid %*% w)
  yhat <- cbind(H%*%w)
  
  
  #Plotando
  
  xl <- c(-15,10) # limite para o eixo x
  yl <- c(-20,80) # limite para o eixo y
  
  plot(x,yr,xlab='',ylab='',col='red',xlim =xl,ylim =yl)
  par(new=t)
  plot(xgrid,ygrid,type = 'l',col='red',xlim =xl,ylim =yl,xlab='',ylab='')
  par(new=T)
  plot(x,yhat,col='blue',xlim =xl,ylim =yl,xlab='',ylab='')
  par(new=T)
  plot(xgrid,yhatgrid,col='blue',xlim =xl,ylim =yl,xlab='x',ylab='y',type='l')
  
  err<-sum((yhat-yr)**2)/length(x)
  
  return(err)
}





