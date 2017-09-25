rm(list=ls())
source('~/trainperceptron.R')
source('~/yperceptron.R')
library('plot3D')
library('ggplot2')

data(iris)

#Número de Amostras do Treinamento
ntrain <- 35 # 70% das amostras ( 0.7 vezes 50 amostras = 35 amostras)

#Amostras das Classes
xc1 <- iris[1:50,1:4]
xc2 <- iris[51:100,1:4]
xc3 <- iris[101:150,1:4]

##Seleção das amostras para o treinamento
#Classe 1
seqc1 <- sample(50)
xc1treina <- xc1[seqc1[1:ntrain],]
yc1treina <- matrix(0,nrow=ntrain)

#Classe 2
seqc2 <- sample(50)
xc2treina <- xc2[seqc2[1:ntrain],]
yc2treina <- matrix(1,nrow=ntrain)

#Classe 3
seqc3 <- sample(50)
xc3treina <- xc3[seqc3[1:ntrain],]
yc3treina <- matrix(1,nrow=ntrain)

# Seleção das Amostras a serem Classificadas
#Classe 1
xc1teste <- xc1[seqc1[(ntrain+1):50],]
yc1teste <- matrix(0,nrow=(50-ntrain))

#Classe 2
xc2teste <- xc2[seqc2[(ntrain+1):50],]
yc2teste <- matrix(1,nrow=(50-ntrain))

#Classe 3
xc3teste <- xc3[seqc3[(ntrain+1):50],]
yc3teste <- matrix(1,nrow=(50-ntrain))
############################

#Separação entre amostras 1 e 2
xin <- as.matrix(rbind(xc1treina,xc2treina))
yd <- rbind(yc1treina,yc2treina)
xinteste <- as.matrix(rbind(xc1teste,xc2teste))
yteste<- rbind(yc1teste,yc2teste)
retlist <- trainperceptron(xin,yd,0.1,0.01,100,1)
wt <- as.matrix(unlist(retlist[1]))
yt <- yperceptron(xinteste,wt,1)
erroteste <- (yteste-yt)^2
hist(erroteste) #Histograma dos erros para a separação entre as amostras 1 e 2.

#resultado da separação
plot(yt,col='red')
#resultado correto
plot(yteste,col='blue')
table(yteste,yt)
############################

#Separação entre amostras 1 e 3
xin <- as.matrix(rbind(xc1treina,xc3treina))
yd <- rbind(yc1treina,yc3treina)
xinteste <- as.matrix(rbind(xc1teste,xc3teste))
yteste<- rbind(yc1teste,yc3teste)
retlist <- trainperceptron(xin,yd,0.1,0.01,100,1)
wt <- as.matrix(unlist(retlist[1]))
yt <- yperceptron(xinteste,wt,1)
erroteste <- (yteste-yt)^2
hist(erroteste)

#resultado da separação
plot(yt,col='red')
#resultado correto
plot(yteste,col='blue')
table(yteste,yt)
############################

#Separação das Amostras 2 e 3
#Classe 2
yc2treina <- matrix(0,nrow=ntrain)
# Seleção das Amostras a serem Classificadas
#Classe 2
yc2teste <- matrix(0,nrow=(50-ntrain))
#separação entre amostras 2 e 3
xin <- as.matrix(rbind(xc2treina,xc3treina))
yd <- rbind(yc2treina,yc3treina)
xinteste <- as.matrix(rbind(xc2teste,xc3teste))
yteste<- rbind(yc2teste,yc3teste)
retlist <- trainperceptron(xin,yd,0.1,0.01,100,1)
wt <- as.matrix(unlist(retlist[1]))
yt <- yperceptron(xinteste,wt,1)
erroteste <- (yteste-yt)^2
hist(erroteste) #Histograma dos erros para a separação entre as amostras 2 e 3

#resultado da separação
plot(yt,col='red')
#resultado correto
plot(yteste,col='blue') 
table(yteste,yt)
############################

#Separação entre amostras 1, 2, 3
#Classe 2
yc2treina <- matrix(1,nrow=ntrain)
# Seleção das Amostras a serem Classificadas
#Classe 2
yc2teste <- matrix(1,nrow=(50-ntrain))
#Separação entre amostras 1, 2 e 3
xin <- as.matrix(rbind(xc1treina,xc2treina,xc3treina))
yd <- rbind(yc1treina,yc2treina,yc3treina)
xinteste <- as.matrix(rbind(xc1teste,xc2teste,xc3teste))
yteste<- rbind(yc1teste,yc2teste,yc3teste)
retlist <- trainperceptron(xin,yd,0.1,0.01,100,1)
wt <- as.matrix(unlist(retlist[1]))
yt <- yperceptron(xinteste,wt,1)
erroteste <- (yteste-yt)^2
hist(erroteste) #Histograma dos erros para a separação entre as amostras 1, 2 e 3

#resultado da separação
plot(yt,col='red')
#resultado correto
plot(yteste,col='blue')
table(yteste, yt)
############################


#ERRO PERCENTUAL DO TREINAMENTO PELO NÚMERO DE ITERAÇÕES
erro = c()
for (i in seq(1,100)){
  #Separação entre amostras 1, 2 e 3
  xin <- as.matrix(rbind(xc1treina,xc2treina,xc3treina))
  yd <- rbind(yc1treina,yc2treina,yc3treina)
  
  xinteste <- as.matrix(rbind(xc1teste,xc2teste,xc3teste))
  yteste<- rbind(yc1teste,yc2teste,yc3teste)
  
  retlist <- trainperceptron(xin,yd,0.1,0.01,100,1)
  
  wt <- as.matrix(unlist(retlist[1]))
  
  yt <- yperceptron(xinteste,wt,1)
  erroteste <- (yteste-yt)^2
  erroaux <- sum(erroteste)/45 *100
  erro <- c(erro,erroaux)
}

plot(erro,type = 'o',xlab ='Iterações',ylab='Erro(%)')
grid()
############################
