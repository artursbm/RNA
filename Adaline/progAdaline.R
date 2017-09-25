rm(list = ls())

source('./trainAdaline.R')
source('./yAdaline.R')

library(ggplot2)

  dataTime <- as.matrix(read.table("./Ex1_t")) #leitura dos valores de tempo
  dataInput <- as.matrix(read.table("./Ex1_x")) #leitura dos valores de sen(t)
  dataOutput <- as.matrix(read.table("./Ex1_y")) #leitura dos valores de y = a + b*sen(t)
  
  #construção dos gráficos de entrada e saída dados
  input <- cbind(dataTime, dataInput)
  output <- cbind(dataTime, dataOutput)
  ggplot(data=as.data.frame(input), aes(input[,1], input[,2])) + geom_point(color="blue") + geom_point(data = as.data.frame(output), aes(input[,1], output[,2]), color="red")

  
  #numero de amostras que serão usadas para treinamento (tamanho de xin) = 70% do tamanho da entrada dada (X)
  Ntrain <- 0.7 * length(input[,2]) 
 
  #seleçao de dados para treinamento
  seqx <- sample(length(input[,2]))
  
  #treinamento da rede
  inputTrain <- as.matrix(dataInput[seqx[1:Ntrain],]) #dados vindos de dataInput (70% das amostras)
  outputTrain <- as.matrix(dataOutput[seqx[1:Ntrain],])
  timeTrain <- as.matrix(dataTime[seqx[1:Ntrain],]) #tempos correspondentes às amostras acima
  listResult <- trainAdaline(inputTrain, outputTrain, eta = 0.1, tol = 0.01, maxepocas = 100, 1)
  wResult <- as.matrix(unlist(listResult[1]))
  
  #seleçao de dados de teste (30% restantes)
  Ntest <- length(input[,2]) - Ntrain 
  inTest <- as.matrix(dataInput[seqx[(Ntrain+1):(Ntrain+Ntest)],])
  timeTest <- as.matrix(dataTime[seqx[(Ntrain+1):(Ntrain+Ntest)],])
  outTest <- as.matrix(dataOutput[seqx[(Ntrain+1):(Ntrain+Ntest)],])
  #Teste realizado com 30% das amostras restantes
  yt_30p <- yadaline(inTest, wResult, 1)
  erroteste_30p <- (outTest-yt_30p)^2/length(yt_30p)
  hist(erroteste_30p)
  
  result_30p <- cbind(timeTest, yt_30p)
  ggplot(data = as.data.frame(result_30p), aes(result_30p[,1], result_30p[,2])) + geom_point(color="red") + geom_point(data = as.data.frame(outTest), color="green", aes(result_30p[,1], outTest[,1]))
  ##
  ##
  ##
  
  
  #resultado final da aproximação, com todas as amostras colocadas em um gráfico,
  #tanto as usadas em treinamento como as usadas em teste
  yt <- yadaline(dataInput, wResult, 1)
  #erro médio quadratico
  erroteste <- (dataOutput-yt)^2/length(yt)
  hist(erroteste)
  
  #plot do resultado final da aproximação
  result <- cbind(dataTime, yt)
  ggplot(data = as.data.frame(result), aes(result[,1], result[,2])) + geom_point(color="red") + geom_point(data = as.data.frame(output), color="green", aes(result[,1], output[,2]))
  
  
  
  
  
  
  
  




