rm(list = ls())

library(RSNNS)
library(nnet)
library(devtools)
library(NeuralNetTools)

importData <- function(directory) {
  files <- list.files(path = directory, pattern = ".csv", full.names = T)
  dataImport <- read.csv(files)
  return(as.data.frame(dataImport))
}

treatData <- function() {
  dataset <- importData(pwd)  
  dataset <- dataset[complete.cases(dataset),]
  
    return(dataset)
}

allData <- treatData()

#input data and the output result for classification
inData <- allData[,1:9]
outExpect <- allData[,10]

#####
##selecting random samples for training (60% of samples):
set.seed(10)
nsample <- as.integer(length(allData$contraceptive_method))
samp <- sample(nsample)
inData <- inData[samp, ]
outExpect <- outExpect[samp]
#decoding classes into a matrix of 3 columns (binary combination for result)
dataDecTargets <- decodeClassLabels(outExpect)
trainData <- splitForTrainingAndTest(inData, dataDecTargets, 0.4)
trainData <- normTrainingAndTestSet(trainData)

pruneFuncParams <- list(max_pr_error_increase = 10.0, pr_accepted_error = 1.0, 
                        no_of_pr_retrain_cycles = 100, min_error_to_stop = 0.01, init_matrix_value = 1e-6, 
                        input_pruning = TRUE, hidden_pruning = TRUE)


model1 <- mlp(x=trainData$inputsTrain, y=trainData$targetsTrain, size = c(10,5), maxit=100,
              initFunc = "Randomize_Weights", learnFuncParams = c(0.1,0.1,0.1),
              learnFunc = "Std_Backpropagation", initFuncParams=c(-0.3,0.15,0.3), linOut = T)

model2 <- mlp(trainData$inputsTrain, trainData$targetsTrain, size = c(10,5),
              learnFuncParams = 0.1, maxit = 100,
              learnFunc = "BackpropWeightDecay",
              d = 0.01,
              initFunc = "Randomize_Weights", linOut = T)

model3 <- mlp(trainData$inputsTrain, trainData$targetsTrain, size = c(10,5),
                  learnFuncParams = c(0.2,0.2), maxit = 100,
                  learnFunc = "Std_Backpropagation", initFunc = "Randomize_Weights",
                  pruneFunc ="OptimalBrainSurgeon", pruneFuncParams = pruneFuncParams, linOut = T)

prediction <- predict(model1, trainData$inputsTest)
prediction2 <- predict(model2, trainData$inputsTest)
prediction3 <- predict(model3, trainData$inputsTest)

plotIterativeError(model1, main='Standard Backpropagation')
plotIterativeError(model2, main='Backpropagation with Weight Decay')
plotIterativeError(model3, main='Std Backprop with Pruning')

plotRegressionError(prediction[,1], trainData$targetsTest[,1], pch=3, main='Standard Backpropagation')
plotRegressionError(prediction2[,1], trainData$targetsTest[,1], pch=3, main='Backpropagation with Weight Decay')
plotRegressionError(prediction3[,1], trainData$targetsTest[,1], pch=3, main='Std Backprop with Pruning')


#confusion matrix: diagonal shows correct results
confMatModel1 <- confusionMatrix(trainData$targetsTrain, fitted.values(model1))
confMatModel2 <- confusionMatrix(trainData$targetsTrain, fitted.values(model2))
confMatModel3 <- confusionMatrix(trainData$targetsTrain, fitted.values(model3))



image(as.matrix(confMatModel1)[,3:1], axes=FALSE)

##Add in the y-axis labels. Similar idea for x-axis.
axis(2, at = seq(0, 1, length=length(colnames(as.matrix(confMatModel1)))), labels=colnames(confMatModel1))

heatmap(t(confMatModel1)[3:1,], Rowv=NA,
        Colv=NA, col = heat.colors(256))

image(as.matrix(confMatModel2)[,3:1], axes=FALSE)

##Add in the y-axis labels. Similar idea for x-axis.
axis(2, at = seq(0, 1, length=length(colnames(as.matrix(confMatModel2)))), labels=colnames(confMatModel2))

heatmap(t(confMatModel2)[3:1,], Rowv=NA,
        Colv=NA, col = heat.colors(256))


image(as.matrix(confMatModel3)[,3:1], axes=FALSE)

##Add in the y-axis labels. Similar idea for x-axis.
axis(2, at = seq(0, 1, length=length(colnames(as.matrix(confMatModel3)))), labels=colnames(confMatModel3))

heatmap(t(confMatModel3)[3:1,], Rowv=NA,
        Colv=NA, col = heat.colors(256))


plotnet(model1)
plotnet(model2)
plotnet(model3)

error1 <- (prediction-trainData$targetsTest)^2/length(prediction)
error2 <- (prediction2-trainData$targetsTest)^2/length(prediction2)
error3 <- (prediction3-trainData$targetsTest)^2/length(prediction3)

hist(error1, main='Standard Backpropagation')
hist(error2, main='Backpropagation with Weight Decay')
hist(error3, main='Std Backprop with Pruning')

