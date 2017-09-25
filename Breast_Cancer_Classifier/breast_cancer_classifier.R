### ------------------ ###
# testing accuracy on the classification of breast cancer samples;
# if sample is benign, result is classified as '0', otherwise, it's '1'
# for malign cases.
#####----------------**-------------------#####
# Percentage for Training samples: 60% > 410 samples;
# Percentage for Testing samples: 40% > 273 samples;

rm(list = ls())

library("RSNNS")
library("pROC")

importData <- function(directory) {
  files <- list.files(path = directory, pattern = ".csv", full.names = T)
  getdata <- read.csv(files)
  getdata[getdata == "?"] <- NA  
  return(as.data.frame(getdata))
}

dataTreat <- function() {
  ##Importing data from the dataset Wisconsin Breast Cancer
Data <- importData("./dataSet/")
  ##removing rows with NA in any column and the ID of the samples (don't care):
Data <- Data[,2:11]
as.numeric(levels(Data$BareNuc))
Data <- Data[complete.cases(Data),]
Data$BareNuc <- factor(Data$BareNuc)
  for (index in 1:10) { 
    Data[,index] <- as.numeric(Data[,index])
  }
  
  ##changing Class value to binary (1 or 0) case
  ##if it's benign, Class = 0
Data$Class[Data$Class == 2] <- 0
  ##if it's malign, Class = 1
Data$Class[Data$Class == 4] <- 1

return(Data)
}

allData <- dataTreat()
  
  ##Important values for beginning
  ###summary of each parameter from the dataset;
summ <- summary(allData)
  ###mean value of the output result, which means that most samples are 'benign' cases;
hope <- mean(allData$Class)

#input parameters: Columns 1 to 9;
inData <- allData[, 1:9]
#output values: column 10;
#expected output (allData$Class column in one array)
outExpect <- allData[, 10]

  ##selecting random samples for training (60% of samples):
nsample <- as.integer(length(allData$Class))
samp <- sample(nsample)
inData <- inData[samp, ]
outExpect <- outExpect[samp]
trainData <- splitForTrainingAndTest(inData, outExpect, 0.4)
normTrainingAndTestSet(trainData)

#training using Back-propagation algorithm
model1 <- mlp(trainData$inputsTrain, trainData$targetsTrain, size = 5,
             learnFuncParams = 0.1, maxit = 100, inputsTest = trainData$inputsTest,
             targetsTest = trainData$targetsTest,  initFunc = "Randomize_Weights",
             initFuncParams = c(-0.5, 0.5), learnFunc = "Std_Backpropagation")

#training using Rprop algorithm
model2 <- mlp(trainData$inputsTrain, trainData$targetsTrain, size = 5,
              learnFuncParams = 0.1, maxit = 100, inputsTest = trainData$inputsTest,
              targetsTest = trainData$targetsTest, learnFunc = "Rprop", initFunc = "Randomize_Weights",
              initFuncParams = c(-0.5, 0.5))

#training using BackProp with Weight Decay algorithm
model3 <- mlp(trainData$inputsTrain, trainData$targetsTrain, size = 5,
              learnFuncParams = 0.1, maxit = 100, inputsTest = trainData$inputsTest,
              targetsTest = trainData$targetsTest, learnFunc = "BackpropWeightDecay",
              initFuncParams = c(-0.5, 0.5))



predictions1 <- predict(model1, trainData$inputsTest)
predictions2 <- predict(model2, trainData$inputsTest)
predictions3 <- predict(model3, trainData$inputsTest)

table_result1 <- cbind(predictions1,trainData$targetsTest)
table_result2 <- cbind(predictions2,trainData$targetsTest)
table_result3 <- cbind(predictions3,trainData$targetsTest)

error1 <- (predictions1-trainData$targetsTest)^2/length(predictions1)
error2 <- (predictions2-trainData$targetsTest)^2/length(predictions2)
error3 <- (predictions3-trainData$targetsTest)^2/length(predictions3)

plotIterativeError(model1)
plotIterativeError(model2)
plotIterativeError(model3)

hist(error1, main='(a)')
hist(error2, main='(b)')
hist(error3, main='(c)')

plotROC(predictions1, trainData$targetsTest)

plotROC(predictions2, trainData$targetsTest)

plotROC(predictions3, trainData$targetsTest)
##### end of code ###
