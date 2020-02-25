library(e1071)
library(caret)

trainGradientDescent <- function(X_train, y_train, learningRate, momentum) {
  X_train = cbind(c(1),X_train) #Add a column of 1 to X_train to cater the bias unit
  w <- sample(x = c(0,1), size = ncol(X_train), replace = TRUE) #Initialize all weights randomly
  
  yhat = c()
  deltaW = 0
  for (itr in 1:100) {
    for (i in 1:nrow(X_train)) {
      yhat = rbind(yhat, sum(X_train[i,]*w))
      ydiff = y_train[i]-yhat[i]
      ydiffn = learningRate * ydiff
      deltaW = momentum*deltaW + ydiffn%*%X_train[i,]
      w <- w + deltaW
    }
  }
  return(w)
}
testGradientDescent <- function(X_test, regressionCoefficients) {
  X_test = cbind(c(1),X_test) #Add a column of 1's to handle bias
  return(X_test%*%t(regressionCoefficients))
}

driver <-function() {
X_train = as.matrix(read.table('train2_5.txt'))
y_train = as.matrix(read.table('train2_5Labels.txt'))
X_test = as.matrix(read.table('test2_5.txt'))
y_test = as.matrix(read.table('test2_5Labels.txt'))

r = X_train[1,] #first digit image, i.e., image in row 1
r = data.matrix(r) 
dim(r) = c(16,16) 
image(r) #view image

#Case1
print('Case 1')
momentum = 0
learningRate = 0.001

regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTrain1 = testGradientDescent(X_train, regressionCoefficients)
predictYTest1 = testGradientDescent(X_test, regressionCoefficients)

mapPredtrainX <- ifelse(predictYTrain1>=mean(predictYTrain1), 5, 2)
mapPredtestX <- ifelse(predictYTest1>=mean(predictYTest1), 5, 2)

print("Case1: RESULTS FOR TRAINING DATA")
print(confusionMatrix(factor(as.matrix(mapPredtrainX)),factor(y_train)))
print("Case1: RESULTS FOR TEST DATA")
print(confusionMatrix(factor(as.matrix(mapPredtestX)),factor(y_test)))

#Case2
print('Case 2')
learningRate = 0.7
momentum = 0.9
regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTrain2 = testGradientDescent(X_train, regressionCoefficients)
learningRate = 0.1
momentum = 0.5
regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTest2 = testGradientDescent(X_test, regressionCoefficients)

mapPredtrainX <- ifelse(predictYTrain2>=mean(predictYTrain2), 5, 2)
mapPredtestX <- ifelse(predictYTest2>=mean(predictYTest2), 5, 2)

print("Case2: RESULTS FOR TRAINING DATA")
print(confusionMatrix(factor(as.matrix(mapPredtrainX)),factor(y_train)))
print("Case2: RESULTS FOR TEST DATA")
print(confusionMatrix(factor(as.matrix(mapPredtestX)),factor(y_test)))

#Case3
print('Case 3')
learningRate = 0.05
momentum = 0.2
regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTrain3 = testGradientDescent(X_train, regressionCoefficients)
learningRate = 0.01
momentum = 0.3
regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTest3 = testGradientDescent(X_test, regressionCoefficients)

mapPredtrainX <- ifelse(predictYTrain3>=mean(predictYTrain3), 5, 2)
mapPredtestX <- ifelse(predictYTest3>=mean(predictYTest3), 5, 2)

print("Case3: RESULTS FOR TRAINING DATA")
print(confusionMatrix(factor(as.matrix(mapPredtrainX)),factor(y_train)))
print("Case3: RESULTS FOR TEST DATA")
print(confusionMatrix(factor(as.matrix(mapPredtestX)),factor(y_test)))

#Case4
print('Case 4')
learningRate = 0.6
momentum = 0.3
regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTrain4 = testGradientDescent(X_train, regressionCoefficients)
learningRate = 0.7
momentum = 0.4
regressionCoefficients = trainGradientDescent(X_train, y_train, learningRate, momentum)
predictYTest4 = testGradientDescent(X_test, regressionCoefficients)

mapPredtrainX <- ifelse(predictYTrain4>=mean(predictYTrain4), 5, 2)
mapPredtestX <- ifelse(predictYTest4>=mean(predictYTest4), 5, 2)

print("Case4: RESULTS FOR TRAINING DATA")
print(confusionMatrix(factor(as.matrix(mapPredtrainX)),factor(y_train)))
print("Case4: RESULTS FOR TEST DATA")
print(confusionMatrix(factor(as.matrix(mapPredtestX)),factor(y_test)))

}

driver()