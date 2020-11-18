library(ranger)
library(caret)
library(data.table)

creditcard_data <- read.csv("[Path]")

dim(creditcard_data)
head(creditcard_data, 5)
tail(creditcard_data, 5)

table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)
sd(creditcard_data$Amount)

creditcard_data$Amount = scale(creditcard_data$Amount)
scaledData = creditcard_data[, -c(1)]
head(scaledData)

library(caTools)
set.seed(123)
data_sample = sample.split(scaledData$Class, SplitRatio =  0.80)
train_data = subset(scaledData, data_sample == TRUE)
test_data = subset(scaledData, data_sample == FALSE)
dim(train_data)
dim(test_data)

logisticModel = glm(Class~ .,test_data, family = binomial())
summary(logisticModel)
#plot(logisticModel)

library(pROC)
lr.predict <- predict(Logistic_Model,test_data, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")

# Decision Tree

library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')
rpart.plot(decisionTree_model)

# Artificial Neural Networks

library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)
