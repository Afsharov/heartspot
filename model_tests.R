library(tree)
library(ISLR)
library(randomForest)
library(e1071)


data <- read.csv(file="/Users/abhinavsharma/Desktop/datasets/training_data.csv")
test.data <- read.csv(file="/Users/abhinavsharma/Desktop/datasets/testing_data.csv")

set.seed(3)
train = sample(nrow(data), nrow(data)*2/3)
test =- train
data_test = data[test,]
data_train = data[train,]

#decision trees
set.seed(3)
tree.data_train = tree(as.factor(data_train$class)~., data_train)

summary(tree.data_train)
plot(tree.data_train)
text(tree.data_train ,pretty =0)

prediction_dt <- predict(tree.data_train, data_test, type="class")
res_dt <- table(prediction_dt, data_test$class)
res_dt

dt.accuracy <- (res_dt[1,1] + res_dt[2,2])/sum(res_dt)
dt.precision <- res_dt[1,1]/(res_dt[1,1]+res_dt[2,1])
dt.recall <- res_dt[1,1]/(res_dt[1,1]+res_dt[1,2])

dt.accuracy
dt.precision
dt.recall

#random forests
set.seed(3)
rf.data <- randomForest(as.factor(data_train$class)~., data_train, ntree=10)

prediction_rf <- predict(rf.data, data_test, type="class")
res_rf <- table(prediction_rf, data_test$class)
res_rf

rf.accuracy <- (res_rf[1,1] + res_rf[2,2])/sum(res_rf)
rf.precision <- res_rf[1,1]/(res_rf[1,1]+res_rf[2,1])
rf.recall <- res_rf[1,1]/(res_rf[1,1]+res_rf[1,2])

rf.accuracy
rf.precision
rf.recall

#support vector machines
set.seed(3)
svm.data <- svm(as.factor(data_train$class)~., data_train, kernel = "polynomial")

prediction_svm <- predict(svm.data, data_test, type="class")
res_svm <- table(prediction_svm, data_test$class)
res_svm

accuracy <- (res_svm[1,1] + res_svm[2,2])/sum(res_svm)
precision <- res_svm[1,1]/(res_svm[1,1]+res_svm[2,1])
recall <- res_svm[1,1]/(res_svm[1,1]+res_svm[1,2])

accuracy
precision
recall
