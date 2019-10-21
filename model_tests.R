library(tree)
library(ISLR)
library(randomForest)
library(e1071)


training_data <- read.csv(file="datasets/training_data.csv")
test_data <- read.csv(file="datasets/testing_data.csv")

#decision trees
set.seed(3)
tree.data_train = tree(as.factor(training_data$class)~., training_data)

summary(tree.data_train)
plot(tree.data_train)
text(tree.data_train ,pretty =0)

prediction_dt <- predict(tree.data_train, test_data, type="class")
res_dt <- table(prediction_dt, test_data$class)
res_dt

dt.accuracy <- (res_dt[1,1] + res_dt[2,2])/sum(res_dt)
dt.precision <- res_dt[1,1]/(res_dt[1,1]+res_dt[2,1])
dt.recall <- res_dt[1,1]/(res_dt[1,1]+res_dt[1,2])

dt.accuracy
dt.precision
dt.recall

#random forests
set.seed(3)
rf.data <- randomForest(as.factor(training_data$class)~., training_data, ntree=10)

prediction_rf <- predict(rf.data, test_data, type="class")
res_rf <- table(prediction_rf, test_data$class)
res_rf

rf.accuracy <- (res_rf[1,1] + res_rf[2,2])/sum(res_rf)
rf.precision <- res_rf[1,1]/(res_rf[1,1]+res_rf[2,1])
rf.recall <- res_rf[1,1]/(res_rf[1,1]+res_rf[1,2])

rf.accuracy
rf.precision
rf.recall

#support vector machines
set.seed(3)
svm.data <- svm(as.factor(training_data$class)~., training_data, kernel = "polynomial")

prediction_svm <- predict(svm.data, test_data, type="class")
res_svm <- table(prediction_svm, test_data$class)
res_svm

accuracy <- (res_svm[1,1] + res_svm[2,2])/sum(res_svm)
precision <- res_svm[1,1]/(res_svm[1,1]+res_svm[2,1])
recall <- res_svm[1,1]/(res_svm[1,1]+res_svm[1,2])

accuracy
precision
recall
