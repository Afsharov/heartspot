library(tree)
library(ISLR)
library(randomForest)
library(e1071)


data <- read.csv(file="datasets/training_data.csv")
# test.data <- read.csv(file="datasets/testing_data.csv")

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

#random forests
set.seed(3)
rf.data <- randomForest(as.factor(data_train$class)~., data_train, ntree=3)

prediction_rf <- predict(rf.data, data_test, type="class")
res_rf <- table(prediction_rf, data_test$class)
res_rf

#support vector machines
set.seed(3)
svm.data <- svm(as.factor(data_train$class)~., data_train, kernel = "linear")

prediction_svm <- predict(svm.data, data_test, type="class")
res_svm <- table(prediction_svm, data_test$class)
res_svm
