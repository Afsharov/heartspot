library(tree)
library(ISLR)
library(randomForest)
library(e1071)

training_data <- read.csv(file="datasets/training_data.csv")
test_data <- read.csv(file="datasets/testing_data.csv")

calculate_performance_table <- function(conmatrix, classnum) {
  # transpose matrix
  conmatrix <- t(conmatrix)
  
  # performance table
  performance <- matrix( rep( 0, len=(6*(classnum+1))), nrow = (classnum+1))
  colnames(performance) <- c("Precision", "Recall", "F1-Score", "TP", "FP", "FN")
  rownames(performance) <- c("Class0", "Class1", "Class2", "Class3", "Class4", "Average")
  
  accuracy <- (conmatrix[1,1] + conmatrix[2,2] + conmatrix[3,3] + conmatrix[4,4] + conmatrix[5,5])/sum(conmatrix)
  # calculate all metrics of all classes and fill the table
  for (i in 1:classnum) {
    # precison
    performance[i,1] <- conmatrix[i,i]/sum(conmatrix[,i])
    # recall
    performance[i,2] <- conmatrix[i,i]/sum(conmatrix[i,])
    # f1-score
    performance[i,3] <- 2 * ((performance[i,1] * performance[i,2])/(performance[i,1] + performance[i,2]))
    # true positives
    performance[i,4] <- conmatrix[i,i]
    # false positives
    performance[i,5] <- sum(conmatrix[,i]) - conmatrix[i,i]
    # false negatives
    performance[i,6] <- sum(conmatrix[i,]) - conmatrix[i,i]
  }
  
  # compute average of all metrics
  for (i in 1:(classnum)+1) {
    performance[(classnum+1), i] <- ((performance[1,i] + performance[2,i] + performance[3,i] + performance[4,i] + performance[5,i]) / classnum)
  }
  
  performance <- as.table(performance)
  return(performance)
}

########## decision trees ##########
set.seed(3)
tree.data_train = tree(as.factor(training_data$class)~., training_data)

prediction_dt <- predict(tree.data_train, test_data, type="class")
res_dt <- table(prediction_dt, test_data$class)

tree.performance <- calculate_performance_table(res_dt, 5)
tree.performance

########## random forests ##########
set.seed(3)
rf.data <- randomForest(as.factor(training_data$class)~., training_data, ntree=10)

prediction_rf <- predict(rf.data, test_data, type="class")
res_rf <- table(prediction_rf, test_data$class)

rf.performance <- calculate_performance_table(res_rf, 5)
rf.performance

########## support vector machines ##########
set.seed(3)
svm.data <- svm(as.factor(training_data$class)~., training_data, kernel = "polynomial")

prediction_svm <- predict(svm.data, test_data, type="class")
res_svm <- table(prediction_svm, test_data$class)

svm.performance <- calculate_performance_table(res_svm, 5)
svm.performance
