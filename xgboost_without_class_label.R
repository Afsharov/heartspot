library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(Matrix)
library(xgboost)

data <- read.csv(file="datasets/training_data.csv")
testing_data <- read.csv(file="datasets/testing_data.csv")

class = factor(data$class)
label = as.integer(data$class) #convert class to label
data$class = NULL
#Split the data for training and testing (75/25 split)
set.seed(3)
n = nrow(data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(data[train.index,])
train.label = label[train.index]
test.data = as.matrix(data[-train.index,])
test.label = label[-train.index]
#Create the xgb.DMatrix objects
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)
#Define the main parameters
# Define the parameters for multinomial classification
num_class = length(levels(class))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)
#Train the model
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train, val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit


# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T) 
xgb.pred = as.data.frame(xgb.pred) 
colnames(xgb.pred) = levels(class)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = test.label
                               
table <- table(xgb.pred$prediction, xgb.pred$label) 
table

##############
##external validation:
test.ext.label = as.integer(testing_data$class)
testing_data$class = NULL

xgb.test.ext = xgb.DMatrix(data=as.matrix(testing_data),label=test.ext.label)
xgb.pred.ext = predict(xgb.fit,xgb.test.ext,reshape=T) 
xgb.pred.ext = as.data.frame(xgb.pred.ext) 
colnames(xgb.pred.ext) = levels(class)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred.ext$prediction = apply(xgb.pred.ext,1,function(x) colnames(xgb.pred.ext)[which.max(x)])
xgb.pred.ext$label = test.ext.label

conmatrix <- table(xgb.pred.ext$prediction, xgb.pred.ext$label) 
conmatrix

#calculating performance
calculate_performance_table <- function(conmatrix, class) {
  # transpose matrix
  conmatrix <- t(conmatrix)
  
  # performance table
  performance <- matrix( rep( 0, len=(6*(class+1))), nrow = (class+1))
  colnames(performance) <- c("Precision", "Recall", "F1-Score", "TP", "FP", "FN")
  rownames(performance) <- c("Class0", "Class1", "Class2", "Class3", "Class4", "Average")
  
  accuracy <- (conmatrix[1,1] + conmatrix[2,2] + conmatrix[3,3] + conmatrix[4,4] + conmatrix[5,5])/sum(conmatrix)
  # calculate all metrics of all classes and fill the table
  for (i in 1:class) {
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
  for (i in 1:(class)+1) {
    performance[(class+1), i] <- ((performance[1,i] + performance[2,i] + performance[3,i] + performance[4,i] + performance[5,i]) / class)
  }
  
  performance <- as.table(performance)
  return(performance)
}
model.performance <- calculate_performance_table(table2, 5)
model.performance
accuracy
