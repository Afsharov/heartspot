library(tree)
library(ISLR)
library(randomForest)
library(e1071)

#load the preprocessed data
data <- read.csv(file="/Users/leeo/Desktop/test/heartspot/pre_pro_dataset/training_data.csv")
test.data <- read.csv(file="/Users/leeo/Desktop/test/heartspot/pre_pro_dataset/testing_data.csv")


data_test = test.data
data_train = data

#decision trees
set.seed(3)
tree.data_train = tree(as.factor(data_train$class)~., data_train)

summary(tree.data_train)
plot(tree.data_train)
text(tree.data_train ,pretty =0)

prediction_dt <- predict(tree.data_train, data_test, type="class")
res_dt <- table(prediction_dt, data_test$class)
res_dt

dt.accuracy <- (res_dt[1,1] + res_dt[2,2]+ res_dt[3,3]+ res_dt[4,4]+ res_dt[5,5])/sum(res_dt)
dt.recall <- res_dt[1,1]/(res_dt[1,1]+res_dt[2,1]+res_dt[3,1]+res_dt[4,1]+res_dt[5,1])
dt.precision <- res_dt[1,1]/(res_dt[1,1]+res_dt[1,2]+res_dt[1,3]+res_dt[1,4]+res_dt[1,5])

dt.accuracy
dt.precision
dt.recall

#random forests
set.seed(3)
rf.data <- randomForest(as.factor(data_train$class)~., data_train, ntree=10)

prediction_rf <- predict(rf.data, data_test, type="class")
res_rf <- table(prediction_rf, data_test$class)
res_rf

rf.accuracy <- (res_rf[1,1] + res_rf[2,2]+ res_rf[3,3]+ res_rf[4,4]+ res_rf[5,5])/sum(res_rf)
rf.recall <- res_rf[1,1]/(res_rf[1,1]+res_rf[2,1]+res_rf[3,1]+res_rf[4,1]+res_rf[5,1])
rf.precision <- res_rf[1,1]/(res_rf[1,1]+res_rf[1,2]+res_rf[1,3]+res_rf[1,4]+res_rf[1,5])

rf.accuracy
rf.precision
rf.recall

#support vector machines
set.seed(3)
svm.data <- svm(as.factor(data_train$class)~., data_train, kernel = "radial")

prediction_svm <- predict(svm.data, data_test, type="class")
res_svm <- table(prediction_svm, data_test$class)
res_svm

svm.accuracy <- (res_svm[1,1] + res_svm[2,2]+ res_svm[3,3]+ res_svm[4,4]+ res_svm[5,5])/sum(res_svm)
svm.recall <- res_svm[1,1]/(res_svm[1,1]+res_svm[2,1]+res_svm[3,1]+res_svm[4,1]+res_svm[5,1])
svm.precision <- res_svm[1,1]/(res_svm[1,1]+res_svm[1,2]+res_svm[1,3]+res_svm[1,4]+res_svm[1,5])

svm.accuracy
svm.precision
svm.recall


###XGBoost
#Label conversion
library(Matrix)
library(xgboost)
species = data$class
label = as.integer(data$class)#convert class to label
#data$class = NULL
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
#num_class = length(as.numeric(levels(species)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=5
)
#Train the model
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit


# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T) 
xgb.pred = as.data.frame(xgb.pred) 
colnames(xgb.pred) = c(0,1,2,3,4)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = test.label

table <- table(xgb.pred$prediction, xgb.pred$label) 
table

##############
##external validation:
testing_data <- read.csv(file="/Users/leeo/Desktop/test/heartspot/pre_pro_dataset/testing_data.csv")
xgb.test.ext = xgb.DMatrix(data=as.matrix(testing_data),label=testing_data[,11])
xgb.pred.ext = predict(xgb.fit,xgb.test.ext,reshape=T) 
xgb.pred.ext = as.data.frame(xgb.pred.ext) 
colnames(xgb.pred.ext) = c(0,1,2,3,4)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred.ext$prediction = apply(xgb.pred.ext,1,function(x) colnames(xgb.pred.ext)[which.max(x)])
xgb.pred.ext$label = testing_data[,11]

table2 <- table(xgb.pred.ext$prediction, xgb.pred.ext$label) 
table2