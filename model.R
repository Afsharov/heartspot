library(caTools)
library(catboost)
library(pROC)
library(readr)
library(verification)

data <- as.data.frame(read.csv('datasets/processed.concat.csv', 
                               header = TRUE, 
                               colClasses = c("numeric", "factor", "factor", "numeric", "numeric", "factor", "factor", 
                                              "numeric", "factor", "numeric", "factor", "numeric", "factor", "numeric"), 
                               na = "?"))

set.seed(123)
sample = sample.split(data$class, SplitRatio = .8)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)


train.labels <- train$class
train.features <- subset(train, select = -c(class))
test.labels <- test$class
test.features <- subset(test, select = -c(class))

train_pool <- catboost.load_pool(data = train.features, label = train.labels)
test_pool <- catboost.load_pool(data = test.features, label = test.labels)

model <- catboost.train(train_pool,  test_pool,
                        params = list(loss_function = 'Logloss',
                                      eval_metric = 'AUC',
                                      iterations = 100, metric_period=10))

catboost.save_model(model, "model/heartspot")

predictions <- catboost.predict(model, test_pool, prediction_type = "Probability")
roc.plot(test.labels, predictions, show.thres = FALSE, legend = TRUE, leg.text = "CatBoost Model   AUC =")

importance <- catboost.get_feature_importance(model, pool = test_pool, type = 'FeatureImportance')
print(importance)
