library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Set the seed to 62433
set.seed(62433)

# predict diagnosis with all the other variables 
# using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model
rf <- train(diagnosis ~., method = "rf", data = training)
pred_rf <- predict(rf, newdata = testing)
c_rf <- confusionMatrix(pred_rf, testing$diagnosis) # 0.7805
gbm <- train(diagnosis ~., method = "gbm", data = training)
pred_gbm <- predict(gbm, newdata = testing)
c_gbm <- confusionMatrix(pred_gbm, testing$diagnosis) # 0.8049
lda <- train(diagnosis ~., method = "lda", data = training)
pred_lda <- predict(lda, newdata = testing)
c_lda <- confusionMatrix(pred_lda, testing$diagnosis) # 0.7683

# Stack the predictions together using random forests ("rf")
all <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
rfall <- train(diagnosis ~., method = "rf", data = all)
pred_rfall <- predict(rfall, newdata = testing)
c_rfall <- confusionMatrix(pred_rfall, testing$diagnosis) # 0.8171

print(paste(c_rf$overall[1], c_gbm$overall[1], c_lda$overall[1], c_rfall$overall[1]))
