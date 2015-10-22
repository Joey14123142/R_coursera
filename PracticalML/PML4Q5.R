set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
library(forecast)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 325
set.seed(325)
# fit a support vector machine using the e1071 package
# to predict Compressive Strength using the default settings. 
fit <- svm(CompressiveStrength ~., training)
# Predict on the testing set.
pred <- predict(fit, testing)
# What is the RMSE?
RMSE(pred, testing$CompressiveStrength)

