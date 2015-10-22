set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 233
set.seed(233)

# fit a lasso model to predict Compressive Strength.
las <- train(CompressiveStrength ~., method = "lasso", data = training)

# Which variable is the last coefficient to be set to zero as the penalty increases? 
# (Hint: it may be useful to look up ?plot.enet).
plot.enet(las$finalModel, xvar = "penalty", use.color = T)
