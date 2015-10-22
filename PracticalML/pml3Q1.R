library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
# Subset the data to a training set and testing set based on the Case variable in the data set. 
dt <- segmentationOriginal
train <- dt[dt$Case=="Train",]
test <- dt[dt$Case=="Test",]

# Set the seed to 125
set.seed(125)

# fit a CART model with the rpart method using all predictor variables and default caret settings
CARTmodel <- train(Class ~., method = "rpart", data = train)

# final model prediction for cases with the following variable values
CARTmodel$finalModel
plot(CARTmodel$finalModel, uniform = T)
text(CARTmodel$finalModel, cex = 1)
