library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)

# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
IL_pred <- predictors[,grep('^IL', colnames(predictors))]
adData = data.frame(diagnosis,IL_pred)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
train = adData[ inTrain,]
test = adData[-inTrain,]

# Build two predictive models
# Use method="glm" in the train function
# one using the predictors as they are
modelFit1 <- train(diagnosis ~., method = "glm", data = train)
predictions1 <- predict(modelFit1, newdata = test)
confu1 <- confusionMatrix(predictions, test$diagnosis) # non-PCA Accuracy : 0.6463

# one using PCA with principal components explaining 80% of the variance in the predictors. 
modelFit2 <- train(diagnosis ~., method = "glm", 
                   preProcess="pca", trControl = trainControl(preProcOptions = list(thresh = 0.8)),
                   data = train)
predictions2 <- predict(modelFit2, newdata = test)
confu2 <- confusionMatrix(predictions2, test$diagnosis) # PCA accuracy : 0.7195
