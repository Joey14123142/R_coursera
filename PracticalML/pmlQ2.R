library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
set.seed(9)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
train <- adData[inTrain,]
test <- adData[-inTrain,]

# find predictor variables begin with IL
IL_train <- train[,grep('^IL', colnames(train))]
# calculate number of components to capture 80% of the variance
preProc <- preProcess(IL_train, method = "pca", thresh = 0.8)
