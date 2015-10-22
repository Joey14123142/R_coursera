library(ElemStatLearn)
library(caret)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# set the seed to 13234
set.seed(13234)

# fit a logistic regression model (method="glm", be sure to specify family="binomial")
# with Coronary Heart Disease (chd) as the outcome
# age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors
logit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family="binomial", data = trainSA)

# Calculate the misclassification rate for your model using this function
# and a prediction on the "response" scale
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
trained <- predict(logit, trainSA)
missClass(trainSA$chd, trained)
tested <- predict(logit, testSA)
missClass(testSA$chd, tested)



