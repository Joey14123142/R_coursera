library(rpart)
load("olive")
train <- olive[,-1]
newdata <- as.data.frame(t(colMeans(train)))
tree <- rpart(Area ~., data = train)
prediction <- predict(tree, newdata = newdata) # 2.875
