library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# Set the variable y to be a factor variable in both the training and test set. 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

# Then set the seed to 33833. 
set.seed(33833)

# Fit (1) a random forest predictor relating the factor variable y to the remaining variables
rf <- train(y ~., method = "rf", data = vowel.train)
pred_rf <- predict(rf, newdata = vowel.test)
c_rf <- confusionMatrix(pred_rf, vowel.test$y) # 0.6061

# (2) a boosted predictor using the "gbm" method. 
gbm <- train(y ~., method = "gbm", data = vowel.train)
pred_gbm <- predict(gbm, newdata = vowel.test)
c_gbm <- confusionMatrix(pred_gbm, vowel.test$y) # 0.5108

# Fit these both with the train() command in the caret package.
both <- data.frame(pred_rf, pred_gbm, y = vowel.test$y)
model_both <- train(y ~., both)
pred_both <- predict(model_both, vowel.test)
c_both <- confusionMatrix(pred_both, vowel.test$y) # 0.6818
