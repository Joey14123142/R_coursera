library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set.
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

# Then set the seed to 33833.
set.seed(33833)

#Fit a random forest predictor relating the factor variable y to the remaining variables. 
# The caret package uses by defualt the Gini importance. 
rf <- train(y ~., method = "rf",data = vowel.train)

# Calculate the variable importance using the varImp function in the caret package
imp <- varImp(rf)