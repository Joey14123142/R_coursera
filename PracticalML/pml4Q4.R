library(lubridate)  # For year() function below
library(forecast)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package 
# to the training time series. 
fit <- bats(tstrain)

# Then forecast this model for the remaining time points.
# For how many of the testing points is the true value within the 95% prediction interval bounds?
fore <- forecast.bats(fit, nrow(testing), level = 95)
acc <- 0
for (i in 1:nrow(testing)){
  if (testing$visitsTumblr[i] < fore$upper[i] & testing$visitsTumblr[i] > fore$lower[i]){
    acc <- acc + 1
  }
}
result <- acc/nrow(testing) # 0.9617021
