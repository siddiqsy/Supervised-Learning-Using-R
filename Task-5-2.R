################################################
##Question - 2
##
###############################################
rm(list = ls())

install.packages("neuralnet")

library(neuralnet)
library(ISLR)
library(leaps)
library(bootstrap)
library(boot)
library(kernlab)


data("spam")

summary(spam)

spam$type <- as.numeric(spam$type)-1

set.seed(1)
train = sample(1:nrow(spam), .80*nrow(spam))
Y.train = spam$type[train]
Y.test = spam$type[-train]

training = spam[train, 1:58]
testing = spam[-train, 1:58]

?regsubsets

fit <- regsubsets(type~., data = training, method = 
                    "backward", nvmax = 50,really.big=T)
my_summary <- summary(fit)

which.min(my_summary$cp) #Cp says 43 variables is best from forward and 43 from backward
which.min(my_summary$bic) #BIC says 32 variables is best from forward and 30 from backward

# Do the selection based on the "hold out method"
select = summary(fit)$outmat
train.error.store <- c()
test.error.store <- c()
for (i in 1:45){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- training[, c(1,temp)]
  red.testing <- testing[,c(1,temp)]
  
  red.fit <- lm(training$type~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training)
  pred.test = predict(red.fit, newdata = red.testing)
  
  test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
  train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
  
  train.error.store <- c(train.error.store, train.error)
  test.error.store <- c(test.error.store, test.error)
  
}

### Plot the results
upper = max(train.error.store, test.error.store)
lower = min(train.error.store, test.error.store)

plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))


neunet <- neuralnet(type~our + over + remove + internet  + free + you + your + num000 + money + hp + re + charExclamation
                    + business + charDollar + capitalLong + capitalTotal, data = spam, hidden = c(2,2), err.fct = 'sse', linear.output = FALSE,stepmax = 100000)

neunet$result.matrix

true_class = spam$type
pred_class = round(neunet$net.result[[1]])
error = sum(abs(true_class - pred_class))/length(pred_class)

plot(neunet)

#########################################
# Logistic Regression
#########################################
glm.fit <- glm(type ~., data = spam, family = "binomial")
summary(glm.fit)
names(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = training, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = testing, type = "response")
y_hat_test <- round(glm.probs.test)

#########################################
#  Calculate the error rates
########################################
train_err <- sum(abs(y_hat_train- Y.train))/length(Y.train)
test_err <- sum(abs(y_hat_test- Y.test))/length(Y.test)

train_err
test_err


