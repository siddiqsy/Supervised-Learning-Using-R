rm(list = ls())

install.packages("ElemStatLearn")
install.packages("bootstrap")
install.packages("psych")

library(ElemStatLearn)

library(glmnet)

library(MASS)
library(leaps)
library(DAAG)
library(bootstrap)
library(boot)  
library(caret)
library(psych)

data(prostate)

head(prostate)

?prostate

prostate$train <- as.numeric(prostate$train)

par(mfrow = c(1,1))

############################################
#Best Subset indicator for prostate dataset
############################################

regfit.full <- regsubsets(train~., data = prostate, nbest = 2, nvmax = 9, method = "exhaustive")
prostate_sum <- summary(regfit.full)

prostate_sum

lm_fit <- lm(train~.,data=prostate)

names(lm_fit)

summary(lm_fit)

#########################################
# Dividing into training and test 
# And testing linear regression models 
# for different number of primary components
#########################################

set.seed(1)
train = sample(1:nrow(prostate), .80*nrow(prostate))
Y.train = prostate$train[train]
Y.test = prostate$train[-train]
X.train = prostate[train, 1:9]
X.test = prostate[-train, 1:9]

fit <- lm(train ~ age + gleason + pgg45+lcp, data = prostate[train,])
pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error_lm <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
train.error_lm <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
test.error_lm
train.error_lm

#############################################
# Best subset selection for the data  and 
# CV for 5 and 10 folds
#############################################

prostate$train <- as.numeric(prostate$train)

train = sample(1:nrow(prostate), .80*nrow(prostate))
Y.train = prostate$train[train]
Y.test = prostate$train[-train]

training = prostate[train, 1:10]
testing = prostate[-train, 1:10]

fit <- regsubsets(train~., data = training, method = 
                    "exhaustive", nvmax = 10)
my_summary <- summary(fit)
names(my_summary)
my_summary$cp
my_summary$bic

which.min(my_summary$cp) #Cp says 2 variables is best
which.min(my_summary$bic) #BIC says 1 variables is best

# Do the selection based on the "hold out method"
select = summary(fit)$outmat

train.error.store <- c()
test.error.store <- c()
for (i in 1:9){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- training[, c(10,temp)]
  red.testing <- testing[,c(10,temp)]
  
  data_ctrl <- trainControl(method = "cv", number = 5)
  red.fit <- train(train~.,data = red.training,trControl = data_ctrl,method = "lm",na.action = na.pass)
  
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

x11()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection for 5 fold CV")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))


train.error.store <- c()
test.error.store <- c()
for (i in 1:9){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- training[, c(10,temp)]
  red.testing <- testing[,c(10,temp)]
  
  data_ctrl <- trainControl(method = "cv", number = 10)
  red.fit <- train(train~.,data = red.training,trControl = data_ctrl,method = "lm",na.action = na.pass)
  
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

quartz()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection for 10 fold CV")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

#############################
## Calculate bootstrap predition error 
## for the best models of size "k"
###########################################
?boot #more generic function
?bootpred

# create functions that feed into "bootpred"
beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create X and Y
X <- prostate[,1:9]
Y <- prostate[,10]

# Practice, WLOG lets look at a single model
select = summary(fit)$outmat
temp <- which(select[7,] == "*") 

res1 <- bootpred(X[,temp], Y, nboot = 500, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 

?bootpred

# Generalize it, and search over the best possible subsets of size "k"
error_store <- c()
for (i in 1:9){
  # Pull out the model
  temp <- which(select[i,] == "*")
  
  res <- bootpred(X[,temp], Y, nboot = 500, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  error_store <- c(error_store, res[[3]])
  
}

quartz()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
lines(error_store, type = "o", lty = 3, col = "green")
legend("topright", c("training", "test", "bootstrap .632"), lty = c(2,1), col = c("blue", "red", "green"))



