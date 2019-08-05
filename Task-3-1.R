rm(list = ls())

install.packages("caret")
install.packages("e1071")
install.packages("MASS")
install.packages("klaR")

library(caret) #install.packages("caret")
library(e1071)
library(caTools)
library(ggplot2)
library(ISLR)
library(klaR) 
library(MASS)
library(leaps)

cor(boston[,])

summary(boston$RAD)

par(mfrow=c(1,1))

boxplot(boston$CRIM , xlab="CRIM" , col=rgb(0.4,0.2,0.3,0.5) , las=2)

hist(boston$CRIM , breaks=10 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="distribution of CRIM" , main="")

my_boston <- boston[,]

head(my_boston)

summary(my_boston)

my_boston_median <- median(my_boston$CRIM)

for (i in 1:nrow(my_boston)){
  if(my_boston[i,"CRIM"]>my_boston_median){
    my_boston[i,"CRIM"] <- 1
  }else{
    my_boston[i,"CRIM"] <- 0
  }
}

#########################################
# Create a training and test set
#########################################
set.seed(1)
train <- sample(1:nrow(my_boston), .80*nrow(my_boston))
boston_train <- my_boston[train,]
boston_test <- my_boston[-train,]

y_true_train <- boston_train$CRIM
y_true_test <- boston_test$CRIM

#########################################
# Logistic Regression
#########################################
glm.fit <- glm(CRIM ~., data = boston_train, family = "binomial")
summary(glm.fit)
names(glm.fit)

varImp(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = boston_train, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = boston_test, type = "response")
y_hat_test <- round(glm.probs.test)

log_train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
log_test_err <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)

log_train_err
log_test_err

#########################################
#  Confusion Matrix
########################################
conf <- confusionMatrix(factor(y_hat_test), factor(y_true_test))
names(conf)
conf$table

##################################
#Cross valivation of the dataset 
#
###################################

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

glm.cv.fit <- train(CRIM~.,  data=my_boston, method="glm", family="binomial", trControl = ctrl, tuneLength = 5)

summary(glm.cv.fit)
varImp(glm.cv.fit)

glm.probs.train <- predict(glm.cv.fit, newdata = boston_train)
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.cv.fit, newdata = boston_test)
y_hat_test <- round(glm.probs.test)

log_train_err_cv <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
log_test_err_cv <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)

log_train_err_cv
log_test_err_cv

##########################################
#Model Selection using Hold out method
#
##########################################
fit <- regsubsets(CRIM~., data = boston_train, method = 
                    "exhaustive", nvmax = 13)
my_summary <- summary(fit)
names(my_summary)

which.min(my_summary$cp) #Cp says 4 variables is best
which.min(my_summary$bic) #BIC says 4 variables is best

# Do the selection based on the "hold out method"
select = summary(fit)$outmat
train.error.store <- c()
test.error.store <- c()
for (i in 1:13){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- boston_train[, c(1,temp)]
  red.testing <- boston_test[,c(1,temp)]
  
  red.fit <- lm(CRIM~., data = red.training)
  
  y_hat_train = predict(red.fit, newdata = red.training)
  y_hat_test = predict(red.fit, newdata = red.testing)
  
  train.error <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
  test.error <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)
  
  train.error.store <- c(train.error.store, train.error)
  test.error.store <- c(test.error.store, test.error)
  
}

### Plot the results
upper = max(train.error.store, test.error.store)
lower = min(train.error.store, test.error.store)

quartz()
plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -0.1, upper +0.1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

############################################
# Logistic Regression after model Selection
# 
############################################
glm.fit.model <- glm(CRIM ~ NOX + RAD + DIS + PT + MV, data = boston_train, family = "binomial")
summary(glm.fit.model)
names(glm.fit.model)

varImp(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit.model, newdata = boston_train, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit.model, newdata = boston_test, type = "response")
y_hat_test <- round(glm.probs.test)

log_train_err_model <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
log_test_err_model <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)

log_train_err_model
log_test_err_model

####################################
## Linear Discriminant Analysis (LDA)
##
####################################
lda.fit <- lda(CRIM~., data = my_boston)
names(lda.fit)
summary(lda.fit)

lda.pred.train <- predict(lda.fit, newdata = boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error


#####################################
#KNN for classification function
#
####################################
require(class)
knnplot <- function(train, test, k){
  KNN <- knn(train[, c('X1', 'X2')], test, train$Y, k)
  test$predict <- KNN
  
  # change factor to numeric
  test$z <- c(0, 1)[sapply(test$predict, as.numeric)]
  
  title = paste("k=", as.character(k), sep ="")
  
  g <- ggplot(data = test, aes(X1,X2)) + geom_point(aes(colour = predict), size = 0.5) + geom_contour(aes(z=z), colour = 'black', size = 0.1) + theme(legend.position = "none") + labs(title = title)
  
  #add the training points in
  g <- g + geom_point(data = train, aes(X1,X2,colour = as.factor(Y), shape = 'x'))
  
  return(g)
  
}

knn.fit_test <- knn(train=boston_train, test=boston_test, cl= boston_train$CRIM, k=9)

knn.fit_test <- as.numeric(knn.fit_test)- 1

knn.fit_train <- knn(train=boston_train, test=boston_train, cl= boston_train$CRIM, k=9)

knn.fit_train <- as.numeric(knn.fit_train)- 1

# Computing the test error
knn_test_error <- sum(abs(y_true_test - knn.fit_test))/length(y_true_test)
knn_train_error <- sum(abs(y_true_train - knn.fit_train))/length(y_true_train)



log_train_err
log_test_err
log_train_err_cv
log_test_err_cv
log_train_err_model
log_test_err_model
lda_train_error
lda_test_error
knn_train_error
knn_test_error





