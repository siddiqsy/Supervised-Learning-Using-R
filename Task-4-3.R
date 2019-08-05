rm(list = ls())

install.packages("gbm")

install.packages("randomForest")

library(rpart)

library(gbm)

library(randomForest)

library(geneplotter)

library(ISLR)

library(caret)
library(e1071)

data("Carseats")

summary(train)

?Carseats

?lm
?pcm

Carseats$Sales

high <- ifelse(Carseats$Sales<=7, "L","H")

high

head(high)

my_data <- data.frame(Carseats[,-1],high)


# divide into test and training
set.seed(12)
test_indis <- sample(1:nrow(my_data), 1/3*nrow(my_data))
test <- my_data[test_indis,]
training <- my_data[-test_indis,]


y_true <- as.numeric(test$high)-1 #L=1,H=0

y_true

##################################################
#Growing a single tree
##################################################

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)

fit <- rpart(reordered~.,data=train,method= "class",control= model.control)

summary(fit)

plot(fit, branch = .4, uniform = T, compress = T, main = "Main Tree")
text(fit, use.n = T, all = T, cex = 0.5)

################################################
#prune the tree back
################################################

plot(fit$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit$cptable[,4])
pruned_fit <- prune(fit, cp = fit$cptable[min_cp,1])

## plot the full tree and the pruned tree
plot(pruned_fit, branch = .4, uniform = T, compress = T, main = "Pruned Tree")
text(pruned_fit, use.n = T, all = T, cex = 0.5)

plot(fit, branch = .3, compress=T, main = "Full Tree")
text(fit, cex = 1)

################################################
#Computing test error for single tree
#################################################

my_pred <- predict(pruned_fit,newdata = test,type="class")

Y_pred <- as.numeric(my_pred)-1

miss_class_tree <- sum(abs(y_true-Y_pred))/length(y_true)

miss_class_tree

#################################################
#Random Forests
#################################################

fit <- randomForest(reordered~.,data=subtrain,n.tree=1000)

names(fit)
varImpPlot(fit)
importance(fit)

y_pred <- predict(fit,newdata=test,type="response")

y_pred <- as.numeric(y_pred)-1

miss_class_rf <- sum(abs(y_true-y_pred))/length(y_true)

miss_class_rf

#################################################
#Bagging
#################################################

fit <- randomForest(high~.,data=training,n.tree=1000,mtry=10)

names(fit)
varImpPlot(fit)
importance(fit)

y_pred <- predict(fit,newdata=test,type="response")

y_pred <- as.numeric(y_pred)-1

miss_class_bagging <- sum(abs(y_true-y_pred))/length(y_true)

miss_class_bagging

#################################################
#Boosting
#################################################

boost.train <- training

boost.train$high <- as.numeric(boost.train$high)-1

boost.test <- test

boost.test$high <- as.numeric(boost.test$high)-1

fit_1 <- gbm(high~.,data=boost.train,n.tree=100,shrinkage=0.1,interaction.depth = 3,distribution = "adaboost")

fit_6 <- gbm(high~.,data=boost.train,n.tree=100,shrinkage=0.6,interaction.depth = 3,distribution = "adaboost")

summary(fit_1)

summary(fit_6)

# Shrinkage of 0.1

y_pred <- predict(fit_1,newdata=boost.test,n.tree=100,type="response")

y_pred <- round(as.numeric(y_pred))

miss_class_bagging_1 <- sum(abs(y_true-y_pred))/length(y_true)

miss_class_bagging_1

# Shrinkage of 0.6

y_pred <- predict(fit_6,newdata=boost.test,n.tree=100,type="response")

y_pred <- round(as.numeric(y_pred))

miss_class_bagging_6 <- sum(abs(y_true-y_pred))/length(y_true)

miss_class_bagging_6

#########################################
# Logistic Regression
#########################################
glm.fit <- glm(high ~., data = training, family = "binomial")
summary(glm.fit)
names(glm.fit)

varImp(glm.fit)

# Predict
glm.probs.train <- predict(glm.fit, newdata = training, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_hat_test <- round(glm.probs.test)

log_test_err <- sum(abs(y_hat_test- y_true))/length(y_true)

log_test_err

#####################################
#KNN for classification function
#
####################################

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(high~. , data = training, method = "knn",
                preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)

valid_pred <- predict(model1,newdata=test, type = "raw")

valid_pred <- as.numeric(valid_pred)-1



# Computing the test error
knn_test_error <- sum(abs(y_true - valid_pred))/length(y_true)

knn_test_error
