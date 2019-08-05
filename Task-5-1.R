################################################
##Question - 1
##
###############################################
rm(list = ls())

library(randomForest)

library(kernlab)

data("spam")

summary(spam)



# divide into test and training
set.seed(12)
test_indis <- sample(1:nrow(spam), 0.2*nrow(spam))
test <- spam[test_indis,]
training <- spam[-test_indis,]

y_true <- as.numeric(test$type)-1

#################################################
#Random Forests
#################################################

miss_class_rf <- c()

par(mfrow=c(2,3))

for (i in c(1,5,10,15,20,25)) {
 
fit <- randomForest(type~.,data=spam,n.tree=500,mtry=i)

y_pred <- predict(fit,newdata=test,type="response")

y_pred <- as.numeric(y_pred)-1

miss_class_rf <- c(miss_class_rf, sum(abs(y_true-y_pred))/length(y_true))

plot(fit$err.rate[,1],type = "l")

}

par(mfrow=c(1,1))

which.min(miss_class_rf)

plot(miss_class_rf)


