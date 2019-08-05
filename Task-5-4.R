################################################
##Question - 3
##
###############################################
rm(list = ls())

library(ISLR)

library(e1071)

data("OJ")

summary(OJ)

# Divide into test and training
set.seed(123)
test_indis <- sample(1:nrow(OJ), 1/3*nrow(OJ))
test <- OJ[test_indis, ]
training <- OJ[-test_indis, ]

# SVM with a linear kernel for training
tune.model <- tune(svm, Purchase~., data = training, kernel = "linear",
                   ranges = list(cost = c(0.01:10)))
tune.model
summary(tune.model)

X11()
plot(tune.model)

bestmod <- tune.model$best.model
bestmod

# SVM with a linear kernel for testing
y_hat <- predict(bestmod, newdata = test)
y_true <- test$Purchase
accur_rad <- length(which(y_hat == y_true))/length(y_true)
accur_rad #0.7921

table(predict = y_hat, truth = y_true)

####################################
# SVM with a radial kernel for training
####################################

?tune

tune.model.rad <- tune(svm, Purchase~., data = training, kernel = "radial",
                       ranges = list(cost = c(.01:10), gamma = c(.5)))

summary(tune.model.rad)
plot(tune.model.rad)
tune.model.rad$best.model
y_hat <- predict(bestmod, newdata = test)
y_true <- test$Purchase
accur_rad <- length(which(y_hat == y_true))/length(y_true)
accur_rad #0.7921

table(predict = y_hat, truth = y_true)


####################################
# SVM with a polynomial kernel for training
####################################

?tune

tune.model.rad <- tune(svm, Purchase~., data = training, kernel = "polynomial",
                       ranges = list(cost = c(.01:10), gamma = c(.5),degree=2))

summary(tune.model.rad)
plot(tune.model.rad)
tune.model.rad$best.model
y_hat <- predict(bestmod, newdata = test)
y_true <- test$Purchase
accur_rad <- length(which(y_hat == y_true))/length(y_true)
accur_rad #0.7921

table(predict = y_hat, truth = y_true)
