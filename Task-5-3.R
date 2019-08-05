################################################
##Question - 3
##
###############################################
rm(list = ls())
library(neuralnet)
ls("package:neuralnet")
## nnet

data(infert)

summary(infert)

case <- infert$case

infert <- infert[ , !(names(infert) %in% "case")]

infert$case <- case

?regsubsets

set.seed(1)
train = sample(1:nrow(infert), .80*nrow(infert))
Y.train = infert$case[train]
Y.test = infert$case[-train]

training = infert[train, 1:8]
testing = infert[-train, 1:8]

fit <- regsubsets(case~., data = infert, method = 
                    "exhaustive", nvmax = 50,really.big=T)
my_summary <- summary(fit)

which.min(my_summary$cp) #Cp says 5 variable
which.min(my_summary$bic) #BIC says 3 variable 

# Do the selection based on the "hold out method"
select = summary(fit)$outmat
train.error.store <- c()
test.error.store <- c()
for (i in 1:8){
  temp <- which(select[i,] == "*")
  temp <- temp
  
  red.training <- training[, c(8,temp)]
  red.testing <- testing[,c(8,temp)]
  
  red.fit <- lm(training$case~., data = red.training)
  
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


?neuralnet
nn <- neuralnet(case ~ parity + induced + spontaneous + pooled.stratum, data = infert, hidden = c(2,2), 
                stepmax=1000000, threshold= 0.01, err.fct = 'sse', linear.output = FALSE)

names(nn)
nn$result.matrix

true_class = infert$case
pred_class = round(nn$net.result[[1]])
error = sum(abs(true_class - pred_class))/length(pred_class)

plot(nn)

######################################################################
#Adding a out lier to one of the data set row. 
#
######################################################################

infert$pooled.stratum[200] <- 70 #10 - 26.95,30 - 20.63,50 - 20.63,70 - 18.02,90 - 0.2415 

summary(infert)
nn_out <- neuralnet(case ~ age + parity + induced + spontaneous + stratum + pooled.stratum, data = infert, hidden = c(2,2), 
                    stepmax=10000000, threshold= 0.01, err.fct = 'sse', linear.output = FALSE)

names(nn_out)
nn_out$result.matrix

true_class = infert$case
pred_class = round(nn_out$net.result[[1]])
error_outlier = sum(abs(true_class - pred_class))/length(pred_class)
error_outlier

plot(nn_out)

#####################################################################
##         Try some deviations on the NN             ##
#####################################################################

nn2 <- neuralnet(case ~ age + parity + induced + spontaneous + stratum + pooled.stratum, data = infert, hidden = 4, err.fct = 'sse', linear.output = FALSE) # stick with single layer, dont take neurons too high.

nn3 <- neuralnet(case ~ age + parity + induced + spontaneous + stratum + pooled.stratum, data = infert, hidden = c(2, 2), err.fct = 'sse', linear.output = FALSE)

quartz()
plot(nn2)

quartz()
plot(nn3)

nn4 <- neuralnet(case ~ age + parity + induced + spontaneous + stratum + pooled.stratum, data = infert, hidden = 3, threshold = .15, err.fct = 'sse', linear.output = FALSE)

quartz()
plot(nn4)


##############################################################
## Prediction..
############################################################
new.output <- compute(nn2, covariate = matrix(c(22,1,0,0,  
												22, 1, 1, 0,
												22, 1, 0, 1,
												22, 1, 1, 1),
												byrow = TRUE, ncol = 4))
predicted_class_new_data <- round(new.output$net.result)
predicted_class_new_data






