##Homework-2

## Question 1

#Installing Package
install.packages("ISLR")
install.packages("MMST")

#Loading librarys
library(MMST)
library(ISLR)

#Loading College Data
data("data")

?College

summary(data)

dim(data)

dim(train)

train <- as.data.frame(data[data$eval_set == "train",])

collegefit.full <- lm(reordered ~.,data=train)

summary(train)

summary(collegefit.full)

## Splitting the data into test and train

sample_size <- floor(0.75*nrow(College))
sample_size

set.seed(1)

train <- sample(seq_len(nrow(College)), size=sample_size)
test <- -train

test

college_train <- College[train,]
college_test <- College[test,]

##Fitting a linear model using least squares on training set

Lm_College_fit <- lm(Apps~., data=college_train)
prediction_test_college <- predict(Lm_College_fit,college_test)

head(prediction_test_college)

##Mean Sqquare Error for the test data
mean((prediction_test_college-college_test$Apps)^2)

##Ridge Model

#Installing glmet packages
install.packages("glmnet")

#Load Library
library(glmnet)

seqence <- seq(4,-2, length=100)

seqence

## Matrix form for the test and train data
train_matrix <- model.matrix(reordered ~., data=subtrain)
test_matrix <- model.matrix(Apps ~., data=college_test)


#Ridge Reggression
ridge_fit <- glmnet(train_matrix, subtrain$reordered, alpha=0)
ridge_cv <- cv.glmnet(train_matrix,subtrain$reordered, alpha=0)
ridge_bestlambda <- ridge_cv$lambda.min
ridge_bestlambda

#Prediction for test data
predict_ridge <- predict(ridge_fit, s=ridge_bestlambda, newx = train_matrix)

predict_ridge

#Error calculation
mean((predict_ridge-subtrain$reordered)^2)

?predict


##LASSO model

lasso_fit <- glmnet(train_matrix, subtrain$reordered,alpha=1)
lasso_cv <- cv.glmnet(train_matrix, subtrain$reordered, alpha=1)
lasso_bestlambda <- lasso_cv$lambda.min

##Prediction for test data in lasso
predict_lasso <- predict(lasso_fit, s=lasso_bestlambda, newx=train_matrix)

predict_lasso

head(predict_lasso)

mean((predict_lasso-subtrain$reordered)^2)

##Finding non zero coefficient estimates

predict(lasso_fit, s=lasso_bestlambda, type="coefficients")


##PCR Model for College training dataset

install.packages("pls")

library("pls")


pcr_fit <- pcr(reordered ~., data = subtrain, scale=TRUE, validation = 'CV')

validationplot(pcr_fit, val.type="MSEP")

pcr_predict <- predict(pcr_fit, subtrain, ncomp=10)

mean((pcr_predict-college_test$Apps)^2)


##PSL Model for the same below

pls_fit <- plsr(reordered ~., data = subtrain, scale=TRUE, validation = 'CV')

validationplot(pls_fit, val.type="MSEP")

pls_predict <- predict(pls_fit, subtrain, ncomp=10)

mean((pls_predict-subtrain$reordered)^2)

##Computind the R2 for the abpve 5 models
test_avg <-mean(college_test$Apps)
R2_LM <- 1 - mean((prediction_test_college-college_test$Apps)^2)/mean((test_avg -college_test$Apps)^2)
R2_ridge <- 1 - mean((predict_ridge-college_test$Apps)^2)/mean((test_avg -college_test$Apps)^2)
R2_lasso <- 1 - mean((predict_lasso-college_test$Apps)^2)/mean((test_avg -college_test$Apps)^2)
R2_pcr <- 1 - mean((pcr_predict-college_test$Apps)^2)/mean((test_avg -college_test$Apps)^2)
R2_pls <- 1 - mean((pls_predict-college_test$Apps)^2)/mean((test_avg -college_test$Apps)^2)
R2_LM
R2_ridge
R2_lasso
R2_pcr
R2_pls

#Question - 2 

rm(list=ls())

insurance_data_training <-read.delim("C:\\Users\\siddi\\Desktop\\HomeWork2\\ticdata2000.txt",header=FALSE,sep="\t")
insurance_data_testing <-read.delim("C:\\Users\\siddi\\Desktop\\HomeWork2\\ticeval2000.txt",header=FALSE,sep="\t")
insurance_data_target <-read.delim("C:\\Users\\siddi\\Desktop\\HomeWork2\\tictgts2000.txt",header=FALSE,sep="\t")

?read.delim

## Pie chart to represent curtomer details
pie(subset,main="Customer Details of Caravana")
box()


#Installing leanps package for processing subset selections forward, backward and Least squares
install.packages("leaps")

library(leaps)

summary(insurance_data_training)

#Forward Stepwise

regfit_forward <- regsubsets(V86~., data=insurance_data_training, method = "forward",nvmax=86)

summary(regfit_forward)

names(summary(regfit_forward))

par(mfrow=c(2,2))
plot(summary(regfit_forward)$rsq, xlab="Number of variables",ylab = "RSQ" ,type='l')
plot(summary(regfit_forward)$rss, xlab="Number of variables",ylab = "RSS" ,type='l')
plot(summary(regfit_forward)$adjr2, xlab="Number of variables",ylab = "ADJR2" ,type='l')
plot(summary(regfit_forward)$bic, xlab="Number of variables",ylab = "BIC" ,type='l')
box()

#Backward Stepwise

regfit_backward <- regsubsets(V86~., data=insurance_data_training, method = "backward",nvmax=86)

summary(regfit_backward)

par(mfrow=c(1,1))
plot(summary(regfit_backward)$rsq, xlab="Number of variables",ylab = "RSQ" ,type='l')
plot(summary(regfit_backward)$rss, xlab="Number of variables",ylab = "RSS" ,type='l')
plot(summary(regfit_backward)$adjr2, xlab="Number of variables",ylab = "ADJR2" ,type='l')
plot(summary(regfit_backward)$bic, xlab="Number of variables",ylab = "BIC" ,type='l')

?regsubsets

subset <- table(insurance_data_training$V86)
subset

summary(subset_target)

subset_target <- subset(insurance_data_training, insurance_data_training$V86>0)

?subset

#Least squares

data_lm <- lm(V86 ~., data = insurance_data_training)

data_predict_lm <- predict(data_lm,insurance_data_testing)

summary(data_predict_lm)

boxplot(predict_ridge)

mean((insurance_data_target$V1-data_predict_lm)^2)

str(data_predict_lm)

data_predict_lm <- data.frame(data_predict_lm)

str(insurance_data_target)

data_predict_lm <- as.numeric(data_predict_lm$data_predict_lm)

insurance_data_target <- data.frame(insurance_data_target)

#Ridge regression

train_matrix <- model.matrix(V86 ~., data=insurance_data_training)
test_matrix <- model.matrix(insurance_data_target$V1 ~., data=insurance_data_testing)

ridge_fit <- glmnet(train_matrix[,-86], insurance_data_training$V86, alpha=0)
ridge_cv <- cv.glmnet(train_matrix,insurance_data_training$V86, alpha=0)
ridge_bestlambda <- ridge_cv$lambda.min
ridge_bestlambda

#Prediction for test data

predict_ridge <- predict(ridge_fit, s=ridge_bestlambda, newx = test_matrix, type="response")


mean((insurance_data_target$V1 - predict_ridge)^2)

#Lasso 

lasso_fit <- glmnet(train_matrix[,-86], insurance_data_training$V86,alpha=1)
lasso_cv <- cv.glmnet(train_matrix, insurance_data_training$V86, alpha=1)
lasso_bestlambda <- lasso_cv$lambda.min

##Prediction for test data in lasso
predict_lasso <- predict(lasso_fit, s=lasso_bestlambda, newx=test_matrix)

predict_lasso

head(predict_lasso)

mean((predict_lasso-insurance_data_target$V1)^2)

#Question 3
rm(list=ls())

library(leaps)

set.seed(12)

##Contsruction of dataset and forming best subset seletion model

X=matrix(rnorm(1000*20),ncol=20,nrow=1000)

beta=rnorm(20,sd=10)

beta[c(1,3,6,14,17,10)]=0

e=rnorm(1000)

Y=as.vector(X*beta+e)

train=sample(1:nrow(X),900)

test=(-train)

X_train=X[train,]

X_test=X[test,]

Y_train=Y[train]

Y_test=Y[test]

data_train=cbind(data.frame(Y=Y_train),X_train)
data_test=cbind(data.frame(Y=Y_test),X_test)

##Training plot

regfit_best=regsubsets(Y~., data=data_train,nvmax=20)
train_matrix <- model.matrix(Y~., data=data_train,nvmax=20)

val_error <- rep('NA',20)

for (i in 1:20) {
  coefi <- coef(regfit_best, id = i)
  prediction <- train_matrix[, names(coefi)] * coefi
  val_error[i] <- mean((prediction - Y_train)^2)
}
plot(val_error, xlab = "Number of predictors", ylab = "Training MSE", pch = 19, type = "b")

#Testing plot

regfit_best=regsubsets(Y~., data=data_test,nvmax=20)
test_matrix <- model.matrix(Y~., data=data_test,nvmax=20)

val_error <- rep('NA',20)

for (i in 1:20) {
  coefi <- coef(regfit_best, id = i)
  prediction <- test_matrix[, names(coefi)] * coefi
  val_error[i] <- mean((prediction - Y_test)^2)
}
plot(val_error, xlab = "Number of predictors", ylab = "Testing MSE", pch = 19, type = "b")

##Minimun varibles for the least error
which.min(val_error)

##Coefficients of the model at min errors.
coef(regfit_best, which.min(val_error))

coef(regfit_best,3)
