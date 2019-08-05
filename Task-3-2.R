rm(list = ls())

install.packages("MASS")
install.packages("klaR")

library(MMST)
library(ggplot2)
library(klaR) 
library(MASS)

data(Dia)

Dia

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(Dia[,2:6], pch = 19,  cex = 2,
      col = my_cols[Dia$Class_Number],
      lower.panel=NULL)

Dia$Class_Number

col3- Blue
  2 - Yellow
  1 - Red
  
cor(Dia[,2:6])

Dia_class1 <- Dia[which(Dia$Class_Number==1),]
Dia_class2 <- Dia[which(Dia$Class_Number==2),]
Dia_class3 <- Dia[which(Dia$Class_Number==3),]
cor(Dia_class1[,2:6])
cor(Dia_class2[,2:6])
cor(Dia_class3[,2:6])

# Create a test and training dataset
set.seed(12)
train = sample(1:nrow(Dia), nrow(Dia)*.75)
Dia_train = Dia[train,2:7 ]
Dia_test = Dia[-train,2:7 ]
dim(Dia_train)
dim(Dia_test)

head(Dia_train$Class_Number)

y_true_train <- Dia_train$Class_Number
y_true_test <- Dia_test$Class_Number

####################################
## Linear Discriminant Analysis (LDA)
##
####################################
lda.fit <- lda(Class_Number~., data = Dia_train)
lda.pred.train <- predict(lda.fit, newdata = Dia_train)
y_hat_train <- as.numeric(lda.pred.train$class)
lda.pred.test <- predict(lda.fit, newdata = Dia_test)
y_hat_test <- as.numeric(lda.pred.test$class)

new_data <- data.frame(0.98,122,544,186,184)

names(new_data) = names(Dia_train)[1:5]

lda_pred <- predict(lda.fit, newdata=new_data)$class

# Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train) # 0.22
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)  # 0.17
lda_train_error
lda_test_error

####################################
#   Quadratic Discriminant Analysis
#
####################################
qda.fit <- qda(Class_Number ~., data = Dia_train)
qda.pred.train = predict(qda.fit, newdata = Dia_train)
y_hat_train <- as.numeric(qda.pred.train$class)
qda.pred.test = predict(qda.fit, newdata = Dia_test)
y_hat_test <- as.numeric(qda.pred.test$class)

qda_pred <- predict(qda.fit, newdata=new_data)$class

# Compute the error
qda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train) 
qda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error
qda_train_error
qda_test_error

lda_pred
qda_pred