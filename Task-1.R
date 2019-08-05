Question 1:
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3 <- do.call(rbind,list(d1,d2))
summary(d3)

str(d3)

names(d3)

model1 <- lm(G1~., data=d3)

summary(model1)

d3_GP_school <- subset(d3,school=="GP")

nrow(d3_GP_school)
summary(d3_GP_school$G1)

d3_SP_school <- subset(d3,school=="MS")

summary(d3_SP_school$G1)

model2 <- d3[c(3,7,8,13,14,15,24,25,26,27,28,29,30,31,32,33)]

#scatter plots for the given variables 
pairs(d3[c(1,2,3,4,5,6,7,8,9,10,31)])
pairs(d3[c(11,12,13,14,15,16,17,18,19,20,31)])
pairs(d3[c(21,22,23,24,25,26,27,28,29,30,31,32,33)])

## Summary of G1 for sex
d3_F <- subset(d3,sex=="F")

summary(d3_F$G1)

d3_M <- subset(d3,sex=="M")

summary(d3_M$G1)

## Summary of G1 for address
d3_R <- subset(d3,address=="R")

summary(d3_R$G1)

d3_U <- subset(d3,address=="U")

summary(d3_U$G1)


## Summary of G1 for famsize
d3_GT3 <- subset(d3,famsize=="GT3")

summary(d3_GT3$G1)

d3_LE3 <- subset(d3,famsize=="LE3")

summary(d3_LE3$G1)


##Box plots

boxplot(d3$absences)
boxplot(d3$failures)


#Cleaning the dataset by removing few columns and merging the datasets

d1_final <- subset(d1[c(1,4,6,8,9,10,11,14,15,16,17,18,21,22,23,25,27,30,31,32,33)])
d2_final <- subset(d2[c(1,4,6,8,9,10,11,14,15,16,17,18,21,22,23,25,27,30,31,32,33)])

d3=merge(d1_final,d2_final,by=c("school","address","Pstatus","Fedu","Mjob","Fjob","reason","internet"))

save(d3,file="student_final.rdata")

Question:2 

model1 <- lm(G1.x~., data=d3)

summary(model1)

model2 <- lm(G1.y~., data=d3)

summary(model2)

## Correlation Plot
cr <- cor(d3)

library(corrplot)
corrplot(cr,method = "number")

##Using * and : for interactions 
model3 <- lm(G1.x~G2.x*G3.x, data=d3)

summary(model3)


model4 <- lm(G1.x~freetime.x:studytime.x, data=d3)

summary(model4)


Question 3:
## To load the data into dataset
library(readxl)
boston <- read_excel("C:/Users/siddi/Downloads/boston.xls")
View(boston)

## Look at the summary for the Boston Housing Dataset
summary(boston)

## Scatterplot for the dataset 
pairs(boston[,1:14])
plot(boston[,1:3])

## Correlation Plot
cr <- cor(boston)

library(corrplot)
corrplot(cr,type = "lower")

corrgram(cr)

?corrgram

library(corrgram)

summary(boston)

library(ggplot2)

qplot(boston$CRIM, binwidth=10 , xlab= "Rate of Crime", ylab = "Number of Suburbs")

qplot(boston$TAX, binwidth=25 , xlab= "Tax rates", ylab = "Number of Suburbs")

qplot(boston$PT, binwidth=1 , xlab= "Puiple - Teacher Ratio", ylab = "Number of Suburbs")

crim_20 <- subset(boston, crim>20)

nrow(crim_20)/nrow(boston)

crim_5 <- subset(boston, crim>5)

nrow(crim_5)/nrow(boston)

tax_500 <- subset(boston, TAX>500)

nrow(tax_500)/nrow(boston)

rm_7 <- subset(boston, RM>7)
nrow(rm_7)

rm_8 <- subset(boston, RM>8)
nrow(rm_8)

cr_8 <- cor(rm_8)

corrplot(cr_8)
summary(rm_8)

Question 4:
  
library(readr)
train <- read_table2("zip.train.csv", col_names = FALSE)
View(train)
dim(train)

library(readr)
test <- read_table2("zip.test.csv", col_names = FALSE)
View(test)

##linear Regression
model <- lm(X1~., data=train[,2:257])


##Taking 2's and 3's from the data
func <- function(x){if (x>2.5) 3 else 2}

test_prediction <- as.character(sapply(predict(model,test),func))
train_prediction <- as.character(sapply(predict(model,train),func))

##To apply KNN
KNN_test <- test[,2:257]
KNN_train <- train[,2:257]

KNN_test.X1 <- as.factor(test$X1)
KNN_train.X1 <- as.factor(train$X1)

##Predictions of KNN
KNN_test_prediction <- sapply (1:15 , function (k) {
  knn ( train = KNN_train ,
        test = KNN_test ,
        cl = KNN_test.X1 ,
        k = k )
})
KNN_train_prediction <- sapply (1:15 , function (k) {
  knn ( train = KNN_train ,
        test = KNN_test ,
        cl = KNN_train.X1 ,
        k = k )
})

##Computation of error rates

errors_xs <- 1:15
errors_knn_test <- apply ( KNN_test_prediction , 2, function ( prediction ) {
  classError ( KNN_test_prediction, as . factor ( test$X1 ) ) $errorRate
})
errors_knn_train <- apply ( KNN_train_prediction , 2, function ( prediction ) {
  classError ( KNN_train_prediction , as . factor ( train$X1 )) $errorRate
})
errors_lm_test <- sapply ( errors_xs , function (k) {
  classError ( test_prediction , as . factor ( test$X1 )) $errorRate
})
errors_lm_train <- sapply ( errors_xs , function (k) {
  classError ( train_prediction , as . factor ( train$X1)) $errorRate
})
errors <- data . frame ("K"= errors . xs ,
                        " KNN. train "= errors_knn_train ,
                        " KNN. test "= errors_knn_train,
                        "LR. Train "= errors_lm_train ,
                        "LR. Test "= errors_lm_test )

##Using ggplot to plot the data
plot . data <- melt ( errors , id ="K")
ggplot ( data = plot . data ,
         aes (x =K , y= value , colour = variable ) ) +
  geom_line () +
  xlab ("k") +
  ylab (" Classification Error ") +
  opts ( title =" Classification Errors for different methods on zipcode data ")
scale_colour_hue ( name =" Classification Method ",
                   labels = c("k-NN ( Train )",
                              "k-NN ( Test )",
                              " Linear Regression ( Train )",
                              " Linear Regression ( Test )")
)
