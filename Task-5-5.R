################################################
##Question - 3
##
###############################################
rm(list = ls())
library(pls)
library(tidyr)
library(ggplot2)
install.packages("devtools")
library(devtools)
install.packages("corrplot")
library(corrplot)

summary(SwissBankNotes)
head(SwissBankNotes)
dim(SwissBankNotes)

genuine <-head(SwissBankNotes,n=100)

summary(genuine)

SwissBankNotes <- as.matrix(SwissBankNotes)

genuine <- as.matrix(genuine)

counterfeit <- tail(SwissBankNotes,n=100)

counterfeit <- as.matrix(counterfeit)

SwissBankNotes <- data.frame(SwissBankNotes)

genuine <- data.frame(genuine)

genuienity <- c()

for (i in 0:199){
  if(i<100){
    genuienity <- c(genuienity,1)
  }else{
    genuienity <- c(genuienity,0)
  }
}

SwissBankNotes$genuienity <- genuienity

summary(SwissBankNotes)

abs(cov(genuine) - cov(counterfeit))


ggplot(gather(SwissBankNotes), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

M<-cor(SwissBankNotes)

corrplot(M, method="circle")

M<-cor(genuine)

corrplot(M, method="circle")

pcr.fit = pcr(genuienity ~. , data = SwissBankNotes, scale = TRUE, validation = "CV")
summary(pcr.fit)

pcr.fit = pcr(genuienity ~., data = genuine, scale = TRUE, validation = "CV")
summary(pcr.fit)

