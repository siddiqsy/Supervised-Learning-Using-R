rm(list=ls())

install.packages("rpart")
install.packages("tree")
install.packages("party")

library("rpart")
library(MASS)
library(tree)
library(party)
library("caret")

head(wine_data)

colnames(wine_data) <- c('Type', 'Alcohol', 'Malic', 'Ash', 
                    'Alcalinity', 'Magnesium', 'Phenols', 
                    'Flavanoids', 'Nonflavanoids',
                    'Proanthocyanins', 'Color', 'Hue', 
                    'Dilution', 'Proline')
wine_data$Type <- as.factor(wine_data$Type)


model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.wine <- rpart(Type~., data = wine_data, method = "class", control = model.control)

plot(fit.wine, branch = .4, uniform = T, compress = T)
text(fit.wine, use.n = T, all = T, cex = 1)

plot(fit.wine$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit.wine$cptable[,4])
pruned_fit_wine <- prune(fit.wine, cp = fit.wine$cptable[min_cp,1])

## plot the full tree and the pruned tree
plot(pruned_fit_wine, branch = .3, compress=T,uniform=T,main = "Pruned Tree")
text(pruned_fit_wine, use.n = T, all = T, cex = 1)

plot(fit.wine, branch = .3, compress=T, main = "Full Tree")
text(fit.wine, cex = 1)


summary(wine_data)

fit_ctree <- ctree(Type~., data = wine_data,controls = ctree_control(maxsurrogate = 3,minsplit = 5))

plot(fit_ctree)

names(fit_ctree)
summary(fit_ctree)

# divide into test and training
set.seed(12)
test_indis <- sample(1:nrow(wine_data), 1/3*nrow(wine_data))
test <- wine_data[test_indis,]
training <- wine_data[-test_indis,]

?ctree

fit_ctree <- ctree(Type~., data = training,controls = ctree_control(maxsurrogate = 3))

plot(fit_ctree,main="Training tree")

fit_ctree <- ctree(Type~., data = test,controls = ctree_control(maxsurrogate = 3))

plot(fit_ctree,main="Test tree")

