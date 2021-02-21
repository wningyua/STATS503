# Lab4 503

# load the data
library(datasets)
data(iris)
set.seed(123) # for reproducibility
table(iris$Species)

# training and testing datasets
iris_setosa = which(iris$Species == "setosa")
iris_versi = which(iris$Species == "versicolor")
iris_virg = which(iris$Species == "virginica")
train_id = c(sample(iris_setosa, size = trunc(0.70 * length(iris_setosa))),
             sample(iris_versi, size = trunc(0.70 * length(iris_versi))),
             sample(iris_virg, size = trunc(0.70 * length(iris_virg))))
iris_train = iris[train_id, ]
iris_test = iris[-train_id, ]

# data visualization 
# scatte plot
pairs(iris_train[1:4], col=c("blue", "green","red")[iris_train$Species], 
      pch=c(1,2,3)[iris_train$Species])
par(xpd=TRUE)
legend(0.34, 0.71, as.vector(unique(iris_train$Species)), 
       col=c("blue", "green", "red"), pch=1:3, cex = 0.5)

#boxplots
par(mfrow=c(2,2))
plot(iris_train$Species,iris_train$Sepal.Length, main = "Sepal.Length vs Species", 
     ylab = "Sepal.Length")
plot(iris_train$Species,iris_train$Sepal.Width, main = "Sepal.Width vs Species",
     ylab = "Sepal.Width")
plot(iris_train$Species,iris_train$Petal.Length, main = "Petal.Length vs Species", 
     ylab = "Petal.Length")
plot(iris_train$Species,iris_train$Petal.Width, main = "Petal.Width vs Species", 
     ylab = "Petal.Width")

# LDA model
library(MASS) # loads the MASS package
iris_lda = lda(Species ~ ., data = iris_train) # trains LDA model using iris_train
iris_lda  

names(predict(iris_lda, iris_train))

head(predict(iris_lda, iris_train)$class, n = 9)
head(predict(iris_lda, iris_train)$posterior, n = 9)

# test and training error
iris_lda_train_pred = predict(iris_lda, iris_train)$class
iris_lda_test_pred = predict(iris_lda, iris_test)$class

train_err = mean(iris_lda_train_pred != iris_train$Species)
test_err = mean(iris_lda_test_pred != iris_test$Species)
train_err
test_err

# QDA Analysis
iris_qda = qda(Species ~ ., data = iris_train)
iris_qda

# note: mean of lda and qda should be same since the only difference
# is in the covariance 
iris_qda_train_pred = predict(iris_qda, iris_train)$class
iris_qda_test_pred = predict(iris_qda, iris_test)$class
train_err = mean(iris_qda_train_pred != iris_train$Species)
train_err
test_err = mean(iris_qda_test_pred != iris_test$Species)
test_err
table(predicted = iris_qda_test_pred, actual = iris_test$Species)

# Naive Bayes: many categorical variables, and we cannot use either LDA
# or QDA because they have assumptuoin of normal distribution
library(foreign)
mydata = read.dta("hsbdemo.dta")
head(mydata)

summary(mydata)

mydata <- mydata[,-1] # remove improper variable
mydata <- mydata[,-ncol(mydata)]

# make a naive bayes classifier
set.seed(231)
mydata_general = which(mydata$prog=="general")
mydata_academic = which(mydata$prog=="academic")
mydata_vocation = which(mydata$prog=="vocation")
trainIndex = c(sample(mydata_general, size = trunc(0.70 * length(mydata_general))),
               sample(mydata_academic, size = trunc(0.70 * length(mydata_academic))),
               sample(mydata_vocation, size = trunc(0.70 * length(mydata_vocation))))
train=mydata[trainIndex, ]
test=mydata[-trainIndex, ]

table(mydata$prog)
table(train$prog)

library(e1071)
NBclassfier=naiveBayes(prog~science+math, data=train)
print(NBclassfier)

pred_train = predict(NBclassfier,newdata = train)
mean(pred_train!=train$prog)

pred_test = predict(NBclassfier,newdata = test)
mean(pred_test!=test$prog)

# add more predictors to the naive bayes model

NBclassfier2=naiveBayes(prog~ses+science+socst+math+write+read+schtyp, data=train)
#NBclassfier2=naiveBayes(prog~., data=train)
print(NBclassfier2)

pred_train2 = predict(NBclassfier2,newdata = train)
mean(pred_train2!=train$prog)

pred_test2 = predict(NBclassfier2,newdata = test)
mean(pred_test2!=test$prog)















