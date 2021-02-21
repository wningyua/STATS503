# Logistic regression 
set.seed(3)
dat = read.csv("train.csv")
dat = dat[complete.cases(dat), ]
dat$Survived = factor(dat$Survived)
levels(dat$Survived) = c("No", "Yes")

n = dim(dat)[1]
train_id = sample(seq(1, n, 1), floor(n*0.7))
test = dat[-train_id, ]
train = dat[train_id, ]

select_variables = c("Age", "Fare", "Parch", "Sex")
pairs(train[, select_variables], col=c("green","red")[train$Survived], 
      pch=c(1,2)[train$Survived])
par(xpd=TRUE)
legend(0.34, 0.51, as.vector(unique(train$Survived)), 
       col=c("green", "red"), pch=1:3, cex = 0.5)

# fit the model
mod_log = glm(Survived ~ Age + Fare + Parch + Sex,
              data = train, family = binomial)
summary(mod_log)

# prediction 
pred = predict(mod_log, test[, select_variables])
head(pred)
exp(0.5)/(1 + exp(0.5))
binomial()$linkinv(.5)

# Get predicted probabilities
predProbs = binomial()$linkinv(pred)
pred_log = rep("No", nrow(test))

# Assign the label to be "Yes" if the probability is greater than 0.5
pred_log[predProbs > .5] = "Yes"
table(pred_log, test$Survived)

err_log = sum(pred_log != test$Survived) / nrow(test)
err_log


# Logistic vs LDA, QDA
library(MASS)  # loads the MASS package
mod_lda = lda(Survived ~ Age + Fare + Parch + Sex, data = train) 
mod_lda

err_lda = sum(pred_lda != test$Survived) / nrow(test)
err_lda

mod_qda = qda(Survived ~ Age + Fare + Parch + Sex, data = train)
mod_qda

pred_lda = predict(mod_lda, test[, select_variables])$class
table(pred_lda, test$Survived)


pred_qda = predict(mod_qda, test[, select_variables])$class
table(pred_qda, test$Survived)


err_qda = sum(pred_qda != test$Survived) / nrow(test)
err_qda

data.frame(Method = c("Logistic Regression", "QDA", "LDA"), Error = c(err_log, err_qda, err_lda))

# conclusion: we do not really need a complex model.

#AUC
tab = table(pred_log, test$Survived)
tab
true_pos = tab[2, 2] / sum(tab[, 2])
false_pos = tab[2, 1] / sum(tab[, 1])

#plot ROC
library(ROCR)
library(Metrics)
pr <- prediction(pred, test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
abline(a = 0, b = 1)
points(c(false_pos), c(true_pos), col = "red")
auc = performance(pr, "auc")
auc








