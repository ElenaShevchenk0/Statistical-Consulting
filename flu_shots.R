install.packages("ranger")
library(ranger)
install.packages("caret")
library(caret)
library(data.table)

View(Data_1)
Data_1$Y = as.factor(Data_1$Y)
table(Data_1$Y)
test_vector = sample(1:length(Data_1$Y), 20)
test_data = Data_1[test_vector,]
train_data = Data_1[-test_vector,]


# logistic regression

logistic_fit = glm(Y ~., data=train_data, family = binomial)
summary(logistic_fit)

install.packages("pROC")
library(pROC)
install.packages("caTools")
library(caTools)

logfit_probs = predict(logistic_fit, newdata = test_data, type="response")
auc_roc = colAUC(logfit_probs, test_data$Y, plotROC = TRUE) # auc = 0.8888889
auc = roc(test_data$Y,logfit_probs, plot = TRUE, col = "blue") # threshold = 0.77777778

library(blorr)
blr_rsq_nagelkerke(glm_flu) # r-squared = 0.4105951

logfit_pred = rep(0, length(logfit_probs))
logfit_pred[logfit_probs > 0.77777778] = 1
table(logfit_pred, test_data$Y)
mean(logfit_pred==test_data$Y)

# compare full model with nested model

glm_without_gender <- glm(Y ~ X1 + X2, data = HW6_1, family = binomial)
blr_test_lr(glm_flu, glm_without_gender) #LRT  p-value = 0.9037, hence remove gender

library(lmtest)
waldtest(glm_flu, glm_without_gender) # Wald p-value = 0.4546, hence remove gender

# model selection

stepAIC(logistic_fit)

# prediction X_1=55, X_2=59.83, X_3=1 using final model

glm_flu = glm(Y ~ X1 + X2, data=Data_1, family = binomial)
summary(glm_flu)
new_case = data.frame(X1 = 55, X2 = 59.83, X3 = "1")
probability_estimate = predict(glm_flu, newdata = new_case, type = "response")
probability_estimate # 0.05436244 







