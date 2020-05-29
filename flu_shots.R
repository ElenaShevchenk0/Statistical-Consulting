install.packages("ranger")
library(ranger)
install.packages("caret")
library(caret)
library(data.table)

View(Data_1)
table(Data_1$Y)
test_vector = sample(1:length(Data_1$Y), 10)
test_data = Data_1[test_vector,]
train_data = Data_1[-test_vector,]

logistic_fit = glm(Y ~., data=train_data, family = binomial)
summary(logistic_fit)

#install.packages("pROC")
#library(pROC)
install.packages("caTools")
library(caTools)

logfit_probs = predict(logistic_fit, newdata = test_data, type="response")
auc_roc = colAUC(logfit_probs, test_data$Y, plotROC = TRUE)
auc = roc(test_data$Y,logfit_probs, plot = TRUE, col = "blue")




