
Credit_card = as.data.frame(Credit_card_data)
Credit_card$card <- as.factor(Credit_card$card)
Credit_card$owner <- as.factor(Credit_card$owner)
Credit_card$selfemp <- as.factor(Credit_card$selfemp)

num_ind <- vector(length=(dim(Credit_card)[2]))
for (i in 1:dim(Credit_card)[2]) {
  num_ind[i] <- is.numeric(unlist(Credit_card[,i]))
}

cor(Credit_card[,num_ind])
pairs(Credit_card)


plot(Credit_card$share,Credit_card$card)
plot(Credit_card$expenditure, Credit_card$card)


# logistic

log_fit = glm(card ~ reports  + age  + income + owner + selfemp + dependents +
                months + majorcards + active, data = Credit_card, family = binomial)
summary(log_fit)

library(MASS)
stepAIC(log_fit)

library(car)
vif(log_fit)
probs = predict(log_fit, type="response")
log_fit_pred = rep("no", length(probs))
log_fit_pred[probs > 0.6] = "yes"
table(log_fit_pred, Credit_card$card)
mean(log_fit_pred == Credit_card$card)
library(caret)
confusionMatrix(as.factor(log_fit_pred), Credit_card$card)

plot(1:length(probs), probs)

# lasso

x = model.matrix(card ~., Credit_card)[,-1]
y = Credit_card$card
train = sample(1: nrow(x), nrow(x)/2)
test=-train
y_test = y[test]

library(glmnet)

lasso_mod = glmnet(x[train,],y[train],alpha =1, lambda = grid, family="binomial")
plot(lasso_mod)

cv_out =cv.glmnet(x[train,],y[train],alpha =1, family="binomial")
plot(cv_out)
bestlam = cv_out$lambda.min
lasso_pred = predict(lasso_mod, s=bestlam, newx=x[test ,], type="response")
out = glmnet(x, y, alpha = 1, lambda =grid, family="binomial")
lasso_coef = predict(out ,type ="coefficients",s=bestlam )
lasso_coef








