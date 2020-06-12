Credit_card = as.data.frame(Credit_card_data)
Credit_card$card <- as.factor(Credit_card$card)
Credit_card$owner <- as.factor(Credit_card$owner)
Credit_card$selfemp <- as.factor(Credit_card$selfemp)
summary(Credit_card)

num_ind <- vector(length=(dim(Credit_card)[2]))
for (i in 1:dim(Credit_card)[2]) {
  num_ind[i] <- is.numeric(unlist(Credit_card[,i]))
}
cor(Credit_card[,num_ind])
pairs(Credit_card)

plot(Credit_card$share,Credit_card$card)
plot(Credit_card$expenditure, Credit_card$card)
dummy_exp = rep("0", length(Credit_card$expenditure))
dummy_exp[Credit_card$expenditure>0] = "1"
Credit_card$expenditure_dummy <- as.factor(dummy_exp)
table(Credit_card$card, Credit_card$expenditure_dummy)

# logistic regression

log_fit = glm(card ~ reports  + age  + income + owner + selfemp + dependents +
                months + majorcards + active , data = Credit_card, family = binomial)
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

x = model.matrix(card ~.- share - expenditure - expenditure_dummy, Credit_card)[,-1]
y = Credit_card$card
train = sample(1: nrow(x), nrow(x)/2)
test=-train
y_test = y[test]

library(glmnet)

lasso_mod = glmnet(x[train,],y[train],alpha =1, lambda = grid, family="binomial")
plot(lasso_mod)

cv_out = cv.glmnet(x[train,],y[train],alpha =1, family="binomial")
plot(cv_out)
bestlam = cv_out$lambda.min
lasso_pred = predict(lasso_mod, s=bestlam, newx=x[test ,], type="response")
out = glmnet(x, y, alpha = 1, lambda =grid, family="binomial")
lasso_coef = predict(out ,type ="coefficients",s=bestlam )
lasso_coef



# tree for the whole data

tree_credit = tree(card ~.-expenditure_dummy, data=Credit_card)
summary(tree_credit)
plot(tree_credit)
text(tree_credit, pretty=0)

tree_train = sample(1:nrow(Credit_card), 660)
Credit_test = Credit_card[-tree_train,]
card_test = Credit_card$card[-tree_train]
tree_credit = tree(card ~.-expenditure_dummy, data=Credit_card, subset=tree_train)

tree_pred = predict(tree_credit, Credit_test, type = "class")
table(tree_pred, card_test)
mean(tree_pred == card_test) # 0.9802731
plot(tree_credit)
text(tree_credit, pretty=0)

credit_tree_cv = cv.tree(tree_credit, FUN=prune.misclass)
credit_tree_cv # min cv error rate is 10
plot(credit_tree_cv$size, credit_tree_cv$dev, type="b")
plot(credit_tree_cv$k, credit_tree_cv$dev, type="b")

prune_credit = prune.misclass(tree_credit, best=2)
plot(prune_credit)
text(prune_credit, pretty=0)

prune_pred = predict(prune_credit, Credit_test, type='class')
table(prune_pred, card_test)
mean(prune_pred == card_test) # 0.9802731
  
# bagging and rf

install.packages("randomForest")
library(randomForest)

bag_credit = randomForest(card ~. -expenditure_dummy, data=Credit_card, subset=tree_train, mtry= 11, importance=TRUE)
bag_credit # OOB estimate of  error rate: 2.27%

bag_pred = predict(bag_credit, Credit_test)
table(unlist(bag_pred), card_test)
mean(unlist(bag_pred) == card_test) # 0.9726859

rf_credit = randomForest(card ~. -expenditure_dummy, data=Credit_card, subset=tree_train, importance=TRUE)
rf_credit # OOB estimate of  error rate: 1.67%

rf_pred = predict(rf_credit, Credit_test)
table(unlist(rf_pred), card_test)
mean(unlist(rf_pred) == card_test) #  0.9787557

importance(rf_credit)
varImpPlot(rf_credit)

# logistic regression for 296+21=317 cases without expenditure, share

Credit_317 = Credit_card[which(Credit_card$expenditure == 0),-c(5,6,13)]
View(Credit_317)
summary(Credit_317)

log_fit_317 = glm(card ~ reports + age + income  + owner + selfemp + dependents +
                    months + majorcards + active, data = Credit_317, family = binomial)
plot(Credit_317$reports,Credit_317$card)

pairs(Credit_317)
summary(log_fit_317)
vif(log_fit_317)
stepAIC(log_fit_317)


probs_317 = predict(log_fit_317, type="response")
log_fit_pred_317 = rep("no", length(probs_317))
log_fit_pred_317[probs_317 > 0.6] = "yes"
table(log_fit_pred_317, Credit_317$card)
mean(log_fit_pred_317 == Credit_317$card)

plot(1:length(probs_317), probs_317)

# lasso for 296+21=317 cases without expenditure, share

x_317 = model.matrix(card ~., Credit_317)[,-1]
y_317 = Credit_317$card
train_317 = sample(1: nrow(x_317), nrow(x_317)/2)
test_317=-train_317
y_test_317 = y_317[test_317]

library(glmnet)

lasso_mod_317 = glmnet(x_317[train_317,],y_317[train_317],alpha =1, lambda = grid, family="binomial")
plot(lasso_mod_317)

cv_out_317 = cv.glmnet(x_317[train_317,],y_317[train_317],alpha =1, family="binomial")
plot(cv_out_317)
bestlam_317 = cv_out_317$lambda.min
lasso_pred_317 = predict(lasso_mod_317, s=bestlam_317, newx=x_317[test_317 ,], type="response")
out_317 = glmnet(x_317, y_317, alpha = 1, lambda =grid, family="binomial")
lasso_coef_317 = predict(out_317, type ="coefficients",s=bestlam_317 )
lasso_coef_317

summary(glm(card ~ reports  + age +  dependents + majorcards + active, data = Credit_317, family = binomial)) 

# tree for 296+21=317 cases without expenditure, share

install.packages("tree")
library(tree)

tree_317 = tree(card ~., data=Credit_317)
summary(tree_317)
plot(tree_317)
text(tree_317, pretty=0)

tree_train_317 = sample(1:nrow(Credit_317), 110)
Credit_test_317 = Credit_317[-tree_train_317,]
card_test_317 = Credit_317$card[-tree_train_317]
tree_317 = tree(card ~., data=Credit_317, subset=tree_train_317)

tree_pred = predict(tree_credit, Credit_test, type = "class")
table(tree_pred, card_test)
mean(tree_pred == card_test) # 0.9802731
plot(tree_credit)
text(tree_credit, pretty=0)

credit_tree_cv = cv.tree(tree_credit, FUN=prune.misclass)
credit_tree_cv # min cv error rate is 10
plot(credit_tree_cv$size, credit_tree_cv$dev, type="b")
plot(credit_tree_cv$k, credit_tree_cv$dev, type="b")

prune_credit = prune.misclass(tree_credit, best=2)
plot(prune_credit)
text(prune_credit, pretty=0)

prune_pred = predict(prune_credit, Credit_test, type='class')
table(prune_pred, card_test)
mean(prune_pred == card_test) # 0.9802731

# rf for 296+21=317 cases without expenditure, share

rf_credit_317 = randomForest(card ~., data=Credit_317, subset=train_317, importance=TRUE)
rf_credit_317 # OOB estimate of  error rate: 7.59%

rf_pred_317 = predict(rf_credit_317, Credit_317[-train_317,])
table(unlist(rf_pred_317), y_test_317)
mean(unlist(rf_pred_317) == y_test_317) #  0.918239

importance(rf_credit_317)
varImpPlot(rf_credit_317)

  