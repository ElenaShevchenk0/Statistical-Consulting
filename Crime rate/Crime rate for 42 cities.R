# regression analysis

pairs(HW3_2)
cor(HW3_2)
plot(lm_crime, 4, id.n = 5)
which(hatvalues(lm_crime) > 12/42)
lm_crime <- lm(Y ~. , data = HW3_2)
summary(lm_crime)
par(mfrow=c(2,2))
plot(lm_crime)
par(mfrow=c(1,1))

# regression without cases 12, 38

HW3_2_12_38 <- HW3_2[c(-12,-38),]
lm_crime_12_38 <- lm(Y ~. , data = HW3_2_12_38)
summary(lm_crime_12_38)
plot(lm_crime_12_38)

vif(lm_crime)
vif(lm_crime_12_38)

# model selection with all cases

library(MASS)
step_crime <- stepAIC(lm_crime, direction = c("backward")) 
step_crime_b <- stepAIC(lm_crime, direction = c("both")) 

# model selection without case 12, 38

step_crime_12_38 <- stepAIC(lm_crime_12_38, direction = c("backward")) 
step_crime_12_38_b <- stepAIC(lm_crime_12_38, direction = c("both")) 
step_crime_12_38_b <- stepAIC(lm(Y ~ 1, data = HW3_2_12_38), scope = list(upper = lm(Y ~. , data = HW3_2_12_38),
                                                                          lower = lm(Y ~ 1, data = HW3_2_12_38)), direction = c("forward"))

power.t.test(n = , delta = 2, sd = 8.544004, sig.level = 0.05,
             power = 0.8, type = "two.sample")



