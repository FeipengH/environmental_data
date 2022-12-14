library(here)
delomys = read.csv(here("data", "delomys.csv"))

summary(delomys$body_mass)
summary(delomys$body_length)
shapiro.test(delomys$body_mass)
shapiro.test(delomys$body_length)

plot(body_mass ~ body_length, data = delomys, xlab = "Body length", ylab = "Body mass", main = "Scatterplot of body mass and body length")
hist(delomys$body_mass, xlab = "Body mass", main = "Histogram of body mass")
hist(delomys$body_length, xlab = "Body length", main = "Histogram of body length")
boxplot(body_mass ~ binomial, data = delomys, xlab = "Species", ylab = "Body mass", main = "Boxplot of body mass conditioned on species")
boxplot(body_mass ~ sex, data = delomys, xlab = "Sex", ylab = "Body mass", main = "Boxplot of body mass conditioned on sex")
boxplot(body_mass ~ sex*binomial, data = delomys, las = 3, names = c("female \n D. dorsalis", "male \n D. dorsalis", "female \n D. sublineatus", "male \n D. sublineatus"), ylab = "Body mass", xlab = "", main = "Boxplot of mass conditioned on sex and species", cex.axis = 0.7, cex.lab = 0.7)

#Q1 The relationship between body mass and length seems to be linear. Mass is positive correlated with length.
#Q2 The data, especially body length, does not appear normally-distributed because the histograms are not symmetrical.
#Q3 I think the (unconditioned) body masses and body length are not normally-distributed (histograms are skewed, p-values < 0.05). My visual assessment of normality matched the results of the numerical normality tests.
#Q4 D. dorsalis seems to weigh more than D. sublineatus and male seems to weigh more than female, but not by much. There is no significant difference.

fit1 = lm(body_length ~ body_mass, data = delomys)
fit2 = lm(body_mass ~ sex, data = delomys)
fit3 = lm(body_mass ~ binomial, data = delomys)
fit4 = lm(body_mass ~ sex + binomial, data = delomys)
fit5 = lm(body_mass ~ sex * binomial, data = delomys)

hist(residuals(fit1))
hist(residuals(fit2))
hist(residuals(fit3))
hist(residuals(fit4))
hist(residuals(fit5))

shapiro.test(residuals(fit1))
shapiro.test(residuals(fit2))
shapiro.test(residuals(fit3))
shapiro.test(residuals(fit4))
shapiro.test(residuals(fit5))

#Q5 Based on the numerical and graphical diagnostics, all models fail to fulfill the assumption of normality of the residuals.
#Q6 Violations of the normality assumption are not equally severe for all the models. The violation is much more severe in model 1.

knitr::kable(coef(summary(fit1)))
#Q7 0.8754988
#Q8 163.6745
76.1246565+(0.8754988*100)
#Q9 76.12466
76.1246565+(0.8754988*0)

knitr::kable(coef(summary(fit2)))
knitr::kable(coef(summary(fit3)))
knitr::kable(coef(summary(fit4)))
knitr::kable(coef(summary(fit5)))

#Q10 female
#Q11 Delomys dorsalis
#Q12 male
#Q13 Delomys dorsalis

knitr::kable(anova(fit2))
knitr::kable(anova(fit3))
knitr::kable(anova(fit4))
knitr::kable(anova(fit5))

#Q14 Sex and species are significant predictors for body mass.
#Q15 There is not a significant interaction.
#Q16 The significance level of sex and the significance level of species do not change much among the different models. All corresponding p-values are very small. 

AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
#Q17 model 4 and 5
#Q18 I would select model 4 because it is the best fit (lowest AIC), and an additive model is easier to understand and explain than an interactive model.
