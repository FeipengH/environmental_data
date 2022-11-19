catrate = read.csv("/Users/stonehuang/Documents/environmental_data/data/catrate.csv")
head(catrate)
summary(catrate)
#Q1.
hist(catrate$cat.rate, xlab = "Catastrophe Rate", main = "Histogram of Catastrophe Rates")
#Q2. p-value = 0.04097
shapiro.test(catrate$cat.rate)
#Q3. The null hypothesis for the Shapiro-Wilk test is that the data were sampled from a normally-distributed population.
#Q4. There is strong evidence that the sample came from a non-normally-distributed population.
#Q5.
t.test(catrate$cat.rate, mu = 2/7)
#Q6. The catastrophic rate is equal to the pond late-filling rate.
#Q7. This is a two-tailed test.
#Q8. p-value = 0.01193. Even if the p-value is very small and we think there is no pattern, there is still a small chance that a pattern exists.
#Q9. 0.3526250 0.7261295, it does not include 0
#Q10. I conclude that there is strong evidence to reject the null hypothesis because the p-value is lower than 0.05.
#Q11.
wilcox.test(catrate$cat.rate, mu = 2 / 7, exact=FALSE)
#Q12. The p-value is 0.006275, lower than the p-value from the t-test.
#Q13. I conclude that there is strong evidence to reject the null hypothesis because the p-value is lower than 0.05.
#Q14. The overall conclusions I could draw from the results of the two tests are the same.
#Q15. The Wilcoxon Rank Sum Test is more appropriate because the data are not normally-distributed.
#Q16.
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
dat_adelie = subset(penguin_dat, species == "Adelie")
shapiro.test(dat_adelie$flipper_length_mm)
dat_Chinstrap = subset(penguin_dat, species == "Chinstrap")
shapiro.test(dat_Chinstrap$flipper_length_mm)
#Q17. I conclude that the flipper lengths are normally-distributed for both species because the p-values (0.72 for Adelie, 0.8106 for Chinstrap) are higher than 0.05. We fail to reject the null hypothesis.
#Q18.
par(mfrow = c(1, 2))
hist(dat_adelie$flipper_length_mm, xlab = "flipper length (mm)", main = "Flipper Length \n of Adelie Penguins")
hist(dat_Chinstrap$flipper_length_mm, xlab = "flipper length (mm)", main = "Flipper Length \n of Chinstrap Penguins")
#Q19. Adelie and Chinstrap penguins have different flipper lengths. (two- tailed test)
#Q20.
t.test(flipper_length_mm ~ species, data = penguin_dat)