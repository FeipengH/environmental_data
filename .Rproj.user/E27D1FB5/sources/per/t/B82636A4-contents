birds = read.csv("/Users/stonehuang/Documents/environmental_data/data/bird.sta.csv")
hab = read.csv("/Users/stonehuang/Documents/environmental_data/data/hab.sta.csv")

birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

#Q1. The presence/absence of Brown Creepers does not vary between the interior and edge of forest stands.
#Q2. Brown Creepers show a significant habitat preference because the p-value is lower than 0.05.
chisq_BRCR = chisq.test(br_creeper_table)
chisq_BRCR

require(palmerpenguins)
head(penguins)
#Q3
fit_species = lm(formula = body_mass_g ~ species, data = penguins)
#Q4
fit_sex = lm(formula = body_mass_g ~ sex, data = penguins)
#Q5
fit_both = lm(formula = body_mass_g ~ sex*species, data = penguins)

#Q6
boxplot(body_mass_g ~ species, data = penguins, main = "Boxplot for the fit_species model")
#Q7
boxplot(body_mass_g ~ sex, data = penguins, main = "Boxplot for the fit_sex model")
#Q8
boxplot(body_mass_g ~ sex*species, data = penguins, las = 3, names = c("female \n Adelie", "male \n Adelie", "female \n Chinstrap", "male \n Chinstrap", "female \n Gentoo", "male \n Gentoo"), ylab = "body mass (g)", xlab = "", main = "Boxplot for the fit_both model")
#Q9
#fit_species model may have problems fulfilling the homogeneity assumption because the boxes have very different width.

#Q10. The null hypothesis of the Bartlett test is that the variances are the same among groups.
#Q11. 0.0501
bartlett.test(body_mass_g ~ species, data = penguins)
#Q12. 0.0319
bartlett.test(body_mass_g ~ sex, data = penguins)
#Q13. 0.1741
dat_groups = aggregate(
  body_mass_g ~ sex*species,
  data = penguins,
  FUN = c)
str(dat_groups)
bartlett.test(dat_groups$body_mass_g)
#Q14
#The fit_sex model has issues with heterogeneity. The p-value is lower than 0.05, so we reject the null hypothesis that the variances are the same among groups.

#Q15
dat_fl = read.csv("/Users/stonehuang/Documents/environmental_data/data/trees_FL.csv")
head(dat_fl)
barplot(table(dat_fl$ProbabilityofFailure), ylab = "Counts of trees", xlab = "Probability of failure class", main = "Barplot of counts of trees 
        in each probability of failure class")
barplot(table(dat_fl$Failure_Standardized), ylab = "Counts of trees", xlab = "Failure classes", main = "Barplot of the counts of trees 
        in each of the failure classes")
hist(dat_fl$DBH_in, xlab = "DBH", main = "Histogram of DBH")
plot(dat_fl$HeighttoTop_ft ~ dat_fl$DBH_in, ylab = "Tree height", xlab = "DBH", main = "Scatterplot of DBH and tree height", cex = 0.1)

#Q16. The null hypothesis for the Kolmogorov-Smirnov test is that there is no difference in DBH between whole-tree failures and intact trees.
#Q17. 0.02125. The distribution of DBH is not the same for the two groups. The p-value is lower than 0.05 so we reject the null hypothesis.
whole = subset(dat_fl, Failure_Standardized == "whole")
none = subset(dat_fl, Failure_Standardized == "none")
ks.test(whole$DBH_in, none$DBH_in)

#Q18. Tree height increases with increasing DBH. The relationship is curved and monotonic. 
#Q19. Spearman
cor.test(
  dat_fl$DBH_in,
  dat_fl$HeighttoTop_ft,
  method='spearman',
  exact=FALSE)
#Q20. p-value < 2.2e-16.I conclude that the two variables are significantly correlated.

#Q21. X-squared = 202.65, p-value < 2.2e-16
dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")
levels(dat_fl$fail) = c("No Fail", "Fail")
fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2
chisq_trees = chisq.test(fl_table_2)
chisq_trees
#Q22. -136
round(
  chisq_trees$observed - chisq_trees$expected,
  digits = 0)
#Q23. There were fewer tree failures than expected by chance in failure probability category #1.
#Q24. There more tree failures than expected by chance in failure probability category #4.
#Q25. I conclude that the probability of failure rating system is effective! There were more tree failures than expected in higher failure probability categories.
