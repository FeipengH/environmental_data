#Q1
rm(list = ls())

rope = read.csv("/Users/stonehuang/Documents/environmental_data/data/rope.csv")
rope$rope.type = factor(rope$rope.type)

levels(rope$rope.type)

n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

grandmean = mean(rope$p.cut)
residuals = grandmean - rope$p.cut

ss_tot = sum(residuals ^ 2)
df_tot = n_obs - 1

agg_resids = 
 aggregate(
    x = rope$p.cut,
    by = list(rope$rope.type), 
    FUN = function(x) x - mean(x))

str(agg_resids)

#sums of squared residuals within each group
agg_sum_sq_resids = 
 aggregate(
    x = rope$p.cut,
    by = list(rope$rope.type), 
    FUN = function(x) sum((x - mean(x))^2))

str(agg_sum_sq_resids)

ss_within = sum(agg_sum_sq_resids$x)
df_within = n_obs - n_groups

ss_among = ss_tot - ss_within
df_among = n_groups - 1

ms_within = ss_within / df_within
ms_among  = ss_among / df_among
 
f_ratio = ms_among / ms_within
f_pval = 1 - pf(f_ratio, df_among, df_within)

#ANOVA in R
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

#Q2 Based on the figure, I think the variances are not equal among the groups because the boxes are in different sizes.
#Q3
bartlett.test(p.cut ~ rope.type, data = rope)
#Q4 I think an ANOVA-type analysis is not appropriate for the raw data because the raw data does not have equal variances of the residuals within groups, which is an assumption of ANOVA.

fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)
#Q5 BLAZE
#Q6 0.36714, it is the intercept from the model coefficient table.
#Q7 0.2655
0.36714 + (-0.10164)

#Q8 p-value = 7.238e-07
shapiro.test(residuals(fit_1))
#Q9 The model residuals do not meet the normality assumption because the p-value is lower than 0.05 so we reject the null hypothesis of the normality test. 
#Q10 3 groups meet the normality assumption.
sapply(X = agg_resids$x, FUN = function(x) shapiro.test(x))
#Q11 I think a one-way ANOVA is not appropriate for this dataset because the assumption of the normality of the residuals is not well met.

require(palmerpenguins)
pen_fem = subset(penguins, sex == "female")
#Q12
boxplot(body_mass_g ~ species, data = pen_fem, main = "Female penguins body mass by species", xlab = "Species", ylab = "Body mass (g)")
#Q13 I do not anticipate problems with residual normality, or homogeneity of variances because the boxes are in similar sizes (similar variance) and symmetrical.
#Q14 p-value = 0.9056, the homogeneity assumption is met because the p-value is higher than 0.05 so we fail to reject the null hypothesis that variances are equal. 
bartlett.test(body_mass_g ~ species, data = pen_fem)
#Q15 p-value = 0.3639, the residual normality assumption is met because the p-value is higher than 0.05 so we fail to reject the null hypothesis that residuals are normally distributed.
fit_penguin = lm(body_mass_g ~ species, data=pen_fem)
shapiro.test(residuals(fit_penguin))
#Q16 All pairs of species have significantly different body masses.
penguin_hsd = TukeyHSD(aov(fit_penguin))
round(penguin_hsd$species, digits = 4)
#Q17 The HSD test results for Gentoo-Adelie and Gentoo-Chinstrap match the graphical insight from the conditional boxplot. Chinstrap-Adelie does not match.
