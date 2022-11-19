veg = read.csv("/Users/stonehuang/Documents/environmental_data/data/vegdata.csv")
dat_bird = read.csv("/Users/stonehuang/Documents/environmental_data/data/bird.sub.csv")
dat_habitat = read.csv("/Users/stonehuang/Documents/environmental_data/data/hab.sub.csv")
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
#install.packages("simpleboot")
require(simpleboot)
dat_adelie = subset(penguins, species == "Adelie")
dat_chinstrap = subset(penguins, species == "Chinstrap")
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
pen_boot = two.boot(dat_adelie$flipper_length_mm, dat_chinstrap$flipper_length_mm, boot_mean, 10000, student = FALSE, weight = NULL)
str(pen_boot)
t = pen_boot[["t"]]
##########Q1##########
sd = sd(t)
#sd = 1.021529
##########Q2##########
hist(t, xlab = "Difference in mean flipper length (mm)
of Adelie and Chinstrap Pengunis", main = "Histogram of 10000 bootstrap differences 
     in mean penguin flipper length")
mean(t)
median(t)
##########Q3##########
quantile(
  pen_boot$t,
  c(0.025, 0.975))
#95% bootstrap CI = -7.897064, -3.855186 
##########Q4##########
#I think the resampled differences in means do not follow a skewed distribution. The mean is similar to the median and the peak of the histogram centers around the mean/median. 
##########Q5##########
pen_ecdf = ecdf(t)
##########Q6##########
1 - pen_ecdf(-4.5)
#0.088
##########Q7##########
pen_ecdf(-8) 
#0.02
##########Q8##########
#null: There is no difference in mean flipper lengths between Adelie and Chinstrap Pengunis.
#alternative: There is difference in mean flipper lengths between Adelie and Chinstrap Pengunis.
head(veg)
boxplot(pine ~ treatment, dat = veg)
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
table(dat_tree$treatment)
##########Q9##########
wilcox.test(pine ~ treatment, data = dat_tree, alternative = "two.sided")
#p-value = 0.1005
#Bootstrap
dat_clipped = subset(dat_tree, treatment == "clipped")
dat_control = subset(dat_tree, treatment == "control")
tree_boot = two.boot(dat_clipped$pine, dat_control$pine, boot_mean, 10000, student = FALSE, weight = NULL)
##########Q10##########
quantile(
  tree_boot$t,
  c(0.025, 0.975))
#4.25000 29.50312
##########Q11##########
observed_difference = mean(dat_clipped$pine) - mean(dat_control$pine)
#The observed difference in mean tree counts is 16 and it falls within the 95% bootstrap CI.

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
#Simpson’s diversity index for breeding birds: b.sidi
#Simpson’s diversity index for vegetation cover types: s.sidi

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)
# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)
##########Q12##########
#Simpson diversity index measures diversity. It quantifies the number of species and the relative abundance of each species.
##########Q13##########
s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)
##########Q14##########
m = 10000 
result_mc = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result_mc[i] = coef(fit_resampled_i)[2]
} 
##########Q15##########
hist(
  result_mc,
  main = "Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v = quantile(result_mc, c(.05)), lty = 2, col = "red", lwd = 2)

quantile(result_mc, c(.05))
##########Q16##########
#-0.01315558. The observed slope is less than the critical value.
##########Q17##########
#The chance of getting the observed value only by chance is very low (<5%), which provides evidence to reject the null hypothesis. It is likely that a negative relationship between vegetation cover diversity and bird diversity exists.

set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)
##########Q18##########
b = 10000 
result_boot = numeric(b) 
for(i in 1:b)
{
  index = sample(nrow(dat_1), replace = TRUE)
 
  dat_boot_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index],
      s.sidi = dat_1$s.sidi[index]
    )
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot_i)
  result_boot[i] = coef(fit_bs1)[2]
} 
hist(
  result_boot,
  main = "Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)
##########Q19##########
plot(
  density(result_mc),
  main = "Null and Alternative Distributions",
  xlab = "Slope Coefficient",
  xlim = c(-0.05, 0.04),
  ylim = c(0, 65),
  col = "blue",
  lwd = 2
  )
lines(density(result_boot), col = "red", lwd = 2)
legend(c("Null", "Alt."), lty = 1, lwd = 2, col = c("blue", "red"), x = "topright", bty = "n") 
##########Q20##########
#The region that falls under both curves is a region of uncertainty. If we observe a slope there, we are not sure if we can reject the null hypothesis.