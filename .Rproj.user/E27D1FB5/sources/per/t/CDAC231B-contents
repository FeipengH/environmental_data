#lab6
require(palmerpenguins)
#Q1
rm(list = ls())
sse_mean = function(x)
{
  sse = sd(x, na.rm = TRUE)/sqrt(length(x[!is.na(x)]))
  
  return(sse)  
}
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)
#Q2
two_group_resample_diff = function(x, n_1, n_2) 
{
  a = mean(sample(x, size = n_1, replace = T), na.rm = T)
  b = mean(sample(x, size = n_2, replace = T), na.rm = T)      
  difference_in_means = a - b
  return(difference_in_means)
}
#Q3
#My function performs Monte Carlo resampling and simulates a null hypothesis because it breaks up associations in the data.
#Q4
dat_pen = droplevels(subset(penguins, species != "Gentoo"))
n = 2000
difference_in_means = c()
for (i in 1:n)
{
  difference_in_means = c(
    difference_in_means,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(difference_in_means)
#Q5
#0
sum(abs(difference_in_means)>5.8)
#Q6
#more than 10 million
#Q7
boxplot(
  bill_length_mm ~ species, data = dat_pen,
  ylab = "bill_length_mm")
#Q8
agg_means = aggregate(
  bill_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])

agg_means
diff_crit
#Q9
t.test(dat_pen$bill_length_mm ~ dat_pen$species)
#Chinstrap penguin has a longer bill than Adelie penguin. This relationship holds true for many individuals. There may be Adelie penguins with long bills, but that’s very rare.
#Q10
#0
n = 1000
difference_in_means = c()
for (i in 1:n)
{
  difference_in_means = c(
    difference_in_means,
    two_group_resample_diff(dat_pen$bill_length_mm, 68, 152)
  )
}
sum(abs(difference_in_means)>10.04243)
#Q11
hist(difference_in_means, main = "Histogram of difference in mean bill length of Adelie and Chinstrap penguins", cex.main = 0.7)
