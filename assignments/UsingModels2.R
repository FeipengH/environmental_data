require(palmerpenguins)
boxplot(body_mass_g ~ sex*species, data = penguins, las = 3, names = c("female \n Adelie", "male \n Adelie", "female \n Chinstrap", "male \n Chinstrap", "female \n Gentoo", "male \n Gentoo"), ylab = "body mass (g)", xlab = "")
#2 Based on the boxplots, I think male penguins are significantly heavier than female penguins of the same species and the difference is significant because the box for male is higher than the box for female of the same species and the boxes do not overlap much.
#3 I think adding sex to a model that already includes species will improve the model fit because there are significant variations in sex that we need to capture.
#4
fit_both = lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)
#5 female Adelie
#6 Intercept, speciesChinstrap
#7 3527.206
summary(fit_both)$coefficient [1 , 1] + summary(fit_both)$coefficient [3 , 1]
#8 3527.206
Chinstrap = subset(penguins, species == "Chinstrap")
Chinstrap_female = subset(Chinstrap, sex == "female")
mean(Chinstrap_female$body_mass_g)

aggregate(body_mass_g ~ species*sex, data = penguins, FUN = mean)
