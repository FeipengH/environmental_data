###Q1
#Basal area is the cross-sectional areas of tree stems. We measure the circumference at breast height (1.3 meters or 4.5 feet from the ground), estimate the diameter, and calculate the area.
#basal area of a forest = total basal area of all trees / area of a forest
library(here)
dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)
my_vec = dat_all$CEWA
my_vec > 0
cewa_present_absent = as.numeric(my_vec > 0)
plot(x = dat_all$elev, y = cewa_present_absent)

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)
plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

#install.packages("tmvnsim")
#install.packages("psych")
require(psych)
names(dat_all)
###Q2
pairs.panels(dat_all[, c("elev", "slope", "aspect")])

TOSO = dat_all$TOSO
TOSO > 0
TOSO_present_absent = as.numeric(TOSO > 0)
###Q3
plot(x = dat_all$elev, y = TOSO_present_absent, xlab = "Elevation (m)", ylab = "TOSO presence", main = "Elevation and Townsend's Solitaire (TOSO)")
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

###Q4
#Townsend's Solitaire preferred mid to high elevations and were not found at elevations below 300 meters. The logistic model is not a good fit because TOSO was absent from many mid to high elevations. More observations of TOSO would better reflect the habitat preference.

###Q5
AMRO = dat_all$AMRO
AMRO > 0
AMRO_present_absent = as.numeric(AMRO > 0)
plot(x = dat_all$ba.tot, y = AMRO_present_absent, xlab = "Basal area of trees (m2 per ha)", ylab = "AMRO presence", main = "Basal Area and American Robin (AMRO)")
curve(logistic_midpoint_slope(x, midpoint = 100, slope = -1), add = TRUE)

###Q6
#American Robin preferred low tree cover (basal area < 50 m2 per ha). However, there are many low basal area sites where robins were absent. So a logistic model is not a good fit. High basal area sites are not representative in this data set.

###Q7
#181 Gray Jays were observed in all of the sampling sites.
###Q8
GRJA = dat_all$GRJA
sum(GRJA)

###Q9
#There are 110 sampling sites in which Gray Jays were observed.
###Q10
GRJA > 0
GRJA_present_absent = as.numeric(GRJA > 0)
sum(GRJA_present_absent)
