#install.packages("here")
require(here)
dat_habitat = read.csv(here("data", "hab.sta.csv"))
hist(dat_habitat$elev, xlab = "elevation", main = "Histogram of sampling site elevation")
hist(dat_habitat$aspect, xlab = "aspect", main = "Histogram of sampling site aspect")
hist(dat_habitat$slope, xlab = "slope", main = "Histogram of sampling site slope")
par(mfrow = c(1, 3))

plot(x = dat_habitat$elev, y = dat_habitat$ba.tot, xlab = "elevation", ylab = "total basal area", main = "Basal area and elevation")
curve(line_point_slope(x, x1 = 300, y1 = 25, slope = 0.1), add = TRUE)

plot(x = dat_habitat$aspect, y = dat_habitat$ba.tot, xlab = "aspect", ylab = "total basal area", main = "Basal area and aspect")
curve(line_point_slope(x, x1 = 175, y1 = 25, slope = 0), add = TRUE)


plot(x = dat_habitat$slope, y = dat_habitat$ba.tot, xlab = "slope", ylab = "total basal area", main = "Basal area and slope")
curve(line_point_slope(x, x1 = 40, y1 = 25, slope = 0.05), add = TRUE)

par(mfrow = c(1, 3))

# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}