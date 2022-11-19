#Q1
exp_fun = function(x, a, b) 
{
return(a * exp(-b * x))
}
#Q2
curve(
  exp_fun(x, 1.9, 0.1), add = FALSE, from = 0, to = 50,
  ylab = "f(x)", col = "black", lty = "solid")

curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, from = 0, to = 50,
  ylab = "f(x)", col = "black", lty = "dotted")

curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, from = 0, to = 50,
  ylab = "f(x)", col = "red", lty = "solid")

curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, from = 0, to = 50,
  ylab = "f(x)", col = "red", lty = "dotted")
#Q3
#The starting height of the curve varies as I vary parameter a. (bigger a - higher starting height)
#Q4
#The rate of decay varies as I vary parameter b. (bigger b - faster decay)
#Q5
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
curve(
  ricker_fun(x, 25, 0.2), add = FALSE, from = 0, to = 50, ylim = c(0, 100),
  ylab = "f(x)", col = "black", lty = "solid")
curve(
  ricker_fun(x, 20, 0.2), add = TRUE, from = 0, to = 50, ylim = c(0, 100),
  ylab = "f(x)", col = "black", lty = "dotted")
curve(
  ricker_fun(x, 10, 0.2), add = TRUE, from = 0, to = 50, ylim = c(0, 100),
  ylab = "f(x)", col = "black", lty = "dotted")
curve(
  ricker_fun(x, 25, 0.3), add = TRUE, from = 0, to = 50, ylim = c(0, 100),
  ylab = "f(x)", col = "red", lty = "solid")
curve(
  ricker_fun(x, 50, 0.3), add = TRUE, from = 0, to = 50, ylim = c(0, 100),
  ylab = "f(x)", col = "red", lty = "dotted")
curve(
  ricker_fun(x, 40, 0.3), add = TRUE, from = 0, to = 50, ylim = c(0, 100),
  ylab = "f(x)", col = "red", lty = "dotted")
#Q6
#The initial slope varies as I vary parameter a. (bigger a - bigger slope)
#Q7
#The highest point of the curve varies as I vary parameter b. (bigger b - lower highest point)
#Q8
###linear model
dat_dispersal<-read.csv("/Users/stonehuang/Documents/environmental_data/dispersal.csv")
plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\nlinear model")
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
curve(line_point_slope(x, 500, 0.4, -0.0005), add = TRUE)
###exponential model
plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\nexponential model")
curve(exp_fun(x, 1, 0.003), add = TRUE)
###ricker model
plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\nricker model")
curve(ricker_fun(x, 0.0015, 0.0025), add = TRUE)

resids_linear =  dat_dispersal$disp.rate.ftb - line_point_slope(dat_dispersal$dist.class, 500, 0.4, -0.0005)
resids_exp = dat_dispersal$disp.rate.ftb - exp_fun(dat_dispersal$dist.class, 1, 0.003)
resids_ricker = dat_dispersal$disp.rate.ftb - ricker_fun(dat_dispersal$dist.class, 0.0015, 0.0025)
df = data.frame(resids_linear, resids_exp, resids_ricker)                                                    
df

hist(resids_linear)
hist(resids_exp)
hist(resids_ricker)
