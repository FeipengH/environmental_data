library(here)
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))
birdhab = merge(
  dat_bird,
  dat_habitat,
  by = c("basin", "sub"))
dim(birdhab)

plot(BRCR ~ ls, data = birdhab)
fit_1 = lm(BRCR ~ ls, data = birdhab)
abline(fit_1)
summary(fit_1)
#Deterministic Model: Linear Function
linear = function(x, y_int, slope)
{
  y = y_int + slope * x
  return(y)
}

linear_simulator = function(x, y_int, slope, st_dev)
{
  y = linear(x, y_int, slope)
  y_2 = y + rnorm(length(x), mean = 0, st_dev)
  return(y_2)
}

n = 200

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x,
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

n = 400

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2),
    axes = FALSE)
  box()
}

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
str(fit_1_summary)
######
int_obs = fit_1_coefs [1]
slope_obs = fit_1_coefs [2]
sd_obs = fit_1_summary$sigma


alpha = 0.05
n_sims = 30
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_powers = numeric(n_sds)

linear_sim_fit = function(x, slope, y_int, st_dev)
{
  y_sim = linear_simulator(
    x = x,
    y_int = y_int,
    slope = slope,
    st_dev = st_dev
  )
  fit_sim = lm(y_sim ~ x)
  return(fit_sim)
}

for(j in 1:length(pop_sds))
{
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = pop_sds[j]
    )
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  pop_sd_powers[j] = sum(p_vals < alpha) / n_sims
}

sim_output_dispersion = data.frame(
  sd = pop_sds,
  power = pop_sd_powers)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(power ~ sd, data = sim_output_dispersion, type = 'l', xlab = 'Population dispersion', ylab = 'Statistical power', main = "Population Dispersion Simulation, reps = 30")

fit_lowess_30 = loess(power ~ sd, data = sim_output_dispersion, span = 0.3)
newdata_sd = data.frame(sd = seq(0, 1.5, length.out = 100)) 
plot(
  x = newdata_sd$sd,
  y = predict(fit_lowess_30, newdata = newdata_sd),
  type = "l",
  ylab = "Statistical power", xlab = "Population dispersion",
  main = "Population Dispersion/Power Simulation 
  LOWESS: 30%")
points(power ~ sd, data = sim_output_dispersion, pch = 16, col = "blue")
legend("topright", legend=c("Smoothed", "Original"), lty = c(1,0), pch = c(-1,16),
       col=c("black", "blue"), )








dat_dispersal<-read.csv("/Users/stonehuang/Documents/environmental_data/data/dispersal.csv")
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
fit_ricker_nls = nls(
  disp.rate.ftb ~ ricker_fun(dist.class, a, b),
  data = dat_dispersal,
  start = list(b = 0, a = 1))
summary(fit_ricker_nls)

plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  pch = 16,
  col = "blue",
  xlim = c(0, 1500),
  xlab = "Distance class", 
  ylab = "Standardized dispersal rate", 
  main = "Plot of Marbled Salamander - first time breeders")

dist_newdata = data.frame(dist.class = seq(0, 1600, length.out = 1600))  
lines(predict(fit_ricker_nls, newdata = dist_newdata))
curve(ricker_fun(x, 0.0015, 0.0025), add = TRUE, lty = 2, col = "red")
legend("topright", legend = c("guess", "nls"), lty = c(2,1) , col = c("red", "black"))

dat_bird = read.csv(here("data", "bird.sta.csv"))
dat_habitat = read.csv(here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
dat_all$GCKI_pres = dat_all$GCKI > 0

fit_GCKI_slope = glm(GCKI_pres ~ slope, data = dat_all, family = binomial)
fit_GCKI_ba_tot = glm(GCKI_pres ~ ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_additive = glm(GCKI_pres ~ slope + ba.tot, data = dat_all, family = binomial)
fit_GCKI_both_interactive = glm(GCKI_pres ~ slope * ba.tot, data = dat_all, family = binomial)

AIC(fit_GCKI_slope)
AIC(fit_GCKI_ba_tot)
AIC(fit_GCKI_both_additive)
AIC(fit_GCKI_both_interactive)
summary(fit_GCKI_both_interactive)

###Logistic Models Graphs###
n = 500
slope_newdata = data.frame(
  slope = seq(
    from = min(dat_all$slope, na.rm = T),
    to = max(dat_all$slope, na.rm = T),
    length.out = n
  )
)
ba_newdata = data.frame(
  ba.tot = seq(
    from = min(dat_all$ba.tot, na.rm = T),
    to = max(dat_all$ba.tot, na.rm = T),
    length.out = n
  )
)
#I added the predicted values as new columns to the data.frame objects.
#You need to use the type = "response" argument so that the predicted values are Pr(present), i.e. that the predicted values are on the scale of probability.
slope_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_slope,
    newdata = slope_newdata,
    type = "response"
  )

ba_newdata$GCKI_predicted = 
  predict(
    fit_GCKI_ba_tot,
    newdata = ba_newdata,
    type = "response"
  )

# Presence/absence data, translucent points:
plot(
  GCKI_pres ~ slope, data = dat_all,
  xlab = "Percent Slope",
  ylab = "GCKI presence/absence",
  main = "GCKI Slope Model Plot",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)
lines(GCKI_predicted ~ slope, data = slope_newdata)

plot(
  GCKI_pres ~ ba.tot, data = dat_all,
  xlab = "Basal Area",
  ylab = "GCKI presence/absence",
  main = "GCKI Basal Area Model Plot",
  pch = 16, cex = 1.5, col = gray(0, 0.2)
)
lines(GCKI_predicted ~ ba.tot, data = ba_newdata)

##Plotting Both Parameters##
n = 50

ba.tot = seq(
  from = min(dat_all$ba.tot, na.rm = T),
  to = max(dat_all$ba.tot, na.rm = T),
  length.out = n)
slope = seq(
  from = min(dat_all$slope, na.rm = T),
  to = max(dat_all$slope, na.rm = T),
  length.out = n)

new_dat_all = expand.grid(
  ba.tot = ba.tot,
  slope = slope)

new_dat_all$pred_add = predict(
  fit_GCKI_both_additive,
  newdata = new_dat_all,
  type = "response")

new_dat_all$pred_int = predict(
  fit_GCKI_both_interactive,
  newdata = new_dat_all,
  type = "response")

z_GCKI_add = matrix(
  new_dat_all$pred_add,
  nrow = length(ba.tot),
  byrow = FALSE)
z_GCKI_int = matrix(
  new_dat_all$pred_int,
  nrow = length(ba.tot),
  byrow = FALSE)

par(mfrow = c(1, 2))
contour(
  x = ba.tot, y = slope,
  z = z_GCKI_add,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Additive")
contour(
  x = ba.tot,
  y = slope,
  z = z_GCKI_int,
  xlab = "Total Basal Area",
  ylab = "Percent Slope",
  main = "Interactive")
