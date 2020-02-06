## geoR and geoRglm

library(geoR)
library(geoRglm)

data("p50")
data("b50")

#First we use the geoR function grf to generate a simulation from a Gaussian random field.
x = seq(1, 10, l = 10)
y = seq(1, 10, l = 10)

sim.g <- grf(grid = expand.grid(x, +
        y), cov.pars = c(0.1, 0.2))

sim.g

sim <- list(coords = sim.g$coords, units.m = c(rep(1,
                                                   +     50), rep(5, 50)))

sim

attr(sim, "class") <- "geodata"
sim$data <- rpois(100, lambda = sim$units.m * exp(sim.g$data))

plot(sim$coords[, 1], sim$coords[, 2], type = "n")
 text(sim$coords[, 1], sim$coords[, 2], format(sim$data)) # interessenta

#The core part of geoRglm consist of generating MCMC simulations from
#the conditional distibution of the random effects at the data locations given
#the  actual  observed  data.   Such  a  simulation  algorithm  is  needed  for  anylikelihood inference in generalised linear spatial models (prediction, Bayesian
#inference and parameter estimation


model2 <- list(cov.pars = c(1, 1), beta = 1, family = "poisson")
mcmc2.test <- mcmc.control(S.scale = 0.2, thin = 1)
test2.tune <- glsm.mcmc(p50, model = model2, mcmc.input = mcmc2.test)

mcmc2.tune <- mcmc.control(S.scale = 0.5, thin = 1)
test2.tune <- glsm.mcmc(p50, model = model2, mcmc.input = mcmc2.tune)

library(coda)
test2.tune.c <- create.mcmc.coda(test2.tune, mcmc.input = mcmc2.tune)

test2.tune.c <- create.mcmc.coda(test2.tune$simulations[45,]
+     , mcmc.input = list(S.scale = 0.5, thin = 1))
 par(mfrow = c(1, 2))
 plot(test2.tune.c, density = FALSE, ask = FALSE, auto.layout = FALSE)
 autocorr.plot(test2.tune.c, ask = FALSE, auto.layout = FALSE)


out2 <- output.glm.control(sim.predict = TRUE)
 pred.test2 <- glsm.krige(test2, locations = cbind(c(0.5,
+     0.5), c(1, 0.4)), output = out2)


## exemplo da função glsm.mcmc do help
if(!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) set.seed(1234)
data(b50)
test <- glsm.mcmc(b50, model = list(family="binomial",
             cov.pars = c(1,1), beta = c(1,0), trend =~ rnorm(50),
             cov.model="spherical", nugget=0.3),
          mcmc.input = mcmc.control(S.scale = 0.2, thin = 1))
## visulalising the MCMC output using the coda package
test.coda <- create.mcmc.coda(test, mcmc.input = list(thin = 1))
library(coda)
## Not run: 
plot(test.coda)
autocorr.plot(test.coda) 


