# Simulating data

rnorm(n = 10)
rnorm(10, 5, 5)

# set.seed() ensures reproducibility of our random number 
# generation

set.seed(1)
rnorm(10)
set.seed(1)
rnorm(10)

# simulate a linear model
# M_t ~ N(mu_t, sigma2)
# mu_t = kt + a

set.seed(1)

# number of data points
n <- 10

# parameters
# install.packages("truncnorm")
a <- 100
k <- -0.2
sigma <- 20

# data
t <- seq(0, 300, length.out = n)

mu_t <- k*t + a
M_t <- rnorm(n, mu_t, sigma)

plot(t, mu_t, ylim = c(0, 100), pch = 20, cex = 0.5)
plot(t, M_t, ylim = c(0, 100), pch = 20, cex = 0.5)

summary(M_t)

decomp <- data.frame(mass = M_t, 
                     time = t)

# Bayesian linear regression
library(greta)

# priors
a <- normal(0, 100)
k <- normal(0, 100)
sigma <- normal(0, 100, truncation = c(0, Inf))

mu_t <- k*decomp$time + a

distribution(decomp$mass) <- normal(mu_t, sigma)

m <- model(a, k, sigma)
plot(m)

draws <- mcmc(m)
plot(draws)
bayesplot::mcmc_dens(draws)

summary(draws)

# Bayesian linear regression with informative priors

# priors
a <- normal(100, 10, truncation = c(0, Inf)) # truncated positive
k <- normal(0, 5, truncation = c(-Inf, 0)) # truncated negative
sigma <- normal(0, 2, truncation = c(0, Inf))

mu_t <- k*decomp$time + a

distribution(decomp$mass) <- normal(mu_t, sigma)
m <- model(a, k, sigma)
plot(m)

draws <- mcmc(m)
plot(draws)

# mtcars example 1 - single linear regression
# without design matrix

mpg_sd <- lognormal(0, 1)
int <- normal(0, 100)
coef_hp <- normal(0, 100)
mpg_mean <- int + coef_hp * mtcars$hp
distribution(mtcars$mpg) <- normal(mpg_mean, mpg_sd)

m <- model(int, coef_hp, mpg_sd)
draws <- mcmc(m)

summary(draws)

# mtcars example 2 multiple linear regression
# without design matrix

mpg_sd <- lognormal(0, 1)
int <- normal(0, 100)
coef_hp <- normal(0, 100)
coef_wt <- normal(0, 100)
mpg_mean <- int + coef_hp * mtcars$hp + coef_wt * mtcars$wt
distribution(mtcars$mpg) <- normal(mpg_mean, mpg_sd)

# mtcars example 2 multiple linear regression
# with design matrix

mpg_sd <- lognormal(0, 1)
X <- model.matrix(~ hp + wt, data = mtcars)
# X <- model.matrix(~ mtcars$hp + mtcars$wt)
betas <- normal(0, 100, dim = ncol(X))
# if each parameter has a different mean and sd
betas <- normal(c(0, 100, 0), c(100, 50, 20), dim = ncol(X))
mpg_mean <- X %*% betas
distribution(mtcars$mpg) <- normal(mpg_mean, mpg_sd)

m <- model(betas)
draws <- mcmc(m)
summary(draws)


# modelling factors with and without contrasts

# with setosa as a contrast
X <- model.matrix(~ Petal.Width + Species, data = iris)

# with a separate intercept for each species
X <- model.matrix(~ -1 + Petal.Width + Species, data = iris)


# interactions between continuous and categorical variables

# with contrasts
X <- model.matrix(~ Petal.Width + Species + Petal.Width * Species,
                  data = iris)

# without contrasts
X <- model.matrix(~ -1 + Petal.Width + Species + Petal.Width * Species,
                  data = iris)

# without contrasts (it's guessing we want the non-interaction terms)
X <- model.matrix(~ -1 + Petal.Width * Species,
                  data = iris)

# without contrasts and telling it we don't want the non-interaction terms
X <- model.matrix(~ -1 + Petal.Width * Species - Petal.Width - Species,
                  data = iris)


# how to have a vector of parameters, from separate parameters
library(greta)
beta1 <- normal(0, 100)
beta2 <- normal(0, 100)
beta3 <- normal(0, 100)
beta4 <- normal(0, 100)

# betas <- normal(0, 100, dim = 4)
betas2 <- rbind(beta1, beta2, beta3, beta4)


# answer to the Exercise on modelling iris data

# design matrix
X <- model.matrix(~ Petal.Width + Species, data = iris)

# parameters and their priors
betas <- normal(0, 10, dim = ncol(X))
sl_sd <- exponential(1/5)

sl_mean <- X %*% betas
distribution(iris$Sepal.Length) <- normal(sl_mean, sl_sd)

m <- model(betas)
draws <- mcmc(m)
bayesplot::mcmc_combo(draws)
colnames(X)
