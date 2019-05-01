# fitting a generalised linear model in greta

# Data for a GLM of mosquito abundance. cx_modestus gives the numbers
# of mosquito larvae collected at a series of locations alongs streams
# in the UK. depth gives the depth of those streams at those locations.

mossie_small <- data.frame(
  cx_modestus = c(0, 5, 0, 0, 5, 0, 1, 0, 0, 0, 5, 0, 1, 10, 2, 1, 0,
                  0, 0, 6, 0, 0, 0, 0, 0, 5, 6, 5, 0, 0, 0, 0, 0, 0,
                  0, 2, 57, 0, 5, 1, 0, 0, 0, 2, 0, 0, 11, 0, 0, 1,
                  0, 0),
  depth = c(-0.729, 0.889, -0.189, 1.609, -0.729, -0.189, -0.729,
            1.788, -1.52, 1.069, -0.549, 2.328, -0.01, -0.729, -0.01,
            -0.549, -0.01, 0.71, 0.35, -0.729, 2.508, 2.508, -0.729,
            1.788, 0.71, 0.53, -0.909, -0.549, -1.52, 2.328, -0.549,
            1.069, 0.71, 0.889, 1.069, -0.549, -0.729, 3.586, -0.549,
            -0.01, -0.189, 2.867, -0.01, 1.069, 1.609, -1.592, -0.549,
            -0.909, -0.01, 1.069, 2.148, -0.909)
)


a <- normal(0, 10)
b <- normal(0, 10)
lambda <- exp(a + b * mossie_small$depth)
distribution(mossie_small$cx_modestus) <- poisson(lambda)

m <- model(a, b)
plot(m)
draws <- mcmc(m)
summary(draws)

# work out what value a new thing will take
depth_0_lambda <- exp(a + b * 0)

# fixed values
calculate(depth_0_lambda, list(a = 5, b = -2))

# with posterior samples
d0lambda_draws <- calculate(depth_0_lambda, draws)
plot(d0lambda_draws)
summary(d0lambda_draws)


# do a presence-absence version

mossie_small$presence <- pmin(mossie_small$cx_modestus, 1)
a <- normal(0, 10)
b <- normal(0, 10)
prob <- ilogit(a + b * mossie_small$depth)
distribution(mossie_small$presence) <- bernoulli(prob)

m <- model(a, b)
plot(m)
draws <- mcmc(m)
summary(draws)


# checking convergence
library(greta)
a <- normal(0, 10)
b <- normal(0, 10)
lambda <- exp(a + b * mossie_small$depth)
distribution(mossie_small$cx_modestus) <- poisson(lambda)

m <- model(a, b)
draws <- mcmc(m, n_samples = 5000)

# visually check convergence - hairy caterpillars
plot(draws)

coda::gelman.diag(draws, multivariate = FALSE)
coda::effectiveSize(draws)

summary(draws)


# prior predictive check for logistic regression
# y ~ bernoulli(p)
# p = ilogit(a)
# a ~ normal(0, 10)
as <- rnorm(1000, 0, 3/2)
ps <- plogis(as)
hist(ps)

# get residuals for poisson glm of cx modestus
residuals <- mossie_small$cx_modestus - lambda

residuals_draws <- calculate(residuals, draws)
residuals_draws_mat <- as.matrix(residuals_draws)
residuals_mean <- colMeans(residuals_draws_mat)

