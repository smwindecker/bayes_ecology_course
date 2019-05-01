
# wiggly data
df <- data.frame(x = seq(0, 10, length.out = 100))
int_true <- -1
sd_true <- 0.3
mu_true <- int_true + sin(df$x)
df$y <- rnorm(100, mu_true, sd_true)
plot(df)

library(greta)

# polynomial regression
X <- model.matrix(~ poly(x, 5), data = df)
beta <- normal(0, 10, dim = ncol(X))

sd <- normal(0, 2, truncation = c(0, 2))
mu <- X %*% beta
distribution(df$y) <- normal(mu, sd)

m <- model(beta)
draws <- mcmc(m)

mu_draws <- calculate(mu, draws)
mu_draws_mat <- as.matrix(mu_draws)
mu_mean <- colMeans(mu_draws_mat)


plot(df)
lines(mu_mean ~ df$x)


# devtools::install_github("goldingn/greta.gam")
library(greta.gam)

sm <- smooths(~ s(x), data = df)
smooth_effect <- sm$X %*% sm$betas
# smooth_effect <- with(sm, X %*% betas)

mu <- smooth_effect
sd <- normal(0, 2, truncation = c(0, Inf))
distribution(df$y) <- normal(mu, sd)
m <- model(sd)
draws <- mcmc(m)

bayesplot::mcmc_trace(draws)
library(greta)
se_draws <- calculate(smooth_effect, draws)
se_draws_mat <- as.matrix(se_draws)
se_mean <- colMeans(se_draws_mat)
se_lower <- apply(se_draws_mat, 2, quantile, 0.025)
se_upper <- apply(se_draws_mat, 2, quantile, 0.975)

plot(df)
lines(mu_mean ~ df$x, lty = 2)

lines(se_mean ~ df$x)
lines(se_lower ~ df$x, lty = 3)
lines(se_upper ~ df$x, lty = 3)
