# Download the data from Figshare
f <- tempfile()
download.file("https://ndownloader.figshare.com/files/2075362", f)
data <- read.csv(f, stringsAsFactors = FALSE)

# Subset the data to only one species, the hierarchical structure, and some covariates
keep_columns <- c(
  # response variable
  "Culex_modestus",
  # covariates
  "depth__cm", "salinity__ppt", "temperature__C", "bulrushes__Typha", "ivy_leafed_duckweed__Lemna_trisulca",
  # spatio-temporal structure
  "dipping_round", "field_site", "dip_site", "longitude", "latitude")
data <- data[, keep_columns]

# convert the dipping round, field and dip site IDs to integers & split one of
# the field sites in two
data$dipping_round <- match(data$dipping_round, unique(data$dipping_round))
data$field_site <- match(data$field_site, unique(data$field_site))
data$field_site <- data$field_site + as.numeric(data$longitude > 0.78)
data$dip_site <- match(data$dip_site, unique(data$dip_site))

# centre and scale the continuous environmental covariates
data$depth__cm <- as.numeric(scale(data$depth__cm))
data$salinity__ppt <- as.numeric(scale(data$salinity__ppt))
data$temperature__C <- as.numeric(scale(data$temperature__C))

# model with a *fixed* effect of dipping round
X <- model.matrix(~ depth__cm + salinity__ppt + temperature__C + factor(dipping_round), 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))
lambda <- exp(X %*% beta)
distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta)
draws <- mcmc(m)

# (ON BOARD)

# # fixed effect of dipping round
# glm(cx_modestus ~ covariates ... + factor(dipping_round)) # << FACTOR
# 
# # random effect of dipping round
# glmer(cx_modestus ~ covariates ... + (1 | dipping_round)) # <<< HIERARCHICAL

# model with a *random* effect of dipping round - but non-identifiable intercept
X <- model.matrix(~ depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

round_sd <- normal(0, 2, truncation = c(0, Inf))
round_effect <- normal(0, round_sd, dim = 4)

lambda <- exp(X %*% beta + round_effect[data$dipping_round])

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, round_sd)
draws <- mcmc(m)


library(greta)
# model with a *random* effect of dipping round - but identifiable intercept
X <- model.matrix(~ depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

round_sd <- normal(0, 2, truncation = c(0, Inf))
round_effect_2_to_4 <- normal(0, round_sd, dim = 3)
round_effect <- rbind(0, round_effect_2_to_4)

lambda <- exp(X %*% beta + round_effect[data$dipping_round])

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, round_sd)
draws <- mcmc(m)

bayesplot::mcmc_trace(draws)


# model with a *random* effect of dipping round - and no overall intercept
X <- model.matrix(~ -1 + depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

round_sd <- normal(0, 2, truncation = c(0, Inf))
round_effect <- normal(0, round_sd, dim = 4)

lambda <- exp(X %*% beta + round_effect[data$dipping_round])

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, round_sd)
draws <- mcmc(m)

bayesplot::mcmc_trace(draws)

re_draws <- calculate(round_effect, draws)
bayesplot::mcmc_trace(re_draws)

library(greta)
# model with *random* effects of dipping round and field site - and no overall intercept
X <- model.matrix(~ -1 + depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

round_sd <- normal(0, 2, truncation = c(0, Inf))
round_effect <- normal(0, round_sd, dim = 4)

field_sd <- normal(0, 2, truncation = c(0, Inf))
field_effect <- normal(0, field_sd, dim = 3)

lambda <- exp(X %*% beta + round_effect[data$dipping_round] + field_effect[data$field_site])

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, round_sd, field_sd)
draws <- mcmc(m)

bayesplot::mcmc_trace(draws)

re_draws <- calculate(round_effect, draws)
bayesplot::mcmc_trace(re_draws)

fe_draws <- calculate(field_effect, draws)
bayesplot::mcmc_trace(fe_draws)


# model with a *random* effect for the combination of dipping round and field site - and no overall intercept
X <- model.matrix(~ -1 + depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

data$field_round <- paste(data$field_site, data$dipping_round)
data$field_round <- factor(data$field_round)
data$field_round <- as.numeric(data$field_round)

field_round_sd <- normal(0, 2, truncation = c(0, Inf))
field_round_effect <- normal(0, field_round_sd, dim = 12)

lambda <- exp(X %*% beta + field_round_effect[data$field_round])

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, field_round_sd)
draws <- mcmc(m)
bayesplot::mcmc_trace(draws)

fre_draws <- calculate(field_round_effect[1:4], draws)
bayesplot::mcmc_trace(fre_draws)
summary(fre_draws)




# model with a *random* intercept and slope (on depth) for dipping round - and no overall intercept
# but there is an identifiability issue with the slopes (coefficients) for depth
X <- model.matrix(~ -1 + depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

# random intercepts for each dipping round 
round_int_sd <- normal(0, 2, truncation = c(0, Inf))
round_int_effect <- normal(0, round_int_sd, dim = 4)

# random *slopes* for each dipping round
round_coef_sd <- normal(0, 2, truncation = c(0, Inf))
round_coef_effect <- normal(0, round_coef_sd, dim = 4)

lambda <- exp(X %*% beta +
                round_int_effect[data$dipping_round] +
                round_coef_effect[data$dipping_round] * data$depth__cm)

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, round_int_sd, round_coef_sd)
draws <- mcmc(m)
bayesplot::mcmc_trace(draws)




# model with a *random* intercept and slope (on depth) for dipping round - and no overall intercept
# this time removing the fixed effect of depth to solve the identifiability issue
X <- model.matrix(~ -1 + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))

# random intercepts for each dipping round 
round_int_sd <- normal(0, 2, truncation = c(0, Inf))
round_int_effect <- normal(0, round_int_sd, dim = 4)

# random *slopes* for each dipping round
round_coef_sd <- normal(0, 2, truncation = c(0, Inf))
round_coef_effect <- normal(0, round_coef_sd, dim = 4)

lambda <- exp(X %*% beta +
                round_int_effect[data$dipping_round] +
                round_coef_effect[data$dipping_round] * data$depth__cm)

distribution(data$Culex_modestus) <- poisson(lambda)
m <- model(beta, round_int_sd, round_coef_sd)
draws <- mcmc(m)
bayesplot::mcmc_trace(draws)

rc_draws <- calculate(round_coef_effect, draws)
bayesplot::mcmc_trace(rc_draws)
summary(rc_draws)

bayesplot::mcmc_intervals(rc_draws)
MCMCvis::MCMCplot(rc_draws)  
bayesplot::mcmc_dens(rc_draws)


