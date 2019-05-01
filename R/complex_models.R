
# BASIC MODEL - NO COVARS. NO RE
# log(mass) ~ normal(mu, sd_obs)
# mu = log(init_mass) - (time / beta)^alpha
# alpha = exp(int_alpha)
# beta = exp(int_beta)

# sd_obs ~ N+(0, 2)
# int_alpha ~ N(0, 2)
# int_beta ~ N(0, 2)

sd_obs <- normal(0, 2, truncation = c(0, Inf))
int_alpha <- normal(0, 2)
int_beta <- normal(0, 2)

alpha <- exp(int_alpha)
beta <- exp(int_beta)

mu <- decay_data$log_mass_init - 
  (decay_data$time / beta)^alpha

distribution(decay_data$log_mass) <- normal(mu, sd_obs)

# WITH TRAITS
# log(mass) ~ normal(mu, sd_obs)
# mu = log(init_mass) - (time / beta)^alpha
# alpha = exp(coefs_alpha * X)
# beta = exp(coefs_beta * X)

# sd_obs ~ N+(0, 2)
# coefs_alpha ~ N(0, 2)
# coefs_beta ~ N(0, 2)

X <- model.matrix(~ LG + N, data = decay_data)

sd_obs <- normal(0, 20, truncation = c(0, Inf))
coefs_alpha <- normal(0, 2, dim = ncol(X))
coefs_beta <- normal(0, 2, dim = ncol(X))

alpha <- exp(X %*% coefs_alpha)
beta <- exp(X %*% coefs_beta)

mu <- decay_data$log_mass_init - 
  (decay_data$time / beta)^alpha

distribution(decay_data$log_mass) <- normal(mu, sd_obs)

# WITH TRAITS AND RE
# log(mass) ~ normal(mu, sd_obs)
# mu = log(init_mass) - (time / beta)^alpha
# alpha = exp(coefs_alpha * X + species_int_alpha)
# beta = exp(coefs_beta * X + species_int_beta)

# sd_obs ~ N+(0, 2)
# coefs_alpha ~ N(0, 2)
# coefs_beta ~ N(0, 2)
# species_int_alpha ~ N(0, species_sd_alpha)
# species_int_beta ~ N(0, species_sd_beta)
# species_sd_alpha ~ N(0, 2)
# species_sd_beta ~ N(0, 2)

n_species <- length(unique(decay_data$species_code))
traits <- as.matrix(decay_data[, c('LG', 'N')])

sd_obs <- normal(0, 2, truncation = c(0, Inf))

species_sd_alpha <- normal(0, 2, truncation = c(0, Inf))
species_sd_beta <- normal(0, 2, truncation = c(0, Inf))
species_int_alpha <- normal(0, species_sd_alpha, dim = n_species)
species_int_beta <- normal(0, species_sd_beta, dim = n_species)

coefs_alpha <- normal(0, 2, dim = ncol(traits))
coefs_beta <- normal(0, 2, dim = ncol(traits))

alpha <- exp(traits %*% coefs_alpha + 
               species_int_alpha[decay_data$species_level])

beta <- exp(traits %*% coefs_beta + 
              species_int_beta[decay_data$species_level])

mu <- decay_data$log_mass_init - 
  (decay_data$time / beta)^alpha

distribution(decay_data$log_mass) <- normal(mu, sd_obs)

