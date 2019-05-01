# Visualising parameters and prediction

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

# Poisson big mossie data

X <- model.matrix(~ depth__cm + salinity__ppt + temperature__C, 
                  data = data)
beta <- normal(0, 10, dim = ncol(X))
lambda <- exp(X %*% beta)
distribution(data$Culex_modestus) <- poisson(lambda)

m <- model(beta)
plot(m)

draws <- mcmc(m)

# forest plot
bayesplot::mcmc_intervals(draws)

# effect/response plot
depth_pred <- seq(min(data$depth__cm),
                  max(data$depth__cm),
                  length.out = 100)
salinity_mid <- rep(mean(data$salinity__ppt), 100)
temperature_mid <- rep(mean(data$temperature__C), 100)

X_depth <- data.frame(int = rep(1, 100),
                      depth_pred = depth_pred,
                      salinity_mid = salinity_mid,
                      temperature_mid = temperature_mid)

lambda_depth <- exp(X_depth %*% beta)
lambda_depth_draws <- calculate(lambda_depth, draws)
lambda_depth_draws_mat <- as.matrix(lambda_depth_draws)

lambda_depth_mean <- colMeans(lambda_depth_draws_mat)
lambda_depth_lower <- apply(lambda_depth_draws_mat, 2, 
                            quantile, 0.025)
lambda_depth_upper <- apply(lambda_depth_draws_mat, 2, 
                            quantile, 0.975)

plot(X_depth$depth_pred, lambda_depth_mean, 
     type = 'l', ylim = c(0, 6))
lines(X_depth$depth_pred, lambda_depth_lower,
      lty = 2)
lines(X_depth$depth_pred, lambda_depth_upper,
      lty = 2)
points(data$depth__cm, data$Culex_modestus,
       pch = 20, col = 'grey', cex = 0.7)
