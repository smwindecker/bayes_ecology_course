
# Download the data from Figshare
f <- tempfile()
download.file("https://ndownloader.figshare.com/files/2075362", f)
data <- read.csv(f, stringsAsFactors = FALSE)

# Subset the data to only one species, the hierarchical structure, and some covariates
keep_columns <- c(
  # response variables
  "Culex_modestus", "Culex_pipiens_sl",
  "Culiseta_annulata", "Anopheles_maculipennis_sl",
  # covariates
  "depth__cm", "salinity__ppt", "temperature__C")
data <- data[, keep_columns]

# centre and scale the continuous environmental covariates
data$depth__cm <- as.numeric(scale(data$depth__cm))
data$salinity__ppt <- as.numeric(scale(data$salinity__ppt))
data$temperature__C <- as.numeric(scale(data$temperature__C))

X <- model.matrix(~ depth__cm + salinity__ppt + temperature__C,
                  data = data)

# multiple response variables
Y <- as.matrix(data[, 1:4])

# make this presence absence
Y[] <- pmin(Y[], 1)

library(greta)

# beta for each species and covariate
beta <- normal(0, 10, dim = c(ncol(X), ncol(Y)))

prob <- ilogit(X %*% beta)

distribution(Y) <- bernoulli(prob)

m <- model(beta)
draws <- mcmc(m)
bayesplot::mcmc_trace(draws)

expected_richness <- rowSums(prob)
rich_draws <- calculate(expected_richness[1], draws)
bayesplot::mcmc_combo(rich_draws)

