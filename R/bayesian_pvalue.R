
# manually compute a Bayesian p-value for the alternative hypothesis that slope is negative
str(draws, 1)
draws_mat <- as.matrix(draws)
head(draws_mat)
mean(draws_mat > 0)

# use like this
# install.packages("MCMCvis")
library(MCMCvis)

# function to get a Bayesian p-value for a parameter
pval <- function (x) {
  hypothesised <- 0
  est <- mean(x)
  if (est > hypothesised) mean(x < hypothesised)
  else mean(x > hypothesised)
}

MCMCsummary(draws, func = pval, func_name = "p-value")
