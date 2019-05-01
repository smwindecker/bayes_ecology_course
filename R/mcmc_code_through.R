# counts <- c(0, 4, 2)
# 
# likelihood <- function(lambda) {
#   probs <- dpois(counts, lambda)
#   prod(probs)
# }
# 
# likelihood(105.37)
# lambdas <- seq(0.001, 5, length.out = 100)
# densities <- sapply(lambdas, likelihood)
# plot(densities ~ lambdas, type = "l")


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

likelihood <- function (parameters) {
  a <- parameters[1]
  b <- parameters[2]
  lambda <- exp(a + b * mossie_small$depth)
  log_probs <- dpois(mossie_small$cx_modestus,
                     lambda,
                     log = TRUE)
  sum(log_probs)
}

prior <- function (parameters) {
  # a <- normal(0, 10)
  # b <- normal(0, 10)
  a <- parameters[1]
  b <- parameters[2]
  log_prob_a <- dnorm(a, 0, 10, log = TRUE)
  log_prob_b <- dnorm(b, 0, 10, log = TRUE)
  log_prob_a + log_prob_b
}

posterior <- function (parameters) {
  log_likelihood <- likelihood(parameters)
  log_prior <- prior(parameters)
  log_likelihood + log_prior
}

propose <- function (parameters, step_size = 1) {
  a <- parameters[1]
  b <- parameters[2]
  proposed_a <- rnorm(1, a, step_size)
  proposed_b <- rnorm(1, b, step_size)
  c(proposed_a, proposed_b)
}

accept <- function(proposed_parameters, old_parameters) {
  log_posterior_old <- posterior(old_parameters)
  log_posterior_proposed <- posterior(proposed_parameters)
  log_ratio <- log_posterior_proposed - log_posterior_old
  log(runif(1)) < log_ratio
}

rwm <- function (initial_parameters, n_iter = 1000, step_size = 1) {
  
  trace <- matrix(NA, nrow = n_iter, ncol = 2)
  parameters <- initial_parameters
  
  for (iter in 1:n_iter) {
    
    proposed_parameters <- propose(parameters, step_size = step_size)
    accepted <- accept(proposed_parameters, parameters)
    
    if (accepted) {
      parameters <- proposed_parameters
    }
    
    trace[iter, ] <- parameters
    
  }
  
  trace
  
}


draws <- rwm(c(-1, 2), n_iter = 10000, step_size = 0.1)
plot(draws[, 1], type = "l")
plot(draws[, 2], type = "l")

