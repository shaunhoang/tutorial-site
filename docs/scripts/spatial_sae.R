# Bayesian area-level SAE example
#
# Expected inputs:
# - direct_results: domain_id, Direct, Variance
# - domain_covariates: domain_id and area-level covariates
# - adjacency_matrix: optional spatial weights/adjacency matrix

library(SUMMER)

direct_est <- direct_results[, c("domain_id", "Direct", "Variance")]

bayes_area_fit <- SUMMER::smoothArea(
  formula = Direct ~ x_income + x_car_ownership + x_urban,
  domain = ~domain_id,
  direct.est = direct_est,
  X.domain = domain_covariates,
  adj.mat = adjacency_matrix, # set NULL for non-spatial model
  transform = "identity",     # no transformation of estimates
  level = 0.95
)
