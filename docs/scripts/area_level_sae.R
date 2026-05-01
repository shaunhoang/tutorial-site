# Bayesian area-level SAE + Spatial
#
# Input objects:
# - direct_results: domain_id, estimate, variance
# - domain_covariates: domain_id, covariate_* columns
# - boundaries: domain_id, domain_name, geometry

library(dplyr)
library(sf)
library(spdep)
library(SUMMER)

# Prepare direct estimates for SUMMER::smoothArea().

direct_est <- direct_results |>
  transmute(
    domain_id,
    Direct = estimate,
    Variance = variance
  )

# Bayesian FH without spatial structure.

bayesian_fh_fit <- SUMMER::smoothArea(
  formula = Direct ~ covariate_1 + covariate_2 + covariate_3,
  domain = ~domain_id,
  direct.est = direct_est,
  X.domain = domain_covariates,
  adj.mat = NULL,
  transform = "identity",
  level = 0.95
)


================================================================
# Bayesian FH with spatial structure.
# Derive a binary adjacency matrix from boundary geometry.

boundaries_ordered <- boundaries |>
  arrange(domain_id)

neighbors <- spdep::poly2nb(
  boundaries_ordered,
  queen = TRUE,       # Queen contiguity
  row.names = boundaries_ordered$domain_id
)

adjacency_matrix <- spdep::nb2mat(
  neighbors,
  style = "B",
  zero.policy = TRUE
)
rownames(adjacency_matrix) <- boundaries_ordered$domain_id
colnames(adjacency_matrix) <- boundaries_ordered$domain_id

spatial_fh_fit <- SUMMER::smoothArea(
  formula = Direct ~ covariate_1 + covariate_2 + covariate_3,
  domain = ~domain_id,
  direct.est = direct_est,
  X.domain = domain_covariates,
  adj.mat = adjacency_matrix,
  transform = "identity",
  level = 0.95
)

================================================================

# Standardise results for diagnostics
bayesian_fh_results <- bayesian_fh_fit$bym2.model.est |>
  transmute(
    domain_id,
    method = "Bayesian area-level FH",
    estimate = mean,
    se = se,
    lower = lower,
    upper = upper,
    uncertainty_type = "credible_interval"
  )

spatial_fh_results <- spatial_fh_fit$bym2.model.est |>
  transmute(
    domain_id,
    method = "Bayesian spatial FH",
    estimate = mean,
    se = se,
    lower = lower,
    upper = upper,
    uncertainty_type = "credible_interval"
  )
