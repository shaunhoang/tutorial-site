# Bayesian unit-level SAE
#
# Input objects:
# - survey_unit_data: unit_id, domain_id, indicator, weight, optional stratum,
#   plus unit-level covariate_* columns used in the model
# - population_unit_data: domain_id and the same unit-level covariate_* columns
#   for the target population or synthetic population

library(dplyr)
library(survey)
library(SUMMER)

survey_design <- survey::svydesign(
  ids = ~1,
  data = survey_unit_data,
  weights = ~weight,
  strata = ~stratum,    # or NULL
  nest = TRUE           # relevant if strata not NULL
)

unit_fit <- SUMMER::smoothUnit(
  formula = indicator ~ covariate_1 + covariate_2 + covariate_3,
  domain = ~domain_id,
  design = survey_design,
  family = "binomial",  # "binomial" for binary outcomes, "poisson" for counts/rates, and "gaussian" for continuous
  X.pop = population_unit_data,
  adj.mat = NULL,       # Add spatial adjacency matrix if relevant
  level = 0.95
)

# Standardise results for diagnostics
unit_level_results <- unit_fit$bym2.model.est |>
  transmute(
    domain_id,
    method = "Bayesian unit-level SAE",
    estimate = mean,
    se = se,
    lower = lower,
    upper = upper,
    uncertainty_type = "credible_interval"
  )
