# Unit-level SAE example
#
# Expected inputs:
# - survey_microdata: indicator, domain_id, weight, design variables, covariates
# - population_microdata: domain_id and matching unit-level covariates

library(survey)
library(SUMMER)

design <- survey::svydesign(
  ids = ~psu,
  strata = ~stratum,
  weights = ~weight,
  data = survey_microdata,
  nest = TRUE
)

unit_fit <- SUMMER::smoothUnit(
  formula = indicator ~ age_group + employment_status + car_access,
  domain = ~domain_id,
  design = design,
  family = "binomial",
  X.pop = population_microdata,
  adj.mat = NULL,
  level = 0.95
)
