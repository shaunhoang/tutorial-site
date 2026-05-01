# Direct estimation examples
#
# Expected inputs:
# - survey_indicator: one row per sampled unit with indicator, domain_id, weight
# - domain_sizes: domain sizes used by sae::direct()

library(sae)

direct_fit <- sae::direct(
  y = survey_indicator$indicator,
  dom = survey_indicator$domain_id,
  sweight = survey_indicator$weight,
  domsize = domain_sizes,
  replace = FALSE # without replacement
)

direct_results <- direct_fit |>
  dplyr::rename(domain_id = Domain) |>
  dplyr::mutate(Variance = SD^2)

# Complex survey design route, if design variables are available.

library(survey)

design <- survey::svydesign(
  ids = ~psu,
  strata = ~stratum,
  weights = ~weight,
  data = survey_indicator,
  nest = TRUE
)

direct_design_fit <- survey::svyby(
  formula = ~indicator,
  by = ~domain_id,
  design = design,
  FUN = svymean,
  vartype = c("se", "ci"),
  na.rm = TRUE
)
