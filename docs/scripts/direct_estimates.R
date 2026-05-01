# Direct estimation
#
# Input objects:
# - survey_indicator: unit_id, domain_id, indicator, weight, optional stratum
# - domain_sizes: domain_id, domain_size

library(dplyr)
library(sae)
library(survey)

# Simple SAE-oriented direct estimates.
# sae::direct() returns Domain, SampSize, Direct, SD, and CV.

direct_fit <- sae::direct(
  y = survey_indicator$indicator,
  dom = survey_indicator$domain_id,
  sweight = survey_indicator$weight,
  domsize = domain_sizes[, c("domain_id", "domain_size")],
  replace = FALSE
)

direct_results <- direct_fit |>
  rename(domain_id = Domain) |>
  transmute(
    domain_id,
    method = "Direct",
    estimate = Direct,
    se = SD,
    lower = Direct - 1.96 * SD,
    upper = Direct + 1.96 * SD,
    uncertainty_type = "confidence_interval",
    variance = SD^2,
    rse = CV
  )

# -------------------------------------------------------------------------
# Complex survey-design (e.g., stratification)
# Uses the same survey_indicator schema.

survey_design <- survey::svydesign(
  ids = ~1,
  weights = ~weight,
  strata = ~stratum,  # remove this line if no stratum column is available
  data = survey_indicator,
  nest = TRUE 
)

direct_design_results <- survey::svyby(
  formula = ~indicator,
  by = ~domain_id,
  design = survey_design,
  FUN = svymean,  # 'svymean' for weighted mean/proportion, 'svytotal' for weighted totals
  vartype = c("se", "ci"),
  na.rm = TRUE
)

# Standardise results for diagnostics
direct_design_results <- direct_design_results |>
  as.data.frame() |>
  transmute(
    domain_id,
    method = "Direct survey design",
    estimate = indicator,
    se = se,
    lower = ci_l,
    upper = ci_u,
    uncertainty_type = "confidence_interval",
    variance = se^2,
    rse = NA_real_
  )
