# Frequentist area-level SAE example
#
# Expected inputs:
# - direct_results: domain_id, Direct, Variance
# - domain_covariates: domain_id and area-level covariates

library(sae)

fh_data <- merge(
  direct_results,
  domain_covariates,
  by = "domain_id"
)

fh_fit <- sae::eblupFH(
  formula = Direct ~ x_income + x_car_ownership + x_urban,
  vardir = fh_data$Variance,
  data = fh_data
)

fh_mse <- sae::mseFH(
  formula = Direct ~ x_income + x_car_ownership + x_urban,
  vardir = fh_data$Variance,
  data = fh_data
)

fh_results <- data.frame(
  domain_id = fh_data$domain_id,
  direct = fh_data$Direct,
  eblup = fh_fit$eblup,
  mse = fh_mse$mse,
  rmse = sqrt(fh_mse$mse)
)
