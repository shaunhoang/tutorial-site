# Minimal Fay-Herriot model helper.

library(dplyr)
library(readr)
library(sae)

fit_fay_herriot <- function(direct_file,
                            covariate_file,
                            domain_id,
                            formula,
                            direct_col = "direct",
                            variance_col = "variance") {
  direct_data <- read_csv(direct_file, show_col_types = FALSE)
  covariate_data <- read_csv(covariate_file, show_col_types = FALSE)

  model_data <- direct_data |>
    left_join(covariate_data, by = domain_id)

  fit <- eblupFH(
    formula = formula,
    vardir = model_data[[variance_col]],
    data = model_data
  )

  model_data |>
    mutate(
      fh_estimate = fit$eblup$eblup,
      fh_mse = fit$mse$mse
    )
}
