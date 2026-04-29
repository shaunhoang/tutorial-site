# Helper for transparent domain-level direct estimates.

library(dplyr)
library(readr)
library(rlang)

calculate_direct_estimates <- function(survey_file,
                                       domain_file,
                                       indicator,
                                       domain_id,
                                       weight,
                                       domain_size) {
  survey_data <- read_csv(survey_file, show_col_types = FALSE)
  domain_data <- read_csv(domain_file, show_col_types = FALSE)

  indicator <- sym(indicator)
  domain_id <- sym(domain_id)
  weight <- sym(weight)
  domain_size <- sym(domain_size)

  survey_data |>
    left_join(domain_data, by = as_string(domain_id)) |>
    group_by(!!domain_id) |>
    summarise(
      n = n(),
      non_missing_n = sum(!is.na(!!indicator)),
      domain_size = first(!!domain_size),
      direct = sum((!!weight) * (!!indicator), na.rm = TRUE) / first(!!domain_size),
      variance = sum((!!weight)^2 * ((!!indicator) - direct)^2, na.rm = TRUE) /
        first(!!domain_size)^2,
      se = sqrt(variance),
      lower = direct - 1.96 * se,
      upper = direct + 1.96 * se,
      .groups = "drop"
    )
}
