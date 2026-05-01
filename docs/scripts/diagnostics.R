# Diagnostics and reporting scaffold
#
# Recommended input: a single long table of estimates from each workflow.
# Each *_results table should use the same result schema:
# - domain_id
# - method
# - estimate
# - se
# - lower
# - upper
# - uncertainty_type
#
# Examples:
# - direct_results
# - bayesian_fh_results
# - spatial_fh_results
# - unit_level_results
# - truth_results: optional domain_id, truth for teaching/evaluation examples

library(dplyr)
library(ggplot2)

estimate_results <- bind_rows(
  direct_results,
  bayesian_fh_results,
  spatial_fh_results,
  unit_level_results
)

direct_baseline <- estimate_results |>
  filter(method == "Direct") |>
  select(domain_id, direct = estimate, direct_se = se)

comparison_table <- estimate_results |>
  left_join(direct_baseline, by = "domain_id") |>
  mutate(
    difference_from_direct = estimate - direct,
    interval_width = upper - lower
  )

direct_vs_model_plot <- comparison_table |>
  filter(method != "Direct", !is.na(direct)) |>
  ggplot(aes(x = direct, y = estimate, colour = method)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  labs(
    x = "Direct estimate",
    y = "Model-based estimate",
    title = "Direct versus model-based estimates"
  )

uncertainty_plot <- ggplot(
  comparison_table,
  aes(x = interval_width, fill = method)
) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(
    x = "Uncertainty interval width",
    y = "Number of domains",
    title = "Distribution of uncertainty by method"
  )
