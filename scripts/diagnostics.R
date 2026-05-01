# Diagnostics and reporting scaffold
#
# Expected inputs, when available:
# - direct_results: domain_id, Direct, Variance, SD/CV if available
# - fh_results: domain_id, direct, eblup, mse, rmse
# - bayes_results: domain_id, posterior_mean/median, credible interval columns
# - spatial_results: domain_id, posterior_mean/median, credible interval columns
# - domains_sf: optional sf object with domain geometry

library(dplyr)
library(ggplot2)

comparison_table <- direct_results |>
  transmute(
    domain_id,
    direct = Direct,
    direct_se = sqrt(Variance)
  ) |>
  left_join(
    fh_results |>
      transmute(
        domain_id,
        fh_eblup = eblup,
        fh_rmse = rmse
      ),
    by = "domain_id"
  )

direct_vs_fh_plot <- ggplot(
  comparison_table,
  aes(x = direct, y = fh_eblup)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  labs(
    x = "Direct estimate",
    y = "Frequentist FH estimate",
    title = "Direct versus model-based estimates"
  )

uncertainty_plot <- ggplot(
  comparison_table,
  aes(x = fh_rmse)
) +
  geom_histogram(bins = 30) +
  labs(
    x = "FH RMSE",
    y = "Number of domains",
    title = "Distribution of model uncertainty"
  )

# Teaching/evaluation route when true area values are available.
# In real SAE applications, replace this with external benchmarks or known totals.

if (exists("truth_results")) {
  compare_eval <- comparison_table |>
    left_join(truth_results, by = "domain_id") |>
    mutate(
      error = fh_eblup - truth,
      abs_error = abs(error),
      sq_error = error^2,
      lower = fh_eblup - 1.96 * fh_rmse,
      upper = fh_eblup + 1.96 * fh_rmse,
      covered = truth >= lower & truth <= upper
    )

  summary_metrics <- compare_eval |>
    summarise(
      n = sum(!is.na(fh_eblup) & !is.na(truth)),
      bias = mean(error, na.rm = TRUE),
      mae = mean(abs_error, na.rm = TRUE),
      rmse = sqrt(mean(sq_error, na.rm = TRUE)),
      coverage_95 = mean(covered, na.rm = TRUE),
      corr_pearson = cor(fh_eblup, truth, use = "complete.obs")
    )

  estimate_vs_truth_plot <- ggplot(
    compare_eval,
    aes(x = truth, y = fh_eblup)
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point() +
    labs(
      x = "True value",
      y = "Model-based estimate",
      title = "Estimate versus truth"
    )
}
