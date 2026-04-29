# Diagnostic plots for comparing direct and model-based SAE estimates.

library(ggplot2)

plot_direct_vs_model <- function(results,
                                 direct_col = "direct",
                                 model_col = "fh_estimate",
                                 size_col = "non_missing_n") {
  ggplot(
    results,
    aes(
      x = .data[[direct_col]],
      y = .data[[model_col]],
      size = .data[[size_col]]
    )
  ) +
    geom_abline(slope = 1, intercept = 0, colour = "grey60") +
    geom_point(alpha = 0.75) +
    coord_equal() +
    labs(
      x = "Direct estimate",
      y = "Model-based estimate",
      size = "Non-missing sample"
    ) +
    theme_minimal()
}

plot_uncertainty <- function(results,
                             estimate_col = "fh_estimate",
                             mse_col = "fh_mse") {
  ggplot(
    results,
    aes(x = .data[[estimate_col]], y = sqrt(.data[[mse_col]]))
  ) +
    geom_point(alpha = 0.75) +
    labs(
      x = "Model-based estimate",
      y = "Model standard error"
    ) +
    theme_minimal()
}
