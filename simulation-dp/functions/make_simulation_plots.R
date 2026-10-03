# wrapper around make_density_plot() and make_forcing_var_plot()
make_simulation_plots <- function(
    dt,
    truth,
    forcing_var,
    estimators,
    prefix,
    plots = c("bias", "forcing_var")) {
  
  # loop density plot and forcing_var plot over estimators
  for (i in estimators) {
    if ("density" %in% plots) {
      make_density_plot(
        dt_effects = dt,
        truth = truth,
        estimator = i,
        filename = paste0(output_folder, prefix, i, "_density_"))
    }
    if ("bias" %in% plots) {
      make_bias_plot(
        dt_effects = dt,
        truth = truth,
        estimator = i,
        filename = paste0(output_folder, prefix, i, "_bias_"))
      }
    if ("forcing_var" %in% plots) {
      make_forcing_var_plot(
        dt_effects = dt,
        truth = truth,
        estimator = i,
        filename = paste0(output_folder, prefix, i, "_forcing_var_"))
      }
  }
}
