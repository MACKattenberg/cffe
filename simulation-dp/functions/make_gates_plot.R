make_gates_plot <- function(dt, forcing_var = "tau_hat_cffe_55", sim){
  dt.in <- copy(dt)
  dt.in[, lb_ols := ols_beta - 1.96 * ols_se]
  dt.in[, ub_ols := ols_beta + 1.96 * ols_se]
  
  # compute values for kappa, lambda to make correct label
  kappa <- stringr::str_sub(forcing_var, -2, -2)
  lambda <- stringr::str_sub(forcing_var, -1, -1)
  cffe_text <- paste0("cffe (\u03BA = ", kappa, ", \u03BB = ", lambda,")")
  
  #plot the figure
  ggplot(dt.in) +
    geom_pointrange(aes(y = group,
                        xmin = lb_fv,
                        xmax = ub_fv,
                        x = mean_fv,
                        colour = cffe_text), size = 0.25) +
    geom_pointrange(aes(y = group,
                        xmin = lb_ols,
                        xmax = ub_ols,
                        x = ols_beta, colour = "OLS"), size = 0.25) +
    geom_point(aes(y = group, x = truth, colour = "truth"),) +
    scale_y_continuous(breaks=seq(0,10), labels=c("all", paste0("dec", 1:10))) +
    labs(colour = "", x = "\u03C4") +
    theme(legend.position="bottom")
  
  # save the plot
  filename <- paste0(output_folder, sim, "gates_cffe_")
  ggsave(paste0(filename, "kl", kappa, lambda, ".png"), width = 6, height = 6)
}
