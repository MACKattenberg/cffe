# plot bias of tau_hats for cffe
make_bias_plot <- function(dt_effects, truth, estimator, # "cffe" or "cfdd"
                              filename){
  
  # rename variables (so they can be called in plot without quatation marks)
  dt.in <- copy(dt_effects)
  orig_names <- c(truth,
                  paste0("tau_hat_", estimator, "_00"),
                  paste0("tau_hat_", estimator, "_05"),
                  paste0("tau_hat_", estimator, "_50"),
                  paste0("tau_hat_", estimator, "_55"))
  setnames(dt.in, orig_names,
           c("truth", "hat00", "hat05",  "hat50", "hat55"))
  
  # make and save the plots: kappa = 0, lambda = 0
  exp_tau_hat <- round(dt.in[, mean(hat00 - truth, na.rm = TRUE)],2)
  sd_tau_hat <- round(dt.in[, sd(hat00 - truth, na.rm = TRUE)],2)
  ggplot(dt.in) +
    geom_density(aes(x = hat00 - truth,
                     fill = paste("\u03BC =", exp_tau_hat, 
                                  ", \u03C3 =", sd_tau_hat,
                                  " (\u03BA = 0, \u03BB = 0)")), alpha=.3) +
    geom_vline(xintercept = exp_tau_hat, linetype= "dashed") +
    geom_vline(xintercept = 0, linetype= "solid") +
    labs(fill = paste("bias", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl00.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 0, lambda = 5
  exp_tau_hat <- round(dt.in[, mean(hat05 - truth, na.rm = TRUE)],2)
  sd_tau_hat <- round(dt.in[, sd(hat05 - truth, na.rm = TRUE)],2)
  ggplot(dt.in) +
    geom_density(aes(x = hat05 - truth,
                     fill = paste("\u03BC =", exp_tau_hat, 
                                  ", \u03C3 =", sd_tau_hat,
                                  " (\u03BA = 0, \u03BB = 5)")), alpha=.3) +
    geom_vline(xintercept = exp_tau_hat, linetype= "dashed") +
    geom_vline(xintercept = 0, linetype= "solid") +
    labs(fill = paste0("bias ", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl05.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 5, lambda = 0
  exp_tau_hat <- round(dt.in[, mean(hat50 - truth, na.rm = TRUE)],2)
  sd_tau_hat <- round(dt.in[, sd(hat50 - truth, na.rm = TRUE)],2)
  ggplot(dt.in) +
    geom_density(aes(x = hat50 - truth,
                     fill = paste("\u03BC =", exp_tau_hat, 
                                  ", \u03C3 =", sd_tau_hat,
                                  " (\u03BA = 5, \u03BB = 0)")), alpha=.3) +
    geom_vline(xintercept = exp_tau_hat, linetype= "dashed") +
    geom_vline(xintercept = 0, linetype= "solid") +
    labs(fill = paste("bias", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl50.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 5, lambda = 5
  exp_tau_hat <- round(dt.in[, mean(hat55 - truth, na.rm = TRUE)],2)
  sd_tau_hat <- round(dt.in[, sd(hat55 - truth, na.rm = TRUE)],2)
  ggplot(dt.in) +
    geom_density(aes(x = hat55 - truth,
                     fill = paste("\u03BC =", exp_tau_hat, 
                                  ", \u03C3 =", sd_tau_hat,
                                  " (\u03BA = 5, \u03BB = 5)")), alpha=.3) +
    geom_vline(xintercept = exp_tau_hat, linetype= "dashed") +
    geom_vline(xintercept = 0, linetype= "solid") +
    labs(fill = paste("bias", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl55.png"), width = 6, height = 6)
  
}
