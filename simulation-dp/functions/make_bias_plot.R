# plot bias of tau_hats for cffe
make_bias_plot <- function(dt_effects, truth, estimator, # "cffe" or "cfdd"
                              filename){
  
  # rename variables (so they can be called in plot without quatation marks)
  dt.in <- copy(dt_effects)
  orig_names <- c(truth, paste0("tau_hat_", estimator, "_", values_kaplam))
  setnames(dt.in, orig_names, c("truth", paste0("hat", values_kaplam)))

  if ("00" %in% values_kaplam) {
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
  }
  
  if ("05" %in% values_kaplam) {
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
  }
  
  if ("50" %in% values_kaplam) {
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
  }
  
  if ("55" %in% values_kaplam) {
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
  
}
