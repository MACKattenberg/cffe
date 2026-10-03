# plot kernel density of tau_hats for cffe
make_density_plot <- function(dt_effects, truth, estimator, # "cffe" or "cfdd"
                              filename){
  
  # rename variables (so they can be called in plot without quatation marks)
  dt.in <- copy(dt_effects)
  orig_names <- c(truth, paste0("tau_hat_", estimator, "_", values_kaplam))
  setnames(dt.in, orig_names, c("truth", paste0("hat", values_kaplam)))

  if ("00" %in% values_kaplam) {
  # make and save the plots: kappa = 0, lambda = 0
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat00,
                     fill = "\u03BA = 0, \u03BB = 0"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl00.png"), width = 6, height = 6)
  }
  
  if ("05" %in% values_kaplam) {
  # make and save the plots: kappa = 0, lambda = 5
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat05,
                     fill = "\u03BA = 0, \u03BB = 5"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl05.png"), width = 6, height = 6)
  }
  
  if ("50" %in% values_kaplam) {
  # make and save the plots: kappa = 5, lambda = 0
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat50,
                     fill = "\u03BA = 5, \u03BB = 0"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl50.png"), width = 6, height = 6)
  }
  
  if ("55" %in% values_kaplam) {
  # make and save the plots: kappa = 5, lambda = 5
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat55,
                     fill = "\u03BA = 5, \u03BB = 5"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl55.png"), width = 6, height = 6)
  }
  
}