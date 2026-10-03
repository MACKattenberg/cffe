# plot kernel density of tau_hats for cffe
make_density_plot <- function(dt_effects, truth, estimator, # "cffe" or "cfdd"
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
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat00,
                     fill = "\u03BA = 0, \u03BB = 0"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl00.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 0, lambda = 5
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat05,
                     fill = "\u03BA = 0, \u03BB = 5"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl05.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 5, lambda = 0
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat50,
                     fill = "\u03BA = 5, \u03BB = 0"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl50.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 5, lambda = 5
  ggplot(dt.in) +
    geom_density(aes(x = truth, fill = "truth"), alpha=.3) +
    geom_density(aes(x = hat55,
                     fill = "\u03BA = 5, \u03BB = 5"), alpha=.3) +
    labs(fill = paste("CATE", estimator), x = "\u03C4") +
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl55.png"), width = 6, height = 6)
  
}