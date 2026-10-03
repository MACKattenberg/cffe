# plot kernel density of tau_hats for cffe
make_forcing_var_plot <- function(dt_effects, truth, estimator, # "cffe" or "cfdd"
                                  filename){
  
  # rename variables (so they can be called in plot without quatation marks)
  dt.in <- copy(dt_effects)
  
  orig_names <- c(truth,
                  paste0("tau_hat_", estimator, "_", values_kaplam),
                  paste0("var_hat_", estimator, "_", values_kaplam))
  setnames(dt.in,
           orig_names,
           c("truth",
             paste0("hat", values_kaplam),
             paste0("var", values_kaplam)))
  
  # compute lower and upper bound
  for (kl in values_kaplam){
    
    hat_name <- paste0("hat", kl)
    var_name <- paste0("var", kl)
    setnames(dt.in, c(hat_name, var_name), c("hat", "var"))
    
    se <- dt.in[, sqrt(var)]
    dt.in[, lb := hat - 1.96 * se]
    dt.in[, ub := hat + 1.96 * se]
    
    lb_name <- paste0("lb", kl) 
    ub_name <- paste0("ub", kl) 
    setnames(dt.in,
             c("hat", "var", "lb", "ub"),
             c(hat_name, var_name, lb_name, ub_name))
  }
  
  
  # make and save the plots: kappa = 0, lambda = 0
  ggplot(dt.in) +
    geom_ribbon(aes(ymin = lb00, ymax = ub00, x = forcing_var, fill = "CFI")) +
    geom_point(aes(y = hat00,  x = forcing_var,
                   color = "\u03BA = 0, \u03BB = 0"), size = 0.5) +
    geom_line(aes(y = truth, forcing_var)) +
    labs(y = paste("treatment effect", estimator),
         x = "forcing variable",
         colour = "",
         fill = "") +
    scale_fill_manual(values="grey60")+
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl00.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 0, lambda = 5
  ggplot(dt.in) +
    geom_ribbon(aes(ymin = lb05, ymax = ub05, x = forcing_var, fill = "CFI")) +
    geom_point(aes(y = hat05,  x = forcing_var,
                   color = "\u03BA = 0, \u03BB = 5"), size = 0.5) +
    geom_line(aes(y = truth, forcing_var)) +
    labs(y = paste("treatment effect", estimator),
         x = "forcing variable",
         colour = "",
         fill = "") +
    scale_fill_manual(values="grey60")+
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl05.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 5, lambda = 0
  ggplot(dt.in) +
    geom_ribbon(aes(ymin = lb50, ymax = ub50, x = forcing_var, fill = "CFI")) +
    geom_point(aes(y = hat50,  x = forcing_var,
                   color = "\u03BA = 5, \u03BB = 0"), size = 0.5) +
    geom_line(aes(y = truth, forcing_var)) +
    labs(y = paste("treatment effect", estimator),
         x = "forcing variable",
         colour = "",
         fill = "") +
    scale_fill_manual(values="grey60")+
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl50.png"), width = 6, height = 6)
  
  # make and save the plots: kappa = 5, lambda = 5
  ggplot(dt.in) +
    geom_ribbon(aes(ymin = lb55, ymax = ub55, x = forcing_var, fill = "CFI")) +
    geom_point(aes(y = hat55,  x = forcing_var,
                   color = "\u03BA = 5, \u03BB = 5"), size = 0.5) +
    geom_line(aes(y = truth, forcing_var)) +
    labs(y = paste("treatment effect", estimator),
         x = "forcing variable",
         colour = "",
         fill = "") +
    scale_fill_manual(values="grey60")+
    theme(legend.position="bottom")
  ggsave(paste0(filename, "kl55.png"), width = 6, height = 6)
}