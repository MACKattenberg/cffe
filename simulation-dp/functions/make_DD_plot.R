make_DD_plot <- function(sim, dt, save_as){
  if (sim == "sim1") {
    # prepare data for dif-in-diff plot
    dd_plot <- dcast(
      convert_to_dd_plot(dt = dt, target = "y_kaplam55"),
      t ~ variable + treat)

dd_plot
    
    # make dif-in-diff plot for simulated data
    ggplot(dd_plot) +
      geom_point(aes(y = y_kaplam55_0, x = t, colour = "control") ) +
      geom_line(aes(y = y_kaplam55_0, x = t, colour = "control"),
                linetype = "solid") +
      geom_point(aes(y = cfl_1, x = t, colour = "treatment")) +
      geom_line(aes(y = cfl_1, x = t, colour = "treatment"),
                linetype = "dashed") +
      geom_point(aes(y = y_kaplam55_1, x = t, colour = "treatment")) +
      geom_line(aes(y = y_kaplam55_1, x = t, colour = "treatment"),
                linetype = "solid") +
      labs(y = "y (\u03BA = 5, \u03BB = 5)", colour = "group") +
      geom_vline(xintercept = 2.5, linetype = "dashed") +
      theme(legend.position="bottom")
    ggsave(paste0(output_folder, save_as))
  }
  
  if (sim == "sim2") {
    # make dif-in-diff plot for simulated data
    dd_plot_stacked <- dcast(
      convert_to_dd_plot(
        dt = dt, target = "y_kaplam55", treat = "treat"),
      t ~ variable + treat)
    
    # make the plot
    ggplot(dd_plot_stacked[t <= 4]) +
      geom_point(aes(y = y_kaplam55_0, x = t, colour = "control")) +
      geom_line( aes(y = y_kaplam55_0, x = t, colour = "control"),
                 linetype = "solid") +
      geom_point(aes(y = cfl_1, x = t, colour = "treatment")) +
      geom_line( aes(y = cfl_1, x = t, colour = "treatment"),
                 linetype = "dashed") +
      geom_point(aes(y = y_kaplam55_1, x = t, colour = "treatment")) +
      geom_line( aes(y = y_kaplam55_1, x = t, colour = "treatment"),
                 linetype = "solid") +
      labs(y = "y (\u03BA = 5, \u03BB = 5)", colour = "group") +
      geom_vline(xintercept = c(2.5), linetype = "dashed") +
      theme(legend.position="bottom")
    ggsave(paste0(output_folder, save_as[[1]] ))
    
    ggplot(dd_plot_stacked[t > 4]) +
      geom_point(aes(y = y_kaplam55_0, x = t, colour = "control")) +
      geom_line( aes(y = y_kaplam55_0, x = t, colour = "control"),
                 linetype = "solid") +
      geom_point(aes(y = cfl_1, x = t, colour = "treatment")) +
      geom_line( aes(y = cfl_1, x = t, colour = "treatment"),
                 linetype = "dashed") +
      geom_point(aes(y = y_kaplam55_1, x = t, colour = "treatment")) +
      geom_line( aes(y = y_kaplam55_1, x = t, colour = "treatment"),
                 linetype = "solid") +
      labs(y = "y (\u03BA = 5, \u03BB = 5)", colour = "group") +
      geom_vline(xintercept = c(6.5), linetype = "dashed") +
      theme(legend.position="bottom")
    ggsave(paste0(output_folder, save_as[[2]]))
  }
}