# functions are hard coded based on the following variable in data:
# individual indicator = i,
# time indicator = t
# event time indicator = t_star
# periods post event = post
# treatment group indicator = treat
# treatment indicator = posttreat

library(purrr)
library(dplyr)
library(data.table)
library(ggplot2)
library(grf)
library(writexl)
library(readxl)
library(xtable)
library(fixest)

# Define settings
rm(list = ls())

kappa_list <- c(0, 5)
lambda_list <- c(0, 5)
N_trees <- 2000 # use lower number (min = 50) for speed
min_node_size <- 5
set.seed(18062014)
set_distr = "uniform"
y_stub <- "y_kaplam"
N <- 1500

# define output folder
output_folder <- paste0("./output/", set_distr, "/")

# make arguments from kappa and lambda
args_kaplam <- expand.grid("kappa" = kappa_list, "lambda" = lambda_list)
values_kaplam <- unlist(lapply(
  1:nrow(args_kaplam),
  function(i) paste0(args_kaplam[i,], collapse = "")))

# make list of yhats and y variables
y_vars <- paste0(y_stub, values_kaplam)
yhats_cffe <- paste0("tau_hat_cffe_", values_kaplam)
yhats_mrcf <- paste0("tau_hat_mrcf_", values_kaplam)
yhats_dcf  <- paste0("tau_hat_dcf_" , values_kaplam)
yhats_cffe_2p <- paste0("tau_hat_cffe_2p_", values_kaplam)
yhats_dcf_2p <- paste0("tau_hat_dcf_2p_", values_kaplam)

# load the functions we need: all .R files except this one
files <- setdiff(
  grep("*.R", list.files(path = getwd()), value = TRUE),
  "simulation-cffe3.R") 
for (f in files) source(f)






## SINGLE EVENT STUDY-----------------------------------------------------------
# make data
data <- make_data()

## make difference-in-differences plot
make_DD_plot(sim = "sim1", dt = data, "dd_plot.png")

# run_cffe requires event time indicator
data[, t_star := t] 

# train models on single event data
cffe <- run_cffe_by_period(dt = data,
                           y_names = paste0("y_kaplam", values_kaplam),
                           treatment_id = "event_id")
mrcf <- run_mrcf_by_period(dt = data, treatment_id = "event_id")
dcf <- run_dcf(dt = data, treatment_id = "event_id")
cffe_2p <- run_cffe_by_period(dt = data[t_star >= 2 & t_star <= 3],
                              y_names = paste0("y_kaplam", values_kaplam),
                              treatment_id = "event_id")
colnames(cffe_2p) <- gsub("cffe_", "cffe_2p_", colnames(cffe_2p))
dcf_2p <- run_dcf(dt = data[t_star <= 3], treatment_id = "event_id")
colnames(dcf_2p) <- gsub("dcf_", "dcf_2p_", colnames(dcf_2p))
         
# add predictions to effects
effects <- collect_estimation_results(
  dt = data[t >= 3],
  estimates = list(cffe, mrcf, dcf, cffe_2p, dcf_2p))


# compute bias
make_bias_table(
  dt = effects,
  yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
  colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
  save_xls = "sim1_bias.xlsx",
  save_tex = "sim1_bias.tex"
)

# compute rmse
make_rmse_table(
  dt = effects,
  yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
  colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
  save_xls = "sim1_rmse.xlsx",
  save_tex = "sim1_rmse.tex"
  )

# compute ATE
vars <- setdiff(
  grep("tau_hat", colnames(effects), value = TRUE),
  grep("2p", colnames(effects), value = TRUE))
make_ate_table(dt = effects,
               yhats = vars,
               varhats = gsub("tau", "var", vars),
               output_folder = output_folder,
               save_xls = "sim1_ate.xlsx",
               save_tex = "sim1_ate.tex")

# make density plot and plot as function of forcing_var for cffe and mrcf
plot_dt <- copy(effects)
sel_rows <- sample(1:nrow(plot_dt), 1000)

make_simulation_plots(
  dt = plot_dt[sel_rows],
  truth = "treat_effect",
  forcing_var = "forcing_var",
  estimators = c("cffe", "mrcf", "dcf"),
  prefix = "sim1_")









## MULTIPLE EVENTS STUDY--------------------------------------------------------

# make data_stacked
data_stacked <- make_stacked_data()
data_stacked <- data_stacked[i <= N]

# make difference-in-differences plot sim 2
make_DD_plot(
  sim = "sim2",
  dt = data_stacked, 
  save_as = c("dd_plot_stacked1.png", "dd_plot_stacked2.png"))

# train models on multiple events data
cffe3 <- run_cffe_by_period(dt = data_stacked, treatment_id = "sec_treatment")
mrcf3 <- run_mrcf_by_period(dt = data_stacked, treatment_id = "sec_treatment")
dcf3 <- run_dcf(dt = data_stacked, treatment_id = "sec_treatment")
cffe3_2p <- run_cffe_by_period(dt = data_stacked[t_star >= 2 & t_star <= 3],
                              y_names = paste0("y_kaplam", values_kaplam),
                              treatment_id = "sec_treatment")
colnames(cffe3_2p) <- gsub("cffe_", "cffe_2p_", colnames(cffe3_2p))
dcf3_2p <- run_dcf(dt = data_stacked[t_star <= 3], treatment_id = "sec_treatment")
colnames(dcf3_2p) <- gsub("dcf_", "dcf_2p_", colnames(dcf3_2p))

# add predictions to effects
effects_stacked <- collect_estimation_results(
  dt = data_stacked[t_star >= 3],
  select_from_dt = "sec_treatment",
  estimates = list(cffe3, mrcf3, dcf3, cffe3_2p, dcf3_2p))

# compute bias for first event
make_bias_table(
  dt = effects_stacked,
  yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
  colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
  save_xls = "sim2_bias.xlsx",
  save_tex = "sim2_bias.tex"
)

# # compute bias for first event
# make_bias_table(
#   dt = effects_stacked[sec_treatment == 0],
#   yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
#   colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
#   save_xls = "sim2_bias_event1.xlsx",
#   save_tex = "sim2_bias_event1.tex"
# )
# 
# # compute bias for second event
# make_bias_table(
#   dt = effects_stacked[sec_treatment == 1],
#   yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
#   colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
#   save_xls = "sim2_bias_event2.xlsx",
#   save_tex = "sim2_bias_event2.tex"
# )

# compute rmse 
make_rmse_table(
  dt = effects_stacked,
  yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
  colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
  save_xls = "sim2_rmse.xlsx",
  save_tex = "sim2_rmse.tex"
)

# # compute rmse for first event
# make_rmse_table(
#   dt = effects_stacked[sec_treatment == 0],
#   yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
#   colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
#   save_xls = "sim2_rmse_event1.xlsx",
#   save_tex = "sim2_rmse_event1.tex"
# )
# 
# # compute rmse for second event
# make_rmse_table(
#   dt = effects_stacked[sec_treatment == 1],
#   yhats = c(yhats_cffe, yhats_mrcf, yhats_dcf, yhats_cffe_2p, yhats_dcf_2p),
#   colnames_yhats = c("cffe", "mrcf", "dcf", "cffe 2p", "dcf 2p"),
#   save_xls = "sim2_rmse_event2.xlsx",
#   save_tex = "sim2_rmse_event2.tex"
# )

# compute ATE
vars <- setdiff(
  grep("tau_hat", colnames(effects_stacked), value = TRUE),
  grep("2p", colnames(effects_stacked), value = TRUE))
make_ate_table(dt = effects_stacked,
               yhats = vars,
               varhats = gsub("tau", "var", vars),
               output_folder = output_folder,
               save_xls = "sim2_ate.xlsx",
               save_tex = "sim2_ate.tex")

# make density plot and plot as function of forcing_var for event1
plot_dt_stacked1 <- copy(effects_stacked)
plot_dt_stacked1 <- plot_dt_stacked1[sec_treatment == 0]
sel_rows1 <- sample(1:nrow(plot_dt_stacked1), 1000)

make_simulation_plots(
  dt = plot_dt_stacked1[sel_rows1,],
  truth = "treat_effect",
  forcing_var = "forcing_var",
  estimators = c("cffe", "mrcf", "dcf"),
  prefix = "sim2_event1_")

# make density plot and plot as function of forcing_var for event2
plot_dt_stacked2 <- copy(effects_stacked)
plot_dt_stacked2 <- plot_dt_stacked2[sec_treatment == 1]
sel_rows2 <- sample(1:nrow(plot_dt_stacked2), 1000)
make_simulation_plots(
  dt = plot_dt_stacked2[sel_rows2,],
  truth = "treat_effect",
  forcing_var = "forcing_var",
  estimators = c("cffe", "mrcf", "dcf"),
  prefix = "sim2_event2_")

