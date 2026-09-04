# make figures case payrolling

input_folder <- "/nas/mack/H/mack/private/DST/causal_fe_forest/case/Export/"
output_folder <- "/nas/mack/H/mack/private/DST/causal_fe_forest/case/results/"

# function to extract estimation results  
compute_cfi <- function(dt){
  res <- data.table(dt[, "ate"],
                    dt[, "se"],
                    dt[, "ate"] - 1.96 * dt[, "se"],
                    dt[, "ate"] + 1.96 * dt[, "se"])
  
  setnames(res, c("beta", "se", "lb", "ub"))
  res[, t_abs := abs(beta) / se]
  res[, p_value := 2*pt(t_abs, 30, lower=FALSE)]
  res[, se := NULL]
  res[, t_abs := NULL]
  
  return(res)
}


ate <- read_excel(paste0(input_folder, "hourly_earnings_basis_1.xlsx"),
  sheet = "ate")
ols <- compute_cfi(ate)

female <- read_excel(paste0(input_folder, "hourly_earnings_basis_female.xlsx"),
  sheet = "ate")
male <- compute_cfi(female[1, ])
female <- compute_cfi(female[2, ])

age <- read_excel(paste0(input_folder, "hourly_earnings_basis_age_group.xlsx"),
  sheet = "ate")
age1 <- compute_cfi(age[1, ])
age2 <- compute_cfi(age[2, ])
age3 <- compute_cfi(age[3, ])
age4 <- compute_cfi(age[4, ])

generation <- read_excel(paste0(input_folder, "hourly_earnings_basis_generation.xlsx"),
  sheet = "ate")
generation0 <- compute_cfi(generation[1, ])
generation1 <- compute_cfi(generation[2, ])
generation2 <- compute_cfi(generation[3, ])

edu_t1 <- read_excel(paste0(input_folder, "hourly_earnings_basis_edu_t1.xlsx"),
  sheet = "ate")
edu1 <- compute_cfi(edu_t1[1, ])
edu2 <- compute_cfi(edu_t1[2, ])
edu3 <- compute_cfi(edu_t1[3, ])
edu9 <- compute_cfi(edu_t1[4, ])

contract_t1 <- read_excel(paste0(input_folder, "hourly_earnings_basis_contract_group_t1.xlsx"),
  sheet = "ate")
contract0 <- compute_cfi(contract_t1[1, ])
contract1 <- compute_cfi(contract_t1[2, ])

tenure_t1 <- read_excel(paste0(input_folder, "hourly_earnings_basis_ten_group_t1.xlsx"),
  sheet = "ate")
tenure1 <- compute_cfi(tenure_t1[1, ])
tenure2 <- compute_cfi(tenure_t1[2, ])
tenure3 <- compute_cfi(tenure_t1[2, ])

res <- rbind(ols,
            male, female,
            age1, age2, age3, age4,
            generation0, generation1, generation2,
            edu1, edu2, edu3, edu9,
            contract0, contract1,
            tenure1, tenure2, tenure3)

res[, var := c("overall",
               "male", "female",
              paste("age_group: ", c("18-24", "25-34", "35-44", "45-75")),
              paste("migration background:", c("native", "minority (1st)", "minority (2nd)")),
              paste("educational degree:",  c("low", "middle", "high", "unknown")),
              paste("contract group:", c("permanent", "temporary")),
              paste("tenure group:", c("0 - 3", "4 - 7", "8 or more")))]
order <- rev(res$var)
# assign proper variable names
 
# indicate significance using holm-bonferroni correction
setorderv(res, "p_value")
res[, rank := seq(1, .N)]
res[, cutoff := 0.05 / (30 + 1 - rank)]
crit_rank <- res[p_value > cutoff, min(rank)]
res[, sign_holmbonferroni := ifelse(rank < crit_rank, 1, 0)]

# plot results
ggplot(res) +
  geom_pointrange(aes(y = var, x = beta, xmin = lb, xmax = ub),
                  size = 0.25, color = ifelse(res$sign_holmbonferroni ==1, "black", "gray60")) +  
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits = order) +
  scale_x_continuous(breaks = round(seq(-0.6, 0.6, 0.15), 2), 
                     limits = c(-0.6, 0.6)) +
  labs(x = "treatment effect", y = NULL)
ggsave(paste0(output_folder, "manual_het_analysis.png"))



## histogram
dt <- as.data.table(read_excel(paste0(input_folder, "predictions.xlsx")))
mean <- dt[, mean(tau_hat)]
bounds <- quantile(dt$tau_hat, probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975))
ggplot(dt) +
  geom_density(aes(x = tau_hat)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-2.5, 2.5, 0.5)) +
  labs(x = "CATE (hourly earnings)")
ggsave(paste0(output_folder, "density_cate_wage.png"))


# TOC plot
rate <- as.data.table(read_excel(paste0(input_folder, "cffe_output.xlsx"),
                                 sheet = "RATES_2"))
base <- rate[Q_q == 1, b_TRUE]
rate[, p := 0.1 + (10 - Q_q)/10]
rate[, effect := b_TRUE - base]
rate[, se := se_TRUE]
rate[, lb := effect - 1.96 * se]
rate[, ub := effect + 1.96 * se]
rate[p == 1, ":="(ub = 0, lb = 0)]

compute_area <- function(dt = rate$effect){
  L <- length(dt)
  area <- sum(0.1 * (dt[1:(L-1)] - dt[2:L]))
}

sample_effects <- function(mean, sd){
  effect <- rnorm(n = 10, mean = rate$effect, sd = rate$se)
}

distr <- rbindlist(lapply(
  1:500, function(i) {
    seed = 18062014 + i
    set.seed(seed)
    area <- compute_area(sample_effects(mean = rate$effect, sd = rate$se))
    res <- data.table(draw = i, area = area)}
  ))

mean <- round(mean(distr$area),3)
lb <- round(quantile(distr$area, prob = 0.025), 3)
ub <- round(quantile(distr$area, prob = 0.975), 3)

ggplot(rate, aes(y = effect, x = p)) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "gray") +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0.1,1,0.1)) +
  labs(y = "ATE(q) - ATE", x = paste0("q \n RATE = ", mean, " (", lb, "-",ub, ")"))
ggsave(paste0(output_folder, "rate.png"))




# dynamic gates plot--------------------------------------------------------------------
gates_b <- as.data.table(read_excel(paste0(input_folder, "cffe_output.xlsx"), sheet = "GATES_Q_twfe_dyn_ate"))
gates_b[, event_time := seq(-3, 3)]
gates_b[, `...1` := NULL]
gates_b[, type := "beta"]

gates_se <- as.data.table(read_excel(paste0(input_folder, "cffe_output.xlsx"), sheet = "GATES_Q_twfe_dyn_se"))
gates_se[, event_time := seq(-3, 3)]
gates_se[, `...1` := NULL]
gates_se[, type := "se"]

# make one data.table
dt <- rbind(
  melt(gates_b, id.var = c("event_time", "type")),
  melt(gates_se, id.var = c("event_time", "type")))
dt <- dcast(dt, event_time ~ type + variable)

lb <- matrix(nrow = 7, ncol = 10)
colnames(lb) <- paste0("lb_X", 1:10)
ub <- matrix(nrow = 7, ncol = 10)
colnames(ub) <- paste0("ub_X", 1:10)

for (g in 1:10){
  sel_b <- paste0("beta_X", g)
  sel_se <- paste0("se_X", g)
  lb[1:7, g] <- dt[, ..sel_b][[1]] - 1.96 * dt[, ..sel_se][[1]]
  ub[1:7, g] <- dt[, ..sel_b][[1]] + 1.96 * dt[, ..sel_se][[1]]
}
dt <- cbind(dt, lb, ub)

# make plots
for (g in 1:10){
  setnames(dt,
           c(paste0("beta_X", g),
             paste0("lb_X", g),
             paste0("ub_X", g)), 
           c("beta", "lb", "ub"))
  ate <- round(dt[event_time >= 0, mean(beta)],2)
  plot <- ggplot(dt, aes(y = beta, x = event_time)) +
    geom_ribbon(aes(ymin = lb, ymax = ub), fill = "gray") +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -0.5, linetype = "dashed") +
    labs(x = "event time", y = paste0("ATE decile ", g,": ", ate))
  ggsave(paste0(output_folder, "dyn_ate_dec_", g, ".png"))
  print(plot)
  
  setnames(dt,
           c("beta", "lb", "ub"),
           c(paste0("beta_X", g),
             paste0("lb_X", g),
             paste0("ub_X", g)))
}






# CLAN

plot_clan <- function(dt, saveas){
  # protect dt :)
  dt.in <- copy(dt)
  
  #compute base entry
  sel <- paste0("dec", 1:10)
  base <- rowMeans(dt.in[, ..sel])
  
  # compute relative difference from base
  for (dec in 1:10){
    oldname <- paste0("dec", dec)
    setnames(dt.in, oldname, "DEC")
    dt.in[, DEC := 100 * ((DEC / base) - 1)]
    setnames(dt.in, "DEC", oldname)
  }
  
  # melt the result (input for geom_tile)
  dt.in <- melt(
    dt.in,
    id.var = "variable",
    variable.name = "decile")
  
  # make group indicator with correct label
  dt.in[, group := ifelse(value > -75  & value <= -50, "(-75, -50]", "")]
  dt.in[, group := ifelse(value > -50  & value <= -25 , "(-50, -25]", group)]
  dt.in[, group := ifelse(value > -25  & value <= 25   , "(-25, 25]"  , group)]
  dt.in[, group := ifelse(value > 25   & value <= 50  , "(25, 50]"  , group)]
  dt.in[, group := ifelse(value > 50   & value <= 100  , "(50, 100]"  , group)]
  dt.in[, group := ifelse(value > 100,   "> 100"   , group)]
  
  # fix order and colors
  group_order <- c("(-75, -50]", "(-50, -25]",
                   "(-25, 25]",
                   "(25, 50]", "(50, 100]", "> 100")
  color_list <- c("firebrick4", "firebrick2",
                  "ghostwhite",
                  "deepskyblue2", "deepskyblue3", "dodgerblue4") 
  
  # plot the shit
  ggplot(dt.in, aes(x = decile, y = variable, fill = as.character(group))) +         
    geom_tile() +
    scale_fill_manual(limits = group_order,
                      values = color_list) +
    labs(fill = " percentage difference \n (compared to sample \n average)")
  
  # save
  height <- 2.75 * 4
  width <- 4.86 * 4
  ggsave(filename = saveas, height = height, width = width)
}


plot_clan <- function(dt, saveas){
  # protect dt :)
  dt.in <- copy(dt)
  
  #compute base entry
  vars <- paste0("dec", 1:10)
  mean <- rowMeans(dt.in[, ..vars])
  sd <- apply(full_clan[, ..vars], 1, sd, na.rm=TRUE)
  dt.in[, ":="(mean = mean, sd = sd)]  
  # compute relative difference from base
  for (dec in 1:10){
    oldname <- paste0("dec", dec)
    setnames(dt.in, oldname, "DEC")
    dt.in[, DEC :=  (DEC - mean) / sd]
    setnames(dt.in, "DEC", oldname)
  }
  
  # melt the result (input for geom_tile)
  dt.in <- melt(
    dt.in,
    id.var = "variable",
    variable.name = "decile")
  
  # make group indicator with correct label
  # dt.in[, group := ifelse(value > -75  & value <= -50, "(-75, -50]", "")]
  # dt.in[, group := ifelse(value > -50  & value <= -25 , "(-50, -25]", group)]
  # dt.in[, group := ifelse(value > -25  & value <= 25   , "(-25, 25]"  , group)]
  # dt.in[, group := ifelse(value > 25   & value <= 50  , "(25, 50]"  , group)]
  # dt.in[, group := ifelse(value > 50   & value <= 100  , "(50, 100]"  , group)]
  # dt.in[, group := ifelse(value > 100,   "> 100"   , group)]
  # 
  # fix order and colors
  #group_order <- c("(-75, -50]", "(-50, -25]",
  #                 "(-25, 25]",
  #                 "(25, 50]", "(50, 100]", "> 100")
  #color_list <- c("firebrick4", "firebrick2",
  #                "ghostwhite",
  #                "deepskyblue2", "deepskyblue3", "dodgerblue4") 
  
  # plot the shit
  plot <- ggplot(dt.in[decile != "mean" & decile != "sd"], aes(x = decile, y = variable, fill = value )) + #, fill = as.character(group))) +         
    geom_tile() +
    labs(fill = " normalized value")
    
    #scale_fill_manual(limits = group_order,
    #                  values = color_list) +
    
  
  print(plot)
  
  # save
  height <- 2.75 * 4
  width <- 4.86 * 4
  ggsave(plot, filename = saveas, height = height, width = width)
}


clan_characteristics <- c("age_group",
                          "contract_group_t1",
                          "edu_enroll_t1",
                          "edu_t1",
                          "female",
                          "generation",
                          "ten_group_t1")



# make CLAN
full_clan <- make_clan(
  path_main = paste0(input_folder, "cffe_output.xlsx"),
  characteristics = clan_characteristics)

plot_clan(dt = full_clan, 
          saveas =paste0(output_folder, "clan_hwage.png"))
       