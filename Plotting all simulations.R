#Plotting results 
library(pacman)
p_load(tidyverse,gdtools, ggplot2, ggthemes)
source("useful_function_soccult.r")

###Simulation 1 - baseline
simulation1 <- read.csv("basetau_neu_rep100_tau25.csv")
simulation1_summed <- sum_data(simulation1, name = "Baseline network")
simulation1_plot <- plot_standard(simulation1_summed, title = "Simulation baseline")

###Simulation 2 - heterogeneity of degree
simulation2 <- read.csv("basetau_scalefree_rep100.csv")
simulation2_summed <- sum_data(simulation2, name = "Heterogeneity of degree")
simulation2_plot <- plot_standard(simulation2_summed, title = "Simulation in a scale-free network")

###Simulation 3 - heterogeneity of thresholds
simulation3 <- read.csv("randomtau_neu_rep100.csv")
simulation3_summed <- sum_data(simulation3, name = "Heterogeneity of thresholds")
simulation3_plot <- plot_standard(simulation3_summed, title = "Simulation with varying thresholds")

###Simulation 4 - heterogeneity of influence
simulation4 <- read.csv("basetau_neu_rep100_highstatus_tau25.csv")
vec <- simulation4$adopters[simulation4$round == 100] > 100
trues <- which(vec, T)
simulation4_success <- simulation4[simulation4$network %in% trues,]
simulation4_fail <- simulation4[!simulation4$network %in% trues,]
simulation4_success_summed <- sum_data(simulation4_success, name = "Heterogeneity of influence - sucess")
simulation4_fail_summed <- sum_data(simulation4_fail, name = "Heterogeneity of influence - fail")
simulation4_plot <- plot_standard(simulation4_success_summed, title = "Simulation with heterogeneity of influence")

###Simulation 5 - stochastic thresholds
simulation5 <- read.csv("basetau_neu_rep100_stochastic_tau_25.csv")
simulation5_summed <- sum_data(simulation5, name = "Stochastic thresholds")
simulation5_plot <- plot_standard(simulation5_summed, title = "Simulation with stochastic thresholds")

###Simulation 6 - Interaction between stochastic thresholds and heterogeneity of thresholds
simulation6 <- read.csv("randomtau_neu_rep100_stochastic.csv")
simulation6_summed <- sum_data(simulation6, name = "Int. stochastic thresholds & heterogeneity of thresholds")
simulation6_plot <- plot_standard(simulation6_summed, title = "Simulation with both varying and stochastic thresholds")

###Simulation 7 - Interaction between heterogeneity degree and heterogeneity of thresholds
simulation7 <- read.csv("randomtau_scalefree_rep100.csv")
simulation7_summed <- sum_data(simulation7, name = "Int. heterogeneity of degree & heterogeneity of thresholds")
simulation7_plot <- plot_standard(simulation7_summed, title = "Simulation in a scale-free network with varying thresholds")

###Simulation 8 - Interaction between stochastic thresholds and heterogeneity of influence
simulation8 <- read.csv("basetau_neu_rep100_highstatus_stochastic.csv")
simulation8_summed <- sum_data(simulation8, name = "Int. stochastic thresholds & heterogeneity of influence")
simulation8_plot <- plot_standard(simulation6_summed, title = "Simulation with stochastic thresholds and  heterogeneity of influence")

all_simulations <- rbind(simulation1_summed, simulation2_summed, simulation3_summed, simulation4_success_summed, simulation5_summed, simulation8_summed)

ggplot(all_simulations, aes(round, sumadopt, color = name))+
  geom_line(size = 1.2)+
  scale_colour_pander()+
  theme_minimal()+
  theme(legend.title = element_blank())+
  geom_hline(yintercept = 22500*0.5, linetype = 2, alpha = 0.7)+
  geom_text(aes(label ="50 % activation", x = 98, y = 12000), color = "Black", alpha = 0.01, size = 3)+
  geom_hline(yintercept = 22500*0.25, linetype = 2, alpha = 0.7)+
   geom_text(aes(label ="25 % activation", x = 98, y = 6300), color = "Black", alpha = 0.01, size = 3) +
  geom_hline(yintercept = 22500*0.75, linetype = 2, alpha = 0.7)+
   geom_text(aes(label ="75 % activation", x = 98, y = 17500),color = "Black", alpha = 0.01, size = 3) 



            