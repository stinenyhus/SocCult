#Plotting results 
library(pacman)
p_load(tidyverse,gdtools, ggplot2, ggthemes, Hmisc, ggpubr, patchwork)
source("useful_function_soccult.r")

#
####Simulation 0 - baseline####
#Loading file 
simulation0_all_sum <- read.csv("simulation0_all_sum.csv")

#Plotting
simulation0_plot <- plot_standard(simulation0_all_sum, 
                                  title = "Simulation 0 - baseline with varying tau")+
  labs(color = "Simulation")

#
####Simulation 1 - heterogeneity of thresholds####
#Loading files
simulation1_all_sum <- read.csv("simulation1_all_sum.csv")

#Plotting
simulation1_plot1 <- simulation1_all_sum %>% 
  filter(tau == ".25") %>% 
  plot_standard_by_name(title = "Baseline against heterogeneity of thresholds, tau = .25")+
  labs(color = "Simulation")+
  scale_y_continuous(breaks = seq(-5000,25000,5000))

simulation1_plot2 <- simulation1_all_sum %>% 
  filter(name == "Heterogeneity of thresholds") %>% 
  plot_standard(title = "Heterogeneity of thresholds with varying tau")+
  labs(color = "tau")

plots_sim1 <- ggarrange(simulation1_plot1,simulation1_plot2, ncol = 2)

#

####Simulation 2 - stochastic thresholds - mangler!####
#Loading files
#Tau .25
simulation2_tau25 <- read.csv("basetau_neu_rep100_stochastic_tau_25.csv")
simulation2_tau25_sum <- sum_data(simulation2_tau25, tau = ".25", name = "Stochastic thresholds")
#Tau .30
simulation2_tau30 <- read.csv("basetau_neu_rep100_stochastic_tau30.csv")
simulation2_tau30_sum <- sum_data(simulation2_tau30, tau = ".30", name = "Stochastic thresholds")
#Tau .35
simulation2_tau35 <- read.csv("basetau_neu_rep100_stochastic_tau35.csv")
simulation2_tau35_sum <- sum_data(simulation2_tau35, tau = ".35", name = "Stochastic thresholds")
#Tau .40
simulation2_tau40 <- read.csv("basetau_neu_rep100_stochastic_tau40.csv")
simulation2_tau40_sum <- sum_data(simulation2_tau40, tau = ".40", name = "Stochastic thresholds")

#Binding data and plotting 
simulation2_all_sum <- rbind(simulation0_tau25_sum,
                             simulation2_tau25_sum, simulation2_tau30_sum,
                             simulation2_tau35_sum, simulation2_tau40_sum)
write.csv(simulation2_all_sum, "simulation2_all_sum.csv")

#Plotting
simulation2_plot1 <- simulation2_all_sum %>% 
  filter(tau == ".25") %>% 
  plot_standard(title = "Simulation 2 - baseline against stochastic thresholds, tau = .25",
                color_by = name)+
  labs(color = "Simulation")

simulation2_plot2 <- simulation2_all_sum %>% 
  filter(name == "Stochastic thresholds") %>% 
  plot_standard(title = "Simulation 2 - stochastic thresholds with varying tau")

plots_sim2 <- ggarrange(simulation2_plot1,simulation2_plot2, ncol = 2)
#


####Simulation 3 - heterogeneity of influence####
#Loading file
simulation3_all_sum <- read.csv("simulation3_all_sum.csv")

#Plotting
plot_sim3 <- simulation3_all_sum %>% 
  plot_standard_by_name(title = "Simulation 3 - baseline against heterogeneous influence, tau = .25")+
  labs(color = "Simulation")

#

####Simulation 4 - heterogeneity of degree####
#Loading file
simulation4_all_sum <- read.csv("simulation4_all_sum.csv")

#Plotting
plot_sim4 <- simulation4_all_sum %>% 
  plot_standard_by_name(title = "Simulation 4 - baseline against heterogeneous degree, tau = .25")+
  labs(color = "Simulation")

#

####Simulation 5 - Interaction between stochastic thresholds and heterogeneity of thresholds####
#Loading file
simulation5_all_sum <- read.csv("simulation5_all_sum.csv")

#Plotting
simulation5_plot1 <- simulation5_all_sum %>% 
  filter(tau == ".25") %>% 
  plot_standard_by_name(title = "Simulation 5 - baseline against stochastic & heterogeneous thresholds, tau = .25")+
  labs(color = "Simulation", title = str_wrap("Simulation 5 - baseline against stochastic & heterogeneous thresholds, tau = .25", 45))

simulation5_plot2 <- simulation5_all_sum %>% 
  filter(name == "Stochastic & heterogeneous thresholds") %>% 
  plot_standard(title = "Simulation 5 - stochastic & heterogeneous thresholds with varying tau")+
  labs(color = "tau", title = str_wrap("Simulation 5 - stochastic & heterogeneous thresholds with varying tau",45))

plots_sim5 <- ggarrange(simulation5_plot1,simulation5_plot2, ncol = 2)
#

####Simulation 6 - Interaction between heterogeneity of thresholds and of influence####
#Loading file
simulation6_all_sum <- read.csv("simulation6_all_sum.csv")

#Plotting
plot_sim6 <- simulation6_all_sum %>% 
  plot_standard_by_name(title = "Simulation 6 - baseline against heterogeneity of degree and stochastic thresholds")+
  labs(color = "Simulation")


#
####Simulation 7 - Interaction between heterogeneity degree and heterogeneity of thresholds####
#Loading file
simulation7_all_sum <- read.csv("simulation7_all_sum.csv")

#Plotting
plot_sim7 <- simulation7_all_sum %>% 
  plot_standard_by_name(title = "Simulation 7 - baseline against heterogeneity of degree and of thresholds, tau = .25")+
  labs(color = "Simulation")

#

####Simulation 8 - Interaction between stochastic thresholds and heterogeneity of degree - stupid!####
#Loading file
simulation8 <- read.csv(".csv")
simulation8_tau25_sum <- sum_data(simulation8, tau = ".25", name = "Heterogeneity of degree and stochastic thresholds")

#Binding data and plotting 
simulation8_all_sum <- rbind(simulation0_tau25_sum,
                             simulation8_tau25_sum)
write.csv(simulationb_all_sum, "simulation8_all_sum.csv")

#Plotting
plot_sim8 <- simulation8_all_sum %>% 
  plot_standard(title = "Simulation 8 - baseline against heterogeneity of degree and stochastic thresholds, tau = .25",
                color_by = name)+
  labs(color = "Simulation")

#
####All together####
#Loading all files 
#All simulations in one 

all_networks <- rbind(
  simulation0_tau25_sum, simulation0_tau30_sum, simulation0_tau35_sum, simulation0_tau40_sum, 
  simulation1_tau25_sum, simulation1_tau30_sum, simulation1_tau35_sum, simulation1_tau40_sum,
  simulation2_tau25_sum, simulation2_tau30_sum, simulation2_tau35_sum, simulation2_tau40_sum,
  simulation3_tau25_sum,
  simulation4_tau25_sum,
  simulation5_tau25_sum, simulation5_tau30_sum, simulation5_tau35_sum, simulation5_tau40_sum,
  simulation6_tau25_sum,
  simulation7_tau25_sum,
  simulation8_tau25_sum)

write.csv(all_networks, "all_data.csv")

#


####Old things####
all_simulations <- read.csv("all_networks.csv")
ggplot(all_simulations, aes(round, sumadopt, color = name))+
  geom_line(size = 1.2)+
  #geom_errorbar(stat = "summary", fun.data = mean_se)+
  geom_errorbar(aes(ymin=sumadopt-sd, ymax=sumadopt+sd), width=.2,
                position=position_dodge(.9), alpha = 0.25) +
  scale_colour_pander()+
  theme_minimal()+
  theme(legend.title = element_blank())+
  geom_hline(yintercept = 22500*0.5, linetype = 2, alpha = 0.7)+
  geom_text(aes(label ="50 % activation", x = 98, y = 12000), color = "Black", alpha = 0.01, size = 3)+
  geom_hline(yintercept = 22500*0.25, linetype = 2, alpha = 0.7)+
   geom_text(aes(label ="25 % activation", x = 98, y = 6300), color = "Black", alpha = 0.01, size = 3) +
  geom_hline(yintercept = 22500*0.75, linetype = 2, alpha = 0.7)+
   geom_text(aes(label ="75 % activation", x = 98, y = 17500),color = "Black", alpha = 0.01, size = 3) 

