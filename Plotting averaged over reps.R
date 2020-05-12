#Running test ABMs of all conditions with fewer nodes and only one repetition

source("useful_function_soccult.r")

library(pacman)
p_load(tidyverse, network, igraph, intergraph, tidygraph, ggraph, ggplot2)

#high status
base_100_r01_neu_highstatus <- contagion_sim(high_node = T, rep = 1, rounds = 100, tau = .25, n = 100, nei = 2, p = 0.1)
write.csv(base_100_r01_neu_highstatus, "base_100_r01_neu_highstatus.csv")

base_100_r01_neu_highstatus %>% 
  group_by(round) %>% 
  summarise(sum_adopt = sum(adopters)) %>%
  ggplot(aes(x = round, y = sum_adopt)) + 
  geom_line()

#stochastic
base_100_r01_neu_stochastic <- contagion_sim(rep = 1, rounds = 100, tau = .25, n = 100, nei = 2, p = 0.1, stochastic = T)
write.csv(base_100_r01_neu_stochastic, "base_100_r01_neu_stochastic")

base_100_r01_neu_stochastic %>% 
  group_by(round) %>% 
  summarise(sum_adopt = sum(adopters)) %>%
  ggplot(aes(x = round, y = sum_adopt)) + 
  geom_line()

#scale free
base_100_scalefree <- contagion_sim(rep = 1, rounds = 100, tau = .25, net_type = "scale_free", degrees = 50, gamma = 2.3, n = 100, nei = 2, p = 0)
write.csv(base_100_scalefree, "base_100_scalefree")

base_100_scalefree %>% 
  group_by(round) %>% 
  summarise(sum_adopt = sum(adopters)) %>%
  ggplot(aes(x = round, y = sum_adopt)) + 
  geom_line()

#varying thresholds
randomtau_100_r01_neu <- contagion_sim(tau_type = "random_tau", rep = 1, rounds = 100, tau = .25, n = 100, nei = 2, p = 0.1) 
write.csv(randomtau_100_r01_neu, "randomtau_100_r01_neu")

randomtau_100_r01_neu %>% 
  group_by(round) %>% 
  summarise(sum_adopt = sum(adopters)) %>%
  ggplot(aes(x = round, y = sum_adopt)) + 
  geom_line()


#stochastic x varyig thresholds
randomtau_100_r01_neu_stochastic <- contagion_sim(tau_type = "random_tau", rep = 1, rounds = 100, tau = .25, n = 100, nei = 2, p = 0.1, stochastic = T) 
write.csv(randomtau_100_r01_neu_stochastic, "randomtau_100_r01_neu_stochastic")

randomtau_100_r01_neu_stochastic %>% 
  group_by(round) %>% 
  summarise(sum_adopt = sum(adopters)) %>%
  ggplot(aes(x = round, y = sum_adopt)) + 
  geom_line()


#scalefree x varying thresholds
randomtau_100_r01_scalefree <- contagion_sim(tau_type = "scale_free", degrees = 50, gamma = 2.3, rep = 1, rounds = 100, tau = .25, n = 100, nei = 2, p = 0.1) 
write.csv(randomtau_100_r01_scalefree, "randomtau_100_r01_scalefree")

#100 reps 
base_100_r01_neu_100rep <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 100, nei = 2, p = 0.1)


#Averaging over rounds 
base_100_r01_neu_100rep %>% group_by(network,round) %>% 
  summarise(sum_adopt = sum(adopters)) %>% 
  group_by(round) %>% summarise(mean = mean(sum_adopt)) %>%
  ggplot(aes(x = round, y = mean)) +
  geom_line()




