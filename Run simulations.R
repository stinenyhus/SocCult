#Running the models

source("useful_function_soccult.r")

library(pacman)
p_load(tidyverse, network, igraph, intergraph, tidygraph, ggraph, ggplot2)


####STINES MODELS####

#Baseline
basetau_neu_rep100 <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1)
write.csv(basetau_neu_rep100, "basetau_neu_rep100.csv")

#With highstatus nodes
basetau_neu_rep100_highstatus <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1, high_node = T)
write.csv(basetau_neu_rep100_highstatus, "basetau_neu_rep100_highstatus.csv")


####ASTRIDS MODELS####

#Stochastic
basetau_neu_rep100_stochastic <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1, stochastic = T)
write.csv(basetau_neu_rep100_stochastic, "basetau_neu_rep100_stochastic.csv")

#Scalefree
basetau_scalefree_rep100 <- contagion_sim(net_type = "scale_free", rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0, degrees = 75, gamma = 2.3)
write.csv(basetau_scalefree_rep100, "basetau_scalefree_rep100.csv")

####MARIES MODELS####

#Varying thresholds
randomtau_neu_rep100 <- contagion_sim(tau_type = "random_tau", rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1)
write.csv(randomtau_neu_rep100, "randomtau_neu_rep100.csv")

#High status + stochastic
basetau_neu_rep100_highstatus_stochastic <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1, stochastic = T, high_node = T)
write.csv(basetau_neu_rep100_highstatus_stochastic, "basetau_neu_rep100_highstatus_stochastic.csv")



####Extra models####

#Scalefree + varying thresholds
randomtau_scalefree_rep100 <- contagion_sim(net_type = "scale_free",tau_type = "random_tau", rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0, degrees = 75, gamma = 2.3)
write.csv(randomtau_scalefree_rep100, "randomtau_scalefree_rep100.csv")


#High status + stochastic + random tau
randomtau_neu_rep100_highstatus_stochastic <- contagion_sim(tau_type = "random_tau", rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1, stochastic = T, high_node = T)
write.csv(randomtau_neu_rep100_highstatus_stochastic, "randomtau_neu_rep100_highstatus_stochastic.csv")





