source("useful_function_soccult.R")

neu_rewire_1 <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.1)
neu_rewire_03 <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.03)
neu_rewire_05 <- contagion_sim(rep = 100, rounds = 100, tau = .25, n = 22500, nei = 2, p = 0.05)

#adding rewiring probability as columns
neu_rewire_1$p <- rep(0.1, nrow(neu_rewire_1))
neu_rewire_1 <- neu_rewire_1 %>% 
  group_by(round) %>% 
  summarise(
    mean = mean(adopters),
    sd = sd(adopters)
  )
neu_rewire_03$p <- rep(0.03, nrow(neu_rewire_03))
neu_rewire_03<- neu_rewire_03 %>% 
  group_by(round) %>% 
  summarise(
    mean = mean(adopters),
    sd = sd(adopters)
  )

neu_rewire_05$p <- rep(0.05, nrow(neu_rewire_05))
neu_rewire_05<- neu_rewire_05 %>% 
  group_by(round) %>% 
  summarise(
    mean = mean(adopters),
    sd = sd(adopters)
  )

#choosing palette
p_load(pander, ggplot2, ggthemes, RColorBrewer)
p_load(RColorBrewer)
scale_colour_brewer(palette = "Set1")

#binding simulations together 
all_rewired <- rbind(neu_rewire_1, neu_rewire_03, neu_rewire_05)
all_rewired$p <- as.factor(all_rewired$p)

ggplot(all_rewired, aes(round, mean, color = p))+
  geom_line(size = 1.2)+
  scale_colour_pander()+
  theme_minimal()+
  theme(legend.title = element_blank(), text = element_text(size = 15, family = "serif")) + ylab("Number of activated nodes") + xlab("Round")

