library(ggplot2)
M = 8
wow = seq(0, 1, by = 0.01)

sto <- tibble(p = wow, L = 1/(1+exp((.5-p)*M)))


ggplot(sto, aes(p,L))+
  geom_point()+
  geom_line()
#for an agents with tau = 4/10, L when p is 0.4 is equal to L for 0.5
#Which means that the L we want for 4/10 activated neighbors is equal to p-tau  + 0.5

tau = 0.6
p = 0.4

q = p-tau + 0.5
1/(1+exp((.5-q)*M))

