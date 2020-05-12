library(ggplot2)
M = 10
wow = seq(0, 1, by = 0.01)

library(tidyverse)

sto <- tibble(p = wow, L = 1/(1+exp((.5-p)*M)))


ggplot(sto, aes(p,L))+
  geom_point()+
  geom_line()

#for an agents with tau = 4/10, L when p is 0.4 is equal to L for 0.5
#Which means that the L we want for 4/10 activated neighbors is equal to p-tau  + 0.5

tau = 0.33
p = 0.1
M=10

q = p-tau + 0.5
q = -1.4
L = 1/(1+exp((.5-q)*M))

rbinom(1,1,prob = L)











sw.net <- igraph::watts.strogatz.game(1, 50, 2, p = 0.00) %>%
  intergraph::asNetwork(.) 
plot(sw.net)

n = 50
# Set up vector to indicate adoption
adopters <- rep(F, 50)
adopters

# Choose a person at random
initial.adopter <- sample(seq_len(n), size = 1)

# Get the list of people they're attached to
initial.neighbors <- get.neighborhood(sw.net, initial.adopter)

# Set them all as "adopters"
adopters[c(initial.adopter, initial.neighbors)] <- T

#Make adjacency matrix
adj <- sw.net[, ]
diag(adj) <- 0

#Calculate percentage
(nei_activated_perc <- adj / rowSums(adj))

rounds = 10
adopt <- vector(mode = "list", length = rounds)
adopt[[1]] <- adopters
adopt

stochastic = T
condition = "base_tau"
tau = 2/6



if(stochastic == T){
  if (condition == "base_tau"){
    tau_vec = as.vector(rep(tau, nrow(adj)))
  }
  else if (condition == "random_tau"){
    tau_vec = rnorm(nrow(adj),tau, tau/3) #Some taus are negative which is not good! 
    #They should only be pos so therefore sd = tau/3
  }
  for (t in 2:rounds){
    list <- adopt[[t-1]]
    m = nei_activated_perc %*% adopt[[t-1]] # m = p = percentage activated neighbors
    q = m - tau_vec + 0.5
    for (s in 1:length(list)){
    L = 1/(1+exp((.5-q[s])*10)) #We just use M = 10 because that is was Macy does but this could be discussed
    activation = rbinom(1,1,prob = L)
    if (activation == 1) {
      list[s] = TRUE}
    else if (activation == 0) {  #If we remove this else if statement, maybe they cant turn of?
      list[s] = FALSE}
    }
    adopt[[t]] <- list
  }
}

adopt

