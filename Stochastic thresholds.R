library(pacman)
p_load(statnet, tidyverse, magrittr, ggnetwork, EpiModel, intergraph, igraph)

#Set up network
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
adj.mat <- sw.net[, ]
diag(adj.mat) <- 0

#Calculate percentage
adj.mat.rn <- adj.mat / rowSums(adj.mat)

max.time = 10
adopt <- vector(mode = "list", length = max.time)
adopt[[1]] <- adopters
adopt

stochastic <- T
tau_vec <- rep(0.1,50)
M <- 10

for (t in 2:max.time){
  p <- adj.mat.rn %*% adopt[[t-1]]
  list <- adopt[[t-1]]
  for (i in 1:length(list)){
    if (stochastic == T){
    L = 1/(1+exp((.5-p[i])*M))
    activation = rbinom(1,1,prob = L)
      if (activation == 1) {
        list[i] = TRUE}
      else if (activation == 0) {
        list[i] = FALSE}
    }
      else {
        list[i] = FALSE
      }
    if (stochastic == F){
      adopt[[t]] <- ifelse(adopters, TRUE, ((adj.mat.rn %*% adopt[[t - 1]]) >= tau_vec))
    }
  }
  adopt[[t]] <- list
}

adopt


