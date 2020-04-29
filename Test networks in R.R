##Trying out the network tutorial

# Following best practices, we begin by setting up the workspace
rm(list = ls())
setwd("~/Uni/4. semester/Exam/SocCult")
library(pacman)
p_load(statnet, tidyverse, magrittr, ggnetwork, EpiModel) #Last two are dedicated diffusion
#install.packages("netdiffuseR") #This does not work


# Typically, the purpose of a diffusion simulation is to illustrate the effects
# of a new diffusion process, or a new type of network structure, on the
# ultimate results of a diffusion process.  As such diffusion simulations are 
# highly variable -- after all, the point for each one is to show something new!

# However, despite the variability, most diffusion simulations build on a
# common set of building blocks:
# - an underlying network structure,
# - a transmission process, and
# - a running timer.
# In essence, we are defining an algorithm, or a set of rules, for how people
# get "infected" with a social contagion, and looking at how those rules perform
# under differnt conditions.

# To simulate random mixing, we construct the following rules:
# 1. Set up a world with a fixed number of people and a small fraction of 
#    initial infections, arranged randomly as a network
# 2. Pick an edge at random
# 3. If the edge is discordant (a susceptible-infected connection), flip a coin
#    to determine whether the non-infected person gets infected
# 4. Repeat steps 1 and 2 until everyone is infected, or until a certain number
#    of steps has passed


###COMPLEX CONTAGIONS 

### Set parameters in advance ###
n <- 100
tau <- 2 / 6
max.time <- 50
sqrt(n)

library(pacman)
p_load(intergraph, igraph)

# Construct a small world network
set.seed(919)
sw.net <- igraph::watts.strogatz.game(1,n, 2, p = 0) %>%
  intergraph::asNetwork(.) 
plot(sw.net)

#Making Moore lattice without rewiring
moore <- make_lattice(c(sqrt(n),sqrt(n)), nei =2) %>% asNetwork(.)
plot(moore)

#Making Moore lattice with rewiring
moore_re <- make_lattice(c(sqrt(n),sqrt(n)), nei =2) %>% rewire(each_edge(p = .005)) %>% asNetwork(.)
plot(moore_re)

#Random degree - how???? 

plot(sw.net)

rnorm(50, 2,1)
pois <- rpois(4800000, 2.3)
summary(pois)



### Step 2: Choose a person & neighbors at random as an initial adopter ###
# Set up vector to indicate adoption
adopters <- rep(F, n)
adopters

# Choose a person at random
initial.adopter <- sample(seq_len(n), size = 1)

# Get the list of people they're attached to
initial.neighbors <- get.neighborhood(sw.net, initial.adopter)
initial.neighbors

initial.neighbors <- get.neighborhood(moore_re, initial.adopter)
initial.neighbors

# Set them all as "adopters"
adopters[c(initial.adopter, initial.neighbors)] <- T
adopters

### Step 3: infect all people who have more than tau neighbors infected ###

# Let's look at one person (20) #This gives us which of 20's neighbors are connected to each other
ego.extract(sw.net, ego = 20, neighborhood = "out") 

# Person 18 hasn't adopted
adopters[20]

# About half of their neighbors have adopted
adopters[c(18, 19, 22, 21)]
mean(adopters[c(18, 19, 22, 25)])

# To decide whether the person adopts, we test whether the fraction of
# adopters is greater than tau
mean(adopters[c(18, 19, 22, 25)]) >= tau  # adoption!

# We can update everyone simultaneously using matrix multiplication
adj.mat <- sw.net[, ]
diag(adj.mat) <- 0  # set the diagonal to 0, b/c people don't weight themselves

adj.mat <- moore_re[, ]
diag(adj.mat) <- 0 

# We could take the sum...
adj.mat %*% adopters

#... or the percentage
(adj.mat.rn <- adj.mat / rowSums(adj.mat))
adj.mat.rn %*% adopters

# And then we calculate the people who are above our threshold
(adj.mat.rn %*% adopters) >= tau

# Note that this actually allows people to abandon the innovation if enough of
# of their neighbors are not adopters.  For now we don't want that to happen, 
# so we'll only test people who are not yet adopters
ifelse(adopters, TRUE, ((adj.mat.rn %*% adopters) >= tau))

### Step 4: Repeat ###
# Again, we can take care of this by wrapping it in a loop
adopt <- vector(mode = "list", length = max.time)
adopt[[1]] <- adopters
adopt

for (t in 2:max.time) {
  adopt[[t]] <- ifelse(adopters, TRUE, ((adj.mat.rn %*% adopt[[t - 1]]) >= tau))
}

# Note that again we get the characteristic S-shaped curve:
data.frame(
  t = 1:max.time,
  n.adopt = sapply(adopt, sum)
) %>%
  ggplot(aes(x = t, y = n.adopt)) + 
  geom_line()

set.seed(330)
sw.net.layout <- ggnetwork(sw.net) %>% 
  rename(id = vertex.names)

sw.net.layout.by.time <- adopt %>%
  lapply(FUN = as.data.frame) %>% 
  lapply(FUN = set_names, value = "adopter") %>% 
  lapply(FUN = mutate, id = 1:n) %>%
  lapply(FUN = right_join, y = sw.net.layout, by = "id") %>% 
  bind_rows(.id = "t") %>% 
  mutate(t = as.integer(t))

sw.net.layout.by.time %>% 
  filter(t > 14 & t < 24) %>% 
  ggplot(aes(xend = xend, yend = yend, x = x, y = y)) + 
  geom_edges(color = "lightgray") +
  geom_nodes(aes(color = adopter)) + 
  facet_wrap(~ t) + 
  theme_blank()

