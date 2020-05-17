
generate_neumann <- function(n, p, nei){
  neumann <-  make_lattice(c(sqrt(n),sqrt(n)), nei = nei) %>% 
    rewire(each_edge(p = p)) 
  return(neumann)
}

generate_scalefree <- function(n, degrees, gamma, p = 0){
  degs <- sample(1:degrees, n, replace=TRUE, prob=(1:degrees)^-gamma)
  if (sum(degs) %% 2 != 0) { degs[1] <- degs[1] + 1 } ### Note, that we correct the degree sequence if its sum is odd
 # degs <- degs * 2
  scale_free <- sample_degseq(degs, method = "vl") 
  scale_free <- scale_free %>% rewire(each_edge(p = p)) #ska være meget lav
  return(scale_free)
} 

contagion_sim <- function(tau_type = "base_tau", high_node = F, rep, net_type = "neumann", rounds, tau = 0, n, nei, p = 0, degrees = 0, gamma = 0, stochastic = F){ #assumption network
  for (i in 1:rep){
    #generate network
    if(net_type == "neumann"){
      network <- generate_neumann(n, p, nei)
      networknetwork <- asNetwork(network)
    }
    if(net_type == "scale_free"){
      network <- generate_scalefree(n, degrees, gamma, p)
      networknetwork <- asNetwork(network)
    }
    #preparing first adopter list 
    adopters <- rep(F, n)
    # Choose a person at random
    initial_adopter <- base::sample(seq_len(n), size = 1) #takes one node from the sequence of nodes and classifies it as the inital 
    #adopter
    initial_neighbors <- get.neighborhood(networknetwork, initial_adopter) #shows the 3 neighbors that the infected adopter has
    adopters[c(initial_adopter, initial_neighbors)] <- T #setting all neighbors to the initial infected node as infected
    print(i)
    #preparing the matrix
    adj <- as.matrix(as_adjacency_matrix(network, type = "both", sparse = T))
    diag(adj) <- 0 
    nei_activated_perc <- adj / rowSums(adj)
    if (high_node == T){
      n_high <- n/12
      extra_influ <- ((tau * 12)-1)*n_high
      penalty <- extra_influ / n /12
      nei_activated_perc <- ifelse(nei_activated_perc !=0, nei_activated_perc - penalty, nei_activated_perc) 
      #now everybody has lost the amount of influence that is distrubted to high status nodes now
      #defining highstat nodes
      high_status_nodes <- sample(seq_len(n), size = n_high)
      #looping through all high stat nodes and asssigning new value to them
      for (b in high_status_nodes){
        node <- b
        neighbors <- get.neighborhood(networknetwork, node)
        nei_activated_perc[neighbors, node] <- tau #now all high nodes have tau as influence
      }
    }
    adopt <- vector(mode = "list", length = rounds) 
    adopt[[1]] <- adopters
    if(tau_type == "base_tau" & stochastic == F){
      #preparing tau list
      tau_vec <- as.vector(rep(tau, nrow(adj)))
      for (t in 2:rounds) {
        print(t)
        adopt[[t]] <- ifelse(adopters, TRUE, ((nei_activated_perc %*% adopt[[t - 1]]) >= tau_vec)) 
        
        df <- data.frame(
          network = i,
          round = t,
          adopters = sum(adopt[[t]]),
          high_node = high_node,
          stochastic = paste(stochastic),
          tau_type = paste(tau_type)
        )
        
        if (t == 2) {
          adopt_dat <- df
        }else{
          adopt_dat <- rbind(adopt_dat, df)
        }
      }
      df_1 <- data.frame(
        network = i,
        round = 1,
        adopters = sum(adopt[[1]]),
        high_node = high_node,
        stochastic = paste(stochastic),
        tau_type = paste(tau_type)
      )
      adopt_dat <- rbind(df_1, adopt_dat)
      if (i == 1){
        all_adopters <- adopt_dat
      }else{
        all_adopters <- rbind(all_adopters, adopt_dat)
      }
    }
    if(tau_type == "random_tau" & stochastic == F){ 
      tau_norm <- rnorm(nrow(adj),tau, tau/3)
      tau_vec<- as.vector(sample(tau_norm))
      for (t in 2:rounds) {
        adopt[[t]] <- ifelse(adopters, TRUE, ((nei_activated_perc %*% adopt[[t - 1]]) >= tau_vec)) 
        
        df <- data.frame(
          network = i,
          round = t,
          adopters = sum(adopt[[t]]),
          high_node = high_node,
          stochastic = paste(stochastic),
          tau_type = paste(tau_type)
        )
        if (t == 2) {
          adopt_dat <- df
        }else{
          adopt_dat <- rbind(adopt_dat, df)
        }
      }
      df_1 <- data.frame(
        network = i,
        round = 1,
        adopters = sum(adopt[[1]]),
        high_node = high_node,
        stochastic = paste(stochastic),
        tau_type = paste(tau_type)
      )
      adopt_dat <- rbind(df_1, adopt_dat)
      if (i == 1){
        all_adopters <- adopt_dat
      }else{
        all_adopters <- rbind(all_adopters, adopt_dat)
      }
    }
    if(stochastic == T){
      if (tau_type == "base_tau"){
        tau_vec = as.vector(rep(tau, nrow(adj)))
      }
      else if (tau_type == "random_tau"){
        tau_vec = rnorm(nrow(adj),tau, tau/3) #Some taus are negative which is not good! 
        #They should only be pos so therefore sd = tau/3
      }
      for (t in 2:rounds){
        print(t)
        list <- adopt[[t-1]]
        m = nei_activated_perc %*% adopt[[t - 1]]
        q = m - tau_vec + 0.5
        for (s in 1:length(list)){
          L = 1/(1+exp((.5-q[s])*10)) 
          activation = rbinom(1,1,prob = L)
          if (activation == 1) {
            list[s] = TRUE}
          else if (activation == 0) {#If we remove this else if statement, maybe they cant turn of?
            list[s] = FALSE}
        }
        adopt[[t]] <- list
        
        df <- data.frame(
          network = i,
          round = t,
          adopters = sum(adopt[[t]]),
          high_node = high_node,
          stochastic = paste(stochastic),
          tau_type = paste(tau_type)
        )
        if(t ==2){
          adopt_dat <- df
        }else{
          adopt_dat <- rbind(adopt_dat, df)
        }
      }
      df_1 <- data.frame(
        network = i,
        round = 1,
        adopters = sum(adopt[[1]]),
        high_node = high_node,
        stochastic = paste(stochastic),
        tau_type = paste(tau_type)
      )
      adopt_dat <- rbind(df_1, adopt_dat)
      
      if(i ==1){
        all_adopters <- adopt_dat
      } else {
        all_adopters <- rbind(all_adopters, adopt_dat)
      }
      
    }
    
  }
  return(all_adopters)
}

plot_standard <- function(dataframe, title = "", x_name = "Rounds", y_name = "Number of activated nodes"){
  data = dataframe
  plot <- ggplot(data, aes(round, sumadopt))+
    geom_line(color = "#339900", size = 1.2)+
    scale_colour_pander() +
    scale_fill_pander() +
    theme_minimal()+
    theme_pander()+
    labs(title = title, x = x_name, y = y_name)
  return(plot)
}

sum_data <- function(dataframe, name = ""){
  summed_data <- dataframe %>% group_by(round) %>% summarise(sumadopt = mean(adopters),
                                                             sd = sd(adopters),
                                                             name = name)
  return(summed_data)
}


calculate_point_estimates <- function(data){
  name = data$name[1]
  vec25 = data$sumadopt >= 22500 *0.25
  round25 = which(vec25, T)[1]
  change25 = (data$sumadopt[round25] - data$sumadopt[round25 - 1])/22500 * 100
  vec50 = data$sumadopt >= 22500 *0.5
  round50 = which(vec50, T)[1]
  change50 = (data$sumadopt[round50] - data$sumadopt[round50 - 1])/22500 * 100
  vec75 = data$sumadopt >= 22500 *0.75
  round75 = which(vec75, T)[1]
  change75 = (data$sumadopt[round75] - data$sumadopt[round75 - 1])/22500 * 100
  all <- data.frame(
    Name = paste(name),
    Round_25_activation = paste(round25),
    Change_in_percent = change25,
    Round_50_activation = round50,
    Change_in_percent = change50,
    Round_75_activation = round75,
    Change_in_percent = change75
  )
  return(all)
}


