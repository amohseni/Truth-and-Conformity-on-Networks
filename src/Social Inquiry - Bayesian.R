# TRUTH AND CONFORMITY ON NETWORKS
# SIMULATIONS
# by Aydin Mohseni


### Install packages
library(animation)
library(ggplot2)
library(igraph)
# set.seed(13)

### Establish Global Variables
N <- 20 # Number of agents
Alpha <- rbeta(N, 1, 1) # Vector of agent types α=(α_1,...,α_N) 
# where α_i denotes the truth-seeking orientation of agent i
# and (1 - α_i) denotes social coordination orientation of agent i
Duration <- 10 # number of rounds of play
# NetworkChoices <- rep(0, N)
NetworkChoices <- rbinom(N, 1, .5) # inital vector of network choices
HistoryOfPlay <- matrix(NA, nrow = (N*Duration + 1), ncol = N) # history of play
HistoryOfPlay[1,] <- NetworkChoices # Save the initial conditions in the first row of history of play
Prior <- c(0.5) # initial ignorance prior for all agents
PublicBelief <- c() # evolution of public belief
PublicDeclarations <- c() # evolution of public declarations
Theta <- 1 # preset the true state of the world


### Define Functions

  # STATES of the WORLD
  # Define the two distributions f0, f1 corresponding to 
  # the two possible STATES OF THE WORLD: 0, 1
  # as inverse transformations of the uniform distribution
  #   State 0 has pdf : f0(x) = 2 - 2x, 
  #            and cdf: F0(x) = 2x - x^2, hence F0^-1(x) = 1-(1-x)^(1/2)
  #   State 1 has pdf : f1(x) = 2x, 
  #           and cdf : F1(x) = x^2, hence F1^-1(x) = x^(1/2)
  F0_inverse <- function(x) { 1-(1-x)^(1/2) }  
  F1_inverse <- function(x) { x^(1/2) }
  
  
  # SIGNALS about STATES of the WORLD
  # Define the function corresponding to the acquisition of a SIGNAL 
  # by an agent about the true state of the world
  Signal <- function(x) { 
    # Draw a realization from the uniform distribution
    S <- array(runif (1, min = 0, max = 1), 1) 
    # and apply the correct distribution transformation, depending on the state of the world Theta
    ifelse(Theta == 0, w <- apply(S , 1 , FUN = F0_inverse), w <- apply(S , 1 , FUN = F1_inverse))
    return(w)
  }
  
  # Define the CREDENCE function for player i for choice C
  # using Bayes' rule, as Pr(C|D) = Pr(D|C)Pr(C)/Pr(D)
  # That is, the poster probability of the truth of C, 
  # given the product of the likelihood of the datat D given C, 
  # over the total probability of D
  Credence <- function(i, C, S) {
    x <- (1 + ( ( 1 - Prior ) / Prior ) * ( (1 - S) / S ) ) ^ -1
    z <- ifelse( C == 1, x, (1 - x) )
    # print(paste("Player", i, "POSTERIOR PROBABILITY for state", C, sep = " "))
    # print(z)
    return(z)
  }
  
  # Define the TRUTH-SEEKING payoff to player i for choice C
  # as her assessement of the proability of the truth of C
  TruthSeekingPayoff <- function(i, C, S) {
    z <- Credence(i, C, S)
    # print(paste("Player", i, "EPISTEMIC PAYOFF for action", C, sep = " "))
    # print(z)
    return(z)
  }
  
  # Define the SOCIAL COORDINATION payoff to player i for choice C
  CoordinationPayoff <- function(i, C) {
      z <- table(factor(NetworkChoices[-i], c(0,1)))[[C+1]] / (N-1)
      # print(paste("Player", i, "COORDINATION PAYOFF for action", C, sep = " "))
      # print(z)
      return(z)
    }
  
  # Define the EXPECTED PAYOFF to player i for choice C
  # as a convex combination of 
  # the product of her truth-seeking orientation α_i and her truth-seeking payoff
  # and the product of her coordination orientation (1 - α_i) and coordination payoff
  EU <- function(i, C, S) {
    z <- Alpha[i] * TruthSeekingPayoff(i, C, S) + (1 - Alpha[i]) * CoordinationPayoff(i, C)
    # print(paste("Player", i, "EXPECTED UTILITY for action", C, sep = " "))
    # print(z)
    return(z)
  }
  
  # Define the BEST RESPONSE function to player i
  # as playing the declaration of the social policy with the highest expected payoff
  BR <- function(i, S) {
    # print("------------------------------")
    # print(paste("Player", i, "TYPE", sep = " "))
    # print(Alpha[i])
    # print(paste("Player", i, "SIGNAL", sep = " "))
    # print(S)
    z <- which.max( c( EU(i, 0, S), EU(i, 1, S) ) ) - 1
    # print(paste("Player", i, "BEST RESPONSE", sep = " "))
    # print(z)
    # print("------------------------------")
    return(z)
  }
  
  # Compute the LIKELIHOODS of the DECLARATION of the focal agent
  # given each of the states of the world Theta and ¬Theta
  # where here type alpha is denoted by x[1], here signal S is denoted x[2],
  # Ni is the proportion of her neighbors declaring C (corresponding to Theta),
  # and P is her private posterior.
  
  # Define the PUBLIC PRIOR function,
  # whereby all players update their beliefs 
  # based on the declaration of the focal player
  PublicPrior <- function(i, z) {
    
    # Retrieve the relevant parameters
    Pr <-  Prior
    Ns <-  CoordinationPayoff(i, z)
    
    # Then, compute the likelihood P(z|Theta) of her action given the state Theta
    fTheta <- function(a) {
      (1-(1/(1+((Pr/(1-Pr))*(((((1-a)*(1-2*Ns)/(2*a))+(1/2))^-1)-1)))^2))
    }
    if (Ns > .5) {
      L1 <- integrate(fTheta, lower = (1-(1/(2*Ns))), upper = 1)[[1]] + (1 - (1/(2*Ns)))
    } else {
      L1 <- integrate(fTheta, lower = ((1-2*Ns)/(2-2*Ns)), upper = 1)[[1]]
    }
    
    # Next, compute the likelihood P(z|¬Theta) of her action given the state ¬Theta
    fNotTheta <- function(a) {
      (1-(1/(1+((Pr/(1-Pr))*(((((1-a)*(1-2*Ns)/(2*a))+(1/2))^-1)-1)))))
    }
    if (Ns > .5) {
      L0 <- 2*integrate(fNotTheta, lower = (1-(1/(2*Ns))), upper = 1)[[1]] + 2*(1-(1/(2*Ns))) - L1
    } else {
      L0 <- 2*integrate(fNotTheta, lower = ((1-2*Ns)/(2-2*Ns)), upper = 1)[[1]] - L1
    }
    
    # print("LIKELIHOOD L1 of declaration given Theta: 1")
    if (z == 0) { L1 <- (1-L1) } # Take the complement of the probability for declaration ¬z
    # print(L1)
    # print("LIKELIHOOD L0 of declaration given ¬Theta: 0")
    if (z == 0) { L0 <- (1-L0) } # Take the complement of the probability for declaration ¬z
    # print(L0)
    
    # Compute P(Theta|z) or P(Theta|¬z)
    w <- (1+((1-Prior)/Prior)*(L0/L1))^-1
    # print("POSTERIOR/PUBLIC BELIEF")
    # print(w)
    return(w)
  }

#### Initialize Simulation
  
  # Nature randomly chooses the state of the world as either 0 or 1,
  # which determines the corresponding pdfs f1, or F0
  # Theta <- rbinom(1, 1, prob = 0.5) + 1
  # print(paste("The true state of the world is", Theta , sep = ))
  # print(Theta)
  
  # print("------------------------------")
  # print("Initial conditions")
  # print(NetworkChoices)
  
### Run a round of the Simulation
  for(t in 1:Duration) {
    
    # print("------------------------------")
    # print(paste("ROUND", t, sep = ))
    
    # Generate the round's random order of play
    agentIndex <- sample(1:N, N, replace = FALSE)
    
    for(i in 1:N) {
      # Have agent i get her private signal S about the state of the world
      S <- Signal()
      
      # Have agent i update her belief about the state of the world, 
      # calculate her current coordination payoff based on the declarations of  
      # and choose how to act.
      # That is, choose which social policy to DECLARE
      z <- BR(agentIndex[i], S)
      
      # Update the public prior, given the declaration of agent i
      Prior <- PublicPrior(agentIndex[i], z)
      # Update the history of belief
      PublicBelief <- append(PublicBelief, Prior, after = length(PublicBelief))
      
      # Update the vector of current network choices
      # with the agent's DECLARATION z
      NetworkChoices[agentIndex[i]] <- z
      
      # Update the history of play for the round
      HistoryOfPlay[(N*(t-1) + i) + 1, ] <- NetworkChoices
      
      # Record the proportion of players declaring Theta
      PublicDeclarations <- append(PublicDeclarations, table(factor(NetworkChoices, c(0,1)))[[2]] / N, after = length(PublicDeclarations))
    }
    
    # Print the history of play
    # print("------------------------------")
    # print("HISTORY of PLAY")
    # print(HistoryOfPlay)
    
    # Replace the agents 
    # by reset the Alpha vector
    Alpha <- rbeta(N, 1, 1)
  }

  # Graph the evolution of the network
  nodes <- data.frame(c(1:N), t(HistoryOfPlay))
  colnames(nodes) <- c("id", 1:(N*Duration + 1))
  REPL <- function(x) { rep(x, N) }
  edges <- data.frame(from = rep(1:N, each = N), to = rep(1:N, N))
  net <- graph_from_data_frame(d = edges, vertices = nodes, directed = F) 
  net <- simplify(net, remove.multiple = T, remove.loops = T)
  l <- layout_in_circle(net)
  # l <- layout_on_sphere(net)
  
  # Run network animation
  oopt = ani.options(interval = .1)
  for (i in 1: (N*Duration + 1)) {
  plot(net, edge.arrow.size = .4, vertex.label = NA, layout = l,
       vertex.frame.color = nodes[[i+1]]+1, vertex.color = nodes[[i+1]]+1,
       edge.color="gray")
    ani.pause()
  }
  ani.options(oopt)
  
  # Print the evolution of the public belief
  PublicBeliefActionPlot <- data.frame(1:(N*Duration), PublicBelief, PublicDeclarations)
  colnames(PublicBeliefActionPlot) <- c("Turn","PublicBelief")
  p <- ggplot() +
    geom_line(data = PublicBeliefActionPlot, aes(x = Turn, y = PublicBelief), colour="black") + 
    geom_line(data = PublicBeliefActionPlot, aes(x = Turn, y = PublicDeclarations), colour="red") +
    ggtitle(expression(paste("Public belief in ", theta, ", and its declaration S, over time"))) +
    labs(x = "Turn Number", y = NULL) +
    theme(legend.position="none") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0, 1)
  print(p)
  
  