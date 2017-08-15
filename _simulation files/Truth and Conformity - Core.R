  
  # TRUTH AND CONFORMITY ON NETWORKS 
  # SIMULATIONS
  
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com
  
  
  ### Install packages
  library(ggplot2)
  library(igraph)
  
  
  ### Establish global variables
  
  N <- 20 # Number of agents
  NumberOfTurns <- 20 # Number of turns of play
  NumberOfRounds <- ceiling(NumberOfTurns / N) # Number of full rounds of play
    # where each round is composed of N individual turns
  
  # Create social network
  nodes <- c(1:N) # Nodes
  NetworkType <- c("Complete") # Choose network type to be one of: 
    # 1. Complete
    # 2. Regular
    # 3. Circle
    # 4. Star
    # 5. Random
  
  regularNetworkDegree <- 0.5 # Degree (scaled by population size) for regular networks
  randomNetworkDensity <- 0.5 # Network density for random networks
  
  InitialDeclarations <- c("ConsensusOnFalseState") # Initial declarations of the population, on of:
    # 1. EvenSplit
    # 2. UniformlyAtRandom
    # 3. ConsensusOnFalseState
  
  # Network type:
  # Complete network
  if (NetworkType == "Complete") { 
    edges <- data.frame(from = rep(1:N, each = N), to = rep(1:N, N))
    selfEdge <- N * c(0:(N-1)) + c(1:N)
    edges <- edges[-selfEdge,]
  }
  # Regular network
  if (NetworkType == "Regular") {
    degreeDensity <- round((regularNetworkDegree * N)/2)
    if (degreeDensity >= 1) {
      x <- rep(1:N, each = degreeDensity)
      y <- x + 1:degreeDensity
      y[y > N] <- y[y > N] - N
      edges <- data.frame(from = x, to = y)
    } else {
      edges <- data.frame(from = c(1), to = c(2))
    }
  }
  # Circle network
  if (NetworkType == "Circle") { 
    edges <- data.frame(from = c(1:N), to = c(2:N, 1))
  }
  # Star network
  if (NetworkType == "Star") { 
    edges <- data.frame(from = rep(1, N-1), to = 2:N)
  }
  # Random graph
  if (NetworkType == "Random") {
    numberOfPossibleEdges <- choose(N, 2)
    numberOfEdges <- ceiling(numberOfPossibleEdges * randomNetworkDensity)
    possibleEdges <- data.frame(t(combn(1:N, 2)))
    colnames(possibleEdges) <- c("from", "to")
    randomEdgesSubset <- sample(1:numberOfPossibleEdges, numberOfEdges, replace = FALSE)
    edges <- possibleEdges[randomEdgesSubset, ]
  }
  
  # Adjacency matrix of the neighbors for each player
  adjacencyMatrix <- data.frame(matrix(NA, nrow = N, ncol = N-1))
  for (m in 1:N) {
    adjMatSub <- subset(edges, from == m | to == m)
    if ( nrow(adjMatSub) != 0 ) { 
      adjMatSub <- unique(adjMatSub[adjMatSub != m]) 
    } else { 
      adjMatSub <- c()
    }
    adjacencyMatrix[m, ] <- c(adjMatSub, rep(NA, N - 1 - length(adjMatSub)))
  }
  
  # Initial declarations of agents:
  # From even split, where half the population is making each declaration
  if (InitialDeclarations == "EvenSplit") {
    NetworkChoices <- sample(c(rep(1, floor(N/2)), rep(0, ceiling(N/2))))
    InitialState <- .5
  }
  # Uniformly at random
  if (InitialDeclarations == "UniformlyAtRandom") {
    NetworkChoices <- rbinom(N, 1, .5)
    InitialState <- table(factor(NetworkChoices))[2]/N
  }
  # Consensus on false state
  if (InitialDeclarations == "ConsensusOnFalseState") {
    NetworkChoices <- rep(0, N)
    InitialState <- 0
  }
  
  # Create new history of play matrix
  HistoryOfPlay <- matrix(NA, nrow = NumberOfTurns + 1, ncol = N) 
  # Save the initial conditions in the first row of history of play
  HistoryOfPlay[1, ] <- NetworkChoices
  
  # Create the population types and initial beliefs
  BetaParameterforTruthSeeking <- 1 # Increasing this will increase the proportion of truth-seeking
  BetaParameterforConformity <- 1 # Increasing this will increase the proportion of conformity
  Alpha <-
    rbeta(N,
          BetaParameterforTruthSeeking,
          BetaParameterforConformity) # Vector of agent types α=(α_1,...,α_N) 
      # where α_i denotes the truth-seeking orientation of agent i
      # and (1 - α_i) denotes her coordination orientation
  Prior <- InitialPrior <- c(0.5) # initial ignorance prior for all agents
  PublicBelief <- c() # evolution of public belief
  PublicDeclarations <- c() # evolution of public declarations
  
  # Set the true state of the world
  Theta <- 1 # where-in plots—state 1 is denoted by Orange, and 0 by White
  
  
  ### Define Functions
  
    # STATES of the WORLD
    # Define the two distributions f1, f0 corresponding to 
    # the two possible STATES OF THE WORLD: 1, 0
    # with the parameters
    Mean1 <- 1 # Mean for distribution of signals if Theta is true: state is 1  
    Mean0 <- -1 # Mean for distribution of signals if Theta is false: state is 0
    Variance <- 1
    StandardDeviation <- sqrt(Variance) # Variance for both distributions of signals
    fTheta1 <- function(Signal) { return( dnorm(Signal, mean = Mean1, sd = StandardDeviation) ) }
    fTheta0 <- function(Signal) { return( dnorm(Signal, mean = Mean0, sd = StandardDeviation) ) }  
    
    
    # SIGNALS about STATES of the WORLD
    # Define the function corresponding to the acquisition of a SIGNAL 
    # by an agent about the true state of the world
    DrawSignal <- function() {
      return(rnorm(1, mean = Mean1, sd = sqrt(Variance)))
    }
    
    # Define the CREDENCE function for the focal player for state Theta = 1, 0
    # using Bayes' rule, as Pr(Theta | Signal) = Pr(Signal | Theta) * Pr(Theta) / Pr(Signal)
    # That is, the poster probability of the state Theta, 
    # given the product of the likelihood of the Signal given Theta, 
    # over the total probability of the Signal
    Credence <- function(State, Signal) {
      w <- (1 + ((1 - Prior) / Prior) * (fTheta0(Signal) / fTheta1(Signal))) ^ -1
      z <- ifelse(State == 1, 
                  w, 
                  (1 - w))
      # print(paste("POSTERIOR PROBABILITY for state ", State, " given signal ", Signal, sep = " "))
      # print(z)
      return(z)
    }
    
    # Define the TRUTH-SEEKING payoff to the focal player for a given declaration
    # as her assessement of the proability of the truth of that declaration
    TruthSeekingPayoff <-
      function(Declaration, Signal) {
        z <- Credence(Declaration, Signal)
        # print(paste("TRUTH-SEEKING PAYOFF for action", Declaration, sep = " "))
        # print(z)
        return(z)
      }
    
    # Define the SOCIAL COORDINATION payoff to the focal player for a given declaration
    CoordinationPayoff <- function(FocalAgent, Declaration) {
      Neighbors <- adjacencyMatrix[FocalAgent, ]
      Neighbors <- Neighbors[!is.na(Neighbors)]
      z <- table(factor(NetworkChoices[Neighbors], c(0, 1)))[[Declaration + 1]] / length(Neighbors)
      # print(paste("COORDINATION PAYOFF for action", Declaration, sep = " "))
      # print(z)
      return(z)
    }
    
    # Define the EXPECTED PAYOFF to the focal player for the declaration of a state
    # as a convex combination of 
    # the product of her truth-seeking orientation α_i and her truth-seeking payoff
    # and the product of her coordination orientation (1 - α_i) and coordination payoff
    # for that state
    EU <- function(FocalAgent, Declaration, Signal) {
      z <- 
        Alpha[FocalAgent] * TruthSeekingPayoff(Declaration, Signal) + (1 - Alpha[FocalAgent]) * CoordinationPayoff(FocalAgent, Declaration)
      # print(paste("Player", FocalAgent, "EXPECTED UTILITY for action", Declaration, sep = " "))
      # print(z)
      return(z)
    }
    
    # Define the BEST RESPONSE function of the focal player
    # as selecting the declaration that yields the highest expected payoff
    BR <- function(FocalAgent, Signal) {
      # print("------------------------------")
      # print(paste("Player", FocalAgent, "TYPE", sep = " "))
      # print(Alpha[FocalAgent])
      # print(paste("Player", FocalAgent, "SIGNAL", sep = " "))
      # print(Signal)
      
      EUvector <- c(EU(FocalAgent, 0, Signal), EU(FocalAgent, 1, Signal))
      # If the payoffs are not tied, choose the declaration with the highest payoff
      if (EUvector[1] != EUvector[2]) {
        z <- which.max(EUvector) - 1
      } else {
        # If there is a payoff tie, choose a declaration at random
        z <- sample(c(1, 0), 1, .5)
      }
      
      # print(paste("Player", FocalAgent, "BEST RESPONSE", sep = " "))
      # print(z)
      # print("------------------------------")
      return(z)
    }

    
    # Define the PUBLIC PRIOR function,
    # whereby all players update their beliefs 
    # based on the declaration z of the focal individual i
    PublicPrior <- function(FocalAgent, Declaration) {
      
      # Check if the focal agent has no neighbors---i.e., she is "isolated"
      # (and hence has no coordination payoff component determining her declaration)
      Neighbors <- adjacencyMatrix[FocalAgent,]
      Isolated  <- (length(Neighbors[!is.na(Neighbors)]) == 0)
      ifelse (Isolated == TRUE, # If the focal agent is indeed isolated,
              Ns <- 1 / 2, # then let her neighbors be 'tied' in their declarations, so only her truth-seeking payoff counts
              Ns <- CoordinationPayoff(FocalAgent, 1)) # If the focal agent is not isolated,
              # Then retrieve the proportion of the focal agent's neighbors
              # who are declaring state 1
      
      # Compute the LIKELIHOODS of the DECLARATION of the focal agent 
      # for each state of the world, Theta = 1, 0
        
      # Compute the threshold signal value needed 
      # for the focal player to have declared state 1
      SignalThreshold <- -(Variance) * log((Ns ^ -1 - 1) * (Prior / (1 - Prior))) / (Mean1 - Mean0) + (Mean0 + Mean1) / 2
      
      # Compute the threshold alpha value needed 
      # for the focal player to have declared state 1
      # multiplied by the PDF of 
      AlphaThreshold <- function(Signal) {
        return(pbeta(
          0.5 * ((1 - 2 * Ns) / (Credence(1, Signal) - Ns)) ,
          BetaParameterforTruthSeeking,
          BetaParameterforConformity
        ))
      }
      
      # Create four integrand functions for each likelihood, 
      # in each case: Ns > 1/2, NS < 1/2
      Integrand.fTheta1.A <- function(Signal) { return(fTheta1(Signal) * AlphaThreshold(Signal)) }
      Integrand.fTheta1.B <- function(Signal) { return(fTheta1(Signal) * (1 - AlphaThreshold(Signal))) }
      Integrand.fTheta0.A <- function(Signal) { return(fTheta0(Signal) * AlphaThreshold(Signal)) }
      Integrand.fTheta0.B <- function(Signal) { return(fTheta0(Signal) * (1 - AlphaThreshold(Signal))) }
      
      # Compute the two likelihood values: 
      # P(Declaration = 1 | Theta = 1) and P(Declaration = 1 | Theta = 0)
      
      epsilon <- .1 # to prevent undefined boundary values in integration
      
      # First, compute P(Declaration = 1 | Theta = 1)
      if (Ns > 1/2) {
        LikelihoodTheta1 <-
          integrate(
            Integrand.fTheta1.A,
            lower = -Inf,
            upper = SignalThreshold - epsilon
          )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = StandardDeviation)
      } else {
        LikelihoodTheta1 <-
          integrate(
            Integrand.fTheta1.B,
            lower = SignalThreshold + epsilon,
            upper = Inf
          )$value
      }
      
      # Next, compute P(Declaration = 1 | Theta = 0)
      if (Ns > 1/2) {
        LikelihoodTheta0 <-
          integrate(
            Integrand.fTheta0.A,
            lower = -Inf,
            upper = SignalThreshold - epsilon
          )$value + 1 - pnorm(SignalThreshold, mean = Mean0, sd = StandardDeviation)
      } else {
        LikelihoodTheta0 <-
          integrate(
            Integrand.fTheta0.B,
            lower = SignalThreshold + epsilon,
            upper = Inf
          )$value
      }
      
      # If the declaration was of state 0 (rather than 1), 
      # then take the complement of the likelihoods just computed
      # to get the correct likelihoods Pr(Declaration = 0 | Theta = 1) and Pr(Declaration = 0 | Theta = 0)
      if (Declaration == 0) {
        LikelihoodTheta1 <- (1 - LikelihoodTheta1) 
        # print("LIKELIHOOD of declaration 0 given Theta = 1")
        LikelihoodTheta0 <- (1 - LikelihoodTheta0)
        # print("LIKELIHOOD of declaration 0 given Theta = 0")
      } 

      
      # Compute the new posterior public belief P(Theta | Declaration of focal agent) 
      z <- (1 + ((1 - Prior) / Prior) * (LikelihoodTheta0 / LikelihoodTheta1)) ^ -1
      # print("PUBLIC BELIEF")
      # print(z)
      return(z)
    }
    
  
  #### Initialize Simulation
    
    # print("------------------------------")
    # print("Initial declarations")
    # print(NetworkChoices)
    
  ### Run a round of the Simulation
    for(t in 1:NumberOfRounds) {
      
      # print("------------------------------")
      # print(paste("ROUND", t, sep = ))
      
      # Set initial ignorance prior for all agents,
      if (t == 1) { Prior <- InitialPrior }
      
      # Generate the round's random order of play
      agentIndex <- sample(1:N, N, replace = FALSE)
      
      # Ensure that, for population sizes that don't perfectly divide the total number of rounds,
      # that the simulation still stops at exactly the right turn
      ifelse(N * t <= NumberOfTurns,
             Nt <- N,
             Nt <- (NumberOfTurns - N * (t - 1)))

        
      for(i in 1:Nt) {
        # Determine which player is the focal player
        # who will receive a signal from nature,
        # and then make her public declaration
        FocalAgent <- agentIndex[i]
        
        # Have the focal agent get her private signal about the state of the world
        Signal <- DrawSignal()
        
        # Have the focal agent update her belief about the state of the world, 
        # calculate her expected payoffs for each declaration,
        # and to make the declaration that is her best response
        DeclarationOfFocalAgent <- BR(FocalAgent, Signal)
        
        # Update the public prior, given the declaration of the focal agent
        Prior <- PublicPrior(FocalAgent, DeclarationOfFocalAgent)
        # Update the history of belief in the state Theta = 1
        PublicBelief <- append(PublicBelief, Prior, after = length(PublicBelief))
        
        # Update the vector of current network choices
        # with the agent's declaration
        NetworkChoices[FocalAgent] <- DeclarationOfFocalAgent
        
        # Record the proportion of players declaring Theta = 1
        PublicDeclarations <-
          append(PublicDeclarations,
                 table(factor(NetworkChoices, c(0, 1)))[[2]] / N,
                 after = length(PublicDeclarations))
        
        # Update the history of play for the round
        HistoryOfPlay[(N * (t - 1) + i) + 1,] <- NetworkChoices
      }
      
      # Print the history of play
      # print("------------------------------")
      # print("HISTORY of PLAY")
      # print(HistoryOfPlay)
      
      # Replace the agents 
      # by reset the Alpha vector
      Alpha <- rbeta(N, 
                     BetaParameterforTruthSeeking,
                     BetaParameterforConformity)
    }
  
    # # Graph the evolution of the network
    # nodesOverTime <- data.frame(nodes, t(HistoryOfPlay))
    # colnames(nodesOverTime) <- c("id", 1:(N*Duration + 1))
    # net <- graph_from_data_frame(d = edges, vertices = nodesOverTime, directed = F) 
    # net <- simplify(net, remove.multiple = T, remove.loops = T)
    # l <- layout_in_circle(net)
    # 
    # Run network animation
    # oopt = ani.options(interval = .1)
    # for (i in 1: (N * Duration + 1)) {
    # plot(net, edge.arrow.size = .4, vertex.label = NA, layout = l,
    #      vertex.frame.color = nodesOverTime[[i+1]]+1, vertex.color = nodesOverTime[[i+1]]+1,
    #      edge.color="gray")
    #   ani.pause()
    # }
    # ani.options(oopt)
    # 
    # Print the evolution of the public belief
    PublicBeliefActionPlot <-
      data.frame(0:NumberOfTurns, c(InitialPrior, PublicBelief), c(InitialState, PublicDeclarations))
    colnames(PublicBeliefActionPlot) <- c("Turn", "PublicBelief", "PublicDeclarations")
    p <- ggplot(PublicBeliefActionPlot, aes()) +
      geom_line(size = 1.5,
                aes(x = Turn, y = PublicBelief, colour = "Public Belief in True State")) +
      geom_line(size = 1.5,
                aes(x = Turn, y = PublicDeclarations, colour = "Declarations of True State  ")) +
      ggtitle("Public Belief in and Declaration of the True State") +
      labs(x = "Time", y = "Proportion") +
      theme_light() +
      scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
      scale_color_manual(values = c("orange2", "dimgray")) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16),
        legend.text = element_text(size = 14)
      )
    print(p)

