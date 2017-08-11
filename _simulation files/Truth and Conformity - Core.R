  
  # TRUTH AND CONFORMITY ON NETWORKS 
  # SIMULATIONS
  
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com
  
  
  ### Install packages
  library(ggplot2)
  library(igraph)
  
  
  ### Establish Global Variables
  
  N <- 10 # Number of agents
  Duration <- 10000 # Number of rounds of play
  
  # Create Social network
  nodes <- c(1:N) # nodes
  NetworkType <- c("Circle")
  
  networkDensity <- 0.5 # Network density for random networks
  regDegree <- 0.5 # Degree (scaled by population size) for regular networks
  
  InitialDeclarations <- c("EvenSplit") # Initial declarations of the population
  
  # Network type:
  # Complete network
  if (NetworkType == "Complete") { 
    edges <- data.frame(from = rep(1:N, each = N), to = rep(1:N, N))
    selfEdge <- N * c(0:(N-1)) + c(1:N)
    edges <- edges[-selfEdge,]
  }
  # Regular graph
  if (NetworkType == "Regular") {
    degreeDensity <- round((regDegree * N)/2)
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
    numberOfEdges <- ceiling(numberOfPossibleEdges * NetworkDensity)
    possibleEdges <- data.frame(t(combn(1:N, 2)))
    colnames(possibleEdges) <- c("from", "to")
    randomEdgesSubset <- sample(1:numberOfPossibleEdges, numberOfEdges, replace = FALSE)
    edges <- possibleEdges[randomEdgesSubset, ]
  }
  
  # (Adjacency) matrix of the neighbors for each player (node)
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
  # Uniformly at random
  if (InitialDeclarations == "UniformlyAtRandom") {
    NetworkChoices <- rbinom(N, 1, .5)
  }
  # Consensus on false state
  if (InitialDeclarations == "ConsensusOnFalseState") {
    NetworkChoices <- rep(0, N)
  }
  if (InitialDeclarations == "EvenSplit") {
    NetworkChoices <- sample(c(rep(1, floor(N/2)), rep(0, ceiling(N/2))))
  }
  
  # Create new history of play matrix
  HistoryOfPlay <- matrix(NA, nrow = (N * Duration + 1), ncol = N) 
  # Save the initial conditions in the first row of history of play
  HistoryOfPlay[1, ] <- NetworkChoices 
  
  # Create the population types and initial beliefs
  BetaParameterforTruthSeeking <- 1
  BetaParameterforConformity <- 1
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
  Theta <- 1 # where state 0 is denoted by Orange, and 1 by Blue
  
  
  ### Define Functions
  
    # STATES of the WORLD
    # Define the two distributions f0, f1 corresponding to 
    # the two possible STATES OF THE WORLD: 0, 1
    # with the parameters
    Mean0 <- -0.1 # Mean for distribution of signals if Theta is false: state is 0
    Mean1 <- 0.1 # Mean for distribution of signals if Theta is true: state is 1
    Variance <- 1
    StandardDeviation <- sqrt(Variance) # Variance for both distributions of signals
    fTheta0 <- function(Signal) { return( dnorm(Signal, mean = Mean0, sd = StandardDeviation) ) }  
    fTheta1 <- function(Signal) { return( dnorm(Signal, mean = Mean1, sd = StandardDeviation) ) }
    
    
    # SIGNALS about STATES of the WORLD
    # Define the function corresponding to the acquisition of a SIGNAL 
    # by an agent about the true state of the world
    DrawSignal <- function() {
      return(rnorm(1, mean = Mean1, sd = sqrt(Variance)))
    }
    
    # Define the CREDENCE function for player i for choice C
    # using Bayes' rule, as Pr(C|D) = Pr(D|C)Pr(C)/Pr(D)
    # That is, the poster probability of the truth of C, 
    # given the product of the likelihood of the datat D given C, 
    # over the total probability of D
    Credence <- function(FocalAgent, Declaration, Signal) {
      w <- (1 + ((1 - Prior) / Prior) * (fTheta0(Signal) / fTheta1(Signal))) ^ -1
      z <- ifelse(C == 1, w, (1 - w))
      # print(paste("Player", i, "POSTERIOR PROBABILITY for state", C, sep = " "))
      # print(z)
      return(z)
    }
    
    # Define the TRUTH-SEEKING payoff to player i for choice C
    # as her assessement of the proability of the truth of C
    TruthSeekingPayoff <-
      function(FocalAgent, Declaration, Signal) {
        z <- Credence(FocalAgent, Declaration, Signal)
        # print(paste("Player", i, "TRUTH-SEEKING PAYOFF for action", C, sep = " "))
        # print(z)
        return(z)
      }
    
    # Define the SOCIAL COORDINATION payoff to player i for choice C
    CoordinationPayoff <- function(FocalAgent, Declaration) {
      Neighbors <- adjacencyMatrix[FocalAgent, ]
      Neighbors <- Neighbors[!is.na(Neighbors)]
      z <- table(factor(NetworkChoices[Neighbors], c(0, 1)))[[C + 1]] / length(Neighbors)
      # print(paste("Player", i, "COORDINATION PAYOFF for action", C, sep = " "))
      # print(z)
      return(z)
    }
    
    # Define the EXPECTED PAYOFF to player i for choice C
    # as a convex combination of 
    # the product of her truth-seeking orientation α_i and her truth-seeking payoff
    # and the product of her coordination orientation (1 - α_i) and coordination payoff
    EU <- function(FocalAgent, Declaration, Signal) {
      z <- 
        Alpha[FocalAgent] * TruthSeekingPayoff(FocalAgent, Declaration, Signal) + (1 - Alpha[FocalAgent]) * CoordinationPayoff(FocalAgent, Declaration)
      # print(paste("Player", i, "EXPECTED UTILITY for action", C, sep = " "))
      # print(z)
      return(z)
    }
    
    # Define the BEST RESPONSE function to player i
    # as playing the declaration of the social policy with the highest expected payoff
    BR <- function(FocalAgent, Signal) {
      # print("------------------------------")
      # print(paste("Player", i, "TYPE", sep = " "))
      # print(Alpha[i])
      # print(paste("Player", i, "SIGNAL", sep = " "))
      # print(S)
      
      EUvector <- c(EU(FocalAgent, 0, Signal), EU(FocalAgent, 1, Signal))
      # If the payoffs are not tied, choose the declaration with the highest payoff
      if (EUvector[1] != EUvector[2]) {
        z <- which.max(EUvector) - 1
      } else {
        # If there is a payoff tie, choose a declaration at random
        z <- sample(c(1, 0), 1, .5)
      }
      
      # print(paste("Player", i, "BEST RESPONSE", sep = " "))
      # print(z)
      # print("------------------------------")
      return(z)
    }
    
   
    
    # Define the PUBLIC PRIOR function,
    # whereby all players update their beliefs 
    # based on the declaration z of the focal individual i
    PublicPrior <- function(FocalAgent, Declaration) {
      
      # Retrieve the proportion of the focal agent's neighbors 
      # who are declaring state 1
      Ns <- CoordinationPayoff(FocalAgent, 1)
      
      # Check if the focal agent has no neighbors---i.e., she is "isolated"
      # (and hence has no coordination payoff component determining her declaration)
      Neighbors <- adjacencyMatrix[FocalAgent, ]
      Isolated  <- (length(Neighbors[!is.na(Neighbors)]) == 0)
      
      # Compute the LIKELIHOODS of the DECLARATION of the focal agent 
      # for each state of the world, Theta = 1, 0
      if (Isolated == FALSE) {
        
        # Calculate the probability of 
        ProbabilityOfSignalGivenTheta1 <- function(Signal) {
          z <- (1 + ((1 - Prior) / Prior) * exp((-1 / (2 * Variance ^ 2)) * (Mean1 - Mean0) * (2 * Signal - Mean0 - Mean1))) ^ -1
          return(z)
        }
        
        # Compute the threshold signal value needed 
        # for the focal player to have declared state 1
        SignalThreshold <- -(Variance ^ 2) * log((Ns ^ -1 - 1) * (Prior / (1 - Prior))) + (Mean0 - Mean1) / 2
        
        # Compute the threshold alpha value needed 
        # for the focal player to have declared state 1
        AlphaThreshold <- function(Signal) {
          z <- pbeta(0.5 * ((1 - 2 * Ns) / (
            2 * ProbabilityOfSignalGivenTheta(Signal) - Ns
          )) ,
          BetaParameterforTruthSeeking,
          BetaParameterforConformity)
          return(z)
        }
        
        # Compute the two likelihoods P(Declaration = 1 | Theta = 1) and P(Declaration = 1 | Theta = 0)
        if (Ns > 1/2) {
          LikelihoodTheta1 <-
            integrate(
              AlphaThreshold * fTheta1,
              lower = -Inf,
              upper = SignalThreshold
            )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = sqrt(Variance))
        } else {
          LikelihoodTheta1 <-
            integrate(
              (1 - AlphaThreshold) * fTheta1,
              lower = SignalThreshold,
              upper = Inf
            )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = sqrt(Variance))
        }
        
        # Next, compute the likelihood P(Declaration = 1 | Theta = 0)
        if (Ns > 1/2) {
          LikelihoodTheta1 <-
            integrate(
              AlphaThreshold * fTheta0,
              lower = -Inf,
              upper = SignalThreshold
            )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = sqrt(Variance))
        } else {
          LikelihoodTheta1 <-
            integrate(
              (1 - AlphaThreshold) * fTheta0,
              lower = SignalThreshold,
              upper = Inf
            )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = sqrt(Variance))
        }
      }
      
      # In the case where the focal agent is isolated, 
      # calculate the simplified likelihoods as follows 
      if (Isolated == TRUE ) {
        # Compute the likelihood P(Declaration = 1 | Theta = 1)
        L1 <- integrate(fTheta1, lower = (1 - Prior), upper = 1)$value
        # Compute the likelihood P(Declaration = 1 | Theta = 0) 
        L0 <- integrate(fTheta0, lower = (1 - Prior), upper = 1)$value
      }
      
      # If the declaration was of state 0 (rather than 1), 
      # then take the complement of the likelihoods
      # to get the Pr(Declaration = 0 | Theta = 1) and Pr(Declaration = 0 | Theta = 0)
      if (Declaration == 0) {
        LikelihoodTheta1 <- (1 - LikelihoodTheta1) 
        # print("LIKELIHOOD of declaration given Theta = 0")
        LikelihoodTheta0 <- (1 - LikelihoodTheta0)
        # print("LIKELIHOOD L0 of declaration given ¬Theta: 0")
      } 

      
      # Compute P(Theta|z) or P(Theta|¬z)
      z <- (1 + ((1 - Prior) / Prior) * (LikelihoodTheta0 / LikelihoodTheta1)) ^ -1
      # print("POSTERIOR/PUBLIC BELIEF")
      # print(w)
      return(z)
    }
    
  
  #### Initialize Simulation
    
    # Nature randomly chooses the state of the world as either 0 or 1,
    # which determines the corresponding pdfs f1, or F0
    # print(paste("The true state of the world is", Theta , sep = ))
    # print(Theta)
    
    # print("------------------------------")
    # print("Initial declarations")
    # print(NetworkChoices)
    
  ### Run a round of the Simulation
    for(t in 1:Duration) {
      
      # print("------------------------------")
      # print(paste("ROUND", t, sep = ))
      
      # Set initial ignorance prior for all agents,
      if (t == 1) { Prior <- InitialPrior }
      
      # Generate the round's random order of play
      agentIndex <- sample(1:N, N, replace = FALSE)
      
      for(i in 1:N) {
        # Determine which player is the focal player
        # who will receive a signal from nature,
        # and make her public declaration
        FocalAgent <- agentIndex[n]
        
        # Have agent i get her private signal S about the state of the world
        Signal <- DrawSignal()
        
        # Have agent i update her belief about the state of the world, 
        # calculate her current coordination payoff based on the declarations of  
        # and choose how to act.
        # That is, choose which social policy to DECLARE
        DeclarationOfFocalAgent <- BR(FocalAgent, S)
        
        # Update the public prior, given the declaration of agent i
        Prior <- PublicPrior(FocalAgent, DeclarationOfFocalAgent)
        # Update the history of belief
        PublicBelief <- append(PublicBelief, Prior, after = length(PublicBelief))
        
        # Update the vector of current network choices
        # with the agent's DECLARATION z
        NetworkChoices[FocalAgent] <- DeclarationOfFocalAgent
        
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
    # # Print the evolution of the public belief
    # PublicBeliefActionPlot <-
    #   data.frame(1:(N * Duration), PublicBelief, PublicDeclarations)
    # colnames(PublicBeliefActionPlot) <- c("Turn", "PublicBelief")
    # p <- ggplot(PublicBeliefActionPlot, aes(Turn)) +
    #   geom_line(data = PublicBeliefActionPlot, size = 1.5,
    #             aes(x = Turn, y = PublicBelief, colour = "Public Belief in True State")) +
    #   geom_line(data = PublicBeliefActionPlot, size = 1.5,
    #             aes(x = Turn, y = PublicDeclarations, colour = "Declarations of True State  ")) +
    #   ggtitle("Public Belief and Declarations") +
    #   labs(x = "Time", y = "Proportion") +
    #   theme_bw() +
    #   scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1)) +
    #   scale_color_manual(values=c("orange2", "dimgray")) +
    #   theme(legend.position = "bottom",
    #         legend.title = element_blank(),
    #         plot.title = element_text(hjust = 0.5),
    #         text = element_text(size = 16),
    #         legend.text = element_text(size = 14)
    #   )
    # print(p)
  
    