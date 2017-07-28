  ###########################################################################
  # TRUTH AND CONFORMITY ON NETWORKS 
  # SIMULATIONS : PARAMETER SWEEP
  ###########################################################################
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com

  
  ### Install packages
  library(animation)
  library(ggplot2)
  library(igraph)
  
  # Set the working directory as you please  
  setwd("/Users/aydin/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results")
  
  
  ### Establish parameter sweep settings
  numberOfSimulationsPerSetting <- 1000 # Number of simulations per parameter seting
  numberOfTurnsPerSimulation <- 1000 # Number of turns per simulation
  NSweep <- c(50) # List of poplulation size settings
  # NSweep <- c(2, 4, 10, 20, 50) # List of poplulation size settings
  numberOfPopulationSizes <- length(NSweep)
  NetworkTypeSweep <- c("Circle") # List of network types
  # NetworkTypeSweep <- c("Complete", "Regular", "Circle", "Star", "Random") # List of network types
  numberOfNetworkTypes <- length(NetworkTypeSweep)
  InitialDeclarationsSweep <- c("EvenSplit") # List of initial conditions                                     
  # InitialDeclarationsSweep <- c("UniformlyAtRandom", "ConsensusOnFalseState") # List of initial conditions
  numberOfInitialConditions <- length(InitialDeclarationsSweep)
  
  NetworkDensity <- 0.5 # Network density for random networks
  regDegree <- 0.5 # Degree (scaled by population size) for regular networks

    
  ### Establish global variables
  
  # Import simulation variables from parameter sweep
  
  for (i in 1:numberOfInitialConditions) {
    
    InitialDeclarations <- InitialDeclarationsSweep[i] # Set initial declarations  
    
    for (j in 1:numberOfNetworkTypes) {
      
      NetworkType <- NetworkTypeSweep[j] # Set network type
      
      for (k in 1:numberOfPopulationSizes) {
        
        N <- NSweep[k] # Set number of agents
        Duration <- ( numberOfTurnsPerSimulation / N ) # Set number of rounds of play
        
        ### Create data frames in which to store the results of each simulation
        DeclarationDF <- data.frame(matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation))
        BeliefDF <- data.frame(matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation))
        
        for (l in 1:numberOfSimulationsPerSetting) { # Simulation count

          # Complete graph
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
          # Circle graph
          if (NetworkType == "Circle") {
            edges <- data.frame(from = c(1:N), to = c(2:N, 1))
          }
          # Star graph
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
            NetworkChoices <- sample(c(rep(1, N/2), rep(0, N/2)))
          }
          
          # Create new history of play matrix
          HistoryOfPlay <- matrix(NA, nrow = (N * Duration + 1), ncol = N) 
          # Save the initial conditions in the first row of history of play
          HistoryOfPlay[1, ] <- NetworkChoices 
          
          # Create the population types and initial beliefs
          Alpha <- rbeta(N, 1, 1) # Vector of agent types α=(α_1,...,α_N) 
            # where α_i denotes the truth-seeking orientation of agent i
            # and (1 - α_i) denotes her coordination orientation
          Prior <- c(0.5) # initial ignorance prior for all agents
          PublicBelief <- c() # evolution of public belief
          PublicDeclarations <- c() # evolution of public declarations
          
          # Set the true state of the world
          Theta <- 1 # where there are two possible states: 0 and 1
          
          
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
            U <- array(runif(1, min = 0, max = 1), 1) 
            # and apply the correct distribution transformation, depending on the state of the world Theta
            ifelse(Theta == 0, z <- apply(U , 1 , FUN = F0_inverse), z <- apply(U , 1 , FUN = F1_inverse))
            return(z)
          }
          
          # Define the CREDENCE function for player i for choice C
          # using Bayes' rule, as Pr(Theta|S) = Pr(S|Theta)Pr(Theta)/Pr(S)
          # That is, the poster probability of the truth of Theta, 
          # given the product of the likelihood of the signal S given Theta, 
          # over the total probability of S
          Credence <- function(i, State, S) {
            x <- (1 + ( ( 1 - Prior ) / Prior ) * ( (1 - S) / S ) ) ^ -1
            z <- ifelse( State == 1, x, (1 - x) )
            # print(paste("Player", i, "POSTERIOR PROBABILITY for state", State, sep = " "))
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
            Neighbors <- adjacencyMatrix[i, ]
            Neighbors <- Neighbors[!is.na(Neighbors)]
            if (length(Neighbors) != 0) {
              z <- table(factor(NetworkChoices[Neighbors], c(0,1)))[[C+1]] / length(Neighbors)
              # print(paste("Player", i, "COORDINATION PAYOFF for action", C, sep = " "))
              # print(z)
            } else { 
              z <- 0
            }
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
          
          # Compute the LIKELIHOODS of the DECLARATION of the focal agent given
          # each of the states of the world Theta and ¬Theta where here type
          # alpha is denoted by x[1], here signal S is denoted x[2], Ni is the
          # proportion of her neighbors declaring C (corresponding to Theta), 
          # and P is her private posterior.
          
          # Define the PUBLIC PRIOR function,
          # whereby all players update their beliefs 
          # based on the declaration z of the focal individual i
          PublicPrior <- function(i, z) {
            
            # Retrieve the relevant parameters
            Pr <-  Prior
            Ns <-  CoordinationPayoff(i, z)
            
            # Check if agent has no neighbors---i.e., is "isolated"
            # (and hence has no coordination payoff component determining her declaration)
            Neighbors <- adjacencyMatrix[i,]
            Neighbors <- Neighbors[!is.na(Neighbors)]
            Isolated  <- ( length(Neighbors) == 0 )
            
            if (Isolated == FALSE ) {
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
            }
            
            # If the focal agent i is isolated, then calculating the likelihood of 
            if (Isolated == TRUE ) {
              fTheta <- function(a) { 2*a }
              fNotTheta <- function(a) { 2 - 2*a }
              # Compute the likelihood P(z|Theta) of her action given the state Theta
              L1 <- integrate(fTheta, lower = (1-Pr), upper = 1)[[1]]
              # Next, compute the likelihood P(z|¬Theta) of her action given the state ¬Theta
              L0 <- integrate(fNotTheta, lower = (1-Pr), upper = 1)[[1]]
            }
            
            # print("LIKELIHOOD L1 of declaration given Theta: 1")
            if (z == 0) { L1 <- (1-L1) } # Take the complement of the probability for declaration ¬z
            # print(L1)
            # print("LIKELIHOOD L0 of declaration given ¬Theta: 0")
            if (z == 0) { L0 <- (1-L0) } # Take the complement of the probability for declaration ¬z
            # print(L0)
            
            # Compute P(Theta|z) or P(Theta|¬z)
            z <- (1 + ((1 - Prior) / Prior)*(L0 / L1)) ^ -1
            # print("POSTERIOR/PUBLIC BELIEF")
            # print(w)
            return(z)
          }
          
          #### Initialize Simulation
          
          # Nature chooses the state of the world as either 0 or 1,
          # which determines the corresponding pdfs f1, or f0
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
            
            for(n in 1:N) {
              # Determine which player is the focal player
              # who will receive a signal from nature,
              # and make her public declaration
              FocalAgent <- agentIndex[n]
              
              # Have the focal agent n get her private signal S about the state of the world
              S <- Signal()
              
              # Have agent n update her belief about the state of the world, 
              # calculate her current coordination payoff based on the declarations of  
              # and choose how to act.
              # That is, choose which social policy to DECLARE
              DeclarationOfFocalAgent <- BR(FocalAgent, S)
              
              # Update the public prior, given the declaration of agent n
              Prior <- PublicPrior(FocalAgent, DeclarationOfFocalAgent)
              
              # Update the history of belief
              PublicBelief <- append(PublicBelief, Prior, after = length(PublicBelief))
              
              # Update the vector of current network choices
              # with the agent's DECLARATION z
              NetworkChoices[FocalAgent] <- DeclarationOfFocalAgent
              
              # Update the history of play for the round
              HistoryOfPlay[(N*(t-1) + n) + 1, ] <- NetworkChoices
              
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
          
          ### Save the results of the simulation
          DeclarationDF[l,] <- PublicDeclarations
          BeliefDF[l,] <- PublicBelief
            
        }
        
        ### Print the saved results of the simulations
        # Note: If population size digit is a single digit number (i.e., N<10)
        # then add a zero before it in the file name (i.e., N=0N), 
        # to have proper file order sorting when importing for data analysis
        if (NSweep[k] < 10) {
          write.csv(DeclarationDF, file = paste("results.Declarations.NetworkType=", NetworkTypeSweep[j],".N=0", NSweep[k],".Init=", InitialDeclarationsSweep[i],".Runs=", numberOfSimulationsPerSetting, ".csv", sep=""))
          write.csv(BeliefDF, file = paste("results.Beliefs.NetworkType=", NetworkTypeSweep[j],".N=0", NSweep[k],".Init=", InitialDeclarationsSweep[i],".Runs=", numberOfSimulationsPerSetting, ".csv", sep=""))
        } else {
          write.csv(DeclarationDF, file = paste("results.Declarations.NetworkType=", NetworkTypeSweep[j],".N=", NSweep[k],".Init=", InitialDeclarationsSweep[i],".Runs=", numberOfSimulationsPerSetting, ".csv", sep=""))
          write.csv(BeliefDF, file = paste("results.Beliefs.NetworkType=", NetworkTypeSweep[j],".N=", NSweep[k],".Init=", InitialDeclarationsSweep[i],".Runs=", numberOfSimulationsPerSetting, ".csv", sep=""))
        }
      }
    }
  }
  