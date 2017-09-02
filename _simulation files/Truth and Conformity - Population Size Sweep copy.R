  # TRUTH AND CONFORMITY ON NETWORKS 
  # SIMULATIONS : PARAMETER SWEEP — POPULATION SIZES

  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com

  

  # Set the working directory as you please  
  setwd("/Users/patience/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results/highVariation")
  
  
  ### Establish parameter sweep settings
  numberOfSimulationsPerSetting <- 100000 # Number of simulations per parameter seting
  numberOfTurnsPerSimulation <- 100 # Number of turns per simulation
  NSweep <- seq(from = 2, to = 20, by = 2) # List of poplulation size settings
  numberOfPopulationSizes <- length(NSweep) # Number of population sizes to simulate
  NetworkTypeSweep <- c("Circle", "Complete", "Random", "Regular", "Star") # List of network types, from among:
  # 1. Complete
  # 2. Regular
  # 3. Circle
  # 4. Star
  # 5. Random
  numberOfNetworkTypes <- length(NetworkTypeSweep) # Number of network types to simulate
  InitialDeclarationsSweep <- c("ConsensusOnFalseState", "EvenSplit") # List of initial conditions, from among:
  # 1. EvenSplit
  # 2. UniformlyAtRandom
  # 3. ConsensusOnFalseState  
  numberOfInitialConditions <- length(InitialDeclarationsSweep) # Number of initial conditions to simulate
  
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
        Duration <- ceiling( numberOfTurnsPerSimulation / N ) # Set number of rounds of play
        
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
          # From even split
          if (InitialDeclarations == "EvenSplit") {
            NetworkChoices <- sample(c(rep(1, N/2), rep(0, N/2)))
          }
          # Uniformly at random
          if (InitialDeclarations == "UniformlyAtRandom") {
            NetworkChoices <- rbinom(N, 1, .5)
          }
          # Consensus on false state
          if (InitialDeclarations == "ConsensusOnFalseState") {
            NetworkChoices <- rep(0, N)
          }
          
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
          Variance <- 50
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
            if (length(Neighbors) != 0) {
              z <- table(factor(NetworkChoices[Neighbors], c(0, 1)))[[Declaration + 1]] / length(Neighbors)
              # print(paste("Player", i, "COORDINATION PAYOFF for action", C, sep = " "))
              # print(z)
            } else { 
              z <- 0
            }
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
            SignalThreshold <-
              -(Variance) * log((Prior / (1 - Prior))) / (Mean1 - Mean0) + (Mean0 + Mean1) / 2
            
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
            
            # First, compute P(Declaration = 1 | Theta = 1)
            if (Ns > 1/2) {
              LikelihoodTheta1 <-
                integrate(
                  Integrand.fTheta1.A,
                  lower = -Inf,
                  upper = SignalThreshold
                )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = StandardDeviation)
            } else {
              LikelihoodTheta1 <-
                integrate(
                  Integrand.fTheta1.B,
                  lower = SignalThreshold,
                  upper = Inf
                )$value
            }
            
            # Next, compute P(Declaration = 1 | Theta = 0)
            if (Ns > 1/2) {
              LikelihoodTheta0 <-
                integrate(
                  Integrand.fTheta0.A,
                  lower = -Inf,
                  upper = SignalThreshold
                )$value + 1 - pnorm(SignalThreshold, mean = Mean0, sd = StandardDeviation)
            } else {
              LikelihoodTheta0 <-
                integrate(
                  Integrand.fTheta0.B,
                  lower = SignalThreshold,
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
            
            # Set initial ignorance prior for all agents
            if (t == 1) { Prior <- InitialPrior }
            
            # Generate the round's random order of play
            agentIndex <- sample(1:N, N, replace = FALSE)
            
            # Ensure that, for population sizes that don't perfectly divide the total number of rounds,
            # that the simulation still stops at exactly the right time
            ifelse(N * t <= numberOfTurnsPerSimulation,
                   Nt <- N,
                   Nt <- (numberOfTurnsPerSimulation - N * (t - 1)))
            
            for(n in 1:Nt) {
              # Determine which player is the focal player
              # who will receive a signal from nature,
              # and make her public declaration
              FocalAgent <- agentIndex[n]
              
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
              
              # Record the proportion of players declaring Theta
              PublicDeclarations <-
                append(PublicDeclarations,
                       table(factor(NetworkChoices, c(0, 1)))[[2]] / N,
                       after = length(PublicDeclarations))
            }
            
            # Replace the agents by resetting the Alpha vector
            Alpha <- rbeta(N, 
                           BetaParameterforTruthSeeking,
                           BetaParameterforConformity)
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
  