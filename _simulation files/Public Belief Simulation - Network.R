# PUBLIC BELIEF SIMULATION
# by Aydin Mohseni
# www.aydinmohseni.com

# Load packages
library(combinat)
library(gtools)
library(ggplot2)
library(reshape2)
### Establish global variables

# Set the true state of the world
Theta <- 1

N <- 10 # Number of agents
Prior <- .5 # Initial prior

# Create social network
nodes <- c(1:N) # Nodes
PossibleNetworkTypes <-
  c("Complete", "Regular", "Circle", "Star", "Random")
NumberOfNetworkTypes <- length(PossibleNetworkTypes)
# 1. Complete
# 2. Regular
# 3. Circle
# 4. Star
# 5. Random

# STATES of the WORLD
# Define the two distributions f1, f0 corresponding to
# the two possible STATES OF THE WORLD: 1, 0
# with the parameters
Mean1 <-
  1 # Mean for distribution of signals if Theta is true: state is 1
Mean0 <-
  -1 # Mean for distribution of signals if Theta is false: state is 0
Variance <- 1
StandardDeviation <-
  sqrt(Variance) # Variance for both distributions of signals
fTheta1 <-
  function(Signal) {
    return(dnorm(Signal, mean = Mean1, sd = StandardDeviation))
  }
fTheta0 <-
  function(Signal) {
    return(dnorm(Signal, mean = Mean0, sd = StandardDeviation))
  }

# Create the population types and initial beliefs
BetaParameterforTruthSeeking <-
  1 # Increasing this will increase the proportion of truth-seeking
BetaParameterforConformity <-
  5 # Increasing this will increase the proportion of conformity


# Define the CREDENCE function for the focal player for state Theta = 1, 0
# using Bayes' rule, as Pr(Theta | Signal) = Pr(Signal | Theta) * Pr(Theta) / Pr(Signal)
# That is, the poster probability of the state Theta,
# given the product of the likelihood of the Signal given Theta,
# over the total probability of the Signal
Credence <- function(State, Signal) {
  w <-
    (1 + ((1 - Prior) / Prior) * (fTheta0(Signal) / fTheta1(Signal))) ^ -1
  z <- ifelse(State == 1,
              w,
              (1 - w))
  # print(paste("POSTERIOR PROBABILITY for state ", State, " given signal ", Signal, sep = " "))
  # print(z)
  return(z)
}

PublicPrior <- function(FocalAgent, Declaration) {
  
  # Retrieve the proportion of the focal agent's neighbors
  # who are declaring state 1
  Neighbors <- adjacencyMatrix[FocalAgent,]
  Neighbors <- Neighbors[!is.na(Neighbors)]
  Ns <-
    table(factor(NetworkChoices[Neighbors], c(0, 1)))[[Declaration + 1]] / length(Neighbors)
  
  # Check if the focal agent has no neighbors---i.e., she is "isolated"
  # (and hence has no coordination payoff component determining her declaration)
  Isolated  <- (length(Neighbors[!is.na(Neighbors)]) == 0)
  
  # If the focal agent is indeed isolated,
  # then let her neighbors be 'tied' in their declarations, 
  # so that only her truth-seeking payoff counts.
    if (Isolated == TRUE) { Ns <- 1 / 2 }
  
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
  Integrand.fTheta1.A <-
    function(Signal) {
      return(fTheta1(Signal) * AlphaThreshold(Signal))
    }
  Integrand.fTheta1.B <-
    function(Signal) {
      return(fTheta1(Signal) * (1 - AlphaThreshold(Signal)))
    }
  Integrand.fTheta0.A <-
    function(Signal) {
      return(fTheta0(Signal) * AlphaThreshold(Signal))
    }
  Integrand.fTheta0.B <-
    function(Signal) {
      return(fTheta0(Signal) * (1 - AlphaThreshold(Signal)))
    }
  
  # Compute the two likelihood values:
  # P(Declaration = 1 | Theta = 1) and P(Declaration = 1 | Theta = 0)
  
  # First, compute P(Declaration = 1 | Theta = 1)
  if (Ns > 1 / 2) {
    LikelihoodTheta1 <-
      integrate(Integrand.fTheta1.A,
                lower = -Inf,
                upper = SignalThreshold)$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = StandardDeviation)
  } else {
    LikelihoodTheta1 <-
      integrate(Integrand.fTheta1.B,
                lower = SignalThreshold,
                upper = Inf)$value
  }
  
  # Next, compute P(Declaration = 1 | Theta = 0)
  if (Ns > 1 / 2) {
    LikelihoodTheta0 <-
      integrate(Integrand.fTheta0.A,
                lower = -Inf,
                upper = SignalThreshold)$value + 1 - pnorm(SignalThreshold, mean = Mean0, sd = StandardDeviation)
  } else {
    LikelihoodTheta0 <-
      integrate(Integrand.fTheta0.B,
                lower = SignalThreshold,
                upper = Inf)$value
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
  
  # Save the likelihood P(Declaration = 1 | Theta = 1)
  y <- LikelihoodTheta1
  
  # Compute the new posterior public belief P(Theta | Declaration of focal agent)
  z <-
    (1 + ((1 - Prior) / Prior) * (LikelihoodTheta0 / LikelihoodTheta1)) ^ -1
  # print("PUBLIC BELIEF")
  # print(z)
  return(list(y, z))
}


# Create a list of matrices to store results
ExpectedPosteriorDf <- rep(list(list()), NumberOfNetworkTypes)
for (Network in 1:NumberOfNetworkTypes) {
  for (NumberOfDeviants in 0:N) {
    ExpectedPosteriorDf[[Network]][[NumberOfDeviants + 1]] <-
      matrix(
        data = NA,
        nrow = choose(N, NumberOfDeviants),
        ncol = N
      )
  }
}

# Loop for each network type
for (Network in 1:NumberOfNetworkTypes) {
  # Network type:
  NetworkType <-
    PossibleNetworkTypes[Network] # Choose network type to be one of:
  
  regularNetworkDegree <-
    0.5 # Degree (scaled by population size) for regular networks
  randomNetworkDensity <- 0.5 # Network density for random networks
  
  # Complete network
  if (NetworkType == "Complete") {
    edges <- data.frame(from = rep(1:N, each = N), to = rep(1:N, N))
    selfEdge <- N * c(0:(N - 1)) + c(1:N)
    edges <- edges[-selfEdge,]
  }
  # Circle network
  if (NetworkType == "Circle") {
    edges <- data.frame(from = c(1:N), to = c(2:N, 1))
  }
  # Star network
  if (NetworkType == "Star") {
    edges <- data.frame(from = rep(1, N - 1), to = 2:N)
  }
  # Regular network
  if (NetworkType == "Regular") {
    degreeDensity <- round((regularNetworkDegree * N) / 2)
    if (degreeDensity >= 1) {
      x <- rep(1:N, each = degreeDensity)
      y <- x + 1:degreeDensity
      y[y > N] <- y[y > N] - N
      edges <- data.frame(from = x, to = y)
    } else {
      edges <- data.frame(from = c(1), to = c(2))
    }
  }
  # Random graph
  if (NetworkType == "Random") {
    numberOfPossibleEdges <- choose(N, 2)
    numberOfEdges <-
      ceiling(numberOfPossibleEdges * randomNetworkDensity)
    possibleEdges <- data.frame(t(combn(1:N, 2)))
    colnames(possibleEdges) <- c("from", "to")
    randomEdgesSubset <-
      sample(1:numberOfPossibleEdges, numberOfEdges, replace = FALSE)
    edges <- possibleEdges[randomEdgesSubset,]
  }
  
  # Adjacency matrix of the neighbors for each player
  adjacencyMatrix <- data.frame(matrix(NA, nrow = N, ncol = N - 1))
  for (m in 1:N) {
    adjMatSub <- subset(edges, from == m | to == m)
    if (nrow(adjMatSub) != 0) {
      adjMatSub <- unique(adjMatSub[adjMatSub != m])
    } else {
      adjMatSub <- c()
    }
    adjacencyMatrix[m, ] <-
      c(adjMatSub, rep(NA, N - 1 - length(adjMatSub)))
  }
  
  # Expected public belief in next period q'= (1/N) Sum^N_i=1 [q(S|i)P(S|i)+q(~S|i)P(~S|i)]
  # Loop through all combinations of possible declarations where some number of agents are deviating
  for (NumberOfDeviants in 0:N) {
    # Create a matrix of all possible configurations of declarations
    # for each number of deviating declarations,
    # where consensus declaration = 0, and deviation = 1
    PossibleDeclarationConfigurations <-
      t(combn(N, NumberOfDeviants, function(x)
        tabulate(x, N)))
    NumberOfPossibleDeclarationConfigurations <-
      nrow(PossibleDeclarationConfigurations)
    
    
    # Loop for a single graph, with specific declarations
    for (DeclarationConfiguration in 1:NumberOfPossibleDeclarationConfigurations) {
      NetworkChoices <-
        PossibleDeclarationConfigurations[DeclarationConfiguration, ]
      
      for (FocalAgent in 1:N) {
        qs1 <- PublicPrior(FocalAgent, 1)
        LikelihoodOfS <- qs1[[1]]
        PosteriorGivenS <- qs1[[2]]
        qs2 <- PublicPrior(FocalAgent, 0)
        LikelihoodOfNotS <- qs2[[1]]
        PosteriorGivenNotS <- qs2[[2]]
        ExpectedPosteriorDf[[Network]][[NumberOfDeviants + 1]][DeclarationConfiguration, FocalAgent] <-
          LikelihoodOfS * PosteriorGivenS + LikelihoodOfNotS * PosteriorGivenNotS
        
      }
    }
  }
}

MeanExpectedPosteriorDf <-
  matrix(data = NA,
         nrow = NumberOfNetworkTypes,
         ncol = N + 1)
for (Network in 1:NumberOfNetworkTypes) {
  for (NumberOfDeviants in 0:N) {
    MeanExpectedPosteriorDf[Network, NumberOfDeviants + 1] <-
      mean(ExpectedPosteriorDf[[Network]][[NumberOfDeviants + 1]])
  }
}


df <- data.frame(MeanExpectedPosteriorDf - .5)
dfgraph <- melt(t(df))
dfgraph[, 1] <- rep(c(0:N) / N, NumberOfNetworkTypes)
dfgraph[, 2] <-
  rep(PossibleNetworkTypes[1:NumberOfNetworkTypes], each = N + 1)
colnames(dfgraph) <-
  c("NumberOfDeviants", "Network", "ChangeInBelief")
ggplot(data = dfgraph,
       aes(x = NumberOfDeviants, y = ChangeInBelief, group = Network)) +
  geom_line(aes(colour = Network), size = 1) +
  ggtitle("Expected Information of Declarations on Different Networks") +
  labs(x = "Proportion of Agents Declaring the True State", y = "Expected Information of Declaration") +
  theme_light() +
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
    axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.85, 0.7),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    text = element_text(size = 14),
    legend.key.height = unit(20, "pt")
  )
