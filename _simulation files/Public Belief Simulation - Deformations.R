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

# STATES of the WORLD
# Define the two distributions f1, f0 corresponding to
# the two possible STATES OF THE WORLD: 1, 0
# with the parameters
Mean1 <-
  1 # Mean for distribution of signals if Theta is true: state is 1
Mean0 <-
  -1 # Mean for distribution of signals if Theta is false: state is 0
Variance <- 10
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

# Truth-seeking/conformity type distribution
# Increasing this will increase the proportion of truth-seeking
BetaParameterforTruthSeeking <- 1
# Increasing this will increase the proportion of conformity
BetaParameterforConformity <- 1

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
  Neighbors <- adjacencyMatrix[FocalAgent, ]
  Neighbors <- Neighbors[!is.na(Neighbors)]
  Ns <-
    table(factor(NetworkChoices[Neighbors], c(0, 1)))[[Declaration + 1]] / length(Neighbors)
  
  # Check if the focal agent has no neighbors---i.e., she is "isolated"
  # (and hence has no coordination payoff component determining her declaration)
  Isolated  <- (length(Neighbors[!is.na(Neighbors)]) == 0)
  
  # If the focal agent is indeed isolated,
  # then let her neighbors be 'tied' in their declarations,
  # so that only her truth-seeking payoff counts.
  if (Isolated == TRUE) {
    Ns <- 1 / 2
  }
  
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


### Create regular networks of degree 2 to N
### then deform them, making them more centralized, step-by-step

# WANT: algorithm that does the following:
# creating a sequence of graphs
# (1) regular networks from degree 2 to N
# (2) from regular networks of centrality 0 to deformed

# Create regular networks of degree 2 to N
# create an empty list to store the networks
regularNetworks.List <- list()

for (degree in 2:(N - 1)) {
  # Create regular network
  if (degree %% 2 == 0) {
    # If the degree is even
    x <- rep(1:N, each = degree / 2)
    y <- x + c(1:(degree / 2))
    y[y > N] <- y[y > N] - N
    edges <- data.frame(from = x, to = y)
  } else {
    # If the degree is odd
    x <- rep(1:N, each = ceiling(degree / 2))
    y <- x + c(1:floor(degree / 2), 0)
    y[seq(
      from = ceiling(degree / 2),
      to = ceiling(degree / 2) * N,
      by = ceiling(degree / 2)
    )] <-
      y[seq(
        from = ceiling(degree / 2),
        to = ceiling(degree / 2) * N,
        by = ceiling(degree / 2)
      )] + N / 2
    y[y > N] <- y[y > N] - N
    edges <- data.frame(from = x, to = y)
  }
  
  adjacencyMatrix <- data.frame(matrix(NA, nrow = N, ncol = N - 1))
  
  for (j in 1:N) {
    adjMatSub <- subset(edges, from == j | to == j)
    adjMatSubUnique <- unique(adjMatSub[adjMatSub != j])
    adjacencyMatrix[j, ] <-
      c(adjMatSubUnique, rep(NA, N - 1 - length(adjMatSubUnique)))
  }
  
  # Save the network to the regular network list
  regularNetworks.List[[degree - 1]] <- adjacencyMatrix
}

# Create a list for the network space
networkSpace.List <- rep(list(list()), N - 1)

for (k in 2:(N - 1)) {
  # Deform the regular networks,
  # increasing centrality with each step
  x <- regularNetworks.List[[k - 1]]
  
  # Create a list in which to save each of the deformed network
  # for the particular starting regular network
  deformedNetworks.List <- list()
  deformedNetworks.List[[1]] <- regularNetworks.List[[k - 1]]
  
  # Identiify all of the links can can be redirected to the hub agent
  # First, find all the nodes that DO link to the hub (so they do no 'double-link')
  u <- c(1)
  for (i in 1:N) {
    if (any(x[i, ] == 1, na.rm = TRUE)) {
      u <- append(u, i, after = length(u))
    }
  }
  # find the set of links NOT to the hub (agent 1)
  w <- which(x != 1, arr.ind = TRUE)
  # take the subset of these that belong to nodes not already linked to the hub agent
  v <- w[-which(w[, 1] %in% u),]
  
  # Proceed only if there are some links that CAN be redirected to a central agent
  if (length(v) > 0) {
    # and limit these so that each agent shifts ONLY ONE
    # of her links to the central agent
    v <- v[c(1:length(unique(v[, 1]))), ]
    if (length(v) > 2) {
      deformations <- dim(v)[1]
    } else {
      demormations <- 1
    }
    
    # Find all the nodes that NOW link to the hub (so they do no 'double-link')
    for (j in 1:deformations) {
      # pick the next link to be deformed
      if (length(v) > 2) {
        z <- v[j, ]
      } else {
        z <- v
      }
      # and replace this link with a link to the hub (agent 1)
      p <- x[z[1], z[2]]
      q <- which(x[p,] == z[1])
      x[p, q] <- NA
      x[z[1], z[2]] <- 1
      deformedNetworks.List[[j + 1]] <- x
    }
  }
  # Save the list of deformed networks for the network space
  networkSpace.List[[k]] <- deformedNetworks.List
}

### Set the number of network groups in the network space
NumberOfNetworkGroups <- length(networkSpace.List) - 1

for (NetworkGroup in 1:NumberOfNetworkGroups) {
  # Load the network group
  NetworkGroup.List <- networkSpace.List[[NetworkGroup + 1]]
  # Set the number of networks
  NumberOfNetworks <- length(NetworkGroup.List)
  
  # Create a list of matrices to store results
  ExpectedPosteriorDf <- rep(list(list()), NumberOfNetworks)
  for (Network in 1:NumberOfNetworks) {
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
  for (Network in 1:NumberOfNetworks) {
    # Import the adjacency matrix of the matrix
    adjacencyMatrix <- NetworkGroup.List[[Network]]
    
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
    
    MeanExpectedPosteriorDf <-
      matrix(data = NA,
             nrow = NumberOfNetworks,
             ncol = N + 1)
    for (Network in 1:NumberOfNetworks) {
      for (NumberOfDeviants in 0:N) {
        MeanExpectedPosteriorDf[Network, NumberOfDeviants + 1] <-
          mean(ExpectedPosteriorDf[[Network]][[NumberOfDeviants + 1]])
      }
    } 
    
  }
  
  df <- data.frame(MeanExpectedPosteriorDf - .5)
  dfgraph <- melt(t(df))
  dfgraph[, 1] <- rep(c(0:N) / N, NumberOfNetworks)
  dfgraph[, 2] <- rep(2:(N - 1), each = N + 1)
  colnames(dfgraph) <-
    c("NumberOfDeviants", "Degree", "ChangeInBelief")
  dfgraph$Degree <-
    factor(dfgraph$Degree, levels = rev(unique(dfgraph$Degree)))
  ggplot(data = dfgraph,
         aes(
           x = NumberOfDeviants,
           y = ChangeInBelief,
           group = rev(Degree)
         )) +
    geom_line(aes(colour = Degree,
                  order = as.numeric(Degree)),
              size = 1) +
    scale_colour_grey() +
    ggtitle("Expected Information of Declarations as a Function of Degree") +
    labs(x = "Proportion of Agents Declaring the True State",
         y = "Expected Information of Declaration") +
    theme_light() +
    scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
      axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
      axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = c(0.9, 0.6),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      text = element_text(size = 14),
      legend.key.height = unit(20, "pt")
    )
}


### Plot the network
for (NetworkDegree in 1:(length(networkSpace.List) - 1)) {
  NetworkGroup <- networkSpace.List[[NetworkDegree + 1]]
  numberOfNetworksInGroup <- length(NetworkGroup)
  
  for (Network in 1:numberOfNetworksInGroup) {
    adjacencyMatrix <- NetworkGroup[[Network]]
    nodeTo <- melt(t(adjacencyMatrix))$value
    nodeFrom <- melt(t(adjacencyMatrix))$Var2
    nodeFrom <- nodeFrom[!is.na(nodeTo)]
    nodeTo <- nodeTo[!is.na(nodeTo)]
    edges <- data.frame(from = nodeFrom, to = nodeTo)
    # g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)
    # edges <- as_edgelist(simplify(g, remove.multiple = TRUE))
    
    # PLOT: format the data for plotting
    net <-
      graph_from_data_frame(d = edges,
                            vertices = 1:N,
                            directed = F)
    net <- simplify(net, remove.multiple = T, remove.loops = T)
    l <- layout_in_circle(net)
    
    # plot network
    plot(
      net,
      edge.arrow.size = .4,
      vertex.label = NA,
      layout = l,
      vertex.frame.color = "black",
      vertex.color = "black",
      edge.color = "gray"
    )
  }
}