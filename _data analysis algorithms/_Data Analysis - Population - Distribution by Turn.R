  ###########################################################################
  # TRUTH AND CONFORMITY ON NETWORKS 
  # DATA ANALYSIS: DISTRIBUTION by TURN
  ###########################################################################
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com
  
  
  # Load packages from library
  library(reshape2)
  library(ggplot2)
  
  # Set the working directory as you please  
  setwd("/Users/patience/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results/distribution.byTurn")
  
  # The number of simulations
  numberOfSimulations <- 10000
  numberOfTurnsPerSimulation <- 100
  
  N <- 10
  NetworkTypeSweep <- c("Circle", "Complete", "Random", "Regular", "Star")
  NumberOfNetworks <- length(NetworkTypeSweep)
  
  # List the files in the directory
  filenames6 <- list.files(pattern = "*.csv", full.names = TRUE)
  # Upload all CSV files
  ldf6 <- lapply(filenames6, read.csv, header = TRUE, sep = ",")
  # Convert the tables to numeric form
  ldf6num <- sapply(ldf6, data.matrix)
  ldf6num <- ldf6num[-c(1:numberOfSimulations), ]
  # Break this up into Belief and Declaration vectors
  dfBelief6 <- ldf6num[, c(1:NumberOfNetworks)]
  dfDeclaration6 <- ldf6num[, c((NumberOfNetworks + 1):(2 * NumberOfNetworks))]
  
  # Get the Belief means for each round, for each network
  df.MeanBelief.PerRound <-
    data.frame(matrix(
      data = NA,
      nrow = numberOfTurnsPerSimulation,
      ncol = NumberOfNetworks
    ))
  for (i in 1:NumberOfNetworks) {
    for (j in 1:numberOfTurnsPerSimulation) {
      df.MeanBelief.PerRound[j,i] <- mean(dfBelief6[c((1 + (j-1) * numberOfSimulations):(j * numberOfSimulations)), i])
    }
  }
  df.MeanBelief.PerRound <- cbind(c(1:numberOfTurnsPerSimulation), df.MeanBelief.PerRound)
  colnames(df.MeanBelief.PerRound) <- c("Turn", NetworkTypeSweep)
  df.MeanBelief.PerRound <- melt(df.MeanBelief.PerRound, id.vars = "Turn")
  colnames(df.MeanBelief.PerRound) <- c("Turn", "Network", "Mean.Belief")
  
  # Get the Declaration means for each round, for each network
  df.MeanDeclaration.PerRound <-
    data.frame(matrix(
      data = NA,
      nrow = numberOfTurnsPerSimulation,
      ncol = NumberOfNetworks
    ))
  for (i in 1:NumberOfNetworks) {
    for (j in 1:numberOfTurnsPerSimulation) {
      df.MeanDeclaration.PerRound[j,i] <- mean(dfDeclaration6[c((1 + (j-1) * numberOfSimulations):(j * numberOfSimulations)), i])
    }
  }
  df.MeanDeclaration.PerRound <- cbind(c(1:numberOfTurnsPerSimulation), df.MeanDeclaration.PerRound)
  colnames(df.MeanDeclaration.PerRound) <- c("Turn", NetworkTypeSweep)
  df.MeanDeclaration.PerRound <- melt(df.MeanDeclaration.PerRound, id.vars = "Turn")  
  colnames(df.MeanDeclaration.PerRound) <- c("Turn", "Network", "Mean.Declaration")
  
  ### Plot results
  # Belief line plot
  ggplot(data = df.MeanBelief.PerRound, aes(x = Turn, y = Mean.Belief)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    # geom_point(aes(shape = Network, color = Network), size = 2) +
    ggtitle("Mean Belief over Time") +
    labs(x = "Turn", y = "Mean Belief") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  # Declaration line plot
  ggplot(data = df.MeanDeclaration.PerRound, aes(x = Turn, y = Mean.Declaration)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    # geom_point(aes(shape = Network, color = Network), size = 2) +
    ggtitle("Mean Declaration over Time") +
    labs(x = "Turn", y = "Mean Belief") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
    
  
  