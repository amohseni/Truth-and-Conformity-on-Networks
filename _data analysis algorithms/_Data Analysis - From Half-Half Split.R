  ###########################################################################
  # TRUTH AND CONFORMITY ON NETWORKS 
  # DATA ANALYSIS: EVEN SPLIT
  ###########################################################################
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com
  
  
  # Load packages from library
  library(reshape2)
  library(ggplot2)
  
  # Set the working directory as you please  
  setwd("/Users/patience/Desktop/results 2/evensplit")
  
  # The number of simulations
  numberOfSimulationsPerSetting <- 10000
  NSweep <- c(2, 4, 10, 20, 50)
  n <- length(NSweep)
  NetworkTypeSweep <- c("Circle", "Complete", "Random", "Regular", "Star")
  
  # List the files in the directory
  filenames4 <- list.files(pattern="*.csv", full.names=TRUE)
  # Upload all CSV files
  ldf4 <- lapply(filenames4, read.csv, header=TRUE, sep=",")
  # Convert the tables to numeric form
  ldf4num <- sapply(ldf4, data.matrix)
  ldf4num <- ldf4num[-c(1:numberOfSimulationsPerSetting),]
  # Get the means of the columns
  allMeans4 <- colMeans(ldf4num)
  # Break this up into Belief and Declaration vectors
  beliefMeans4 <- allMeans4[1:(length(allMeans4)/2)]
  declarationMeans4 <- allMeans4[((length(allMeans4)/2)+1):length(allMeans4)] 
  
  # Break the vectors up further for each the network values
    # Network Belief vectors
    for (c in 1:length(NetworkTypeSweep)) {
      assign(paste(NetworkTypeSweep[c], ".Belief", sep=""), beliefMeans4[(1+n*(c-1)):(c*n)]) 
    }
    # Network Declaration vectors
    for (c in 1:length(NetworkTypeSweep)) {
      assign(paste(NetworkTypeSweep[c], ".Declaration", sep=""), declarationMeans4[(1+n*(c-1)):(c*n)]) 
    }
  
  # Create data frame 
  dfBelief4 <- data.frame(N = NSweep, Complete = Complete.Belief, Regular = Regular.Belief, Circle = Circle.Belief, Star = Star.Belief, Random = Random.Belief)
  dfBelief4Melt <- melt(dfBelief4, id.vars=1)
  colnames(dfBelief4Melt) <- c("N", "Network", "MeanBelief")
  dfDeclaration4 <- data.frame(N = NSweep, Complete = Complete.Declaration, Regular = Regular.Declaration, Circle = Circle.Declaration, Star = Star.Declaration, Random = Random.Declaration)
  dfDeclaration4Melt <- melt(dfDeclaration4, id.vars = 1)
  colnames(dfDeclaration4Melt) <- c("N", "Network", "MeanDeclaration")
  
  print(dfBelief4)
  print(dfDeclaration4)
  
  ### Plot results
  # Belief line plot
  ggplot(data = dfBelief4Melt, aes(x = N, y = MeanBelief)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    geom_point(aes(shape = Network, color = Network), size = 2) +
    ggtitle("Mean Belief in True State as a Function of Population Size") +
    labs(x = "Population Size", y = "Mean Belief") +
    # scale_y_continuous(limits = c(0, .1)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  # Declaration line plot
  ggplot(data = dfDeclaration4Melt, aes(x = N, y = MeanDeclaration)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    geom_point(aes(shape = Network, color = Network), size = 2) +
    ggtitle("Mean Declaration of True State as a Function of Population Size") +
    labs(x = "Population Size", y = "Mean Declaration") +
    # scale_y_continuous(limits = c(0, .92)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
    
  
  