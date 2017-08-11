  ###########################################################################
  # TRUTH AND CONFORMITY ON NETWORKS 
  # DATA ANALYSIS: RANDOM INITIAL CONDITIONS
  ###########################################################################
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com
  
  
  # Load packages from library
  library(reshape2)
  
  # Set the working directory as you please  
  setwd("/Users/aydin/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results/from random initial conditions")
  
  # The number of simulations
  numberOfSimulationsPerSetting <- 1000
  NSweep <- c(2, 5, 10, 20, 50)
  n <- length(NSweep)
  NetworkTypeSweep <- c("Circle", "Complete", "Random", "Regular", "Star")
  
  # List the files in the directory
  filenames1 <- list.files(pattern="*.csv", full.names=TRUE)
  # Upload all CSV files
  ldf1 <- lapply(filenames1, read.csv, header=TRUE, sep=",")
  # Convert the tables to numeric form
  ldf1num <- sapply(ldf1, data.matrix)
  ldf1num <- ldf1num[-c(1:numberOfSimulationsPerSetting),]
  # Get the means of the columns
  allMeans1 <- colMeans(ldf1num)
  # Break this up into Belief and Declaration vectors
  beliefMeans1 <- allMeans1[1:(length(allMeans1)/2)]
  declarationMeans1 <- allMeans1[((length(allMeans1)/2)+1):length(allMeans1)] 
  
  # Break the vectors up further for each the network values
    # Network Belief vectors
    for (i in 1:length(NetworkTypeSweep)) {
      assign(paste(NetworkTypeSweep[i], ".Belief", sep=""), beliefMeans1[(1+n*(i-1)):(i*n)]) 
    }
    # Network Declaration vectors
    for (i in 1:length(NetworkTypeSweep)) {
      assign(paste(NetworkTypeSweep[i], ".Declaration", sep=""), declarationMeans1[(1+n*(i-1)):(i*n)]) 
    }
  
  # Create data frame 
  dfBelief1 <- data.frame(N = NSweep, Complete = Complete.Belief, Regular = Regular.Belief, Circle = Circle.Belief, Star = Star.Belief, Random = Random.Belief)
  dfBelief1Melt <- melt(dfBelief1, id.vars=1)
  colnames(dfBelief1Melt) <- c("N", "Network", "MeanBelief")
  dfDeclaration1 <- data.frame(N = NSweep, Complete = Complete.Declaration, Regular = Regular.Declaration, Circle = Circle.Declaration, Star = Star.Declaration, Random = Random.Declaration)
  dfDeclaration1Melt <- melt(dfDeclaration1, id.vars = 1)
  colnames(dfDeclaration1Melt) <- c("N", "Network", "MeanDeclaration")
  
  print(dfBelief1)
  print(dfDeclaration1)
  
  ### Plot results
  # Belief line plot
  ggplot(data = dfBelief1Melt, aes(x = N, y = MeanBelief)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    geom_point(aes(shape = Network, color = Network), size = 2) +
    ggtitle("Mean Belief in True State as a Function of Population Size") +
    labs(x = "Population Size", y = "Mean Belief") +
    scale_y_continuous(limits = c(.5, .9)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  # Declaration line plot
  ggplot(data = dfDeclaration1Melt, aes(x = N, y = MeanDeclaration)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    geom_point(aes(shape = Network, color = Network), size = 2) +
    ggtitle("Mean Declaration of True State as a Function of Population Size") +
    labs(x = "Population Size", y = "Mean Declaration") +
    scale_y_continuous(limits = c(.5, .9)) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
    
  
  