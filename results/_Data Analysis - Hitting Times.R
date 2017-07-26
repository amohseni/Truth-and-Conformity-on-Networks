  ###########################################################################
  # TRUTH AND CONFORMITY ON NETWORKS 
  # DATA ANALYSIS
  ###########################################################################
  # Created by: Aydin Mohseni 
  # Contact: aydin.mohseni@gmail.com
  # URL: www.aydinmohseni.com
  
  
  # Load packages from library
  library(reshape2)
  
  # Set the working directory as you please  
  setwd("/Users/aydin/GitHub/Truth-and-Conformity-on-Networks/results/from false consensus")
  
  # The number of simulations
  numberOfSimulationsPerSetting <- 1000
  
  # List the files in the directory
  filenames <- list.files(pattern="*.csv", full.names=TRUE)
  # Upload all CSV files
  ldf2 <- lapply(filenames, read.csv, header=TRUE, sep=",")
  # Convert the tables to numeric form
  ldfnum2 <- sapply(ldf2, data.matrix)
  ldfnum2 <- ldfnum2[-c(1:numberOfSimulationsPerSetting),]
  # Get the means of the columns
  allMeans2 <- colMeans(ldfnum2)
  # Break this up into Belief and Declaration vectors
  beliefMeans2 <- allMeans2[1:(length(allMeans2)/2)]
  declarationMeans2 <- allMeans2[((length(allMeans2)/2)+1):length(allMeans2)] 
  
  # Break the vectors up further for each the network values
  NSweep <- c(2, 5, 10, 20, 50)
  n <- length(NSweep)
  NetworkTypeSweep <- c("Circle", "Complete", "Random", "Regular", "Star")
  
  # Network Belief vectors
  for (i in 1:length(NetworkTypeSweep)) {
    assign(paste(NetworkTypeSweep[i], ".Belief", sep=""), beliefMeans2[(1+n*(i-1)):(i*n)]) 
  }
  # Network Declaration vectors
  for (i in 1:length(NetworkTypeSweep)) {
    assign(paste(NetworkTypeSweep[i], ".Declaration", sep=""), declarationMeans2[(1+n*(i-1)):(i*n)]) 
  }
  
  # Create data frame for beliefs as a function of population size
  dfBelief <- data.frame(N = NSweep, Complete = Complete.Belief, Regular = Regular.Belief, Circle = Circle.Belief, Star = Star.Belief, Random = Random.Belief)
  dfBeliefMelt <- melt(dfBelief2, id.vars=1)
  colnames(dfBeliefMelt) <- c("N", "Network", "MeanBelief")
  # Create data frame for declarations as a function of population size
  dfDeclaration <- data.frame(N = NSweep, Complete = Complete.Declaration, Regular = Regular.Declaration, Circle = Circle.Declaration, Star = Star.Declaration, Random = Random.Declaration)
  dfDeclarationMelt <- melt(dfDeclaration2, id.vars = 1)
  colnames(dfDeclarationMelt) <- c("N", "Network", "MeanDeclaration")
  
  print(dfBelief)
  print(dfDeclaration)
  
  ### Plot results
  # Belief line plot
  ggplot(data = dfBeliefMelt, aes(x = N, y = MeanBelief)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    geom_point(aes(shape = Network, color = Network), size = 3) +
    ggtitle("Mean Belief in True State and Population Size") +
    labs(x = "Population Size", y = "Mean Belief") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
  # Declaration line plot
  ggplot(data = dfDeclarationMelt, aes(x = N, y = MeanDeclaration)) +
    geom_line(aes(group = Network, color = Network), size = 1) +
    geom_point(aes(shape = Network, color = Network), size = 3) +
    ggtitle("Mean Declaration of True State as a Function of Population Size") +
    labs(x = "Population Size", y = "Mean Declaration") +
    theme_light()
  theme(plot.title = element_text(hjust = 0.5))
  
