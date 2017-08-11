###########################################################################
# TRUTH AND CONFORMITY ON NETWORKS 
# DATA ANALYSIS: HITTING TIMES
###########################################################################
# Created by: Aydin Mohseni 
# Contact: aydin.mohseni@gmail.com
# URL: www.aydinmohseni.com


# Load packages from library
library(reshape2)
library(ggplot2)

# Set the working directory as you please  
setwd("/Users/patience/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results/degreeData.N=10.r=10000/FromFalseConsensus")

# The number of simulations
numberOfSimulationsPerSetting <- 10000
numberOfTurnsPerSimulation <- 100 
N <- 10
NetworkTypeSweep <- c("Regular", "Random")
RegDegreeSweep <- seq(0, 1, by = 0.2)
DegreeSweep <- N * RegDegreeSweep
numberOfDegreeSettings <- length(RegDegreeSweep) 


# List the files in the directory
filenames5 <- list.files(pattern="*.csv", full.names=TRUE)
# Upload all CSV files
ldf5 <- lapply(filenames5, read.csv, header=TRUE, sep=",")
# Convert the tables to numeric form
ldf5num <- sapply(ldf5, data.matrix)
ldf5num <- ldf5num[-c(1:numberOfSimulationsPerSetting),]
# Keep only the declaration data
ldf5num <- ldf5num[,c((ncol(ldf5num)/2+1):ncol(ldf5num))]
# Break this up into the regular and random network matrices
ldf5numRegular <- ldf5num[,c(1:length(RegDegreeSweep))]
ldf5numRandom <- ldf5num[,c((length(RegDegreeSweep)+1):(2*length(RegDegreeSweep)))]

# Reorganize each simulation as a row of a data frame
dfNames <- c()
# first for regular networks
for (i in 1:length(RegDegreeSweep)) {
  assign(paste("df", NetworkTypeSweep[1], ".d", RegDegreeSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", NetworkTypeSweep[1], ".d", RegDegreeSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
    a <- 1 + (j - 1) * numberOfSimulationsPerSetting
    b <- j * numberOfSimulationsPerSetting
    dfRegular.d0[,j] <- ldf5num[c(a : b), 1]
    dfRegular.d0.2[,j] <- ldf5num[c(a : b), 2]
    dfRegular.d0.4[,j] <- ldf5num[c(a : b), 3]
    dfRegular.d0.6[,j] <- ldf5num[c(a : b), 4]
    dfRegular.d0.8[,j] <- ldf5num[c(a : b), 5]
    dfRegular.d1[,j] <- ldf5num[c(a : b), 6]
}

# next for random networks
for (i in 1:length(RegDegreeSweep)) {
  assign(paste("df", NetworkTypeSweep[2], ".d", RegDegreeSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", NetworkTypeSweep[2], ".d", RegDegreeSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
  a <- 1 + (j - 1) * numberOfSimulationsPerSetting
  b <- j * numberOfSimulationsPerSetting
  dfRandom.d0[,j] <- ldf5num[c(a : b), 7]
  dfRandom.d0.2[,j] <- ldf5num[c(a : b), 8]
  dfRandom.d0.4[,j] <- ldf5num[c(a : b), 9]
  dfRandom.d0.6[,j] <- ldf5num[c(a : b), 10]
  dfRandom.d0.8[,j] <- ldf5num[c(a : b), 11]
  dfRandom.d1[,j] <- ldf5num[c(a : b), 12]
}


# Find the hitting times for each
# the regular networks
regularNetworkMeanHittingTimes <- c()
for (m in 1:length(RegDegreeSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", NetworkTypeSweep[1], ".d", RegDegreeSweep[m], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  regularNetworkMeanHittingTimes[m] <- mean(hittingTimeVector)
}
print(regularNetworkMeanHittingTimes)

# the random networks
randomNetworkMeanHittingTimes <- c()
for (n in 1:length(RegDegreeSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", NetworkTypeSweep[2], ".d", RegDegreeSweep[n], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  randomNetworkMeanHittingTimes[n] <- mean(hittingTimeVector)
}
print(randomNetworkMeanHittingTimes)

# Plot the outcomes
HittingTimePlot <-
  data.frame(Degree = RegDegreeSweep,
             Regular = regularNetworkMeanHittingTimes,
             Random = randomNetworkMeanHittingTimes)
HittingTimePlot <- melt(HittingTimePlot, id.vars = 1)
colnames(HittingTimePlot) <- c("Degree", "Network", "Hitting.Time")
p <- ggplot(HittingTimePlot, aes(x = Degree, y = Hitting.Time, colour = Network)) +
  geom_line(size = 1) +
  geom_point(aes(shape = Network), size = 2) +
  ggtitle("Hitting Times as a Function of Network Degree") +
  labs(x = "Mean Degree", y = "Mean Hitting Time") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1))
print(p)
