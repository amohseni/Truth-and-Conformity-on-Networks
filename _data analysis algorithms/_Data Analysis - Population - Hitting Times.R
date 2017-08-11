###########################################################################
# TRUTH AND CONFORMITY ON NETWORKS 
# DATA ANALYSIS: HITTING TIMES for POPULATION SIZE
###########################################################################
# Created by: Aydin Mohseni 
# Contact: aydin.mohseni@gmail.com
# URL: www.aydinmohseni.com


# Load packages from library
library(reshape2)
library(ggplot2)

# Set the working directory as you please  
setwd("/Users/patience/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results/populationData.N=10.r=10000/fromFalseConsensus")

# The number of simulations
numberOfSimulationsPerSetting <- 10000
numberOfTurnsPerSimulation <- 100 
NSweep <- seq(from = 2, to = 20, by = 2)
NetworkTypeSweep <- c("Circle", "Complete", "Random", "Regular", "Star")
InitialDeclarationsSweep <- c("ConsensusOnFalseState")


# List the files in the directory
filenames6 <- list.files(pattern="*.csv", full.names=TRUE)
# Upload all CSV files
ldf6 <- lapply(filenames6, read.csv, header=TRUE, sep=",")
# Convert the tables to numeric form
ldf6num <- sapply(ldf6, data.matrix)
ldf6num <- ldf6num[-c(1:numberOfSimulationsPerSetting),]
# Break up the data in Declaration and Belief parts
ldf6num <- ldf6num[,c((ncol(ldf6num)/2+1):ncol(ldf6num))]
# Break this up into the regular and random network matrices
ldf6numCircle <- ldf6num[,c(1:length(NSweep))]
ldf6numComplete <- ldf6num[,c((length(NSweep)+1):(2*length(NSweep)))]
ldf6numRandom <- ldf6num[,c((2*length(NSweep)+1):(3*length(NSweep)))]
ldf6numRegular <- ldf6num[,c((3*length(NSweep)+1):(4*length(NSweep)))]
ldf6numStar <- ldf6num[,c((4*length(NSweep)+1):(5*length(NSweep)))]


# Reorganize each simulation as a row of a data frame
dfNames <- c()


# for CIRCLE networks
for (i in 1:length(NSweep)) {
  assign(paste("df", "Circle.", "N.", NSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", "Circle.", "N.", NSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
  a <- 1 + (j - 1) * numberOfSimulationsPerSetting
  b <- j * numberOfSimulationsPerSetting
  dfCircle.N.2[,j] <- ldf6num[c(a : b), 1]
  dfCircle.N.4[,j] <- ldf6num[c(a : b), 2]
  dfCircle.N.6[,j] <- ldf6num[c(a : b), 3]
  dfCircle.N.8[,j] <- ldf6num[c(a : b), 4]
  dfCircle.N.10[,j] <- ldf6num[c(a : b), 5]
  dfCircle.N.12[,j] <- ldf6num[c(a : b), 6]
  dfCircle.N.14[,j] <- ldf6num[c(a : b), 7]
  dfCircle.N.16[,j] <- ldf6num[c(a : b), 8]
  dfCircle.N.18[,j] <- ldf6num[c(a : b), 9]
  dfCircle.N.20[,j] <- ldf6num[c(a : b), 10]
}


# for COMPLETE networks
for (i in 1:length(NSweep)) {
  assign(paste("df", "Complete.", "N.", NSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", "Complete.", "N.", NSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
  a <- 1 + (j - 1) * numberOfSimulationsPerSetting
  b <- j * numberOfSimulationsPerSetting
  dfComplete.N.2[,j] <- ldf6num[c(a : b), 11]
  dfComplete.N.4[,j] <- ldf6num[c(a : b), 12]
  dfComplete.N.6[,j] <- ldf6num[c(a : b), 13]
  dfComplete.N.8[,j] <- ldf6num[c(a : b), 14]
  dfComplete.N.10[,j] <- ldf6num[c(a : b), 15]
  dfComplete.N.12[,j] <- ldf6num[c(a : b), 16]
  dfComplete.N.14[,j] <- ldf6num[c(a : b), 17]
  dfComplete.N.16[,j] <- ldf6num[c(a : b), 18]
  dfComplete.N.18[,j] <- ldf6num[c(a : b), 19]
  dfComplete.N.20[,j] <- ldf6num[c(a : b), 20]
}


# for RANDOM networks
for (i in 1:length(NSweep)) {
  assign(paste("df", "Random.", "N.", NSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", "Random.", "N.", NSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
  a <- 1 + (j - 1) * numberOfSimulationsPerSetting
  b <- j * numberOfSimulationsPerSetting
  dfRandom.N.2[,j] <- ldf6num[c(a : b), 21]
  dfRandom.N.4[,j] <- ldf6num[c(a : b), 22]
  dfRandom.N.6[,j] <- ldf6num[c(a : b), 23]
  dfRandom.N.8[,j] <- ldf6num[c(a : b), 24]
  dfRandom.N.10[,j] <- ldf6num[c(a : b), 25]
  dfRandom.N.12[,j] <- ldf6num[c(a : b), 26]
  dfRandom.N.14[,j] <- ldf6num[c(a : b), 27]
  dfRandom.N.16[,j] <- ldf6num[c(a : b), 28]
  dfRandom.N.18[,j] <- ldf6num[c(a : b), 29]
  dfRandom.N.20[,j] <- ldf6num[c(a : b), 30]
}


# for REGULAR networks
for (i in 1:length(NSweep)) {
  assign(paste("df", "Regular.", "N.", NSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", "Regular.", "N.", NSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
  a <- 1 + (j - 1) * numberOfSimulationsPerSetting
  b <- j * numberOfSimulationsPerSetting
  dfRegular.N.2[,j] <- ldf6num[c(a : b), 31]
  dfRegular.N.4[,j] <- ldf6num[c(a : b), 32]
  dfRegular.N.6[,j] <- ldf6num[c(a : b), 33]
  dfRegular.N.8[,j] <- ldf6num[c(a : b), 34]
  dfRegular.N.10[,j] <- ldf6num[c(a : b), 35]
  dfRegular.N.12[,j] <- ldf6num[c(a : b), 36]
  dfRegular.N.14[,j] <- ldf6num[c(a : b), 37]
  dfRegular.N.16[,j] <- ldf6num[c(a : b), 38]
  dfRegular.N.18[,j] <- ldf6num[c(a : b), 39]
  dfRegular.N.20[,j] <- ldf6num[c(a : b), 40]
}


# for STAR networks
for (i in 1:length(NSweep)) {
  assign(paste("df", "Star.", "N.", NSweep[i], sep = ""),
         data.frame(
           matrix(data = NA, nrow = numberOfSimulationsPerSetting, ncol = numberOfTurnsPerSimulation)
         ))
  dfNames <- append(dfNames, paste("df", "Star.", "N.", NSweep[i], sep = ""))
}
for (j in 1:numberOfTurnsPerSimulation) {
  a <- 1 + (j - 1) * numberOfSimulationsPerSetting
  b <- j * numberOfSimulationsPerSetting
  dfStar.N.2[,j] <- ldf6num[c(a : b), 41]
  dfStar.N.4[,j] <- ldf6num[c(a : b), 42]
  dfStar.N.6[,j] <- ldf6num[c(a : b), 43]
  dfStar.N.8[,j] <- ldf6num[c(a : b), 44]
  dfStar.N.10[,j] <- ldf6num[c(a : b), 45]
  dfStar.N.12[,j] <- ldf6num[c(a : b), 46]
  dfStar.N.14[,j] <- ldf6num[c(a : b), 47]
  dfStar.N.16[,j] <- ldf6num[c(a : b), 48]
  dfStar.N.18[,j] <- ldf6num[c(a : b), 49]
  dfStar.N.20[,j] <- ldf6num[c(a : b), 50]
}

# Find the hitting times for each
# the CIRCLE networks
CircleMeanHittingTimes <- c()
for (m in 1:length(NSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", "Circle.", "N.", NSweep[m], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  CircleMeanHittingTimes[m] <- mean(hittingTimeVector)
}
print(CircleMeanHittingTimes)

# the COMPLETE networks
CompleteMeanHittingTimes <- c()
for (m in 1:length(NSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", "Complete.", "N.", NSweep[m], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  CompleteMeanHittingTimes[m] <- mean(hittingTimeVector)
}
print(CompleteMeanHittingTimes)

# the RANDOM networks
RandomMeanHittingTimes <- c()
for (m in 1:length(NSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", "Random.", "N.", NSweep[m], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  RandomMeanHittingTimes[m] <- mean(hittingTimeVector)
}
print(RandomMeanHittingTimes)

# the REGULAR networks
RegularMeanHittingTimes <- c()
for (m in 1:length(NSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", "Regular.", "N.", NSweep[m], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  RegularMeanHittingTimes[m] <- mean(hittingTimeVector)
}
print(RegularMeanHittingTimes)


# the STAR networks
StarMeanHittingTimes <- c()
for (m in 1:length(NSweep)) {
  hittingTimeVector <- c()
  df <- eval(parse(text = paste("df", "Star.", "N.", NSweep[m], sep = "")))
  for (k in 1:numberOfSimulationsPerSetting) {
    hittingTimeVector[k] <- which(df[k, ] == 1)[1]
  }
  hittingTimeVector[which(is.na(hittingTimeVector))] <- 100
  StarMeanHittingTimes[m] <- mean(hittingTimeVector)
}
print(StarMeanHittingTimes)


# Plot the outcomes
PopulationHittingTimePlot <-
  data.frame(N = NSweep,
             Complete = CompleteMeanHittingTimes,
             Regular = RegularMeanHittingTimes,
             Circle = CircleMeanHittingTimes,
             Star = StarMeanHittingTimes,
             Random = RandomMeanHittingTimes
             )
PopulationHittingTimePlotMELT <- melt(PopulationHittingTimePlot, id.vars = 1) 
colnames(PopulationHittingTimePlotMELT) <- c("N", "Network", "Hitting.Time")
levels(PopulationHittingTimePlotMELT) = c("Complete", "Regular", "Circle", "Star", "Random")
# PopulationHittingTimePlotMELT <- PopulationHittingTimePlotMELT[c(c(11:20), c(21:30), c(1:10), c(41:50), c(31:40)),]
p <- ggplot(PopulationHittingTimePlotMELT, aes(x = N, y = Hitting.Time)) +
  geom_line(aes(x = N, y = Hitting.Time, colour = Network), size = 1) +
  geom_point(aes(shape = Network, colour = Network), size = 2) +
  ggtitle("Hitting Times as a Function of Population Size") +
  labs(x = "Mean Degree", y = "Mean Hitting Time", colour = "Network") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60))
print(p)

