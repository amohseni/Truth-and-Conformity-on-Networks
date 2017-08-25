###########################################################################
# TRUTH AND CONFORMITY ON NETWORKS 
# DATA ANALYSIS: DEGREE-BELIEF CORRELATION for REGULAR and RANDOM NETWORKS
###########################################################################
# Created by: Aydin Mohseni 
# Contact: aydin.mohseni@gmail.com
# URL: www.aydinmohseni.com


# Load packages from library
library(reshape2)
library(ggplot2)

# Set the working directory as you please  
setwd("/Users/patience/Global/Professional/Logic and Philosophy of Science/6. Projects/Model | Truth, Conformity, and Networks/_results/degreeSweep/False Consensus N=20")

# The number of simulations
numberOfSimulationsPerSetting <- 10000
N <- 20
NetworkTypeSweep <- c("Regular", "Random")
RegDegreeSweep <- seq(0, 1, by = 0.1)
DegreeSweep <- N * RegDegreeSweep
numberOfDegreeSettings <- length(RegDegreeSweep)  

# List the files in the directory
filenames3 <- list.files(pattern="*.csv", full.names=TRUE)
# Upload all CSV files
ldf3 <- lapply(filenames3, read.csv, header=TRUE, sep=",")
# Convert the tables to numeric form
ldf3num <- sapply(ldf3, data.matrix)
ldf3num <- ldf3num[-c(1:numberOfSimulationsPerSetting),]
# Get the means of the columns
allMeans3 <- colMeans(ldf3num)
# Break this up into Belief and Declaration vectors
beliefMeans3 <- allMeans3[1:(length(allMeans3)/2)]
declarationMeans3 <- allMeans3[((length(allMeans3)/2)+1):length(allMeans3)] 

# Break the vectors up further for each the network values
# Network Belief vectors
for (i in 1:length(NetworkTypeSweep)) {
  assign(paste(NetworkTypeSweep[i], ".Belief", sep=""), beliefMeans3[(1+numberOfDegreeSettings*(i-1)):(i*numberOfDegreeSettings)]) 
}
# Network Declaration vectors
for (i in 1:length(NetworkTypeSweep)) {
  assign(paste(NetworkTypeSweep[i], ".Declaration", sep=""), declarationMeans3[(1+numberOfDegreeSettings*(i-1)):(i*numberOfDegreeSettings)]) 
}

# Create data frame 
dfBelief3 <- data.frame(Degree = RegDegreeSweep, Regular = Regular.Belief, Random = Random.Belief)
dfBelief3Melt <- melt(dfBelief3, id.vars=1)
colnames(dfBelief3Melt) <- c("Degree", "Network", "MeanBelief")
dfDeclaration3 <- data.frame(Degree = RegDegreeSweep, Regular = Regular.Declaration, Random = Random.Declaration)
dfDeclaration3Melt <- melt(dfDeclaration3, id.vars = 1)
colnames(dfDeclaration3Melt) <- c("Degree", "Network", "MeanDeclaration")

print(dfBelief3)
print(dfDeclaration3)

### Plot results
# Belief line plot
ggplot(data = dfBelief3Melt, aes(x = Degree, y = MeanBelief)) +
  geom_line(aes(group = Network, color = Network), size = 1) +
  scale_color_manual(values = c("yellow3", "orchid")) +
  geom_point(aes(shape = Network, color = Network), size = 2) +
  scale_shape_manual(values = c(17, 7)) +
  ggtitle("Mean Belief in True State as a Function of Degree") +
  labs(x = "Degree Density", y = "Mean Belief") +
  scale_y_continuous(limits = c(.5, .75)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0,1,.2)) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
# Declaration line plot
ggplot(data = dfDeclaration3Melt, aes(x = Degree, y = MeanDeclaration)) +
  geom_line(aes(group = Network, color = Network), size = 1) +
  scale_color_manual(values = c("yellow3", "orchid")) +
  geom_point(aes(shape = Network, color = Network), size = 2) +
  scale_shape_manual(values=c(17, 7)) +
  ggtitle("Mean Declaration of True State as a Function of Degree") +
  labs(x = "Degree Density", y = "Mean Declaration") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0,1,.2)) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))
