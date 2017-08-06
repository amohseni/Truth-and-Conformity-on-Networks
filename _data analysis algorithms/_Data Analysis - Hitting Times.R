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
setwd("/Users/patience/Desktop/results 2/fromfalseconsensus")

# The number of simulations
numberOfSimulationsPerSetting <- 10000
N <- 10
NetworkTypeSweep <- c("Regular", "Random")
RegDegreeSweep <- seq(0, 1, by = 0.2)
DegreeSweep <- N * RegDegreeSweep
numberOfDegreeSettings <- length(RegDegreeSweep) 



# List the files in the directory
filenames5 <- list.files(pattern="*.csv", full.names=TRUE)
# Upload all CSV files
ldf5 <- lapply(filenames3, read.csv, header=TRUE, sep=",")
# Convert the tables to numeric form
ldf5num <- sapply(ldf3, data.matrix)
ldf5num <- ldf3num[-c(1:numberOfSimulationsPerSetting),]

# Reorgize the data:
