# Data Analysis

# Set the working directory as you please  
setwd("/Users/aydin/GitHub/Truth-and-Conformity-on-Networks/results/random initial conditions")

# The number of simulations
numberOfSimulationsPerSetting <- 100

# List the files in the directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)
# Upload all CSV files
ldf <- lapply(filenames, read.csv, header=TRUE, sep=",")
# Convert the tables to numeric form
ldfnum <- sapply(ldf, data.matrix)
ldfnum <- ldfnum[-c(1:numberOfSimulationsPerSetting),]

summary(ldfnum)
