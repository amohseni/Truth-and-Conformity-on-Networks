# Data Analysis

# Set the working directory as you please  
setwd("/Users/aydin/GitHub/Truth-and-Conformity-on-Networks/results")

filenames <- list.files(pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
res <- lapply(ldf, summary)

read.csv("./results.Beliefs.NetworkType=Circle.N=2.Init=ConsensusOnFalseState.Runs=1.csv", header = FALSE)