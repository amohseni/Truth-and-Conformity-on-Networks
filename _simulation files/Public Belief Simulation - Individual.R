# INFORMATION OF INDIVIDUAL DECLARATIONS
# by Aydin Mohseni
# www.aydinmohseni.com

# Load packages
library(combinat)
library(gtools)
library(ggplot2)
library(reshape2)
### Establish global variables

# Set the true state of the world
Theta <- 1

N <- 10 # Number of agents
Prior <- .5 # Initial prior

# STATES of the WORLD
# Define the two distributions f1, f0 corresponding to
# the two possible STATES OF THE WORLD: 1, 0
# with the parameters
Mean1 <-
  1 # Mean for distribution of signals if Theta is true: state is 1
Mean0 <-
  -1 # Mean for distribution of signals if Theta is false: state is 0
Variance <- 1
StandardDeviation <-
  sqrt(Variance) # Variance for both distributions of signals
fTheta1 <-
  function(Signal) {
    return(dnorm(Signal, mean = Mean1, sd = StandardDeviation))
  }
fTheta0 <-
  function(Signal) {
    return(dnorm(Signal, mean = Mean0, sd = StandardDeviation))
  }

# Create the population types and initial beliefs
BetaParameterforTruthSeeking <-
  1 # Increasing this will increase the proportion of truth-seeking
BetaParameterforConformity <-
  1 # Increasing this will increase the proportion of conformity


# Define the CREDENCE function for the focal player for state Theta = 1, 0
# using Bayes' rule, as Pr(Theta | Signal) = Pr(Signal | Theta) * Pr(Theta) / Pr(Signal)
# That is, the poster probability of the state Theta,
# given the product of the likelihood of the Signal given Theta,
# over the total probability of the Signal
Credence <- function(State, Signal) {
  w <-
    (1 + ((1 - Prior) / Prior) * (fTheta0(Signal) / fTheta1(Signal))) ^ -1
  z <- ifelse(State == 1,
              w,
              (1 - w))
  # print(paste("POSTERIOR PROBABILITY for state ", State, " given signal ", Signal, sep = " "))
  # print(z)
  return(z)
}

PublicPrior <- function(Declaration, ProportionOfNeighborsDeclarations) {
  
  # Retrieve the proportion of the focal agent's neighbors
  # who are declaring state 1
  Ns <- ProportionOfNeighborsDeclarations
  
  # Compute the LIKELIHOODS of the DECLARATION of the focal agent
  # for each state of the world, Theta = 1, 0
  
  # Compute the threshold signal value needed
  # for the focal player to have declared state 1
  SignalThreshold <-
    -(Variance) * log((Prior / (1 - Prior))) / (Mean1 - Mean0) + (Mean0 + Mean1) / 2
  
  # Compute the threshold alpha value needed
  # for the focal player to have declared state 1
  # multiplied by the PDF of
  AlphaThreshold <- function(Signal) {
    return(pbeta(
      0.5 * ((1 - 2 * Ns) / (Credence(1, Signal) - Ns)) ,
      BetaParameterforTruthSeeking,
      BetaParameterforConformity
    ))
  }
  
  # Create four integrand functions for each likelihood,
  # in each case: Ns > 1/2, NS < 1/2
  Integrand.fTheta1.A <-
    function(Signal) {
      return(fTheta1(Signal) * AlphaThreshold(Signal))
    }
  Integrand.fTheta1.B <-
    function(Signal) {
      return(fTheta1(Signal) * (1 - AlphaThreshold(Signal)))
    }
  Integrand.fTheta0.A <-
    function(Signal) {
      return(fTheta0(Signal) * AlphaThreshold(Signal))
    }
  Integrand.fTheta0.B <-
    function(Signal) {
      return(fTheta0(Signal) * (1 - AlphaThreshold(Signal)))
    }
  
  # Compute the two likelihood values:
  # P(Declaration = 1 | Theta = 1) and P(Declaration = 1 | Theta = 0)
  
  # First, compute P(Declaration = 1 | Theta = 1)
  if (Ns > 1 / 2) {
    LikelihoodTheta1 <-
      integrate(Integrand.fTheta1.A,
                lower = -Inf,
                upper = SignalThreshold)$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = StandardDeviation)
  } else {
    LikelihoodTheta1 <-
      integrate(Integrand.fTheta1.B,
                lower = SignalThreshold,
                upper = Inf)$value
  }
  
  # Next, compute P(Declaration = 1 | Theta = 0)
  if (Ns > 1 / 2) {
    LikelihoodTheta0 <-
      integrate(Integrand.fTheta0.A,
                lower = -Inf,
                upper = SignalThreshold)$value + 1 - pnorm(SignalThreshold, mean = Mean0, sd = StandardDeviation)
  } else {
    LikelihoodTheta0 <-
      integrate(Integrand.fTheta0.B,
                lower = SignalThreshold,
                upper = Inf)$value
  }
  
  # If the declaration was of state 0 (rather than 1),
  # then take the complement of the likelihoods just computed
  # to get the correct likelihoods Pr(Declaration = 0 | Theta = 1) and Pr(Declaration = 0 | Theta = 0)
  if (Declaration == 0) {
    LikelihoodTheta1 <- (1 - LikelihoodTheta1)
    # print("LIKELIHOOD of declaration 0 given Theta = 1")
    LikelihoodTheta0 <- (1 - LikelihoodTheta0)
    # print("LIKELIHOOD of declaration 0 given Theta = 0")
  }
  
  # Save the likelihood P(Declaration = S | Theta = 1)
  y <- LikelihoodTheta1
  
  # Compute the new posterior public belief P(Theta | Declaration of focal agent)
  z <-
    (1 + ((1 - Prior) / Prior) * (LikelihoodTheta0 / LikelihoodTheta1)) ^ -1
  # print("PUBLIC BELIEF")
  # print(z)
  return(list(y, z))
}


# Create a vector to store results
ExpectedPosteriorDf <- c()
vectorLikelihoodOfS <- c()
vectorPosteriorGivenS <- c()
vectorLikelihoodOfNotS <- c()
vectorPosteriorGivenNotS <- c()
# Expected public belief in next period q'= (1/N) Sum^N_i=1 [q(S|i)P(S|i)+q(~S|i)P(~S|i)]
# Loop through all combinations of possible declarations where some number of agents are deviating
for (NumberOfDeviants in 0:N) {
  
  ProportionOfNeighborsDeclarations <- (NumberOfDeviants / N)
  qs1 <- PublicPrior(1, ProportionOfNeighborsDeclarations)
  LikelihoodOfS <- qs1[[1]]
  PosteriorGivenS <- qs1[[2]]
  
  vectorLikelihoodOfS[NumberOfDeviants + 1] <- LikelihoodOfS
  vectorPosteriorGivenS[NumberOfDeviants + 1] <- PosteriorGivenS
  
  qs2 <- PublicPrior(0, ProportionOfNeighborsDeclarations)
  LikelihoodOfNotS <- qs2[[1]]
  PosteriorGivenNotS <- qs2[[2]]
  
  vectorLikelihoodOfNotS[NumberOfDeviants + 1] <- LikelihoodOfNotS
  vectorPosteriorGivenNotS[NumberOfDeviants + 1] <-
    PosteriorGivenNotS
  
  ExpectedPosteriorDf[NumberOfDeviants + 1] <-
    LikelihoodOfS * PosteriorGivenS + LikelihoodOfNotS * PosteriorGivenNotS
  
}

# SANITY-CHECK all compoent of calculations
dfCheck <-
  data.frame(
    Ns = seq(from = 0, to = 1, by = 1 / N), 
    LikelihoodOfS = vectorLikelihoodOfS,
    PosteriorGivenS = vectorPosteriorGivenS,
    LHS = (vectorLikelihoodOfS * vectorPosteriorGivenS),
    LikelihoodOfNotS = vectorLikelihoodOfNotS,
    PosteriorGivenNotS = vectorPosteriorGivenNotS,
    RHS = (vectorLikelihoodOfNotS * vectorPosteriorGivenNotS)
  )
dfCheckMelt <- melt(dfCheck, id.vars = Ns)
replace(x = dfCheckMelt, list = "RHS", values = expression(paste("P(S)L(S|", theta, ")", sep = "")))
ggplot(data = dfCheckMelt, aes(x = Ns, y = value, group = variable)) + 
  geom_line(size = 1, aes(colour = variable))

# Posterior-Likelihood Product Graph
dfCheck <-
  data.frame(
    Ns = seq(from = 0, to = 1, by = 1 / N),
    LHS = (vectorLikelihoodOfS * vectorPosteriorGivenS),
    RHS = (vectorLikelihoodOfNotS * vectorPosteriorGivenNotS)
  )
dfCheckMelt <- melt(dfCheck, id.vars = Ns, stringsAsFactors = FALSE)
dfCheckMelt$variable <- as.character(dfCheckMelt$variable)
dfCheckMelt[1:11, 2] <- c("Posterior(S) • Likelihood(S)")
dfCheckMelt[12:22, 2] <- c("Posterior(¬S) • Likelihood(¬S)")
ggplot(data = dfCheckMelt, aes(x = Ns, y = value, group = variable)) + 
  geom_line(size = 1, aes(linetype = variable)) +
  ggtitle("Posterior-Likelihood Product for Each Declaration") +
  labs(x = "Proportion of Neighbors Declaring the True State", y = "") +
  theme_light() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
    axis.title.x = element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.75, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    text = element_text(size = 14),
    legend.key.height = unit(20, "pt")
  )

# Posterior Graph
dfCheck <-
  data.frame(
    Ns = seq(from = 0, to = 1, by = 1 / N), 
    PosteriorGivenS = vectorPosteriorGivenS,
    PosteriorGivenNotS = vectorPosteriorGivenNotS
  )
dfCheckMelt <- melt(dfCheck, id.vars = Ns, stringsAsFactors = FALSE)
dfCheckMelt$variable <- as.character(dfCheckMelt$variable)
dfCheckMelt[1:11, 2] <- c("Posterior(S)")
dfCheckMelt[12:22, 2] <- c("Posterior(¬S)")
ggplot(data = dfCheckMelt, aes(x = Ns, y = value, group = variable)) + 
  geom_line(size = 1, aes(linetype = variable)) +
  ggtitle("Posterior on the True State for Each Declaration") +
  labs(x = "Proportion of Neighbors Declaring the True State", y = "") +
  theme_light() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
    axis.title.x = element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.85, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    text = element_text(size = 14),
    legend.key.height = unit(20, "pt")
  )


# Likelihood Graph
dfCheck <-
  data.frame(
    Ns = seq(from = 0, to = 1, by = 1 / N), 
    LikelihoodOfS = vectorLikelihoodOfS,
    LikelihoodOfNotS = vectorLikelihoodOfNotS
  )
dfCheckMelt <- melt(dfCheck, id.vars = Ns, stringsAsFactors = FALSE)
dfCheckMelt$variable <- as.character(dfCheckMelt$variable)
dfCheckMelt[1:11, 2] <- c("Likelihood(S)")
dfCheckMelt[12:22, 2] <- c("Likelihood(¬S)")
ggplot(data = dfCheckMelt, aes(x = Ns, y = value, group = variable)) + 
  geom_line(size = 1, aes(linetype = variable)) +
  ggtitle("Likelihood of Each Declaration Given the True State") +
  labs(x = "Proportion of Neighbors Declaring the True State", y = "") +
  theme_light() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
    axis.title.x = element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.85, 0.7),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    text = element_text(size = 14),
    legend.key.height = unit(20, "pt")
  )

### Expected Information of an Individual Declaration  
ChangeInBelief <- ExpectedPosteriorDf - 0.5
df <-
  data.frame(NumberOfDeviants = (0:N / N),
             ChangeInBelief = ChangeInBelief)
ggplot(data = df,
       aes(x = NumberOfDeviants, y = ChangeInBelief)) +
  geom_area(fill = "gray", alpha = .5) +
  geom_line(size = 1) +
  ggtitle("Expected Information of an Individual Declaration") +
  labs(x = "Proportion of Neighbors Declaring the True State", y = "Expected Information of Declaration") +
  theme_light() +
  # scale_y_continuous(limits = c(.00, .3)) +
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
    axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.85, 0.7),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    text = element_text(size = 14),
    legend.key.height = unit(20, "pt")
  )

# Information of an Individual Declaration  
ChangeInBelief <- vectorPosteriorGivenS - 0.5
df <-
  data.frame(NumberOfDeviants = (0:N / N),
             ChangeInBelief = ChangeInBelief)
ggplot(data = df,
       aes(x = NumberOfDeviants, y = ChangeInBelief)) +
  geom_area(fill = "gray", alpha = .5) +
  geom_line(size = 1) +
  ggtitle("Information of an Individual Declaration") +
  labs(x = "Proportion of Neighbors Declaring the Same State", y = "Information of Declaration") +
  theme_light() +
  ylim(0,.5) +
  # scale_y_continuous(limits = c(.00, .3)) +
  scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 12, unit = "pt")),
    axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = c(0.85, 0.7),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    text = element_text(size = 14),
    legend.key.height = unit(20, "pt")
  )