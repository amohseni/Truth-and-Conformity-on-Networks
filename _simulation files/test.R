  Declaration <- 1
  Mean1 <- 0.1 # Mean for distribution of signals if Theta is true: state is 1
  Mean0 <- -0.1 # Mean for distribution of signals if Theta is false: state is 0
  Variance <- 1 # Variance for both distributions of signals
  BetaParameterforTruthSeeking <- 1
  BetaParameterforConformity <- 1
  Signal <- 0
  Prior <- .5
  Ns <- .1 # Proportion of agent i's neighbors declaring the state 1
  
  SignalProbabilityGivenTheta <- function(Signal) {
    z <-
      (1 + ((1 - Prior) / Prior) * exp((-1 / (2 * Variance ^ 2)) * (Mean1 - Mean0) * (2 * Signal - Mean0 - Mean1))) ^ -1
    return(z)
  }
  
  SignalThreshold <-
    -(Variance ^ 2) * log((Ns ^ -1 - 1) * (Prior / (1 - Prior))) + (Mean0 - Mean1) / 2
  
  AlphaThreshold <- function(Signal) {
    z <- pbeta(0.5 * ((1 - 2 * Ns) / (
      2 * SignalProbabilityGivenTheta(Signal) - Ns
    )) ,
    BetaParameterforTruthSeeking,
    BetaParameterforConformity)
    return(z)
  }
  
  GuassianPDF <- function(Signal) {
    z <- dnorm(Signal, mean = Mean1, sd = sqrt(Variance))
    return(z)
  }
  
  if (Ns >= 1/2) {
    LikelihoodTheta1 <-
      integrate(
        AlphaThreshold * fTheta1,
        lower = -Inf,
        upper = SignalThreshold
      )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = sqrt(Variance))
  }
  
  if (Ns < 1/2) {
    LikelihoodTheta1 <-
      integrate(
        (1 - AlphaThreshold) * fTheta1,
        lower = SignalThreshold,
        upper = Inf
      )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = sqrt(Variance))
  }
  
  if (Declaration == 0) {
  LikelihoodTheta1 <- 1 - LikelihoodTheta1
  }
