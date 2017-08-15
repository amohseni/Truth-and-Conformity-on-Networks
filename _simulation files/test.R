Declaration <- 1
Mean1 <- 0.1 
Mean0 <- -0.1 
Variance <- 1 
BetaParameterforTruthSeeking <- 1
BetaParameterforConformity <- 1

PriorSweep <- seq(from = 0, to = 1, by = .1)
NsSweep <- seq(from = 0, to = 1, by = .1) 


for (i in 1:PriorSweep) {
  Prior <- PriorSweep[i]
    
    for (j in 1:NsSweep) {
      Ns <- NsSweep[j]
      
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
      
      if (Ns >= 1 / 2) {
        LikelihoodTheta1 <-
          integrate(AlphaThreshold * fTheta1,
                    lower = -Inf,
                    upper = SignalThreshold)$value + 1 - pnorm(SignalThreshold,
                                                               mean = Mean1,
                                                               sd = sqrt(Variance))
      }
      
      if (Ns < 1 / 2) {
        LikelihoodTheta1 <-
          integrate((1 - AlphaThreshold) * fTheta1,
                    lower = SignalThreshold,
                    upper = Inf)$value + 1 - pnorm(SignalThreshold,
                                                   mean = Mean1,
                                                   sd = sqrt(Variance))
      }
      
      if (Declaration == 0) {
        LikelihoodTheta1 <- (1 - LikelihoodTheta1)
        LikelihoodTheta0 <- (1 - LikelihoodTheta0)
      }
      
    }
  
}  