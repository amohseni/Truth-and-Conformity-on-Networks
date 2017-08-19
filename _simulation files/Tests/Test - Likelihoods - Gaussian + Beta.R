## TEST: PUBLIC PRIOR - GENERAL (BETA + GAUSSIAN)

Mean1 <- 1 
Mean0 <- -1 
Variance <- 1
StandardDeviation <- sqrt(Variance) 
epsilon <- .0001

PriorSweep <- seq(from = 0, to = 1, by = .1)
NsSweep <- seq(from = 0, to = 1, by = .1)

df.L1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.L0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
  rownames(df.L1) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
  colnames(df.L1) <- sprintf("Ns=%s", seq(from = 0, to = 1, by = .1))
  rownames(df.L0) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
  colnames(df.L0) <- sprintf("Ns=%s", seq(from = 0, to = 1, by = .1))

  fTheta1 <- function(Signal) { return( dnorm(Signal, mean = Mean1, sd = StandardDeviation) ) }
  fTheta0 <- function(Signal) { return( dnorm(Signal, mean = Mean0, sd = StandardDeviation) ) }  
  
  Credence <- function(State, Signal) {
    w <- (1 + ((1 - Prior) / Prior) * (fTheta0(Signal) / fTheta1(Signal))) ^ -1
    z <- ifelse(State == 1, 
                w, 
                (1 - w))
    return(z)
  }
  
    
for (i in 1:length(PriorSweep)) {
  Prior <- PriorSweep[i]
  
  for (j in 1:length(NsSweep)) {
    Ns <- NsSweep[j]
    
    AlphaThreshold <- function(Signal) {
      Ps <- Credence(1, Signal)
      z <- pbeta(0.5 * ((1 - 2 * Ns) / (Ps - Ns)),
                 BetaParameterforTruthSeeking,
                 BetaParameterforConformity)
      return(z)
    }
    
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
    
    SignalThreshold <-
      -(Variance) * log((Prior / (1 - Prior))) / (Mean1 - Mean0) + (Mean0 + Mean1) / 2
    
    # First, compute P(Declaration = 1 | Theta = 1)
    if (Ns > 1 / 2) {
      LikelihoodTheta1 <-
        integrate(
          Integrand.fTheta1.A,
          lower = -Inf,
          upper = SignalThreshold - epsilon,
          stop.on.error = FALSE
        )$value + 1 - pnorm(SignalThreshold, mean = Mean1, sd = StandardDeviation)
    } else {
      LikelihoodTheta1 <-
        integrate(Integrand.fTheta1.B,
                  lower = SignalThreshold + epsilon,
                  upper = Inf,
                  stop.on.error = FALSE)$value
    }
    
    # Next, compute P(Declaration = 1 | Theta = 0)
    if (Ns > 1 / 2) {
      LikelihoodTheta0 <-
        integrate(Integrand.fTheta0.A,
                  lower = -Inf,
                  upper = SignalThreshold - epsilon,
                  stop.on.error = FALSE)$value + 1 - pnorm(SignalThreshold, mean = Mean0, sd = StandardDeviation)
    } else {
      LikelihoodTheta0 <-
        integrate(Integrand.fTheta0.B,
                  lower = SignalThreshold + epsilon,
                  upper = Inf,
                  stop.on.error = FALSE)$value
    }
    
    df.L1[i, j] <- LikelihoodTheta1
    df.L0[i, j] <- LikelihoodTheta0
    
  }
}
  
  round(df.L1, digits = 3)
  round(df.L0, digits = 3)
  