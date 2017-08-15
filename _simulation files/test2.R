Mean1 <- 1 
Mean0 <- -1 
Variance <- 1
StandardDeviation <-
  sqrt(Variance)
epsilon <- .001

PriorSweep <- seq(from = .1, to = 1, by = .1)
NsSweep <- seq(from = 0, to = 1, by = .1)

df.D1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.D0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
rownames(df.D1) <- sprintf("Pr=%s", seq(from = .1, to = 1, by = .1))
colnames(df.D1) <- sprintf("Ns=%s", seq(from = .1, to = 1, by = .1))
rownames(df.D0) <- sprintf("Pr=%s", seq(from = .1, to = 1, by = .1))
colnames(df.D0) <- sprintf("Ns=%s", seq(from = .1, to = 1, by = .1))
df.D1.LikelihoodTheta1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.D1.LikelihoodTheta0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.D0.LikelihoodTheta1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.D0.LikelihoodTheta0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))

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
    
    PublicPrior <- function(FocalAgent, Declaration) {
      SignalThreshold <-
        -(Variance) * log((Ns ^ -1 - 1) * (Prior / (1 - Prior))) / (Mean1 - Mean0) + (Mean0 + Mean1) / 2
      
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
      
      # Compute the two likelihood values:
      # P(Declaration = 1 | Theta = 1) and P(Declaration = 1 | Theta = 0)
      
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
                    upper = SignalThreshold,
                    stop.on.error = FALSE)$value + 1 - pnorm(SignalThreshold, mean = Mean0, sd = StandardDeviation)
      } else {
        LikelihoodTheta0 <-
          integrate(Integrand.fTheta0.B,
                    lower = SignalThreshold,
                    upper = Inf,
                    stop.on.error = FALSE)$value
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

      
      
      # Compute the new posterior public belief P(Theta | Declaration of focal agent)
      z <-
        (1 + ((1 - Prior) / Prior) * (LikelihoodTheta0 / LikelihoodTheta1)) ^ -1
      # print("PUBLIC BELIEF")
      # print(z)
      return(z)
    }
    
    df.D1[i, j] <- PublicPrior(x, 1)
    df.D1.LikelihoodTheta1[i, j] <- LikelihoodTheta1
    df.D1.LikelihoodTheta0[i, j] <- LikelihoodTheta0
    
    df.D0[i, j] <- PublicPrior(x, 0)
    df.D0.LikelihoodTheta1[i, j] <- LikelihoodTheta1
    df.D0.LikelihoodTheta0[i, j] <- LikelihoodTheta0
    
  }
}

round(df.D1 , digits = 3)
round(df.D0 , digits = 3)
