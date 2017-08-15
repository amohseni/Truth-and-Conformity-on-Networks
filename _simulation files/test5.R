Cred1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
Cred0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
rownames(Cred1) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
colnames(Cred1) <- sprintf("S=%s", seq(from = -2, to = 2, by = .4))
rownames(Cred0) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
colnames(Cred0) <- sprintf("S=%s", seq(from = -2, to = 2, by = .4))

PriorSweep <- seq(from = 0, to = 1, by = .1)
SignalSweep <- seq(from = -2, to = 2, by = .4)

for (i in 1:length(PriorSweep)) {
  Prior <- PriorSweep[i]
  
  for (j in 1:length(SignalSweep)) {
    Signal <- SignalSweep[j]
    
      Credence <- function(State, Signal) {
        w <- (1 + ((1 - Prior) / Prior) * (fTheta0(Signal) / fTheta1(Signal))) ^ -1
        z <- ifelse(State == 1, 
                    w, 
                    (1 - w))
        return(z)
      }

      Cred1[i, j] <- Credence(1, Signal)
      Cred0[i, j] <- Credence(0, Signal)
      
  }
}

round(Cred1, digits = 3)
round(Cred0, digits = 3)