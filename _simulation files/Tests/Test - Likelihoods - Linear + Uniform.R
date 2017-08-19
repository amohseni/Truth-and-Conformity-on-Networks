## TEST: PUBLIC PRIOR - SIMPLE (UNIFORM + LINEAR)

PriorSweep <- seq(from = 0, to = 1, by = .1)
NsSweep <- seq(from = 0, to = 1, by = .1)

df.PP1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.PP0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
  rownames(df.PP1) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
  colnames(df.PP1) <- sprintf("Ns=%s", seq(from = 0, to = 1, by = .1))
  rownames(df.PP0) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
  colnames(df.PP0) <- sprintf("Ns=%s", seq(from = 0, to = 1, by = .1))
df.L1 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
df.L0 <- data.frame(matrix(data = NA, nrow = length(PriorSweep), ncol = length(NsSweep)))
  rownames(df.L1) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
  colnames(df.L1) <- sprintf("Ns=%s", seq(from = 0, to = 1, by = .1))
  rownames(df.L0) <- sprintf("Pr=%s", seq(from = 0, to = 1, by = .1))
  colnames(df.L0) <- sprintf("Ns=%s", seq(from = 0, to = 1, by = .1))

  fTheta <- function(a) {
    (1 - (1 / (1 + ((Prior / (1-Prior)) * (((((1 - a) * (1 - 2 * Ns) / ( 2 * a)) + (1 / 2)) ^ -1) - 1))) ^ 2))
  }
  
for (i in 1:length(PriorSweep)) {
  Prior <- PriorSweep[i]
  
  for (j in 1:length(NsSweep)) {
    Ns <- NsSweep[j]
      
      # PublicPrior <- function(z) {
        if (Ns > .5) {
          L1 <- integrate(fTheta, lower = (1 - (1 / (2 * Ns))), upper = 1)$value + (1 - (1/(2*Ns)))
        } else {
          L1 <- integrate(fTheta, lower = ((1 - 2 * Ns) / (2 - 2 * Ns)), upper = 1)$value
        }
        
        fNotTheta <- function(a) {
          (1-(1/(1+((Prior/(1-Prior))*(((((1-a)*(1-2*Ns)/(2*a))+(1/2))^-1)-1)))))
        }
        if (Ns > .5) {
          L0 <- 2 * integrate(fNotTheta, lower = (1 - (1 / (2 * Ns))), upper = 1)$value + 2 * (1 - (1 / (2 * Ns))) - L1
        } else {
          L0 <- 2 * integrate(fNotTheta, lower = ((1 - 2 * Ns) / (2 - 2 * Ns)), upper = 1)$value - L1
        }
        
        # if (z == 0) { L1 <- (1 - L1) } 
        # if (z == 0) { L0 <- (1 - L0) } 
        
      #   z <- (1 + ((1 - Prior) / Prior)*(L0 / L1)) ^ -1
      #   return(z)
      # }
      
      df.L1[i, j] <- L1
      df.L0[i, j] <- L0
      
      # df.PP1[i, j] <- PublicPrior(1)
      # df.PP0[i, j] <- PublicPrior(0)
      
  }
}
  
  round(df.L1, digits = 3)
  round(df.L0, digits = 3)
  # round(df.PP1, digits = 3)
  # round(df.PP0, digits = 3)
  
  