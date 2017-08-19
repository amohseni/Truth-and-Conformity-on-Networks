## TEST - GAUSSIAN DISTRIBUTIONS
Mean1 <- 0.1 # Mean for distribution of signals if Theta is true: state is 1  
Mean0 <- -0.1 # Mean for distribution of signals if Theta is false: state is 0
Variance <- 1
StandardDeviation <- sqrt(Variance) # Variance for both distributions of signals

x <- seq(-15, 15, length = 1000)
y1 <- dnorm(x, mean = Mean1, sd = StandardDeviation)
y2 <- dnorm(x, mean = Mean0, sd = StandardDeviation)
dat <- data.frame(x, y1, y2)
colnames(dat) <- c("x", "y1", "y2")

ggplot(data = dat, aes(x = x)) +
  geom_area(aes(y = y1), colour = "gray20", fill = "gray20", alpha = 0.8) +
  geom_area(aes(y = y2), colour = "gray80", fill = "gray80", alpha = 0.8) +
  labs(x = NULL, y = NULL) +
  ylim(low = 0, high = .4) +
  theme_bw() +
  scale_x_continuous(minor_breaks = seq(-10, 10, 1)) +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_text(size = 14),
    panel.border = element_blank()
  )
       

