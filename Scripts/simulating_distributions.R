library(ggplot2)

mean_lambda <- 4.5

area <- 0.7853982

lambda <- exp(mean_lambda)

samples <- rpois(n = 1000, lambda = lambda)

sample_dat <- data.frame(samples)

ggplot(sample_dat, aes(x = samples)) +
  geom_density(lwd = 2, col = "blue") +
  theme_classic()

