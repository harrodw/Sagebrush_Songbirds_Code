library(ggplot2)

mean_lambda <- 4.5

area <- 0.7853982

lambda <- exp(mean_lambda)

samples <- rpois(n = 1000, lambda = lambda)

sample_dat <- data.frame(samples)

ggplot(sample_dat, aes(x = samples)) +
  geom_density(lwd = 2, col = "blue") +
  theme_classic()




# Distance sampling half normal ##################################

# Constants
B <- 5
delta <- 0.025
trunc_dist <- 0.125
sigma <- -2
midpt <- seq(from = delta, to = trunc_dist, length.out = B)

# Define functions
g_nat <- rep(NA, B)
g <- rep(NA, B)
f <- rep(NA, B)
pi_pd <- rep(NA, B)
pi_pd_c <- rep(NA, B)
p_dct <- rep(NA, B)

# Simulate
  # Loop through distance bins
  for(b in 1:B){
    # Half-normal detection function
    g_nat[b] <- -midpt[b] * midpt[b]/(2 * sigma^2)  
             
    g[b] <- exp(g_nat[b])
    # Prob density function out to max truncation distance
    f[b] <- ((2 * midpt[b])  / trunc_dist^2) * delta
    # Detection cell probability
    pi_pd[b] <- g[b] * f[b]                             
  } # End loop through bins

p_dct <- sum(pi_pd)

for(g in 1:nD){       # midpt = mid-point of each distance band
  log(p[s,g]) <- -midpt[g] * midpt[g] / (2 * sigma[s]^2)
  pi[s,g] <- ((2 * midpt[g] ) / newB^2) * delta # prob. per interval
  f[s,g] <- p[s,g] * pi[s,g]
  fc[s,g] <- f[s,g] / pcap[s] 
}
# Rectangular approx. of integral that yields the Pr(capture)
pcap[s] <- sum(f[s,])

  # Loop through distance bins
for(b in 1:B){
   pi_pd_c[b] <- f[b] / p_dct
}

pi_pd
pi_pd_c
p_dct
