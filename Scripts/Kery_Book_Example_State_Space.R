#State space models
library(jagsUI)

####################################################################################################
#Ibex counts 

#Build the data --------------------------------------------------------------------------------------
n_years <-25 # Number of years
N1 <-30 # Initial population size
mean_lambda<-1.02#Mean annual population growth rate
sigma2_lambda<-0.02#Process(temporal) variation of the growth rate
sigma2_y <-20 # Variance of the observation error

#Simulate population size
y <- rep(NA, n.years)
N <- rep(NA, n.years)
N[1] <- N1

#Population growth rate for each year
lambda <-rnorm(n_years - 1, mean_lambda, sqrt(sigma2_lambda))

#fill in the actual population sizes
for (t in 1:(n.years - 1)){
  N[t+1] <-N[t]*lambda[t]
}
#View the data
N

#Model --------------------------------------------------------------------------------------------
#JAGS code
sink("bayes_files\\ibex_ssm.bug")
cat("
  model{
    #Priors
    N_est[1] ~ dunif(0, 500) #prior for initial population size
    mean_lambda ~ dunif(0, 10)
    sigma_proc ~ dunif(0, 10)
    tau_proc <- pow(sigma_proc, -2)
    sigma2_proc <- pow(sigma_proc, 2)
    sigma_obs ~ dunif(0, 10)
    tau_obs <- pow(sigma_obs, -2)
    sigma2_obs <- pow(sigma_obs, 2)
    
    #Likelihood
        #State Process
        for(t in 1:(T - 1)){
        lambda[t] ~ dnorm(mean_lambda, tau_proc)
        N_est[t + 1] <- N_est[t] * lambda[t]
        }
        #Observation process
        for(t in 1:T){
        y[t] ~ dnorm(N_est[t], tau_obs)
        }
  
  }#End model 
", fill = TRUE)
sink()

#Bundle data
bugs_data <- list(y = y, T = n_years)

#Initial values
inits <-function(){list(sigma_proc=runif(1,0,5), 
                        mean_lambda = runif(1, 0.1,2), 
                        sigma_obs = runif(1,0,10),
                        N_est = c(runif(1,20, 40), rep(NA,(n_years - 1))))}

# Parameters monitored
parameters<-c("lambda","mean_lambda","sigma2_obs","sigma2_proc", "N_est")

# MCMCsettings
ni <-25000
nt <-3
nb <-10000
nc <-3

# CallWinBUGSfromR(BRT<1min)
ibex_ssm <-jags(bugs.data,inits,parameters,"bayes_files\\ibex_ssm.bug", n.chains=nc,
           n.thin = nt, n.iter=ni, n.burnin=nb) 


#######################################################################################################
#Modeling counts of house martins and predict population growth rate 
#Build the data ----------------------------------------------------------------

#Number of years to predict
pyears <- 6

#Martin counts
hm <- c(271, 261, 309, 318, 231, 216, 208, 226, 195, 226, 233, 209, 226,
        192, 191, 225, 245, 205, 191, 174, rep(NA, pyears))

#Years for prediction
year <- 1990:(2009 + pyears)

#JAGS model --------------------------------------------------------------------

# Bundle data
bugs.data <- list(y = log(hm), T = length(year))

# Specify model in BUGS language
sink("bayes_files\\martins_ssm.bug")
cat("
model {

#Priors ----
logN.est[1] ~ dnorm(5.6, 0.01) #initial pop size
mean.r ~ dnorm(1, 0.001) #mean growth rate
tau.proc <- pow(sigma.proc, -2) 
sigma.proc ~ dunif(0, 1)
sigma2.proc <- pow(sigma.proc, 2)
sigma.obs ~ dunif(0, 1)
sigma2.obs <- pow(sigma.obs, 2)
tau.obs <- pow(sigma.obs, -2)


#Likelihood ----

  #State process
  for(t in 1:(T-1)){
  r[t] ~ dnorm(mean.r, tau.proc)
  logN.est[t + 1] <- logN.est[t] + r[t]
  } 

  #Observation Process
  for(t in 1:T) {
    y[t] ~ dnorm(logN.est[t], tau.obs)
  }
  
  #Switch counts to a real world scale
  for(t in 1:T) {
    N.est[t] <- exp(logN.est[t])
  }


}
", fill = TRUE)
sink()

#Initial values
inits <- function(){list(sigma.proc = runif(1, 0, 1), mean.r = rnorm(1),
                         sigma.obs = runif(1, 0, 1), 
                         logN.est = c(rnorm(1, 5.6, 0.1), rep(NA, (length(year) - 1))))}

# Parameters monitored
parameters <- c("r", "mean.r", "sigma2.obs", "sigma2.proc", "N.est")

# MCMC settings
ni <- 200000
nt <- 6
nb <- 100000
nc <- 3

# Call WinBUGS from R (BRT 3 min)
hm.ssm <- jagsUI::jags(bugs.data, inits, parameters, "bayes_files\\martins_ssm.bug", n.chains = nc,
                       n.thin = nt, n.iter = ni, n.burnin = nb)

#View model output
print(hm.ssm, digets = 2)

#Plot
fitted <-lower<-upper<-numeric()
year <-1990:2015
n.years <-length(hm)
for(i in 1:n.years){
  fitted[i] <-mean(hm.ssm$sims.list$N.est[,i])
  lower[i] <-quantile(hm.ssm$sims.list$N.est[,i],0.025)
  upper[i] <-quantile(hm.ssm$sims.list$N.est[,i],0.975)
  }
m1 <-min(c(fitted,hm,lower),na.rm=TRUE)
m2 <-max(c(fitted,hm,upper),na.rm=TRUE)
par(mar =c(4.5,4,1,1))
plot(0, 0,ylim=c(m1,m2),xlim=c(1,n.years),ylab="Population
size", xlab="Year",col="black",type="l",lwd=2,axes=FALSE,
     frame =FALSE)
axis(2, las=1)
axis(1, at=1:n.years,labels=year)
polygon(x =c(1:n.years,n.years:1),y=c(lower,upper[n.years:1]),
        col ="gray90",border="gray90")
points(hm,type="l",col="black",lwd=2)
points(fitted,type="l",col="blue",lwd=2)
legend(x =1,y=150,legend=c("Counts","Estimates"),lty=c(1,1),
       lwd =c(2,2),col=c("black","blue"),bty="n",cex=1)