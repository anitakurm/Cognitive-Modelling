## copied from RW.R, comments not updated
kernel <- function(payoff,ntrials,a,beta){ #a - learning rate, b - inverse heat
  
  #set up arrays you wanna fill
  X <- array(0, c(ntrials)) #choices
  r <- array(0, c(ntrials))  #reward!
  Q <- array(0, c(ntrials, 2)) #the valence, the thing that updates, sense of valye the agent has, two-dimensional bc 2 bandits, updates for both
  CK <- array(0, c(ntrials, 2)) #choice kernel
  
  CKchosen <- array(0, c(ntrials, 2))
  CKunchosen <- array(0, c(ntrials, 2))
  
  #probabilities of choosing the bandit given stuff
  exp_p <- array(0, c(ntrials, 2))  #exponential
  p <- array(0, c(ntrials, 2))
  
  #set value for trial one 
  #set values for trial 1
  CK[1,1] <- 1 #trial 1 bandit 1
  CK[1,2] <- 1  
  # now 1st is done the agent has no bias yet, wanna put him through the payoff matrix and see what he learns
  
  
  #these
  X[1] <- 1
  r[1] <- 1
  
  for (t in 2:ntrials) {
    
    for (k in 1:2) {
      
      #update utility Q for chosen option ONLY, with reward on last trial
      #unchosen option stays the same
      CKchosen[t,k] <- CK[t-1,k] + (a*(1 - CK[t-1,k]))
      CKunchosen[t,k] <- CK[t-1,k] + (a*(0 - CK[t-1,k]))
      
      CK[t,k] <- ifelse(k==X[t-1],CKchosen[t,k],CKunchosen[t,k]) #we only want to update the one that got chosen; there's no decay in this model
      
      #calculate the luce choice
      exp_p[t,k] <- exp(beta * CK[t,k])
      
    }
    
    #luce choice rule!
    for (k in 1:2) {
      
      #lets calculate p
      p[t,k] <- exp_p[t,k]/sum(exp_p[t,])
      
    } #k is index for bandit
    
    X[t] <- rcat(1, p[t,])
    r[t] <- payoff[t, X[t]] #just indexing a payoff matrix
    
  }
  
  results <- list(x=X, r=r, CK=CK)
  return(results)
}