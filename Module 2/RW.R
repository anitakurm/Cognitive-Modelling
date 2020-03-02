#beta - inverse heat -> consistency parameter, the higher beta the more consistent the choices are
#beta - also exploration/exploitatopn parameter?
RW <- function(payoff, ntrials, a, beta) {
  
  #arrays to be filled
  x <- array(0, c(ntrials)) #choices
  r <- array(0, c(ntrials)) #rewards
  Q <- array(0, c(ntrials, 2))  #expected value, 2 columns for 2 bandit-machines
  Qupdate <- array(0, c(ntrials, 2))
  exp_p <-array(0, c(ntrials, 2))  #exponential that's at the top of the choice rule
  p <-array(0, c(ntrials, 2)) #what we use to feed into our categorical distribution
  
  #set values for trial 1
  #for every following trial, we consider the previous trial -> we put some valence to trial 1 and then the model starts at trial 2
  Q[1, 1] <- 1 #for bandit 1
  Q[1, 2] <- 1 #for bandit 2
  
  x[1] <- 1
  r[1] <- 1
  
  #get values for the rest of trials
  for (t in 2:ntrials) {
    ##k is the index for bandit
    for (k in 1:2) {
      #update utility Q for chosen option ONLY, with reward on last trial
      #unchosen option stays the same
      Qupdate[t, k] <- Q[t - 1, k] + (a * (r[t - 1] - Q[t - 1, k])) #the delta rule updatung on both options
      Q[t, k] <- ifelse(k == x[t - 1], Qupdate[t, k], Q[t - 1]) #but keeping the updated Q only for the chosen option 
      
      exp_p[t, k] <-  exp(beta * Q[t, k])
    }
    
    #a separate loop, because we want to sum after all the exponentiations (as representation of all possible options)
    for (k in 1:2) {
      p[t, k] <-
        exp_p[t, k] / sum(exp_p[t, ]) #prob of choosing bandit k on trial t
    }
    
    
    x[t] <-rcat(1, p[t, ])  #choice at trial t, we want it to be stochastic -> from a categorical robability distribution; going to represent relative probability of choosing one machine over other
    r[t] <- payoff[t, x[t]] #reward at trial t based on the choice
    
  }
  
  results <- list(x = x,
                  r = r,
                  Q = Q)
  
  return(results)
  
}
