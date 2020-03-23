ORL <- function(payoff, ntrials, alpha_rew, alpha_pun, beta_f, beta_per, K, theta){
  
  
  
  #generate data from known parameters -> generate empty arrays for choices and their outcomes/rewards
  x <- array(0, c(ntrials)) #choice
  X <- array(0, c(ntrials)) #rewards
  
  Ev <- array(0, c(ntrials, 4))  #expected valence
  Ev_update <- array(0, c(ntrials, 4))  #update expected value
  
  Ef <- array(0,c(ntrials, 4)) #expected frequency of reward
  Ef_chosen <- array(0, c(ntrials, 4)) #to update all, but keep only the chosen one of them  
  Ef_not <- array(0, c(ntrials, 4))  #to update not chosen decks' reward frequency values
  
  PS <- array(0, c(ntrials, 4)) #perseverence variable (similar to CK)
  V<- array(0, c(ntrials, 4)) # Valence - linear combination of Ev, Ef, and Persev
  
  exp_p <- array(0, c(ntrials, 4))  #exponentialised part of probability value for softmax
  p <- array(0, c(ntrials,4)) #probability values utilized in the softmax
  
  x[1] <- rcat(1, c(.25,.25,.25,.25))
  
  
  #Building the model
  for (t in 2:ntrials){
    
    signX <-  ifelse(X[t-1]<0,-1,1) #sign of the outcome (loss or gain)
    
    for (d in 1:4) { #for every deck 
      Ev_update[t,d] <- ifelse(X[t-1] >= 0, Ev[t-1, d] +(alpha_rew*(X[t-1] - Ev[t-1,d])) ,Ev[t-1, d] +(alpha_pun*(X[t-1] - Ev[t-1,d])))
      Ev[t,d] <- ifelse(x[t-1] == d, Ev_update[t,d], Ev[t-1, d])
      
      Ef_chosen[t,d] <- ifelse(X[t-1] >= 0, Ef[t-1, d] + (alpha_rew*(signX - Ef[t-1, d])), Ef[t-1, d] + (alpha_pun*(signX - Ef[t-1, d])))                                                     
      Ef_not[t,d] <- ifelse(X[t-1]>= 0, Ef[t-1, d] + (alpha_pun * ((-signX/3)- Ef[t-1,d])), Ef[t-1, d] + (alpha_rew*((-signX/3)- Ef[t-1,d])))
      Ef[t,d] <- ifelse(x[t-1] == d, Ef_chosen[t,d], Ef_not[t,d])
      
      PS[t,d] <- ifelse(x[t-1] == d, 1/(1+K), PS[t-1, d]/(1+K))
      
      V[t,d] <- Ev[t,d] + Ef[t,d]*beta_f + PS[t,d]*beta_per
      
      #softmax
      exp_p[t,d] <- exp(theta*V[t,d])
      }
    
    for (d in 1:4){
      p[t,d] <- exp_p[t,d]/sum(exp_p[t,])
    }
    
    #sample choice for the trial
    x[t] <- rcat(1,p[t,])
    
    #value
    X[t] <- payoff[t, x[t]]
    
  }
  
  result <- list(x=x,
                 X = X,
                 Ev = Ev,
                 Ef = Ef,
                 PS = PS,
                 V = V,
                 p=p)
  
  
}