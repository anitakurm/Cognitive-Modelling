PVL <- function(payoff, ntrials, w, A,a, theta){
  
  #generate data from known parameters -> generate empty arrays for choices and their outcomes/rewards
  x <- array(0, c(ntrials)) #choice
  X <- array(0, c(ntrials)) #rewards
  u <- array(0, c(ntrials, 4)) #utility for 4 decks
  Ev <- array(0, c(ntrials, 4))  #expected value
  Ev_update <- array(0, c(ntrials, 4))  #update expected value
  exp_p <- array(0, c(ntrials, 4))  #exponentialised part of probability value
  p <- array(0, c(ntrials,4)) #probability values utilized in the softmax
  
  x[1] <- rcat(1, c(.25,.25,.25,.25))
  
  #Building the model
  for (t in 2:ntrials){
    for (d in 1:4) { #for every deck
      u[t,d] <- ifelse(X[t-1]<0, -w*abs(X[t-1])^A,abs(X[t-1])^A)  #utility for trial t deck d; IF REWARD WAS LESS THAN 0, WE WANT TO TAKE ITS ABSOLUTE VALUE, take that number, put to the power of A for the curve and multiply by w; otherwise just put to the power of A
      
      Ev_update[t,d] <- Ev[t-1, d] +(a*(u[t,d] - Ev[t-1,d]))  #Update valence for every deck as if it was chosen: Expected valence on previous trial plus new information
      Ev[t,d] <- ifelse(x[t-1] == d, Ev_update[t,d], Ev[t-1, d])
      
      #softmax
      exp_p[t,d] <- exp(theta*Ev[t,d])
      
      
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
                 Ev = Ev)
}
  