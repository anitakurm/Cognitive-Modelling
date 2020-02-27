random <- function(payoff, ntrials, theta) {
  #bias towards option 1 (theta) and option 2
  b <- c(theta, 1-theta)
  
  #empty array to be filled
  x <- array(0, c(ntrials))
  
  #rewards array to be filled
  r <- array(0, c(ntrials))
  
  for (t in 1:ntrials) {
    #agent that chooses randomly between options 1 and 2 with bias ( to 1) = theta
    #binomial sample (but really from a categorical distribution)
    x[t]  <-  rcat(1, b)
    
    #what reward does the agent get?
    r[t] <- payoff[t, x[t]]
  }
  
  result <- list(x=x, r=r)
  return(result) 
}