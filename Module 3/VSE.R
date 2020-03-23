VSE <- function(Gain_matrix, Loss_matrix, ntrials, theta, delta, alpha, phi, beta ){
  
  #generate data from known parameters -> generate empty arrays for choices and their outcomes/rewards
  x <- array(0, c(ntrials)) # deck choices
  Gain <- array(0, c(ntrials))     #part of X
  Loss <- array(0, c(ntrials))    #another part of X
  
  value <- array(0, c(ntrials))
  
  Exploit_chosen <- array(0, c(ntrials, 4))
  Exploit_not <-  array(0, c(ntrials, 4))
  Exploit <-  array(0, c(ntrials, 4))
  
  Explore_chosen <-  array(0, c(ntrials, 4))
  Explore_not <- array(0, c(ntrials, 4))
  Explore <- array(0, c(ntrials, 4))
  
  consistency <-  (3^beta)-1 ## a higher value means choices are strongly drived by the calculated values
  
  exp_p <- array(0, c(ntrials, 4))
  p <- array(0, c(ntrials, 4))
  
  x[1] <- rcat(1, c(.25,.25,.25,.25)) #choose first deck randomly
  
  
  #Building the model
  for (t in 1:(ntrials-1)){
    
    value[t] = Gain[t]^theta - Loss[t]^theta
    
    for (d in 1:4) { #for every deck 
      Exploit_chosen[t+1,d] <-  (Exploit[t,d] * delta) + value[t]
      Exploit_not[t+1, d] = Exploit[t,d]*delta
      Exploit[t+1, d] = ifelse(x[t]==d, Exploit_chosen[t+1,d], Exploit_not[t+1,d])

      Explore_chosen[t+1,d] = 0
      Explore_not[t+1,d] = Explore[t,d] + alpha*(phi-Explore[t,d])
      Explore[t+1, d] = ifelse(x[t] == d, Explore_chosen[t+1,d], Explore_not[t+1, d])
      
      exp_p[t+1,d] <- exp((Explore[t+1,d] + Exploit[t+1,d])*consistency)
      
      }
    
    for (d in 1:4){
      p[t+1,d] <- exp_p[t+1,d]/sum(exp_p[t+1,])
    }
    
    #sample choice for the trial
    x[t+1] <- rcat(1,p[t+1,])
    
    #value
    Gain[t+1] <- Gain_matrix[t+1, x[t+1]]
    Loss[t+1] <- Loss_matrix[t+1, x[t+1]]
    
  }
  
  result <- list(x=x,
                 Gain = Gain,
                 Loss = Loss,
                 Exploit = Exploit,
                 Explore = Explore,
                 p=p,
                 value=value)
  
  
}