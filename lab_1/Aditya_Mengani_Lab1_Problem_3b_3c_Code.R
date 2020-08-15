

#problem 3.c
#This method generates a random variable between 1-100 assuming 
#that rv represents the number of checkups patient is well. 
# if x = 3 then,  w1,w2,w3, I4 or D4 where 4th state is the exit
#state. It then calculates the pdf for each x, x=1,x=2,x=3
# finally it does the sum(x.pdf(x)) for x = 1,2,3 and returns 
#the expectation of random variable


cal_single_trajectory <- function(){
  #generate a sample rv and create a list
  x <- sample(1:100,1,replace=T)
  z <- sample(0,x,replace=T)
  x_transition <- sample(0,x,replace=T)
  
  for(i in 1:x)
  {
    #define intial pdf variable, well and ill state probabilities
    well_state <- 0.2
    ill_state <- 0.8
    
    # create a transition trajectory for x
    x_transition[i] <- i
    
    if(i==1){
      x_pdf <- 1
    }
    else{
      exponent_well <- well_state^(i-1)
      x_pdf <- exponent_well*ill_state
    }
    z[i] <- x_pdf
  }
  print("printing pdf distribution vector z")
  print(z)
  print("transitioning vector x")
  print(x_transition)
  print("The expectation of the rv X is ")
  print(sum(x_transition*z))
  return (sum(x_transition*z))
}










#problem 3.d
##method to simulate 1000 patient trajectories and calculate their mean
sim_1000_trajectory <- function(){
  patients_1000 <- sample(0,1000,replace=T)
  for(i in 1:1000)
  {
    patients_1000[i] <- cal_single_trajectory()
  }
  print("Sample mean for each patient")
  print(patients_1000)
  patients_1000_mean <- mean(patients_1000)
  print("Sample mean for 1000 patients is")
  return (patients_1000_mean)
}


