
# Placement function checks to see if individual is active then, if active, places them in the environment
# x is the size of the landscape x axis, y is the size of the landscape y axis


placementpoll <- function(poll, x = 200, y = 200, active = 2, x_loc = 6, y_loc = 7){ 
  for(i in 1:length(poll)[, active]){
    if(poll[i, active] == 1){
      poll[i, x_loc]<-sample(x = 1:x, size = 1, replace = TRUE);
      poll[i, y_loc]<-sample(x = 1:y, size = 1, replace = TRUE);
    }
  }
  return(poll)
}

## Need a separate function for plants as multiple individuals can't occupy the same space. Therefore need to check whether there are any other plants there: if so, emerging plant dies 

placementplant <- function(plant, x = 200, y = 200, active = 2, dead = 3, x_loc = 6, y_loc = 7){ 
  for(i in 1:length(plant[,active])){
    if(plant[i, active] == 1){
      plant[i, x_loc]<-sample(x = 1:x, size = 1, replace = TRUE);
      plant[i, y_loc]<-sample(x = 1:y, size = 1, replace = TRUE);
      on_cell <- sum(plant[, x_loc] == plant[i, x_loc] & plant[, y_loc] == plant[i, y_loc]); # Check whether any other inds at plant emergence location
      if(on_cell > 1){
        plant[i, dead] <- 1 # If there are, emerging plant dies  
      }  else             
    }
  }
  return(plant)
}


# Check that plant placement/mortality works (when running this I set x to 2 and y to 2 to constrain landscape so there must be density-based mortality)

plantcheck <- array(data=0, dim = c(5,7)) # Create plant array
plantcheck[,2] <- 1 # Make all active 

# Run function

plantcheck <- placementplant(plant = plantcheck)




