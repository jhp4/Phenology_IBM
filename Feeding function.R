
## Function for pollinator feeding which needs to:
# 1. Get x/y loc for each pollinator (if active)
# 2. Get which flowers are at that x/y loc
# 3. Check whether those flowers are species that pollinator can interact with
# 4. If yes then nothing happens (currently, might want to add the maturity ticker here)
# 5. If no, or if no flowers at x/y loc, then uptick hunger counter

feeding <- function(poll, plant, x_loc = 6, y_loc = 7, hunger = 4, dead = 3, species = 1, active = 2, ncol = 12){
  for(p in 1:length(poll[,1])){      
    xloc   <- poll[p, x_loc]; # Get poll locations
    yloc   <- poll[p, y_loc];
    flowers <- sum( plant[, x_loc] == xloc & plant[, y_loc] == yloc); # Total of flowers at that location
    if(poll[p, active] == 1){ # Check pollinator is active
      if(flowers > 0){ # Check there are flowers at that location 
        flowerinds <- which( plant[, x_loc] == xloc & plant[, y_loc] == yloc); # Get the flower individual at that location 
        species <- plant[flowerinds, species] # Extract the species number of that flower 
        if(poll[p, (ncol+species)] == 1){ # Check that this flower species is one which pollinator interacts with 
          poll[p, hunger] <- poll[p, hunger] # If poll can interact with flower, hunger level remains the same
        } else {
          poll[p, hunger] <- poll[p, hunger] + 1 # If poll can't interact with flower then uptick hunger
        }}else {
          poll[p, hunger] <- poll[p, hunger] + 1 # If flower not at location uptick hunger
        }
      } else {
        poll[p, hunger] <- poll[p, hunger]  # If pollinator not active then leave hunger counter the same
      }}
  return(poll);
}


### Checking that function works with defined dataset 

pollcheck<-array(data=0, dim = c(4,13)) # Create pollinator array
pollcheck[,2] <- c(1,1,1,0) # Make all polls except 1 active
pollcheck[,6] <- c(1,1,2,0) # Create xlocations (2 at same location, one at different, one inactive)
pollcheck[,7] <- c(1,1,2,0) # Same with ylocations
pollcheck[,13] <- c(1,0,1,1) # Make sure all except 1 can interact with plant species 1 

plantcheck <- array(data = 0, dim = c(1,13)) # Create plant array
plantcheck[,1] <- 1 # Make plant species no 1
plantcheck[,6] <- 1 # Put plant at location 1,1 so is present for first 2 poll species
plantcheck[,7] <- 1 # Yloc for above


## When the feeding function is applied to pollcheck from the above dataset it should uptick hunger (col 4) for the second and third rows and keep hunger at 0 for rows 1 and 4

pollcheck<-feeding(poll = pollcheck, plant = plantcheck)

#Seems to work! 

