

# Reproduction function for plants, differs from pollinators in that it needs to check pollination status and apply it to offspring number, otherwise similar. Function:
# 1. Creates a list of plant inds which are active (state 1 or 2) and have either 1/ reached maturity or 2/ reached full pollination (pollination column >=1)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number
# In plants, offspring calculated by multiplying offspring number by pollination proportion (rounding to nearest whole number). If pollination proportion >1, use 1. 
# 3. Tidies up column information for offspring (makes sure dead, pollination, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing plant array
# 5. Makes reproducing/fully mature individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway



plantreproduction <- function(plant, active = 2, dead = 3, pollinated = 4, maturity = 5, maturity.threshold = 5, emergence = 8,  seeds = 6, young = 13){
  
  reproducers <- which(plant[,active] == c(1,2) & plant[, maturity] >= maturity.threshold | plant[, pollinated] >= 1); # Check which individuals are active (state 1 or 2) and ready to reproduce (fully matured OR fully pollinated)
  
  for( i in reproducers) {
    plant[i, young] <- round((pmin(plant[i, pollinated], 1) * seeds), digits = 0); # Populate 'offspring' column for reproducers
    return(plant)}
  
  reproducers <- which(plant[,young] != 0) # Redefine reproducers to those which have at least 1 young (I think this is necessary as number of young calculated in previous step can equal 0 which would break next part of function)
  
  offspring <- as.vector(subset(plant[, young], plant[, young] != 0)) # Create vector of offspring numbers by taking all non-0 values from 'young' column
  
  new.gen <- array(data=0, dim = c((nrow(plant) + sum(offspring)), ncol(plant))); # Create new array that's as long as parent array + offspring
  
  
  
  new.gen[1:nrow(plant),] <- plant; # Populate new array with original array info
  
  start.point <- nrow(plant);  # Get total row number of original plant array and define as start point
  
  for (j in (1:length(offspring))){ # Loop through offspring vector to populate array
    
    offspring.number <- offspring[j]; # Get number of offspring to be generated each loop from offspring vector
    
    parent <- reproducers[j]; # Get ID of parent producing these offspring
    
    for (k in (1:offspring.number)){ # Then loop through offspring number to add a row for each offspring 
      
      row.no = start.point + k; # Set within-loop start point. Should move down rows with each j and the initial start.point will be redefined for each batch of offspring
      
      new.gen[row.no, ] <- plant[parent,]; # Populate row with parent info  
      new.gen[row.no, active] <- 3; # Make all offspring active level 3 so they are dormant and picked up in next season 
      new.gen[row.no, dead] <- 0; # Make sure all offspring are not dead 
      new.gen[row.no, pollinated] <- 0; # Reset all offspring pollination level to 0 
      new.gen[row.no, maturity] <- 0; # Reset all offspring maturity 
      new.gen[row.no, emergence] <- 0; # Reset all offspring emergence to 0 
    }
    start.point = start.point + offspring.number # Redefine start.point (so next loop starts from correct point and doesn't overwrite previous offspring inds information)
    return(new.gen)
  }
  
}




# Check function works with dummy data

plantcheck <- array(data=0, dim = c(4,14)) # Create plant array 
plantcheck[,1] <- c(1,2,3,4) # Make different species
plantcheck[,2] <- c(0,2,1,1) # Make some active
plantcheck[, 4] <- c(0.1, 1, 0.2, 0.8) # Set pollination proportion
plantcheck[,5] <- c(5,4,5,4) # Make one mature, one not
plantcheck[,6] <- c(1,2,3,4) # Create xlocations 
plantcheck[,7] <- c(1,2,3,4) # Same with ylocations
plantcheck[,10] <- c(35,40,45,50) # Make midpoint data
plantcheck[,11] <- c(0.4,0.6,0.7,0.8) # Make scale data
plantcheck[,14] <- c(1,0,1,0) # Make some able to interact with plant species 1

# Run function

plantcheck <- plantreproduction(plant = plantcheck)




reproducers <- which(plantcheck[,active] == c(1,2) & plantcheck[, maturity] >= maturity.threshold | plantcheck[, pollinated] >= 1); # Check which individuals are active (state 1 or 2) and ready to reproduce (fully matured OR fully pollinated)

for( i in reproducers) {
  plantcheck[i, young] <- round((pmin(plantcheck[i, pollinated], 1) * seeds), digits = 0); # Populate 'offspring' column for reproducers
  return(plantcheck)}

reproducers <- which(plantcheck[,young] != 0) # Redefine reproducers to those which have at least 1 young (I think this is necessary as number of young calculated in previous step can equal 0 which would break next part of function)

offspring <- as.vector(subset(plantcheck[, young], plantcheck[, young] != 0)) # Create vector of offspring numbers by taking all non-0 values from 'young' column

new.gen <- array(data=0, dim = c((nrow(plantcheck) + sum(offspring)), ncol(plantcheck))); # Create new array that's as long as parent array + offspring



new.gen[1:nrow(plantcheck),] <- plantcheck; # Populate new array with original array info

start.point <- nrow(plantcheck);  # Get total row number of original plant array and define as start point

for (j in (1:length(offspring))){ # Loop through offspring vector to populate array
  
  offspring.number <- offspring[j]; # Get number of offspring to be generated each loop from offspring vector
  
  parent <- reproducers[j]; # Get ID of parent producing these offspring
  
  for (k in (1:offspring.number)){ # Then loop through offspring number to add a row for each offspring 
    
    row.no = start.point + k; # Set within-loop start point. Should move down rows with each j and the initial start.point will be redefined for each batch of offspring
    
    new.gen[row.no, ] <- plantcheck[parent,]; # Populate row with parent info  
    new.gen[row.no, active] <- 3; # Make all offspring active level 3 so they are dormant and picked up in next season 
    new.gen[row.no, dead] <- 0; # Make sure all offspring are not dead 
    new.gen[row.no, pollinated] <- 0; # Reset all offspring pollination level to 0 
    new.gen[row.no, maturity] <- 0; # Reset all offspring maturity 
    new.gen[row.no, emergence] <- 0; # Reset all offspring emergence to 0 
  }
  start.point = start.point + offspring.number # Redefine start.point (so next loop starts from correct point and doesn't overwrite previous offspring inds information)
return(new.gen)
}