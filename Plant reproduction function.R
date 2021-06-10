


# Reproduction function for plants, differs from pollinators in that it needs to check pollination status and apply it to offspring number, otherwise similar. Function:
# 1. Creates a list of plant inds which are active (state 1 or 2) and have either 1/ reached maturity or 2/ reached full pollination (pollination column >=1)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number
# In plants, offspring calculated by multiplying offspring number by pollination proportion (rounding to nearest whole number). If pollination proportion >1, use 1. 
# 3. Tidies up column information for offspring (makes sure dead, pollination, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing plant array
# 5. Makes reproducing/fully mature individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway



plantreproduction <- function(plant, active = 2, dead = 3, pollinated = 4, maturity = 5, maturity.threshold = 5, emergence = 8,  seeds = 6){
  reproducers <- which(plant[, maturity] >= maturity.threshold | plant[, pollinated] >= 1 & plant[,active] == c(1,2)); # Check which individuals are active (state 1 or 2) and ready to reproduce (fully matured OR fully pollinated)
  
  for(i in reproducers){
    offspring <- round((pmin(plant[i, pollinated], 1) * seeds), digits = 0) # Offspring equal to either individual's pollinated proportion or 1 (whichever is lower) multiplied by seeds (maximum offspring value) rounded to nearest whole number
    new_plants     <- plant[rep(i,offspring),, drop = FALSE]; # Replicate reproducing individual's details to create offspring
    new_plants[, active] <- 3; # Make all offspring active level 3 so they are dormant and picked up in next season 
    new_plants[, dead] <- 0; # Make sure all offspring are not dead (dead inds shouldn't be getting to this stage but additional check)
    new_plants[, pollinated] <- 0; # Reset all offspring pollination level to 0 
    new_plants[, maturity] <- 0; # Reset all offspring maturity 
    new_plants[, emergence] <- 0; # Reset all offspring emergence to 0 (shouldn't matter as gets overwritten in new season anyway)
    plant <- rbind(plant, new_plants) # Bind offspring to plant dataframe 
    plant[i, 3] <- 1 }# Mark parent as dead after reproducing
  
  return(plant)
}



# Check function works with dummy data

plantcheck <- array(data=0, dim = c(4,13)) # Create plant array 
plantcheck[,1] <- c(1,2,3,4) # Make different species
plantcheck[,2] <- c(1,2,1,0) # Make some active
plantcheck[, 4] <- c(0.1, 1, 1.5, 1.5) # Set pollination proportion
plantcheck[,5] <- c(5,4,5,4) # Make one mature, one not
plantcheck[,6] <- c(1,2,3,4) # Create xlocations 
plantcheck[,7] <- c(1,2,3,4) # Same with ylocations
plantcheck[,10] <- c(35,40,45,50) # Make midpoint data
plantcheck[,11] <- c(0.4,0.6,0.7,0.8) # Make scale data
plantcheck[,13] <- c(1,0,1,0) # Make some able to interact with plant species 1

# Run function

plantcheck <- plantreproduction(plant = plantcheck)
