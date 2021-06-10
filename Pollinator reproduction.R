

# Reproduction function for pollinators which:
# 1. Creates a list of pollinator inds which are active (state 1 or 2) and have reached reproductive maturity (defined by maturity reaching  repro.threshold value)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number (currently a set number  of offspring but would be straightforward to make this a sample from a distribution)
# 3. Tidies up column information for offspring (makes sure dead, hunger, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing pollinator array
# 5. Makes reproducing individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway

 

pollreproduction <- function(poll, species = 1, active = 2, dead = 3, hunger = 4, maturity = 5, emergence = 8, repro.threshold = 5, offspring = 3){
reproducers <- poll[poll[,maturity] >= repro.threshold & poll[,active] == 1|2,, drop = FALSE]; # Extract all reproducing pollinator inidividuals

for(i in 1:length(reproducers[,1])){
  new_polls     <- reproducers[rep(i,offspring),]; # Replicate reproducing individual's details to create offspring
  new_polls[, active] <- 3; # Make all offspring active level 3 so they are dormant and picked up in next season 
  new_polls[, dead] <- 0; # Make sure all offspring are not dead (dead inds shouldn't be getting to this stage but additional check)
  new_polls[, hunger] <- 0; # Reset all offspring hunger to 0 
  new_polls[, maturity] <- 0; # Reset all offspring maturity 
  new_polls[, emergence] <- 0; # Reset all offspring emergence to 0 (shouldn't matter as gets overwritten in new season anyway)
  poll <- rbind(poll, new_polls) # Bind offspring to pollinator dataframe 
  poll[i, 3] <- 1 }# Mark parent as dead after reproducing

return(poll)
}


# Check function works with dummy data

pollcheck <- array(data=0, dim = c(2,13)) # Create pollinator array with 2 inds
pollcheck[,1] <- c(1,2) # Make inds different species
pollcheck[,2] <- c(1,2) # Make both active 
pollcheck[,5] <- c(5,5) # Make one mature, one not
pollcheck[,6] <- c(2,1) # Create xlocations 
pollcheck[,7] <- c(3,1) # Same with ylocations
pollcheck[,10] <- c(35,40) # Make midpoint data
pollcheck[,11] <- c(0.4,0.6) # Make scale data
pollcheck[,12] <- c(0.1, 0.35) # Make efficacy data
pollcheck[,13] <- c(1,0) # Make one able to interact with plant species 1

# Run function

pollcheck <- pollreproduction(poll = pollcheck)


  

