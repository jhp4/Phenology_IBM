

# Reproduction function for plants, differs from pollinators in that it needs to check pollination status and apply it to offspring number, otherwise similar. Function:
# 1. Creates a list of plant inds which are active (state 1 or 2) and have either 1/ reached maturity or 2/ reached full pollination (pollination column >=1)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number
# In plants, offspring calculated by multiplying offspring number by pollination proportion (rounding to nearest whole number). If pollination proportion >1, use 1. 
# 3. Tidies up column information for offspring (makes sure dead, pollination, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing plant array
# 5. Makes reproducing/fully mature individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway




plantreproduction <- function(plant, active = 2, dead = 3, pollinated = 4, maturity = 5, maturity.threshold = 5, emergence = 8,  seeds = 6, young = 13){
  
  # Check which individuals are active (state 1 or 2) and ready to reproduce (fully matured OR fully pollinated)
  reproducers <- which((plant[,active] == 1 | plant[,active] == 2) & 
                         (plant[, maturity] >= maturity.threshold | plant[, pollinated] >= 1)); 
  
  if(length(reproducers) > 0){ # No need to do any of this if not.
    
    for( i in 1:length(reproducers)) { # Take each reproducer and calculate young
      
      parent <- reproducers[i];
      
      # Young calculated by taking lower of pollinated proportion or 1, multiplying it by seeds variable and rounding to nearest whole number 
      plant[parent, young] <- round((pmin(plant[parent, pollinated], 1) * seeds), digits = 0); 
      
      
      # Then mark parent/reproducer as dead
      plant[parent, dead] <- 1;
    }
    
    # Create vector of offspring numbers (necessary to remove 0s first? If so can add subset instruction for  plant[, young] != 0))
    offspring <- as.vector(subset(plant[, young], plant[, young] != 0)); 
    reproducers <- which(plant[, young] != 0);
    
    # Create new array to populate with new plant information
    num_old_plant    <- dim(plant)[1]; # Number of rows in plant
    num_offspring    <- sum(offspring)
    
    new_plant_a <- array(data = NA,  # Array of correct size
                         dim = c((num_old_plant + num_offspring), dim(plant)[2]));
    
    # Populate new array with old plant info
    new_plant    <- as.data.frame(new_plant_a);
    
    new_plant[1:num_old_plant,] <- plant;
    
    # Create a record of offspring parents as a product of parent info * number of offspring per parent (here as vector)
    offspring_parents <- rep(x = reproducers, times = offspring);
    
    act_row <- num_old_plant + 1;
    end_row <- dim(new_plant)[1];
    
    while(act_row <= end_row){
      # A couple subsets, but just replacing the offspring row with the row
      # indicated by offspring_parents[act_row];
      new_plant[act_row, ]          <- plant[ offspring_parents[(act_row-num_old_plant)] , ];
      new_plant[act_row, active]    <- 3;
      new_plant[act_row, dead]      <- 0;
      new_plant[act_row, maturity]  <- 0;
      new_plant[act_row, emergence] <- 0;
      new_plant[act_row, young]     <- 0;
      act_row                       <- act_row + 1; # Super important
    }
    
    colnames(new_plant) <- colnames(plant);
    
  }else{ # Else just replace with the original plant info
    new_plant <- plant;
  }
  return(new_plant)
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

