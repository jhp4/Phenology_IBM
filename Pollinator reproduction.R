

# Reproduction function for pollinators which:
# 1. Creates a list of pollinator inds which are active (state 1 or 2) and have reached reproductive maturity (defined by maturity reaching  repro.threshold value)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number (currently a set number  of offspring but would be straightforward to make this a sample from a distribution)
# 3. Tidies up column information for offspring (makes sure dead, hunger, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing pollinator array
# 5. Makes reproducing individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway

 



pollreproduction <- function(poll, species = 1, active = 2, dead = 3, hunger = 4, maturity = 5, emergence = 8, repro.threshold = 5, offspring = 3){
  
  reproducers <- which(poll[,maturity] >= repro.threshold & 
                         (poll[,active] == 1 | poll[,active] == 2)); # Extract all reproducing pollinator inidividuals
  
  if(length(reproducers) > 0){ # BD No need to do any of this if not.
    # BD: Here's an attempt to avoid the rbind below
    num_old_poll    <- dim(poll)[1]; # Number of rows in poll
    num_reproducers <- length(reproducers); # Get the number of reproducers
    num_offspring   <- offspring * num_reproducers; # Could adj for ind var
    
    new_polls_a  <- array(data = NA,  # Array of correct size
                          dim = c(num_old_poll + num_offspring, dim(poll)[2]));
    
    # Not sure if I like doing this -- might be best to work with arrays rather
    # than data frames, but this might not be so bad (just have to do once).
    new_polls    <- as.data.frame(new_polls_a);
    
    new_polls[1:num_old_poll,] <- poll;
    # Now we have the old list and a bunch of NAs that we need to replace with
    # offspring values -- again, all to avoid the rbind.
    
    # Need to keep a record of both the parent and the offspring rows
    offspring_parents <- rep(x = reproducers, times = 3);
    # Can `sort` the above, but it's really not necessary, I don't think
    # Now we have the IDs of the parents of each offspring
    # Could also substitute `times = 3` with a vector of length(reproducers)
    
    act_row <- num_old_poll + 1;
    end_row <- dim(new_polls)[1];
    
    while(act_row <= end_row){
      # A couple subsets, but just replacing the offspring row with the row
      # indicated by offspring_parents[act_row];
      new_polls[act_row, ]          <- poll[ offspring_parents[(act_row-num_old_poll)] , ];
      new_polls[act_row, active]    <- 3;
      new_polls[act_row, dead]      <- 0;
      new_polls[act_row, hunger]    <- 0;
      new_polls[act_row, maturity]  <- 0;
      new_polls[act_row, emergence] <- 0;
      act_row                       <- act_row + 1; # Super important
    } # It looks longer, but we've avoided the rbind
    
    colnames(new_polls) <- colnames(poll);
  }else{ # Else just replace with the original polls
    new_polls <- poll;
  }
  return(new_polls)
}



# Tried inserting this for loop to mark reproducers as dead (after reproducers indentified, before if statement) but R didn't like it
for( i in reproducers) {
  
  poll[i, dead] <- 1;
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


  

