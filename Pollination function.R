
## Function for plant/flower pollination which is broadly similar to pollinator feeding function in that it needs to:
# 1. Get x/y loc for each plant (if active)
# 2. Get which pollinators are at that x/y loc
# 3. Check whether those pollinators are species that flower can interact with
# 4. If yes then increase 'pollination' measure by efficacy of visiting pollinators  
# 5. If no then nothing happens (could add maturity uptick here but might be best saving to next step)

pollination <- function(plant, poll, x_loc = 6, y_loc = 7, efficacy = 12, pollination = 4, dead = 3, species = 1, active = 2, ncol = 12){
  for(p in 1:length(plant[,1])){      
    xloc   <- plant[p, x_loc]; # Get plant locations
    yloc   <- plant[p, y_loc];
    polls <- sum( poll[, x_loc] == xloc & poll[, y_loc] == yloc); # Total of pollinators at that location
    if(plant[p, active] == 1){ # Check flower is active
      if(polls > 0){ # Check there are pollinators at that location 
        pollinds <- which(poll[, x_loc] == xloc & poll[, y_loc] == yloc); # Get the pollinator individual(s) at that location 
        for(i in 1:length(pollinds)){
        species <- poll[i, species]; # Extract the species number of pollinator(s) 
        efficacy<-poll[i, efficacy] # Exract pollinator efficacy for pollinators
        if(plant[p, (ncol+species)] == 1){ # Check that this pollinator species is one which pollinator interacts with 
          plant[p, pollination] <- plant[p, pollination] + efficacy # If poll can interact with flower, hunger level remains the same
        } else {
          plant[p, pollination] <- plant[p, pollination]  # If poll can't interact with flower then pollination remains same
        }}}else {
          plant[p, pollination] <- plant[p, pollination]  # If pollinator not at location then pollination remains same
        }
    } else {
      plant[p, pollination] <- plant[p, pollination]  # If flower not active then pollination remains the same
    }}
  return(plant);
}


### Checking that function works with defined dataset 

pollcheck<-array(data=0, dim = c(4,12)) # Create pollinator array
pollcheck[,1] <- c(1,1,2,2) # Make 2x species 1 and 2x species 2
pollcheck[,6] <- c(1,2,1,2) # Create xlocations (1 of each species at 2 different locations)
pollcheck[,7] <- c(1,2,1,2) # Same with ylocations
pollcheck[,12] <- c(0.1,0.1,0.3,0.3) # Create some species-specific efficacy values 

plantcheck <- array(data = 0, dim = c(3,14)) # Create plant array
plantcheck[,1] <- c(1,1,2) # Make plant species no 1 and no 2
plantcheck[,2] <- 1 # Make al individuals active
plantcheck[,6] <- c(1,3,2) # Want 1 plant at location 1,1 one at location 3,3 one at location 2,2
plantcheck[,7] <- c(1,3,2) # Yloc for above
plantcheck[,13] <- c(1,1,0) # Species 1 can interact with pollinator species 1, species 2 can't
plantcheck[,14] <- c(1,1,1) # All species can interact with pollinator species 2 

## This should create a scenario where first plant individual gets pollinated by 2 species (for total of 0.4 pollination in col 4), second individual can interact but isn't at a location with any pollinators (pollination remains 0 in col4), third is at a location with 2 pollinators but can only interact with one of them (for total of 0.3 pollination in col 4)

plantcheck <- pollination(plant = plantcheck, poll = pollcheck)


