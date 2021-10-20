
## Here trying to collapse all the information relating to setting up the arrays for species and individuals into functions 


## Initialise pollinator species info and individuals ####

## When starting up new pollinator species will need to define number of new species to be created, number of plant species (which will need to be consistent with the number of plant species created otherwise species interactions will not be consistent) and a number of min/max parameters for both pollinator efficacy and defining the logistic model specifications that emergence time is generated from. Currently all these parameters are uniformly sampled from a range defined by the min/max values but would be straightfoward to draw these samples from a Gaussian distribution around a midpoint or from a Poisson distribution with a defined lambda if needed. I have added a cols line just to make it easier to see for other functions that need to refer to it (eg the interactions function which is run next). The current default for this is 6 named columns to correspond to the 6 named species traits  - might be preferable to rewrite this so that named columns are generated individually and then bound together at the end so it isn't necessary to predefine the array dimensions but this method seems fine for now. 

make_poll_species<- function(species_number     , 
                             plant_species      , ## This line will need to be equal to the number of species in the make_plant_species function
                             cols,
                             max_interactions   , 
                             min_interactions   , 
                             max_midpoint       , 
                             min_midpoint       ,
                             max_scale          ,
                             min_scale          ,
                             max_efficacy       ,
                             min_efficacy       
){
  poll_species<-array(data=0, dim=c(species_number,(cols+plant_species)));
  colnames(poll_species)<-c("species","interactions","midpoint","scale","efficacy", c(1:plant_species));
  poll_species[, 1]<-seq_len(nrow(poll_species));
  poll_species[,2]<-sample(1:plant_species, dim(poll_species)[1], replace = F);
  poll_species[,3]<-sample(seq(from = min_midpoint, to = max_midpoint, by=1),size=dim(poll_species)[1]);
  poll_species[,4]<-sample(seq(from = min_scale, to = max_scale, by=0.05),size=dim(poll_species)[1]);
  poll_species[,5]<-sample(seq(from = min_efficacy, to = max_efficacy, by=0.05),size=dim(poll_species)[1]);
  return(poll_species)
}

## Here is the code to actually generate a species information array for 5 species using the make_poll_species function 

poll_species<-make_poll_species(species_number=5, cols=5, plant_species=5, max_interactions=5, min_interactions=1, max_midpoint=50, min_midpoint=30, max_scale=0.6, min_scale=0.2, max_efficacy=0.3, min_efficacy=0.1)

## A function that generates interaction partners for each species.
#'Interactions' needs to match to the interactions column in the initial poll_species array (default is column 2).
# int_species needs to match the number of potential species that can be interacted with, which should equal the total number of plant species if running this function on the pollinator species info and vice-versa
#Number of partners is currently randomly sampled from 1: max(int_species) but again could make this more subtle if we want to directly tweak generalism/specialism rates among species

int_partners<-function(species, interactions = 2, int_species=5, cols = 5){ # Here the 'cols' variable is the number of columns in the array before species numbers starts (currently 5)
  for (i in 1:dim(species)[1]){
    n<-species[i,interactions];
    ints<-(sample(1:int_species, n, replace = F));
    species[i,(cols+ints)]<-1
  }
  return(species)
}

## Run interaction function on poll_species

poll_species<-int_partners(poll_species, int_species=5)

## Start up individuals array
# Species number = number of species to be added to the population (should be consistent with the number defined in the make_poll_species function)
# Cols = number of named columns (default is 8, will need to change if more traits added)
# Individuals = number of individuals of each species to be added to population (total number of individuals will always be individuals*species number). Currently set to add equal numbers of each species although this could be edited 


make_poll_individuals<- function(species_number,
                                 cols,
                                 individuals
){
  rows=species_number*individuals
  poll<-array(data=0, dim=c((species_number*individuals),cols));
  colnames(poll)<- c("species","active","dead", "hunger", "maturity", "x_loc", "y_loc", "emergence");
  poll[,1]<-rep(1:species_number, each=1, length.out=(individuals/species_number))
  return(poll)
}


## Make individuals and save as poll 

poll<-make_poll_individuals(species_number=5, cols=8, individuals=50)

#Merge with species info. At the moment this turns it into a df

poll<-merge(poll, poll_species, by = "species")

## Initialise plant species info and individuals ####


# Function for plant species which is almost identical to that of pollinator species

make_plant_species<- function(species_number     , 
                              poll_species      , ## This line will need to be equal to the number of species in the make_poll_species function
                              cols,
                              max_interactions   , 
                              min_interactions   , 
                              max_midpoint       , 
                              min_midpoint       ,
                              max_scale          ,
                              min_scale          ,
                              max_efficacy       ,
                              min_efficacy       
){
  plant_species<-array(data=0, dim=c(species_number,(cols+poll_species)));
  colnames(plant_species)<-c("species","interactions", "midpoint","scale", "blank", c(1:poll_species)); # At this point I've just added a blank column so the number of columns for plant species is equal to that in pollinator species so I can reference same columns for shared functions
  plant_species[, 1]<-seq_len(nrow(plant_species));
  plant_species[,2]<-sample(1:poll_species, dim(plant_species)[1], replace = F);
  plant_species[,3]<-sample(seq(from = min_midpoint, to = max_midpoint, by=1),size=dim(plant_species)[1]);
  plant_species[,4]<-sample(seq(from = min_scale, to = max_scale, by=0.05),size=dim(plant_species)[1]);
  plant_species[,5]<-sample(seq(from = min_efficacy, to = max_efficacy, by=0.05),size=dim(plant_species)[1]);
  return(plant_species)
}

## Here is the code to actually generate a species information array for 5 species using the make_plant_species function 

plant_species<-make_plant_species(species_number=5, cols=5, poll_species=5, max_interactions=5, min_interactions=1, max_midpoint=50, min_midpoint=30, max_scale=0.6, min_scale=0.2, max_efficacy=0.3, min_efficacy=0.1)

## Run interaction function on plant_species

plant_species<-int_partners(plant_species, int_species=5)

## Create individuals array for plant individuals, same function as with pollinator individuals but with different column names

make_plant_individuals<- function(species_number,
                                  cols,
                                  individuals
){
  rows=species_number*individuals
  plant<-array(data=0, dim=c((species_number*individuals),cols));
  colnames(plant) <- c("species","active","dead", "pollinated", "maturity", "x_loc", "y_loc", "emergence");
  plant[,1]<-rep(1:species_number, each=1, length.out=(individuals/species_number))
  return(plant)
}


## Make individuals and save as plant 

plant<-make_plant_individuals(species_number=5, cols=8, individuals=50)

#Merge with species info. At the moment this turns it into a df

plant<-merge(plant, plant_species, by = "species")



# Define emergence function & initialise emergence ####
# Generates a random number between 0 and 1 for each individual and then back-calculates an emergence date from a logistic equation using the midpoint and scale values pulled from species information (for a full description of the logistic sampling see Logistic sample .Rmd file in project folder)


emergence <- function(inds,midpoint=10,scale=11){
  rand<-runif(length(inds[,8]),0,1)
  timestep<- inds[,midpoint] - (log((1/rand)-1)/inds[,scale])
  return(round(timestep, digits=0))
}

poll[,8]<-emergence(poll)
plant[,8]<-emergence(plant)







# Activate individuals according to emergence ####

# Each individual will have an 'emergence' timestep as generated by the emergence function. At each timestep an individual's emergence will be checked against the current timestep. If equal then that individual becomes active and is first placed in the landscape then able to move (if pollinator). A 1 signifies it's ready to be placed ( has emerged onto the landscape). A 2 signifies it has already been placed but is still active (this stops the 'movement' function placing individuals on any timestep past emergence. An alternative would be to just not have individuals move when they first emerge)

activation<- function(inds, active = 2, emergence = 8){
  for(i in 1:dim(inds)[1]){ 
    if(inds[i, active] != 3){
      if(inds[i, emergence] == timestep){         # If timestep is equal to emergence
        inds[i, active] <- 1;    # Make individual active ('active' column = 1)
      }
      if(inds[i, emergence] < timestep){         # If timestep is greater than emergence
        inds[i, active] <- 2;    # Make individual active but not placed ('active' column = 2) 
      }
    }}
  return(inds)}




# Landscape placement and movement ####


# Placement function checks to see if individual is active then, if active, places them in the environment
# x is the size of the landscape x axis, y is the size of the landscape y axis


placementpoll <- function(poll, x = 20, y = 20, active = 2, x_loc = 6, y_loc = 7){ 
  for(i in 1:length(poll[, active])){
    if(poll[i, active] == 1){
      poll[i, x_loc]<-sample(x = 1:x, size = 1, replace = TRUE);
      poll[i, y_loc]<-sample(x = 1:y, size = 1, replace = TRUE);
    }
  }
  return(poll)
}

## Need a separate function for plants as multiple individuals can't occupy the same space. Therefore need to check whether there are any other plants there: if so, emerging plant dies 

placementplant <- function(plant, x = 20, y = 20, active = 2, dead = 3, x_loc = 6, y_loc = 7){ 
  for(i in 1:length(plant[,active])){
    if(plant[i, active] == 1){
      plant[i, x_loc]<-sample(x = 1:x, size = 1, replace = TRUE);
      plant[i, y_loc]<-sample(x = 1:y, size = 1, replace = TRUE);
      on_cell <- sum(plant[, x_loc] == plant[i, x_loc] & plant[, y_loc] == plant[i, y_loc]); # Check whether any other inds at plant emergence location
      if(on_cell > 1){
        plant[i, dead] <- 1 # If there are, emerging plant dies  
      }              
    }
  }
  return(plant)
}


movement <- function(inds, active = 2, x_loc = 6, y_loc = 7, xmax = 20, ymax = 20){
  distance <- c(-2,0,2); # Total movement range
  for(i in 1:dim(inds)[1]){
    if(inds[i, active] == 1 || inds[i, active] == 2){ # For every active individual move its xloc and yloc according to distance sample
      inds[i,x_loc] <- inds[i, x_loc] + sample(x = distance, size = 1);
      inds[i,y_loc] <- inds[i, y_loc] + sample(x = distance, size = 1);
    }
    if(inds[i,x_loc] > xmax){ # If individual has reached end of landscape then put it back to the start (torus landscape)
      inds[i,x_loc] <- inds[i,x_loc] - xmax;
    }
    if(inds[i,y_loc] > ymax){
      inds[i,y_loc] <- inds[i,y_loc] - ymax;
    }
    # Do you not need something like the below for the other edges?
    if(inds[i, x_loc] < 0){
      inds[i, x_loc] <- inds[i, x_loc] + xmax;
    }
    if(inds[i, y_loc] < 0){
      inds[i, y_loc] <- inds[i, y_loc] + ymax;
    }
  }
  return(inds)
}


# Pollinator interaction (feeding) ####

## Pollinator feeding function which:
# 1. Gets x/y loc for each pollinator (if active)
# 2. Checks there are flowers are at that x/y loc
# 3. Gets flowers species # and checks whether those flowers are species that pollinator can interact with
# 4. If yes then nothing happens (currently, might want to add the maturity ticker here)
# 5. If no, or if no flowers at x/y loc, then uptick hunger counter

feeding <- function(poll, plant, x_loc = 6, y_loc = 7, hunger = 4, dead = 3, species = 1, active = 2, ncol = 12){
  for(p in 1:length(poll[,1])){      
    xloc   <- poll[p, x_loc]; # Get poll locations
    yloc   <- poll[p, y_loc];
    flowers <- sum( plant[, x_loc] == xloc & plant[, y_loc] == yloc); # Total of flowers at that location
    if(poll[p, active] == 1 | poll[p, active] == 2){ # Check pollinator is active
      if(flowers > 0){ # Check there are flowers at that location 
        flowerinds <- which( plant[, x_loc] == xloc & plant[, y_loc] == yloc); # Get the flower individual at that location 
        speciesref <- plant[flowerinds, species] # Extract the species number of that flower 
        if(poll[p, (ncol+speciesref)[1]] == 1){ # Check that this flower species is one which pollinator interacts with  
          poll[p, hunger] <- 0 # If poll can interact with flower, hunger level resets to 0
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

# Plant interaction (pollination) #### 

## Function for plant/flower pollination which is broadly similar to pollinator feeding function in that it:
# 1. Gets x/y loc for each plant (if active)
# 2. Gets which pollinators are at that x/y loc
# 3. Check whether those pollinators are species that flower can interact with
# 4. If yes then increases 'pollination' measure by efficacy of visiting pollinators  
# 5. If no then nothing happens (could add maturity uptick here but might be best saving to next step)

pollination <- function(plant, poll, x_loc = 6, y_loc = 7, efficacy = 12, pollination = 4, dead = 3, species = 1, active = 2, ncol = 12){
  for(p in 1:length(plant[,1])){      
    xloc   <- plant[p, x_loc]; # Get plant locations
    yloc   <- plant[p, y_loc];
    polls <- sum( poll[, x_loc] == xloc & poll[, y_loc] == yloc); # Total of pollinators at that location
    if(plant[p, active] == 1 | plant[p, active] == 2){ # Check flower is active
      if(polls > 0){ # Check there are pollinators at that location 
        pollinds <- which(poll[, x_loc] == xloc & poll[, y_loc] == yloc); # Get the pollinator individual(s) at that location 
        for(i in pollinds){
          eff <- poll[i, efficacy]; # Pull efficacy values for those pollinators
          sp <- poll[i, species]; # Pull species numbers for those pollinators 
          if(plant[p, (ncol+sp)] == 1){ # Check that this pollinator species is one which pollinator interacts with 
            plant[p, pollination] <- plant[p, pollination] + eff # If flower can interact with pollinator add pollinator's efficacy to pollination column 
          } }}else {
            plant[p, pollination] <- plant[p, pollination]  # If pollinator not at location then pollination remains same
          }
    } else {
      plant[p, pollination] <- plant[p, pollination]  # If flower not active then pollination remains the same
    }}
  return(plant);
}


# Maturity counter 
# Pollinator maturation ####

## Maturation function for pollinators which:
#1. Checks pollinatior ind active
#2. Checks pollinator ind alive
#3. Checks hunger count - if threshold exceeded then dead
#4. Otherwise, upticks maturation count


pollmature <- function(poll, active = 2, dead = 3, hunger = 4, maturity = 5, threshold = 5){
  for(p in 1:length(poll[,1])){
    if (poll[p, active] == 1 | poll[p, active] == 2){ # Check if pollinator active
      if (poll[p, dead] != 1){ # Check if pollinator is dead
        if (poll[p, hunger] > threshold){ # Check hunger against threshold
          poll[p, dead] <- 1} else{ # If threshold exceeded, mark pollinator as dead
            poll[p,maturity] <- poll[p, maturity] + 1 } # Otherwise uptick pollinator maturity count
      }}} 
  return(poll) }

# Plant maturation ####

## Maturation function for plants which:
#1. Checks plant ind active
#2. Checks plant ind alive
#3. Upticks maturity count

# Currently this does not incorporate a check for whether a plant is fully-pollinated or not (which is the other condition that can trigger reproduction) because I have incorporated this into the plant reproduction function. This should be fine in terms of sequencing - it doesn't matter which of these criteria is reached first for reproduction and individuals will die after reproduction so should be picked up in the death check of this function at the next time step 


plantmature <- function(plant, active = 2, dead = 3, maturity = 5){
  for(p in 1:length(plant[,1])){
    if (plant[p, active] == 1 | plant[p, active] == 2){ # Check if plant active
      if (plant[p, dead] != 1){ # Check if plant is dead
        plant[p,maturity] <- plant[p, maturity] + 1 } # Otherwise uptick plant maturity count
    }} 
  return(plant) }

# Pollinator reproduction ####

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
  
  if(length(reproducers) > 0){
    poll[reproducers, dead] <- 1; # This should stick a 1 in the third column
  }                               # For all reproducers (if there are any)
  
  
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

# Plant reproduction #### 


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




# Run model ####

timestep<- 48;
time_steps<- 53;

plant[,8]<-48
poll[,8]<-48

## Load packages for IBM

library(dplyr)


## Set condition and season

condition <- TRUE

season <- 1

## Create initial species & population summary tables 

plantsummary <- plant %>% 
  group_by(species) %>%
  summarise(population = length(species),
            season = 0)

pollsummary <- poll %>% 
  group_by(species) %>%
  summarise(population = length(species),
            season = 0)


plantspeciesinfo <- plant %>% 
  distinct(species, .keep_all = TRUE) %>% 
  mutate(season = 0)

pollspeciesinfo <- poll %>% 
  distinct(species, .keep_all = TRUE) %>% 
  mutate(season = 0)


## Run IBM

while(season < 3){
  
  ## Set all individuals to 0 for activity (this will make dormant individuals from prior generations able to be picked up by functions)
  ## Reset timestep & conidtion 
  
  poll[,2] <- 0
  plant[,2] <- 0
  timestep <- 0
  condition <- TRUE
  
  while(condition == TRUE){
    
    if(length(poll[,1]) == 0) {
      stop("All pollinators dead")
    }
    
    if(length(plant[,1]) ==0) {
      stop("All plants dead")
    }
    
    poll     <- activation(poll);
    plant    <- activation(plant);
    poll     <- placementpoll(poll);
    plant    <- placementplant(plant);
    plant    <- plant[plant[, 3] == 0,]; # Remove dead plants (may happen as result of placement)
    poll     <- movement(poll);
    poll     <- feeding(poll = poll, plant = plant);
    plant    <- pollination(plant = plant, poll = poll);
    poll     <- poll[poll[, 3] == 0,]; # Some pollinators will have died as a result of feeding function so need to remove BEFORE reproducing
    poll     <- pollmature(poll = poll);
    plant    <- plantmature(plant = plant);
    poll     <- pollreproduction(poll = poll);
    plant    <- plantreproduction(plant = plant);
    poll     <- poll[poll[, 3] == 0,]
    plant    <- plant[plant[, 3] == 0,];
    timestep <- timestep +1
    
    plant.living <- length(which(plant[,2] == 1 | plant[,2] == 2 | plant[,2] == 0));
    poll.living <- length(which(poll[,2] == 1 | poll[,2] == 2 | poll[,2] == 0));
    inds.living <- sum(plant.living + poll.living);
    if (inds.living <= 0){ 
      condition <- FALSE
    }
    
  }
  
  ## Create population summary information for plants and pollinators  
  
  currentplantsummary <- plant %>% 
    group_by(species) %>%
    summarise(population = length(species),
              season = season)
  
  plantsummary <- bind_rows(plantsummary, currentplantsummary)
  
  
  currentpollsummary <- poll %>% 
    group_by(species) %>%
    summarise(population = length(species),
              season = season)
  
  pollsummary <- bind_rows(pollsummary, currentpollsummary)  
  
  ## Create species info summary for plants and pollinators 
  
  currentplantspeciesinfo <- plant %>% 
    distinct(species, .keep_all = TRUE) %>% 
    mutate(season = season)
  
  currentpollspeciesinfo <- poll %>% 
    distinct(species, .keep_all = TRUE) %>% 
    mutate(season = season)
  
  
  pollspeciesinfo <- bind_rows(pollspeciesinfo, currentpollspeciesinfo)
  plantspeciesinfo <- bind_rows(plantspeciesinfo, currentplantspeciesinfo)
  
  ## uptick season count 
  
  season <- season + 1
  
}
