
# First of all I need to make it so that pollinator individuals at a flower location can't all feed simultaneously - needs to be a maximum limit of individuals at a flower and then if that's exceeded randomly sample (say 100 from 1000 without replacement) 
# Determine plant placement at season start and trigger any landscape competition mortality at that point 

rm(list = ls())

library("tidyverse")

seed.reference <- 18

set.seed(18)

#### Set global parameters (as part of start-up outside model run) ####

# Number of pollinator species to create 

poll_species_number <- 30

# Number of plant species to create 

plant_species_number <- 20

# Number of individuals of each species to create

poll_individuals <- 150

plant_individuals <- 150

# Midpoint range - this defines the maximum and minimum values from which the midpoint parameter for each species will be drawn. The midpoint parameter feeds into the logistic curve equation which describes the phenology of a species. Broadly speaking the midpoint will determine the point in the logistic curve (in Julian days) when the rate of increase of the event is at its highest, although there is some interaction with other parameters in the model (notably scale) that can alter this. Therefore is roughly equivalent to the half-way point of the phenology event and is the Julian date at which species individuals are most likely to emerge.

# Currently both plant and pollinator species sample from the same range for midpoint

max_midpoint <- 150
min_midpoint <- 90


# Scale range - this defines the maximum and minimum values from which the scale parameter for each species will be drawn. The scale parameter feeds into the logistic curve equation which describes species' phenology. The scale parameter defines the rate of increase/decrease of the phenology event - therefore a higher scale parameter will result in a tighter spread of emergence dates for species' individuals while a lower scale will result in a more dispersed spread of dates. 

# Currently both plant and pollinator species sample from the same scale range 

max_scale <- 0.6
min_scale <- 0.3

# Skew range - this defines the maximum and minimum skew from which the skew parameter for each species will be drawn. The skew parameter affects the rate at which the lower symptote of the logistic curve reaches the midpoint. In practical terms, the more the value deviates from +/-1 the greater the skew. A value of 0-1 wil result in a greater to lesser left-skew to the distribution curve. A value of -1-0 will results in a greater to lesser right-skew. 

# Currently both plant and pollinator species sample from the same skew range 
# Size of skew and direction of skew are determined separately. The skew range values below determine the size of the skew. The direction of the skew is separately coded as a binomial sampling that results in skew being either positive or negative (somewhat counterintuitively, this is applied to the SCALE parameter rather than the skew parameter. See generalised logistic curve.Rmd file for details)

max_skew <- 2
min_skew <- 1

# Midpoint sensitivity range - this determines the max/min sensitivity from which the midpoint sensitivity will be drawn. This determines how sensitive the midpoint parameter is to shift (assumed to be caused by temperature but the driver is not explicit in the model - could be driven by any or a mixture of abiotic variables). There is no variability to the sensitivity - it is a continuous value applied each season (so a sensitivity of -2 will always deduct 2 from the species' midpoint parameter each season)

# Currently both plant and pollinator species sample from the same sensitivity range 

# This approach now defunct as using mean/sd taken from literature for expected phenological sensitivity
#max_midpoint_sensitivity <-  2
#min_midpoint_sensitivity <- -2

poll_midpoint_sensitivity_mean <- -3.729871
poll_midpoint_sensitivity_sd <- 6.856

plant_midpoint_sensitivity_mean <- -4
plant_midpoint_sensitivity_sd <- 4



# Scale sensitivity range - this determines the max/min sensitivity from which the scale parameter sensitivity will be drawn. This determines how sensitive the scale parameter is to changing environmental drivers (assumed to be caused by temperature but the driver is not explicit in the model - could be driven by any or a mixture of abiotic variables). There is no variability to the sensitivity - it is a continuous value applied each season (so a sensitivity of -0.02 will always deduct 0.02 from the species' scale parameter each season)

# Currently both plant and pollinator species sample from the same sensitivity range

max_scale_sensitivity <-  0.01
min_scale_sensitivity <- -0.005

max_skew_sensitivity <- 0.04
min_skew_sensitivity <- -0.01

# Efficacy range - this determines pollinator efficacy. Pollinator efficacy influences plant reproductive success (which is continuous - a flower which matures and is only partially pollinated will produce a lower number of offspring than a fully-pollinated flower). It also introduces an element of resource competition between pollinators as a fully-pollinated flower will mature and set seed, removing it as a food source for other pollinators. 

# Only pollinator species use efficacy range

max_efficacy <- 0.1
min_efficacy <- 0.01

## Generalism likelihood - this determines whether a plant or insect species is generalist or specialist. Depending on this, number of interaction partners are drawn from one of two Poisson distributions with lamda set to either higher or lower value

# Used by both plants and pollinators 

generalism_likelihood <- 0.3

## Generalist/specialist lambda - this determines the lambda for the Poisson distribution from which the number of interactions partners a species has will be drawn. This should be set in reference to the number of plant/pollinator species in the model. The values for specialists/generalists should also slightly exceed those in published research as in this simulation they represent the number of potential interaction partners a species has rather than the realised number (i.e. species can potentially interact with species that, at simulation start, they do not spatially or temporally co-occur with))

generalist.lambda <- 12
specialist.lambda <- 3


# Pollinator lifespan - this determines the lambda value for the Poisson distribution from which pollinator species lifespan will be drawn (in days)

poll_lifespan <- 35

# Plant lifespan - this determines the lambda value for the Poisson distribution from which plant species blooming period will be drawn (in days)

plant_lifespan <- 30

# Names of columns in pollinator species array (should be all traits that are species-specific. Currently 11 columns - species number, number of interaction partners, phenological model parameters (midpoint, scale, skew), parameter sensitivity (midpoint/scale), pollinator efficacy, generalist)

# Various functions refer to the relevant column in individual/species arrays by name in order to pull the column number that refers to (i.e. speciesid is column 1). Those references are also defined here and should be consistent with the column names sequence. References should be the same for plants and pollinators


pollspeciesnames <- c("speciesid","interactions","midpoint","scale", "skew", "midpoint.sensitivity", "scale.sensitivity","skew.sensitivity", "generalism", "lifespan", "efficacy", c(1:plant_species_number))


# Names of columns in plant species array (should be all traits that are species-specific. Currently 11 columns (although 1 unused) - species number, number of interaction partners, phenological model parameters (midpoint, scale, skew), parameter sensitivity). This should be kept equal to the number of named columns in the poll array for referencing of common functions. Currently this requires one 'blank' column at the end. 

plantspeciesnames <- c("speciesid","interactions","midpoint","scale", "skew", "midpoint.sensitivity", "scale.sensitivity","skew.sensitivity", "generalism", "lifespan", "seedlings", c(1:poll_species_number))

# Various functions refer to the relevant column in individual/species arrays by name in order to pull the column number that refers to (i.e. speciesid is column 1). Those references are also defined here and should be consistent with the column names sequence order. References should be the same for plants and pollinators

speciesid <- 1
interactions <- 2
midpoint <- 3
scale <- 4
skew <- 5
midpoint.sensitivity <- 6
scale.sensitivity <- 7
skew.sensitivity <- 8
generalism <- 9
lifespan <- 10
efficacy <- 11
seedlings <- 11

# Number of species columns, used in building arrays. Should be equal to the number of named columns in pollspeciesnames and plantspeciesnames (see above)

species_cols <- 11

# Names of columns in pollinator and plant individual arrays, for individual traits such as emergence, active state, location etc. Common between plants and pollinators with the exception of column 4 (hunger in pollinators and pollinated in plants)

pollindsnames <- c("speciesid","active","dead", "hunger", "maturity", "x_loc", "y_loc", "emergence")
plantindsnames <- c("speciesid","active","dead", "pollinated", "maturity", "x_loc", "y_loc", "emergence")

# Various functions refer to the relevant column in individual arrays by name to pull the column number. References are defined here and should be consistent with above column names. Only point of difference between plants/polls is column 4

speciesid <- 1
active <- 2
dead <- 3
hunger <- 4
pollinated <- 4
maturity <- 5
x_loc <- 6
y_loc <- 7
emergence <- 8

# Number of columns for individual traits, used in building arrays. Should be equal to the number of named columns in pollindssnames and plantindsnames (see above)

inds_cols <- 8

# Once the species and individual arrays are merged some of the species traits columns will have changed. The species id columns merge and then all the species traits will be r-joined to the individual traits so require new references

inds.interactions <- interactions + inds_cols - 1
inds.midpoint <- midpoint + inds_cols - 1
inds.scale <- scale + inds_cols - 1
inds.skew <- skew + inds_cols - 1
inds.midpoint.sensitivity <- midpoint.sensitivity+ inds_cols - 1
inds.scale.sensitivity <- scale.sensitivity + inds_cols - 1
inds.skew.sensitivity <- skew.sensitivity + inds_cols -1
inds.generalism <- generalism + inds_cols - 1
inds.lifespan <- lifespan + inds_cols - 1
inds.efficacy <- efficacy + inds_cols - 1
inds.seedlings <- seedlings + inds_cols -1

total_inds_cols <- species_cols + inds_cols -1

## Landscape & movement parameters
# These are used in the placement and movement functions. When individuals emerge onto the landscape they are placed at a cell (x/y location) and pollinators then move around each timestep. Pollinators can occupy the same cell, plants can't (only one survives if >1 on a cell, chosen at random)

# X and Y limits for landscape (currently set at 70*70 for a landscape of 4900 cells for starting population of 3,000)

xmax <- 40
ymax <- 40

# Dispersal distance in cells for pollinator offspring from parent start location

poll_dispersal <- 4

# Dispersal distance in cells for plant offspring from parent start location  

plant_dispersal <- 15

# Movement distance for pollinator individuals. Movement distance is not fixed - rather, poll_movement sets the sample range from which movement distance will be sampled at each timestep (movement = sample(-poll_movement:poll_movement))

poll_movement <- 5

# Hunger threshold of pollinators. Used in both pollinator feeding and maturity functions. Each timestep active pollinators are checked against this - if they can feed, it reverts to 0. If they can't feed, hunger column gets upticked. If it exceeds threshold, individual dies. Currently constant for all species/individuals 

poll_hunger <- 12

# Maximum number of pollinators that can feed from the same flower in one timestep

pollfeeders <- 1

# Number of offspring produced by pollinators once sexual maturity reached, used in pollinator reproduction function 

poll_offspring <- 2

# Number of seeds produced by each flower once fully pollinated or lifespan reached

plant_offspring <- 6

##### Climate variable parameters ####

## Annual mean temperature increase taken from UK climate projections with sd taken from residual variance of lm fitted to 1959-2016 temperature data

mean.annual.increase <- 0.078
sd.annual.increase <- 0.9164



#### Pollinator species creation function ####

# All parameters named in the function should be defined in the above lines of code under 'set species parameters' heading. Parameters are currently sampled from a uniform distribution from min parameter value to max parameter value with two exceptions: 1/ Generalism/specialism is sampled from a binomial distribution 2/ Lifespan is sampled from a Poisson distribution 




make_poll_species<- function(poll_species_number    , 
                             plant_species_number   , 
                             species_cols       ,
                             pollspeciesnames   ,
                             max_midpoint       , 
                             min_midpoint       ,
                             max_scale          ,
                             min_scale          ,
                             max_efficacy       ,
                             min_efficacy       ,
                             min_skew           ,
                             max_skew           ,
                             min_skew_sensitivity ,
                             max_skew_sensitivity ,
                             poll_midpoint_sensitivity_sd,
                             poll_midpoint_sensitivity_mean,
                             min_scale_sensitivity    ,
                             max_scale_sensitivity    ,
                             generalism_likelihood,
                             poll_lifespan
                             
                             
){
  poll_species <- array(data=0, dim=c(poll_species_number,(species_cols + plant_species_number)));
  colnames(poll_species) <- pollspeciesnames;
  poll_species[,1] <- seq_len(nrow(poll_species));
  poll_species[,2] <- sample(1:plant_species_number, dim(poll_species)[1], replace = T);
  poll_species[,3] <- runif( dim(poll_species)[1], min_midpoint, max_midpoint);
  direction <- sample(c(-1,1), length(poll_species[,4]), replace = TRUE);# This bit of code seeds a random -1 or 1 in order to determine whether the scale parameter is positive or negative
  poll_species[,4] <- runif( dim(poll_species)[1], min_scale, max_scale);
  poll_species[,4] <- poll_species[,4] * direction;
  poll_species[,5] <- runif( dim(poll_species)[1], min_skew, max_skew);
  poll_species[,6] <- rnorm( dim(poll_species)[1], poll_midpoint_sensitivity_mean, poll_midpoint_sensitivity_sd);
  poll_species[,7] <- runif( dim(poll_species)[1], min_scale_sensitivity, max_scale_sensitivity);
  poll_species[,8] <- runif(dim(poll_species)[1], min_skew_sensitivity, max_skew_sensitivity);
  poll_species[,9] <- rbinom(n = dim(poll_species)[1], size = 1, prob = generalism_likelihood);
  poll_species[,10] <- rpois(n = dim(poll_species)[1], lambda = poll_lifespan);
  poll_species[,11] <- runif( dim(poll_species)[1], min_efficacy, max_efficacy);
  return(poll_species)
}



#### Create list of poll species and trait values using make_poll_species function (as part of start-up outside model run) ####

pollspeciestraits <- make_poll_species( poll_species_number = poll_species_number    , 
                                        plant_species_number = plant_species_number   , 
                                        species_cols = species_cols ,
                                        pollspeciesnames = pollspeciesnames  ,
                                        max_midpoint = max_midpoint       , 
                                        min_midpoint = min_midpoint       ,
                                        max_scale = max_scale          ,
                                        min_scale = min_scale          ,
                                        max_efficacy = max_efficacy       ,
                                        min_efficacy = min_efficacy       ,
                                        min_skew = min_skew           ,
                                        max_skew = max_skew           ,
                                        poll_midpoint_sensitivity_mean = poll_midpoint_sensitivity_mean,
                                        poll_midpoint_sensitivity_sd = poll_midpoint_sensitivity_sd ,
                                        min_scale_sensitivity = min_scale_sensitivity    ,
                                        max_scale_sensitivity = max_scale_sensitivity    ,
                                        min_skew_sensitivity = min_skew_sensitivity    ,
                                        max_skew_sensitivity = max_skew_sensitivity    ,
                                        generalism_likelihood = generalism_likelihood,
                                        poll_lifespan = poll_lifespan)

##### Plant species creation function ####

# All parameters named in the function should be defined in the above lines of code under 'set species parameters' heading. Parameters are currently sampled from a uniform distribution from min parameter value to max parameter value with two exceptions: 1/ Generalism/specialism is sampled from a binomial distribution 2/ Lifespan is sampled from a Poisson distribution 




make_plant_species<- function(plant_species_number    , 
                              poll_species_number   , 
                              species_cols       ,
                              plantspeciesnames   ,
                              max_midpoint       , 
                              min_midpoint       ,
                              max_scale          ,
                              min_scale          ,
                              min_skew           ,
                              max_skew           ,
                              plant_midpoint_sensitivity_mean ,
                              plant_midpoint_sensitivity_sd ,
                              min_skew_sensitivity,
                              max_skew_sensitivity,
                              min_scale_sensitivity    ,
                              max_scale_sensitivity    ,
                              generalism_likelihood,
                              plant_lifespan
                              
                              
){
  plant_species <- array(data=0, dim=c(plant_species_number,(species_cols + poll_species_number)));
  colnames(plant_species) <- plantspeciesnames;
  plant_species[,1] <- seq_len(nrow(plant_species));
  plant_species[,2] <- sample(1:poll_species_number, dim(plant_species)[1], replace = T);
  plant_species[,3] <- runif( dim(plant_species)[1], min_midpoint, max_midpoint);
  direction <- sample(c(-1,1), length(plant_species[,4]), replace = TRUE);# This bit of code seeds a random -1 or 1 in order to determine whether the scale parameter is positive or negative
  plant_species[,4] <- runif( dim(plant_species)[1], min_scale, max_scale);
  plant_species[,4] <- plant_species[,4] * direction;
  plant_species[,5] <- runif( dim(plant_species)[1], min_skew, max_skew);
  plant_species[,6] <- rnorm( dim(plant_species)[1], plant_midpoint_sensitivity_mean, plant_midpoint_sensitivity_sd);
  plant_species[,7] <- runif( dim(plant_species)[1], min_scale_sensitivity, max_scale_sensitivity);
  plant_species[,8] <- runif( dim(plant_species)[1], min_skew_sensitivity, max_skew_sensitivity);
  plant_species[,9] <- rbinom(n = dim(plant_species)[1], size = 1, prob = generalism_likelihood);
  plant_species[,10] <- rpois(n = dim(plant_species)[1], lambda = plant_lifespan);
  return(plant_species)
}


#### Create list of plant species and trait values using make_plant_species function (as part of start-up outside model run) ####

plantspeciestraits <- make_plant_species( poll_species_number = poll_species_number    , 
                                          plant_species_number = plant_species_number   , 
                                          species_cols = species_cols ,
                                          plantspeciesnames = plantspeciesnames  ,
                                          max_midpoint = max_midpoint       , 
                                          min_midpoint = min_midpoint       ,
                                          max_scale = max_scale          ,
                                          min_scale = min_scale          ,
                                          min_skew = min_skew           ,
                                          max_skew = max_skew           ,
                                          plant_midpoint_sensitivity_mean = plant_midpoint_sensitivity_mean,
                                          plant_midpoint_sensitivity_sd = plant_midpoint_sensitivity_sd ,
                                          min_scale_sensitivity = min_scale_sensitivity    ,
                                          max_scale_sensitivity = max_scale_sensitivity    ,
                                          min_skew_sensitivity = min_skew_sensitivity    ,
                                          max_skew_sensitivity = max_skew_sensitivity    ,
                                          generalism_likelihood = generalism_likelihood,
                                          plant_lifespan = plant_lifespan)





#### Interaction count function ####

# This generates a number of interactions for a species from a Poisson distribution. The lambda of the distribution (defined under global parameters) is fixed to a lower value for specialists and a higher value for generalists

interaction_count <- function(species, interactions, generalism, generalist.lambda, specialist.lambda){
  for(i in 1:length(species[,1])){
    if (species[i, generalism] == 1){
      species[i, interactions] <- rpois(1, lambda = generalist.lambda)
    }
    else{
      species[i, interactions] <- rpois(1, lambda = specialist.lambda)
    }
  }
  return(species)}

#### Apply interaction count function to pollinator and plant species arrays (as part of start-up outside model run) ####

# Apply to pollinators

pollspeciestraits <- interaction_count(species = pollspeciestraits,
                                       interactions = interactions,
                                       generalism = generalism,
                                       generalist.lambda = generalist.lambda,
                                       specialist.lambda = specialist.lambda
)


## This additional bit of code necessary for limiting range of interaction counts generated. If 0, fixes to 1. If greater than the number of species available to interact with, fixes to the maximum

pollspeciestraits[,interactions][pollspeciestraits[,interactions]<1] <- 1
pollspeciestraits[,interactions][pollspeciestraits[,interactions]>plant_species_number] <- plant_species_number

# Apply to plants

plantspeciestraits <- interaction_count(species = plantspeciestraits,
                                        interactions = interactions,
                                        generalism = generalism,
                                        generalist.lambda = generalist.lambda,
                                        specialist.lambda = specialist.lambda
)


## This additional bit of code necessary for limiting range of interaction counts generated. If 0, fixes to 1. If greater than the number of species available to interact with, fixes to the maximum

plantspeciestraits[,interactions][plantspeciestraits[,interactions]<1] <- 1
plantspeciestraits[,interactions][plantspeciestraits[,interactions]>poll_species_number] <- poll_species_number

#### Interaction partners function ####

# Once a number of interactions has been generated for each species, this function randomly selects which other are interaction partners and adds it to the species traits array by representing interactions partners with a 1 in the relevant column

int_partners<-function(species, interactions, int_species, cols){ 
  
  for (i in 1:dim(species)[1]){
    n <- species[i,interactions];
    ints <- (sample(1:int_species, n, replace = F));
    species[i,(cols+ints)] <- 1
  }
  return(species)
}

#### Apply interaction partners function to pollinator/plant species arrays (as part of start-up outside model run) ####

pollspeciestraits <- int_partners(species = pollspeciestraits,
                                  interactions = interactions,
                                  int_species = plant_species_number,
                                  cols = species_cols)

plantspeciestraits <- int_partners(species = plantspeciestraits,
                                   interactions = interactions,
                                   int_species = poll_species_number,
                                   cols = species_cols)


#### Function for generating array of individuals based on species traits ####

## Create a function for pollinator individuals array


make_poll_individuals<- function(species_number,
                                 cols,
                                 individuals
){
  rows=species_number*individuals
  poll<-array(data=0, dim=c((species_number * individuals),cols));
  colnames(poll)<- pollindsnames;
  poll[,1]<-rep(1:species_number, each= individuals, length.out=rows)
  return(poll)
}

## Create a function for plant individuals array

make_plant_individuals<- function(species_number,
                                  cols,
                                  individuals
){
  rows=species_number*individuals
  plant<-array(data=0, dim=c((species_number * individuals),cols));
  colnames(plant) <- plantindsnames;
  plant[,1]<-rep(1:species_number, each= individuals , length.out=rows)
  return(plant)
}

#### Create plant and pollinator individual arrays (as part of start-up outside model run) ####

# Create arrays with individual traits 

pollinds <- make_poll_individuals(species_number = poll_species_number,
                                  cols = inds_cols,
                                  individuals = poll_individuals)

plantinds <- make_plant_individuals(species_number = plant_species_number,
                                    cols = inds_cols,
                                    individuals = plant_individuals)

# Merge individual trait arrays with species info (this turns it into a df)

pollinds  <- merge(pollinds, pollspeciestraits, by = "speciesid")
plantinds <- merge(plantinds, plantspeciestraits, by = "speciesid")


#### Function for individual emergence according to phenological traits ####

# Emergence function generates a random number seed between 0 and 1 for each individual and then back-calculates an emergence date from a logistic equation using the midpoint, scale and skew values pulled from species information (for a full description of the logistic sampling see Logistic sample .Rmd and generalised logistic curve.Rmd files in project folder)


emergence.function <- function(inds){
  
  for (i in 1:dim(inds)[1]){
    rand <- runif(1, 0, 1);
    timestep <- inds[i, inds.midpoint] - (log(((1/rand) ^ inds[i, inds.skew]) - 1)/inds[i, inds.scale]);
    inds[i, emergence] <- (round(timestep, digits=0))
  }
  return(inds)
}

#### Activation function ####

# Each individual will have an 'emergence' timestep as generated by the emergence function. At each timestep an individual's emergence will be checked against the current timestep. If equal then that individual becomes active and is first placed in the landscape then able to move (if pollinator). A 1 signifies it's ready to be placed ( has emerged onto the landscape). A 2 signifies it has already been placed but is still active (this stops the 'movement' function placing individuals on any timestep past emergence. An alternative would be to just not have individuals move when they first emerge)

activation<- function(inds, a.active = active, a.emergence = emergence){
  for(i in 1:dim(inds)[1]){ 
    if(inds[i, a.active] != 3){
      if(inds[i, a.emergence] == timestep){         # If timestep is equal to emergence
        inds[i, a.active] <- 2;    # Make individual active ('active' column = 2)
      }
    }}
  return(inds)}

#### Placement function  #### 


# Placement function checks to see if individual is active then, if active, places them in the environment. Initial placement is at random. Placement of subsequent generations is done by taking parents x/y coordiantes then randomly sampling from a dispersal range and adding the result to existing coordiantes. The check for this is simply whether the season is equal to 1 (as in any seasons subsequent to the first any emerging individuals will be offspring of the previous season's population)
# x is the size of the landscape x axis, y is the size of the landscape y axis



placementpoll <- function(poll, p.xmax = xmax, p.ymax = ymax, p.active = active, p.x_loc = x_loc, p.y_loc = y_loc, p.poll_dispersal = poll_dispersal, p.season = season){ 
  for(i in 1:length(poll[, p.active])){
    if(poll[i, p.active] == 0){ # Check for what p.season it is - if 1 then just place on landscape
      if( p.season == 0){
        poll[i, p.x_loc]<-sample(x = 1:p.xmax, size = 1, replace = TRUE);
        poll[i, p.y_loc]<-sample(x = 1:p.ymax, size = 1, replace = TRUE);
      } else # If p.season >0 then disperse according to parent's last coordinates
        poll[i, p.x_loc]<-poll[i, p.x_loc] + sample(x = -(p.poll_dispersal):p.poll_dispersal, size = 1);
      poll[i, p.y_loc]<-poll[i, p.y_loc] + sample(x = -(p.poll_dispersal):p.poll_dispersal, size = 1);
    }}
  if(poll[i,p.x_loc] > p.xmax){ # If individual has reached end of landscape then put it back to the start (torus landscape)
    poll[i,p.x_loc] <- poll[i,p.x_loc] - p.xmax;
  }
  if(poll[i,p.y_loc] > p.ymax){
    poll[i,p.y_loc] <- poll[i,p.y_loc] - p.ymax;
  }
  # Same for other edges 
  if(poll[i, p.x_loc] < 1){
    poll[i, p.x_loc] <- poll[i, p.x_loc] + p.xmax;
  }
  if(poll[i, p.y_loc] < 1){
    poll[i, p.y_loc] <- poll[i, p.y_loc] + p.ymax;
  }
  return(poll)
}


## Need a separate function for plants as multiple individuals can't occupy the same space. Therefore need to check whether there are any other plants there: if so, emerging plant dies 
## First function generates x/y locations for all plants at season start

placementplant <- function(plant, p.xmax = xmax, p.ymax = ymax, p.active = active, p.dead = dead, p.x_loc = x_loc, p.y_loc = y_loc, p.plant_dispersal = plant_dispersal, p.season = season){ 
  for(i in 1:length(plant[,p.active])){
    if(plant[i, p.active] == 0){
      if( p.season == 0){
        plant[i, p.x_loc]<-sample(x = 1:p.xmax, size = 1, replace = TRUE);
        plant[i, p.y_loc]<-sample(x = 1:p.ymax, size = 1, replace = TRUE);
      } else
        plant[i, p.x_loc] <- plant[i, p.x_loc] + sample(x = -p.plant_dispersal:p.plant_dispersal, size = 1, replace = TRUE);
      plant[i, p.y_loc] <- plant[i, p.y_loc] + sample(x = -p.plant_dispersal:p.plant_dispersal, size = 1, replace = TRUE);
      if(plant[i,p.x_loc] > p.xmax){ # If individual has reached end of landscape then put it back to the start (torus landscape)
        plant[i,p.x_loc] <- plant[i,p.x_loc] - p.xmax;
      }
      if(plant[i,p.y_loc] > p.ymax){
        plant[i,p.y_loc] <- plant[i,p.y_loc] - p.ymax;
      }
      # Same for other edges 
      if(plant[i, p.x_loc] < 1){
        plant[i, p.x_loc] <- plant[i, p.x_loc] + p.xmax;
      }
      if(plant[i, p.y_loc] < 1){
        plant[i, p.y_loc] <- plant[i, p.y_loc] + p.ymax;
      }
      on_cell <- sum(plant[, p.x_loc] == plant[i, p.x_loc] & plant[, p.y_loc] == plant[i, p.y_loc] & (plant[, p.active] == 1 | plant[,p.active] == 2)); # Check whether any other inds at plant emergence location
    }
  }
  return(plant)
}

## Second function checks all x/y cells and if >1 plant individual on location, culls all but 1

competitionplant <- function(plant){
  
  for(i in 0:xmax){
    for(j in 0:ymax){
      count_plant <- sum(plant[,x_loc] == i & plant[,y_loc] == j);
      if(count_plant > 1){
        temp_plant <- which(plant[,x_loc] == i & plant[,y_loc] == j);
        plant[temp_plant, dead] <- 1;
        survivor <- sample(temp_plant, 1);
        plant[survivor, dead] <- 0
        
      }
    }
  }
  return(plant)
}

#### Movement function ####

# Moves each active pollinator (active state 1) a random number of x/y co-ordinates (sampled from poll_movement parameter) across a torus landscape 

movement <- function(inds, m.active = active, m.x_loc = x_loc, m.y_loc = y_loc, m.xmax = xmax, m.ymax = ymax, m.distance = poll_movement){
  for(i in 1:dim(inds)[1]){
    if(inds[i, m.active] == 1 | inds[i, m.active] == 2){ # For every active individual move its xloc and yloc according to m.distance sample
      inds[i,m.x_loc] <- inds[i, m.x_loc] + sample(x = -m.distance:m.distance, size = 1);
      inds[i,m.y_loc] <- inds[i, m.y_loc] + sample(x = -m.distance:m.distance, size = 1);
    }
    if(inds[i,m.x_loc] > m.xmax){ # If individual has reached end of landscape then put it back to the start (torus landscape)
      inds[i,m.x_loc] <- inds[i,m.x_loc] - m.xmax;
    }
    if(inds[i,m.y_loc] > m.ymax){
      inds[i,m.y_loc] <- inds[i,m.y_loc] - m.ymax;
    }
    # Do you not need something like the below for the other edges?
    if(inds[i, m.x_loc] < 1){
      inds[i, m.x_loc] <- inds[i, m.x_loc] + m.xmax;
    }
    if(inds[i, m.y_loc] < 1){
      inds[i, m.y_loc] <- inds[i, m.y_loc] + m.ymax;
    }
  }
  return(inds)
}

#### Pollinator feeding function ####

## Pollinator feeding function which:
# 1. Gets x/y loc for each pollinator (if active state = 1)
# 2. Checks there are flowers are at that x/y loc
# 3. Gets flowers species # and checks whether those flowers are species that pollinator can interact with
# 4. If yes then nothing happens (currently, might want to add the maturity ticker here)
# 5. If no, or if no flowers at x/y loc, then uptick hunger counter

feeding <- function(poll, plant, f.x_loc = x_loc, f.y_loc = y_loc, f.hunger = hunger,  f.species = speciesid, f.active = active, f.ncol = total_inds_cols){
  
  ## BD: Here's a trick to try to do this efficiency with minimual disruption
  count_fed <- matrix(data = 0, nrow = ymax, ncol = xmax); 
  
  ## Insert this to scramble order in which loop processes pollinator inds (so that feeding preference isn't given to first lines of inds array)
  scrambled_polls <- sample(x = 1:length(poll[,1]), size = length(poll[,1]),
                            replace = FALSE);
  for(p in scrambled_polls){      
    xloc   <- poll[p, f.x_loc]; # Get poll locations
    yloc   <- poll[p, f.y_loc];
    flowers <- sum( plant[, f.x_loc] == xloc & plant[, f.y_loc] == yloc); # Total of flowers at that location
    if(poll[p, f.active] == 1 | poll[p, f.active] == 2){ # Check pollinator is f.active
      poll[p, f.hunger] <- poll[p, f.hunger] +1;
      if(flowers > 0){ # Check there are flowers at that location 
        flowerinds <- which( plant[, f.x_loc] == xloc & plant[, f.y_loc] == yloc); # Get the flower individual at that location 
        f.speciesref <- plant[flowerinds, f.species] # Extract the f.species number of that flower 
        if(poll[p, (f.ncol+f.speciesref)[1]] == 1){ # Check that this flower f.species is one which pollinator interacts with  
          
          count_fed[xloc, yloc] <- count_fed[xloc, yloc] + 1; # Increase count
          ## The new 'if' below checks the count location. If more than 10 have
          ## fed, then the pollinator gets blocked from doing anything now
          ## I think that you will probably need an 'else' here to increase
          ## hunger if it goes unfed? Also, you will want to replace the 10 hard
          ## coded with a variable that goes in the feeding function
          ## The variable will specify the maximum number of pollinating feeders
          if(count_fed[xloc, yloc] <= pollfeeders){
            poll[p, f.hunger] <- 0 # If poll can interact with flower, f.hunger level resets to 0
          }}}
    }}
  return(poll);
}

#### Plant pollination function ####

## Function for plant/flower pollination which is broadly similar to pollinator feeding function in that it:
# 1. Gets x/y loc for each plant (if active)
# 2. Gets which pollinators are at that x/y loc
# 3. Check whether those pollinators are species that flower can interact with
# 4. If yes then increases 'pollination' measure by efficacy of visiting pollinators  
# 5. If no then nothing happens (could add maturity uptick here but might be best saving to next step)

p.pollination <- function(plant, poll, p.x_loc = x_loc, p.y_loc = y_loc, p.efficacy = inds.efficacy, p.pollination = pollinated, p.dead = dead, p.species = speciesid, p.active = active, ncol = total_inds_cols){
  for(p in 1:length(plant[,1])){      
    xloc   <- plant[p, p.x_loc]; # Get plant locations
    yloc   <- plant[p, p.y_loc];
    polls <- sum( poll[, p.x_loc] == xloc & poll[, p.y_loc] == yloc); # Total of pollinators at that location
    if(plant[p, p.active] == 1 | plant[p, p.active] == 2){ # Check flower is p.active
      if(polls > 0){ # Check there are pollinators at that location 
        pollinds <- which(poll[, p.x_loc] == xloc & poll[, p.y_loc] == yloc); # Get the pollinator individual(s) at that location 
        for(i in pollinds){
          eff <- poll[i, p.efficacy]; # Pull p.efficacy values for those pollinators
          sp <- poll[i, p.species]; # Pull p.species numbers for those pollinators 
          if(plant[p, (ncol+sp)] == 1){ # Check that this pollinator p.species is one which pollinator interacts with 
            plant[p, p.pollination] <- plant[p, p.pollination] + eff # If flower can interact with pollinator add pollinator's p.efficacy to p.pollination column 
          } }}else {
            plant[p, p.pollination] <- plant[p, p.pollination]  # If pollinator not at location then p.pollination remains same
          }
    } else {
      plant[p, p.pollination] <- plant[p, p.pollination]  # If flower not p.active then p.pollination remains the same
    }}
  return(plant);
}

#### Pollinator maturation function ####

## Maturation function for pollinators which:
#1. Checks pollinatior ind active
#2. Checks pollinator ind alive
#3. Checks hunger count - if threshold exceeded then dead
#4. Otherwise, upticks maturation count


pollmature <- function(poll, m.active = active, m.dead = dead, m.hunger = hunger, m.maturity = maturity, m.hunger.threshold = poll_hunger ){
  for(p in 1:length(poll[,1])){
    if (poll[p, m.active] == 1 | poll[p, m.active] == 2){ # Check if pollinator active
      if (poll[p, m.dead] != 1){ # Check if pollinator is dead
        if (poll[p, m.hunger] > m.hunger.threshold){ # Check hunger against threshold
          poll[p, m.dead] <- 1} else{ # If threshold exceeded, mark pollinator as m.dead
            poll[p,m.maturity] <- poll[p, m.maturity] + 1 } # Otherwise uptick pollinator maturity count
      }}} 
  return(poll) }

#### Plant maturation ####

## Maturation function for plants which:
#1. Checks plant ind active
#2. Checks plant ind alive
#3. Upticks maturity count

# Currently this does not incorporate a check for whether a plant is fully-pollinated or not (which is the other condition that can trigger reproduction) because I have incorporated this into the plant reproduction function. This should be fine in terms of sequencing - it doesn't matter which of these criteria is reached first for reproduction and individuals will die after reproduction so should be picked up in the death check of this function at the next time step 


plantmature <- function(plant, m.active = 2, m.dead = 3, m.maturity = 5){
  for(p in 1:length(plant[,1])){
    if (plant[p, m.active] == 1 | plant[p, m.active] == 2){ # Check if plant m.active
      if (plant[p, m.dead] != 1){ # Check if plant is m.dead
        plant[p,m.maturity] <- plant[p, m.maturity] + 1 } # Otherwise uptick plant m.maturity count
    }} 
  return(plant) }



#### Pollinator reproduction ####

# Reproduction function for pollinators which:
# 1. Creates a list of pollinator inds which are active (state 1 or 2) and have reached reproductive maturity (defined by maturity reaching  repro.threshold value)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number (currently a set number  of offspring but would be straightforward to make this a sample from a distribution)
# 3. Tidies up column information for offspring (makes sure dead, hunger, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing pollinator array
# 5. Makes reproducing individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway



pollreproduction <- function(poll, pr.active = active, pr.dead = dead, pr.hunger = hunger, pr.maturity = maturity, pr.emergence = emergence, repro.threshold = inds.lifespan, offspring = poll_offspring){
  
  reproducers <- which(poll[,pr.maturity] >= poll[, repro.threshold] & 
                         (poll[,pr.active] == 1 | poll[,pr.active] == 2)); # Extract all reproducing pollinator inidividuals
  
  if(length(reproducers) > 0){
    poll[reproducers, pr.dead] <- 1; # This should stick a 1 in the third column
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
    offspring_parents <- rep(x = reproducers, times = offspring);
    # Can `sort` the above, but it's really not necessary, I don't think
    # Now we have the IDs of the parents of each offspring
    # Could also substitute `times = 3` with a vector of length(reproducers)
    
    act_row <- num_old_poll + 1;
    end_row <- dim(new_polls)[1];
    
    while(act_row <= end_row){
      # A couple subsets, but just replacing the offspring row with the row
      # indicated by offspring_parents[act_row];
      new_polls[act_row, ]          <- poll[ offspring_parents[(act_row-num_old_poll)] , ];
      new_polls[act_row, pr.active]    <- 3;
      new_polls[act_row, pr.dead]      <- 0;
      new_polls[act_row, pr.hunger]    <- 0;
      new_polls[act_row, pr.maturity]  <- 0;
      new_polls[act_row, pr.emergence] <- 0;
      act_row                       <- act_row + 1; # Super important
    } # It looks longer, but we've avoided the rbind
    
    colnames(new_polls) <- colnames(poll);
  }else{ # Else just replace with the original polls
    new_polls <- poll;
  }
  return(new_polls)
}

#### Plant reproduction #### 


# Reproduction function for plants, differs from pollinators in that it needs to check pollination status and apply it to offspring number, otherwise similar. Function:
# 1. Creates a list of plant inds which are active (state 1 or 2) and have either 1/ reached maturity or 2/ reached full pollination (pollination column >=1)
# 2. Loops through each reproducing individual and replicates their row information based on offspring number
# In plants, offspring calculated by multiplying offspring number by pollination proportion (rounding to nearest whole number). If pollination proportion >1, use 1. 
# 3. Tidies up column information for offspring (makes sure dead, pollination, maturity and emergence columns are all set to 0, makes sure active state is set to 3 which makes them dormant for current season)
# 4. Binds new offspring rows to existing plant array
# 5. Makes reproducing/fully mature individuals dead (set dead column to 1)

## Offspring are currently identical to their parents. Could add variance to this but phenological traits and emergence will be adjusted and recalculated in new season anyway



plantreproduction <- function(plant){
  
  # Check which individuals are active (state 1 or 2) and ready to reproduce (fully matured OR fully pollinated)
  
  reproducers <- which((plant[,active] == 1 | plant[,active] == 2) & 
                         (plant[, maturity] >= plant[,lifespan] | plant[, pollinated] >= 1)); 
  
  if(length(reproducers) > 0){ # No need to do any of this if not.
    
    for( i in 1:length(reproducers)) { # Take each reproducer and calculate young
      
      parent <- reproducers[i];
      
      # Young calculated by taking lower of pollinated proportion or 1, multiplying it by seeds variable and rounding to nearest whole number 
      plant[parent, inds.seedlings] <- round((pmin(plant[parent, pollinated], 1) * plant_offspring), digits = 0); 
      
      # Then mark parent/reproducer as dead
      plant[parent, dead] <- 1;
    }
    
    # Create vector of offspring numbers (necessary to remove 0s first? If so can add subset instruction for  plant[, inds.seedlings] != 0))
    offspring <- as.vector(subset(plant[, inds.seedlings], plant[, inds.seedlings] != 0)); 
    reproducers <- which(plant[, inds.seedlings] != 0);
    
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
      new_plant[act_row, pollinated] <- 0;
      new_plant[act_row, inds.seedlings]     <- 0;
      act_row                       <- act_row + 1; # Super important
    }
    
    colnames(new_plant) <- colnames(plant);
    
  }else{ # Else just replace with the original plant info
    new_plant <- plant;
  }
  return(new_plant)
}




#### Model run code ####




#### Set start-up conditions #### 

condition <- TRUE

season <- 0 # Season starts at 1

annual.temp <- 0 # Temp starts at 0 (as temperature changes parameters of binomial model that defines emergence will be redefined)


#### Create initial species & population summary tables (frames to be added to with data pull from each season) ####  

plantsummary <- plantinds %>% 
  group_by(speciesid) %>%
  summarise(population = length(speciesid),
            season = -1,
            mean.emergence = 0,
            q1 = 0,
            q3 = 0)

pollsummary <- pollinds %>% 
  group_by(speciesid) %>%
  summarise(population = length(speciesid),
            season = -1,
            mean.emergence = 0,
            q1 = 0,
            q3 = 0)


plantspeciesinfo <- plantinds %>% 
  distinct(speciesid, .keep_all = TRUE) %>% 
  mutate(season = -1)

pollspeciesinfo <- pollinds %>% 
  distinct(speciesid, .keep_all = TRUE) %>% 
  mutate(season = -1)

temp.summary <- array(data=0, dim = c(50,4))
temp.summary[, 4] <- seed.reference


#### Run IBM loop #### 

season <- 0
annual.temp.change <- 0

start_time <- Sys.time()


while(season < 21){ # Run for an initial 65 seasons (15 to burn in/stabilise, 50 to apply phenology shifts) 
  
  ## Generate annual temperature change and track temperature data 
  
  if(season >21){
    annual.temp.change <- rnorm(1, mean= mean.annual.increase, sd = sd.annual.increase);
    temp.summary[(season-15), 1] <- season;
    temp.summary[(season-15), 2] <- annual.temp.change;
    annual.temp <- annual.temp + annual.temp.change;
    temp.summary[(season-15), 3] <- annual.temp
    
  }
  
  ## Set all individuals to 0 for activity (this will make dormant individuals from prior generations able to be picked up by functions)
  
  pollinds[, active] <- 0
  plantinds[, active] <- 0
  
  ## Recalculate phenological parameters if temperature has changed. Only midpoint sensitivity being used for first run
  
  if(annual.temp.change != 0){
    pollinds$midpoint <- pollinds$midpoint + (annual.temp.change * pollinds$midpoint.sensitivity)
    #pollinds$scale <- pollinds$scale + (annual.temp.change * pollinds$scale.sensitivity)
    #pollinds$scale[pollinds$scale < 0.1] <- 0.1
    #pollinds$skew <- pollinds$skew + (annual.temp.change * pollinds$skew.sensitivity)
    #pollinds$skew[pollinds$skew < 1] <- 1
    plantinds$midpoint <- plantinds$midpoint + (annual.temp.change * plantinds$midpoint.sensitivity)
    #plantinds$scale <- plantinds$scale + (annual.temp.change * plantinds$scale.sensitivity)
    #plantinds$scale[plantinds$scale < 0.1] <- 0.1
    #plantinds$skew <- plantinds$skew + (annual.temp.change * plantinds$skew.sensitivity)
    #plantinds$skew[plantinds$skew < 1] <- 1
  }
  
  
  ## Calculate emergence date for individuals (and set hard limits)
  
  pollinds <- emergence.function(inds = pollinds)
  plantinds <- emergence.function(inds = plantinds)
  
  pollinds$emergence[pollinds$emergence < 1] <- 1
  pollinds$emergence[pollinds$emergence > 330] <- 330
  plantinds$emergence[plantinds$emergence < 1] <- 1
  plantinds$emergence[plantinds$emergence > 330] <- 330
  
  
  ## Place individuals
  
  pollinds     <- placementpoll(poll = pollinds);
  plantinds    <- placementplant(plant = plantinds);
  plantinds    <- competitionplant(plant = plantinds);
  plantinds    <- plantinds[plantinds[, dead] == 0,]; # Remove all plants killed by landscape competition
  
  #### Collect data BEFORE reproduction takes place ####
  
  ##Create population summary information for plants and pollinators  
  
  currentplantsummary <- plantinds %>% 
    group_by(speciesid) %>%
    summarise(population = length(speciesid),
              season = season,
              mean.emergence = mean(emergence),
              q1 = quantile(emergence, 0.25),
              q3 = quantile(emergence, 0.75),
              first = quantile(emergence, 0.05),
              last = quantile(emergence, 0.95)
    )
  
  plantsummary <- bind_rows(plantsummary, currentplantsummary)
  
  
  currentpollsummary <- pollinds %>% 
    group_by(speciesid) %>%
    summarise(population = length(speciesid),
              season = season,
              mean.emergence = mean(emergence),
              q1 = quantile(emergence, 0.25),
              q3 = quantile(emergence, 0.75),
              first = quantile(emergence, 0.05),
              last = quantile(emergence, 0.95)
    )
  
  pollsummary <- bind_rows(pollsummary, currentpollsummary)  
  
  ## Create species info summary for plants and pollinators 
  
  currentplantspeciesinfo <- plantinds %>% 
    distinct(speciesid, .keep_all = TRUE) %>% 
    mutate(season = season)
  
  currentpollspeciesinfo <- pollinds %>% 
    distinct(speciesid, .keep_all = TRUE) %>% 
    mutate(season = season)
  
  
  pollspeciesinfo <- bind_rows(pollspeciesinfo, currentpollspeciesinfo)
  plantspeciesinfo <- bind_rows(plantspeciesinfo, currentplantspeciesinfo)
  
  
  # Reset timestep and condition
  
  timestep <- 0
  
  condition <- TRUE
  
  while(condition == TRUE){
    
    if(length(pollinds[,1]) == 0) { ## A couple of stops in case all individuals die
      stop("All pollinators dead")
    }
    
    if(length(plantinds[,1]) == 0) {
      stop("All plants dead")
    }
    
    startloop <- Sys.time()
    
    
    
    
    #### Continue loop ####
    
    pollinds     <- activation(inds = pollinds);
    plantinds    <- activation(inds = plantinds);
    pollinds     <- movement(inds = pollinds);
    pollinds     <- feeding(poll = pollinds, plant = plantinds);
    plantinds    <- p.pollination(plant = plantinds, poll = pollinds);
    pollinds     <- pollinds[pollinds[, dead] == 0,]; # Some pollinators will have died as a result of feeding function so need to remove BEFORE reproducing
    pollinds     <- pollmature(poll = pollinds);
    pollinds     <- pollinds[pollinds[, dead] == 0,];
    plantinds    <- plantmature(plant = plantinds);
    pollinds     <- pollreproduction(poll = pollinds);
    plantinds    <- plantreproduction(plant = plantinds);
    pollinds     <- pollinds[pollinds[, dead] == 0,];
    plantinds    <- plantinds[plantinds[, dead] == 0,];
    
    timestep <- timestep +1 # Uptick timestep at end of function run 
    
    # Check whether there are still living individuals, if not then end run
    
    plant.living <- length(which(plantinds[, active] == 1 | plantinds[, active] == 2 | plantinds[, active] == 0));
    poll.living <- length(which(pollinds[, active] == 1 | pollinds[, active] == 2 | pollinds[, active] == 0));
    inds.living <- sum(plant.living + poll.living);
    if (inds.living <= 0){ 
      condition <- FALSE
      
    }
    
    endloop <- Sys.time()
    timeloop <- endloop - startloop
  }
  
  
  
  #### Uptick season count, increase temperature #### 
  
  season <- season + 1
  
}


pollsummary$run <- seed.reference; pollsummary$type <- run.type
plantsummary$run <- seed.reference; plantsummary$type <- run.type
pollspeciesinfo$run <- seed.reference; pollspeciesinfo$type <- run.type
plantspeciesinfo$run <- seed.reference; plantspeciesinfo$type <- run.type

write.csv(pollsummary,"pollsummary18.csv", row.names = FALSE)
write.csv(plantsummary,"plantsummary18.csv", row.names = FALSE)
write.csv(pollspeciesinfo, "pollspeciesinfo18.csv", row.names = FALSE)
write.csv(plantspeciesinfo, "plantspeciesinfo18.csv", row.names = FALSE)
write.csv(temp.summary, "tempsummary18.csv", row.names = FALSE)



