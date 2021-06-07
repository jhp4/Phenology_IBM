
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


