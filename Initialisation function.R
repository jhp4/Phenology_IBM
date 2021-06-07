
make_poll_species<- function(species_number     , 
                             plant_species      , ## This line will need to be equal to the number of species in the make_plant_species function
                             max_interactions   , 
                             min_interactions   , 
                             max_midpoint       , 
                             min_midpoint       ,
                             max_scale          ,
                             min_scale          ,
                             max_efficacy       ,
                             min_efficacy       
){
  poll_species<-array(data=0, dim=c(species_number,6));
  colnames(poll_species)<-c("species","interactions","midpoint","scale","efficacy","partners");
  poll_species[, 1]<-seq_len(nrow(poll_species));
  poll_species[,3]<-sample(seq(from = min_midpoint, to = max_midpoint, by=1),size=dim(poll_species)[1]);
  poll_species[,4]<-sample(seq(from = min_scale, to = max_scale, by=0.05),size=dim(poll_species)[1]);
  poll_species[,5]<-sample(seq(from = min_efficacy, to = max_efficacy, by=0.05),size=dim(poll_species)[1]);
  poll_species[,6]<-sample(1:plant_species, dim(poll_species)[1], replace = F);
  return(poll_species)
}

int_partners<-function(species, partners = 6, plant_species=5){
  for (i in 1:dim(species)[1]){
    n<-species[i,partners];
    ints<-(sample(1:plant_species, n, replace = F));
    species[i,2]<-ints
  }
}

int_partners(poll_species)
  
