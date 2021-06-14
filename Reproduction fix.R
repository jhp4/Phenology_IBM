
parents <- array(data = 0, dim = c(20,2)) # Make a dummy array with parent info
parents[, 1] <- seq(1:length(parents[,1])) # Make a column with individual number
parents[, 2] <- rep(letters, length.out = 20) # Populate another column with info so we know if function works 

parent.inds <- c(2,4,6,9,10,14,17) # Vector storing the reproducing individuals (values correlate to row numbers)
offspring.inds <- c(3,3,5,2,5,5,2) # Vector storing how many offspring each individual is producing 
offspring <- sum(offspring.inds) # Total offspring to be produced 

new.gen <- array(data=0, dim = c((length(parents[, 1]) + offspring), 2)) # Create new array that's as long as parent array + offspring
new.gen[1:length(parents[,1]),] <- parents # Populate new array with parent information 

offspring.deets <- function(offspring.inds, parent.inds, parents, new.gen){
  start.point = length(parents[, 1]); # Define parent length (should be 20 according to dummy data)
  for (i in (1:length(offspring.inds))){ # Loop through offspring.inds vector to populate array
    offspring.number = offspring.inds[i]; # Get number of offspring from offspring.inds vector
    parent.ind = parent.inds[i]; # Get ID of parent producing these offspring
    new.rows = rep(parents[parent.ind,], offspring.number); # Generate new rows of information for offspring based on parent info 
    new.gen[((start.point+1):(start.point + offspring.number)),] <- new.rows; # Populate relevant rows with offsrping information
    start.point = start.point + offspring.number # Redefine start.point (so next i starts from correct point and doesn't overwrite previous inds information)
  }
  return(new.gen)
}



new.gen <- offspring.deets(new.gen = new.gen, offspring.inds = offspring.inds, parent.inds = parent.inds, parents = parents)
