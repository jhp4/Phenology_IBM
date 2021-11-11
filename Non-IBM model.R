# set working directory
setwd("/Users/philipstephens/Box Sync/Stuff to comment on or read/John Paterson")

# load a required package
require(ggplot2)

# set some constants
graphit <- T # set this to F if don't want graphs of each insect's timing relative to that of the flowers
nPlants <- 30 # 450
nInsects <- 30 # 1000

# create some plants with some flowering characteristics
plants <- data.frame(ID=1:nPlants, 
                     flower.start=sample(90:190,nPlants, replace=T), 
                     flower.duration=sample(3:28,nPlants, replace = T))

# determine flowering end date for each plant
plants$flower.end <- plants$flower.start + plants$flower.duration

# create some insects, each of which has a random number of plants it interacts with
insects <- data.frame(ID=1:nInsects, nPlants=sample(1:20, nInsects, replace = T))

# randomly assign the plants that each insect interacts with
interactions <- lapply(1:nInsects, function(i){
  n <- insects$nPlants[i]
  return(sample(1:nPlants, n, replace = F))
})

# based on the plants an insect interacts with, determine the start and end dates of the insect's flight periods (min and max)
flight.times <- lapply(1:nInsects, function(i){
  interacting.plants <- interactions[[i]]
  n.interactions <- length(interacting.plants)
  st.dates <- plants$flower.start[interacting.plants]
  max.flight.start <- min(st.dates)
  min.flight.start <- max(st.dates)
  end.dates <- plants$flower.end[interacting.plants]
  max.flight.end <- max(end.dates)
  min.flight.end <- min(end.dates)
})
# what if I want to depict these to get my head around what's happening with the flight times?
if (graphit == T){
  # make sure the output folder exists
  if (!dir.exists("Graphical output")) dir.create("Graphical output")
  # put the flowering times on the graph
  f <- ggplot() + theme_classic()
  for (L in 1:n.interactions) {
    Ys <- c(L,L)+2
    Xs <- c(plants$flower.start[interacting.plants[L]],plants$flower.end[interacting.plants[L]])
    f <- f + geom_line(aes_string(Xs,Ys),col='darkgreen',size=3)
  }
  # and add the minimum flight period
  Ys <- c(2,2)
  Xs <- c(min.flight.start,min.flight.end)
  f <- f + geom_line(aes_string(Xs,Ys),col='grey70',size=3)
  # and add the maximum flight period
  Ys <- c(1,1)
  Xs <- c(max.flight.start,max.flight.end)
  f <- f + geom_line(aes_string(Xs,Ys),col='grey30',size=3)
  # make the graph look better
  f <- f + xlab("Julian day") + ylab("") + scale_y_continuous(breaks=c(1,2),labels=c("Max","Min"))
  ggsave(sprintf("Graphical output/Pollinator %3d at start.jpg",i),plot = f,width = 4,height = 4,units = 'in')
}

return(data.frame(min.flight.start,min.flight.end,max.flight.start,max.flight.end))
})

# turn list of flight times into a data frame
flight.times <- do.call(rbind,flight.times)

# and combine that data frame with the pollinator characteristics
insects <- cbind(insects,flight.times)

Test edit for Github
