
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

## Check on dummy data

plantcheck<-array(data=0, dim = c(2,5)) # Create plant array with 2 inds
plantcheck[,2] <- c(1,2) # Set activity of inds (can check it works for states of 0, 1, 2)
plantcheck[,3] <- c(0,1) # Run if want to check whether detection of death working correctly 

## Run check 

plantcheck <- plantmature(plant = plantcheck)


