
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

## Check on dummy data

pollcheck<-array(data=0, dim = c(2,5)) # Create pollinator array with 2 inds
pollcheck[,4] <- c(6,4) # Make one exceed hunger threshold (of 5) and one not
pollcheck[,3] <- c(0,0) # Run if want to check whether detection of death working correctly 
pollcheck[,2] <- c(1,1) # Set activity of inds (can check it works for states of 0, 1, 2)

## Run check 

pollcheck <- pollmature(poll = pollcheck)


