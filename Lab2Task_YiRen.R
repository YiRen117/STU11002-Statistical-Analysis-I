n_stay <- 0    #set the time of the player, who always chooses to stay, wins to 0
n_switch <- 0  #set the time of the player, who always chooses to switch, wins to 0
n_random <- 0  #set the time of the player, who randomly chooses to switch or stay, wins to 0

for ( i in 1:100) {     #play the game 100 times
   door <- c(1,2,3)     #set up the three doors
   cardoor <- sample(door,1)  #randomly select one of the doors to have the car behind it
   choice <- sample(door,1)   #randomly select the player's choice of door
   goatdoors <- setdiff(door, cardoor)    #create a new vector that holds the values corresponding to goats
   reveal_options <- setdiff(goatdoors, choice)    #identify the options we have for the reveal
   if (choice == cardoor) {      #the situation where there are two goats to choose from for the reveal
      reveal <- sample(reveal_options,1)  }     #select one goat door randomly and assign it to the variable reveal
   else {      #the situation where there is only one goat to choose from for the reveal
      reveal <- reveal_options      #assign the only goat element in reveal_option to the variable reveal
   }
   remaining_doors <-setdiff(door, reveal)      #create a new vector which identifies the two remaining unrevealed doors
   newchoice <- setdiff(remaining_doors, choice)   #record the final choice of door if the player switches
   randomchoice <- sample(remaining_doors,1)
   
   if (choice == cardoor) {      #the situation where the player who chooses to stay wins
      n_stay <- n_stay + 1    #add one to the time of the player, who always chooses to stay, wins
   }
   
   if (newchoice == cardoor) {      #the situation where the player who chooses to switch wins
      n_switch <- n_switch + 1    #add one to the time of the player, who always chooses to switch, wins
   }
   
   if (randomchoice == cardoor) {      #the situation where the player who chooses randomly wins
      n_random <- n_random + 1    #add one to the time of the player, who randomly chooses, wins
   }
}
print(n_stay/100)    #calculate the win rate of the player who always chooses to stay
print(n_switch/100)    #calculate the win rate of the player who always chooses to switch
print(n_random/100)    #calculate the win rate of the player who randomly chooses to switch

