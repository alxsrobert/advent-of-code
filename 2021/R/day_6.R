fish_invasion <- function(file.path, days){
  # Import the initial conditions
  fish_init <- scan(file.path, sep = ",")
  # Sort the number of days left for each fish
  fish <- sort(unique(fish_init))
  # Compute the number of fish corresponding to each day
  n_fish <- as.numeric(table(fish_init))
  for(i in 1:days){
    # Update internal timer
    fish <- fish - 1
    # If a fish was created that day, its internal timer would be 9
    fish <- c(fish, 8)
    # Compute the number of fish created that day (i.e. the number of fish with
    # their internal timer below 0)
    n_fish <- c(n_fish, sum(n_fish[fish < 0])) 
    # If any internal timer is below 0, set to 6
    fish[fish < 0] <- 6
  }
  # Return the sum of the number of fish generated each day
  return(sum(n_fish)) 
}

file.path <- "2021/Input/6/data.txt"
print(fish_invasion(file.path, 80))
print(as.character(fish_invasion(file.path, 256)))
