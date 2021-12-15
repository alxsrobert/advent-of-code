# Function to find the neighbours of an octopus i
find_neighbours <- function(i, dims){
  # If i is on the bottom row, they have 6 neighbours at most (no neighbour below)
  if(i %% dims[1] == 0){
    nei_i <- i + c(-1, dims[1] + c(-1, 0), -dims[1] + c(-1, 0))
    # If i is on the top row, they have 6 neighbours at most (no neighbour above)
  } else if(i %% dims[1] == 1){
    nei_i <- i + c(1, dims[1] + c(0, 1), -dims[1] + c(0,1))
    # Otherwise, they  have 9 neighbours at most
  } else
    nei_i <- i + c(-1, 1, dims[1] + c(-1, 0, 1), -dims[1] + c(-1, 0,1))
  # Remore the out of bounday neighbours (nei_i below 0 or above the product of the dimensions)
  nei_i <- nei_i[nei_i > 0 & nei_i <= prod(dims)]
  return(nei_i)
}

part1 <- function(file.path){
  # Import the dataset, and create mat_energy (each element corresponds to 1 octopus)
  data_vec <- scan(file.path)
  data <- unlist(strsplit(as.character(data_vec), ""))
  mat_energy <- matrix(as.numeric(data), nrow = length(data_vec), byrow = T)
  dims <- dim(mat_energy)
  # Initialise the step count, and total number of flashes 
  step <- 1
  flash_tot <- 0
  while(step <= 100){
    # Add 1 to each element
    mat_energy <- mat_energy + 1
    while(any(mat_energy > 9)){
      # Add the number of octopus that are flashing to the total number of flashes
      flash_tot <- flash_tot + sum(mat_energy > 9)
      # Find the neighbours of the flashing octopuses
      nei <- c()
      for(i in which(mat_energy > 9)){
        nei <- c(nei, find_neighbours(i, dims))
      }
      # The level of energy of flashing octopuses is set to 0, and will not be further changed
      # at this step
      mat_energy[mat_energy > 9] <- 0
      nei <- nei[mat_energy[nei] > 0]
      # Add energy to the neighbours of flashing octopuses
      mat_energy[nei] <- mat_energy[nei] + tabulate(nei)[nei]
    }
    step <- step + 1
  }
  return(flash_tot) 
} 

part2 <- function(file.path){
  # Import the dataset, and create mat_energy (each element corresponds to 1 octopus)
  data_vec <- scan(file.path)
  data <- unlist(strsplit(as.character(data_vec), ""))
  mat_energy <- matrix(as.numeric(data), nrow = length(data_vec), byrow = T)
  # Initialise the step count, and number of flashes per step
  dims <- dim(mat_energy)
  step <- 1
  flash_step <- 0
  # The maximum number of flashes is the product of the dimensions of mat_energy 
  synchro <- prod(dims)
  while(flash_step != synchro){
    # Add 1 to each element
    mat_energy <- mat_energy + 1
    flash_step <- 0
    while(any(mat_energy > 9)){
      # Add the number of octopus that are flashing to the number of flashes at this step
      flash_step <- flash_step + sum(mat_energy > 9)
      # Find the neighbours of the flashing octopuses
      nei <- c()
      for(i in which(mat_energy > 9)){
        nei <- c(nei, find_neighbours(i, dims))
      }
      # The level of energy of flashing octopuses is set to 0, and will not be further changed
      # at this step
      mat_energy[mat_energy > 9] <- 0
      nei <- nei[mat_energy[nei] > 0]
      # Add energy to the neighbours of flashing octopuses
      mat_energy[nei] <- mat_energy[nei] + tabulate(nei)[nei]
    }
    step <- step + 1
  }
  return(step - 1) 
} 

file.path <- "2021/Input/11/data.txt"
print(part1(file.path))
print(part2(file.path))