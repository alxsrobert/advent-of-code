## Function to compute decimal value from binary vector
binary_to_decimal <- function(binary){
  binary_values <- 2**rev(seq_along(binary)-1)
  return(sum(binary_values * binary))
}
# Function for part 1
part1 <- function(file.path){
  # Import the dataset as a chain of characters, and store it as a vector 
  vec <- read.table(file = file.path, colClasses = "character")[,1]
  # Split the chains, and store it in a matrix (each entry corresponds to a row)
  dt <- matrix(as.numeric(unlist(strsplit(x = vec, split = ""))), 
               ncol = nchar(vec[1]), byrow = T)
  ## Compute the binary gamma and epsilon rates:
  # Binary gamma: compute the most common entry in each column
  gamma_rate_bin <- as.numeric(colSums(dt) >= nrow(dt)/2)
  # Binary epsilon: compute the least common entry in each column
  epsilon_rate_bin <- as.numeric(colSums(dt) < nrow(dt)/2)
  # Convert the binary values into decimal values
  gamma_rate <- binary_to_decimal(gamma_rate_bin)
  epsilon_rate <- binary_to_decimal(epsilon_rate_bin)
  return(gamma_rate * epsilon_rate)
}
part2 <- function(file.path){
  # Import the dataset as a chain of characters, and store it as a vector 
  vec <- read.table(file = file.path, colClasses = "character")[,1]
  # Split the chains, and store it in a matrix (each entry corresponds to a row)
  dt <- matrix(as.numeric(unlist(strsplit(x = vec, split = ""))), ncol = nchar(vec[1]), 
               byrow = T)
  ## Use a "while" loop to filter out the entries
  # Initialisation of the while loop
  i <- 1
  # oxy_rank and co2_rank will be filtered out until there is only 1 remaining entry 
  oxy_rank <- seq_len(nrow(dt))
  co2_rank <- seq_len(nrow(dt))
  while(i <= ncol(dt)){
    # If there is more than one oxy entry, compute the most common value among
    # the remaining entries and select the entries equal to this value
    if(length(oxy_rank) > 1){
      ref_value_oxy <- sum(dt[oxy_rank,i]) >= length(oxy_rank)/2
      oxy_rank <- oxy_rank[match(which(dt[, i] == ref_value_oxy), oxy_rank)]
      oxy_rank <- oxy_rank[!is.na(oxy_rank)]
    }
    # If there is more than one oxy entry, compute the least common value among
    # the remaining entries and select the entries equal to this value
    if(length(co2_rank) > 1){
      ref_value_co2 <- sum(dt[co2_rank,i]) < length(co2_rank)/2
      co2_rank <- co2_rank[match(which(dt[, i] == ref_value_co2), co2_rank)]
      co2_rank <- co2_rank[!is.na(co2_rank)]
    }    
    
    i = i + 1
  }
  # Extract the binary values
  oxy_bin <- dt[oxy_rank,]
  co2_bin <- dt[co2_rank,]
  # Convert to decimal
  oxygen_rating <- binary_to_decimal(oxy_bin)
  co2_rating <- binary_to_decimal(co2_bin)
  # Return the product
  return(oxygen_rating * co2_rating)
}

file.path <- "2021/Input/3/data.txt"
print(part1(file.path))
print(part2(file.path))
