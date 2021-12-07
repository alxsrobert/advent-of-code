part1 <- function(file.path){
  # Import the dataset
  input <- scan(file.path, sep = ",")
  # Compute the median position
  pos <- median(input)
  # Return the sum of fuel used by each crab
  return(sum(abs(input - pos)))
}

part2 <- function(file.path){
  # Import the dataset
  input <- scan(file.path, sep = ",")
  # Loop over each value of "input" to find the position where the fuel cost
  # is minimal
  val_ref <- Inf
  for(i in seq_len(max(input))){
    dist_to_i <- abs(input - i)
    # "cumsum(seq_len(max(dist_to_i)))" is the amount of fuel used for each position
    # "tabulate(dist_to_i)" is the number of crabs at each position
    val <- sum(cumsum(seq_len(max(dist_to_i))) * tabulate(dist_to_i))
    # If the value is below the previous reference point, replace the reference point
    if(val < val_ref){
      val_ref <- val
    } 
  }
  
  return(val_ref)
}

file.path <- "2021/Input/7/data.txt"
print(part1(file.path))
print(part2(file.path))
