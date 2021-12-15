# Same function for part1 and part2
res <- function(file.path, part2){
  # Import data and extract the correspondence between column 1 and 2
  data <- read.table(file.path, sep = "-")
  loc <- data[,1]
  names(loc) <- data[,2]
  # The path start with "start"
  paths <- matrix("start")
  # Initialise i and the number of exits
  i <- 1
  n_exit <- 0
  while(!all(paths[,i] == "end")){
    # For each path, find the next possible elements
    new_elements <- apply(paths, 1, function(X){
      # The small caves are the elements of X where X == tolower(X)
      small_caves <- X[X == tolower(X) & X != "start"]
      # If part1, the elements of small_caves cannot be visited anymore
      # If part 2, they can be visited if none has already been visited
      if(part2 == TRUE){
        if(!any(duplicated(small_caves))) small_caves <- NULL
      }
      # Find the next values given the last location is X[i]
      new <- c(names(loc)[loc == X[i]], loc[names(loc) == X[i]])
      # Remove the values of new that corresponds to small caves we already visited
      new <- new[!is.element(new, small_caves)]
      new <- new[new != "start"]
      return(new)
    })
    # If there is only one row, convert new_elements to a list
    if(nrow(paths) == 1) new_elements <- list(c(new_elements))
    
    # Number of possible caves for each path
    iter <- rep(seq_len(nrow(paths)), unlist(lapply(new_elements, length)))
    # Update the paths
    paths <- cbind(paths[iter,], unlist(new_elements))
    
    i <- i + 1
    # Remove the path that have reached the end, and update n_exit
    n_exit <- n_exit + sum(paths[,i] == "end")
    paths <- paths[which(paths[,i] != "end"),]
  }
  return(n_exit)
}

file.path <- c("2021/Input/12/data.txt")
print(res(file.path, part2 = FALSE))
print(res(file.path, part2 = TRUE))
