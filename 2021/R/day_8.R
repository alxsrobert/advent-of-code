part1 <- function(file.path){
  # Import the dataset, select the last four columns (i.e. the output)
  output <- unlist(read.table(file.path, sep = " ")[12:15])
  # Compute the sum of all elements with 2, 3, 4, or 7 characters
  nb_unique_dig <- sum(is.element(nchar(output), c(2, 3, 4, 7)))
  
  return(nb_unique_dig)
}

part2 <- function(file.path){
  # Import the dataset
  data <- unlist(read.table(file.path, sep = " "))
  # Convert to the matrix format
  mat_data <- matrix(data[data != "|"], ncol = 14)
  # Vector of correspondence between unique number of characters and digits
  corres_nchar <- c(1, 4, 7, 8)
  names(corres_nchar) <- c(2, 4, 3, 7)
  output <- apply(mat_data, 1, function(X){
    n_char <- nchar(X)
    # Identify the digits that have a unique number of characters
    digits <- as.numeric(corres_nchar[as.character(n_char)])
    # If the last four digits are not all identified, then find the key
    if(any(is.na(digits[-c(seq_len(10))]))){
      # Split every entry into vector of characters, and sort the characters
      # This will make the identification between chains easier
      unique_char <- strsplit(X, "")
      sorted_char <- lapply(unique_char, sort)
      # Extract the characters in the digits already identified
      code1 <- sorted_char[[which(digits == 1)[1]]]
      code4 <- sorted_char[[which(digits == 4)[1]]]
      code7 <- sorted_char[[which(digits == 7)[1]]]
      code8 <- sorted_char[[which(digits == 8)[1]]]
      # Identify which entries are still to be identified
      entries_nchar5 <- which(nchar(X) == 5)
      entries_nchar6 <- which(nchar(X) == 6)
      ## Identifying the chains with 5 characters:
      # If the concatenation of the chain and code4 contains every letter
      # Then return 2
      # Otherwise, if the concatenation of the chain and code1 contains 6 letters
      # Return 5
      # Otherwise, return 3
      id5 <- lapply(sorted_char[entries_nchar5], function(Y){
        if(length(unique(c(Y, code4))) == 7) return(2)
        if(length(unique(c(Y, code1))) == 6) return(5)
        return(3)
      })
      ## Identifying the chains with 6 characters:
      # If the concatenation of the chain and code4 contains 6 letters
      # Then return 9
      # Otherwise, if the concatenation of the chain and code1 contains every letter
      # Return 6
      # Otherwise, return 0
      id6 <- lapply(sorted_char[entries_nchar6], function(Y){
        if(length(unique(c(Y, code4))) == 6) return(9)
        if(length(unique(c(Y, code1))) == 7) return(6)
        return(0)
      })
      # Add id5 and id6 to digits
      digits[entries_nchar5] <- unlist(id5)
      digits[entries_nchar6] <- unlist(id6)
    }
    # Return the concatenation of the last 4 digits
    return(as.numeric(paste0(digits[-c(seq_len(10))], collapse = "")))
  })
  # Return the sum of all outputs
  return(sum(output))
}

file.path <- "2021/Input/8/data.txt"
part1(file.path) 
part2(file.path) 
