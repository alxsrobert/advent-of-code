# Function to compute the score for each entry. 
# X: vector of character corresponding to an entry 
# incomplete: Boolean indicating whether to compute the scores associated to 
# corrupted (part 1) or incomplete (part 2) entries
compute_score <- function(X, incomplete){
  # closing symbols  
  closing <- c(")", "]", "}", ">")
  # Initialise the score of the entry (which will be the returned value)
  score_X <- NA
  # Whether or not an error has been spotted (0 or 1)
  error <- 0
  # Numeric value iterating over X
  i <- 2
  while(error == 0 & i <= length(X)){
    # Value indicating whether a match has been found to a closing symbol
    match = 0
    # If i is equal to 1, move to i = 2
    if(i == 1){
      i <- i + 1
    } 
    # If X[i] is a closing symbold and X[i-1] is the corresponding opening symbol, 
    # then match = 1
    if((X[i] == ")" & X[i-1] == "(") | (X[i] == ">" & X[i-1] == "<") |
       (X[i] == "}" & X[i-1] == "{") | (X[i] == "]" & X[i-1] == "[")) 
      match = 1
    ## if there has been a match, remove i and i-1 from X (since they are not 
    ## incomplete nor corrupted)
    if(match == 1){ 
      X <- X[-c(i-1, i)]
      # Move i to i-1 to scan every entry
      i <- i - 1
      ## If there was no match, but X[i] is a closing symbol, then there is an error
    } else if(match == 0 & any(X[i] == closing)) error <- 1 else{
      # Otherwise, continue scanning
      i <- i + 1
    } 
  }
  # If part 1 and there was an error in the tree, 
  # compute the value associated with the entry
  if(error == 1 & incomplete == F) 
    score_X <- (X[i] == closing) * c(3, 57, 1197, 25137)
  # If part 2 and there was no error in the tree, 
  # compute the value associated with the entry
  if(error == 0 & incomplete == T){
    # Initialise the score associated with each character
    opening <- c("(", "[", "{", "<")
    score <- c(1,2,3,4)
    names(score) <- opening
    # Reverse X the last opened symbold should be closed first)
    X <- rev(X)
    score_X <- 0
    # For each value of X, multiply score_X by 5 and add the score associated with X[i]
    for(i in seq_along(X)) score_X <- (score_X * 5 + score[X[i]])
  }
  # Return the score for the entry X, NA corresponds to an incomplete entry in part 1,
  # or a corrupted entry in part 2
  return(score_X)
}


part1 <- function(file.path){
  # Import the data file
  data <- readLines(file.path)
  # Compute the score for each entry
  score_corrupt_line <- (unlist(lapply(strsplit(data, ""), function(X){
    error_X <- compute_score(X, incomplete = FALSE)
    return(error_X)
  })))
  # Return the sum of each score, excluding NAs
  return(sum(score_corrupt_line, na.rm = T))
}

part2 <- function(file.path){
  # Import the data file
  data <- readLines(file.path)
  # Compute the score for each entry
  score_incomp_line <- (unlist(lapply(strsplit(data, ""), function(X){
    error_X <- compute_score(X, incomplete = TRUE)
    return(error_X)
  })))
  # Return the median of the score vector, excluding NAs
  return(median(score_incomp_line, na.rm = T))
}

file.path <- "2021/Input/10/data.txt"
print(part1(file.path)) 
print(part2(file.path)) 
