# Function to read the input file and extract the vectors of draws and entries
data_import <- function(file.path){
  # The first line corresponds to the vector of draws
  draws <- strsplit(readLines(file.path, n = 1), ",")[[1]]
  # The rest of the input file corresponds to the entries. We remove the space 
  # characters to only get a vector of all entries
  entries <- unlist(strsplit(readLines(file.path)[-c(1)], " "))
  entries <- entries[entries != ""]
  # Return a list with two elements: a vector of draws and a vector of entries
  return(list(draws = draws, entries = entries))
}
# Function that takes a vector of draws and a vector of entries, and return a
# vector corresponding to the position of "draws" that will complete each board 
pos_board_complete <- function(draws, entries){
  # Create a vector iterating along "draws". The name of each element corresponds
  # to the value of draws at this position
  seq_draws <- seq_along(draws)
  names(seq_draws) <- draws
  # Create a vector corresponding to the position of each entry in draws (i.e.
  # when will each entry be drawn)
  rank <- seq_draws[entries]
  # Split the vector "rank" into 5*5 matrices, corresponding to each board
  list_mat_rank <- lapply(split(rank, (seq_along(rank) - 1) %/% 25), 
                          matrix, nrow = 5, ncol = 5)
  # For each list, "rank_bingo" corresponds to the position of the winning draw.
  # apply(X, 1, max) computes the value of the last draw for each row
  # apply(X, 2, max) computes the value of the last draw for each column
  # The minimum value of apply(X, 1, max) and apply(X, 2, max) is the last draw
  rank_bingo <- unlist(lapply(list_mat_rank, function(X){
    return(min(apply(X, 1, max), apply(X, 2, max)))
  }))
  # Return the vector of ranks.
  return(rank_bingo)
}

# function part 1
part1 <- function(file.path){
  # Import the data, and extract the vectors draws and entries
  data <- data_import(file.path)
  draws <- data$draws
  entries <- data$entries
  # Compute the winning position for each board 
  rank_bingo <- pos_board_complete(draws, entries)
  # Extract the winning board (i.e. the board with the lowest winning position)
  board_nb <- which.min(rank_bingo)
  winning_board <- as.numeric(entries[(board_nb - 1) *25 + 1:25])
  # Compute the value of draw corresponding to the winning board
  last_draw <- min(rank_bingo)
  last_number <- as.numeric(draws[last_draw])
  # Compute the sum of the uncalled numbers in "winning_board"
  sum_uncalled <- sum(winning_board[!is.element(winning_board, 
                                                draws[seq_len(last_draw)])])
  
  return(sum_uncalled * last_number)
}
# Function part 2
part2 <- function(file.path){
  # Import the data, and extract the vectors draws and entries
  data <- data_import(file.path)
  draws <- data$draws
  entries <- data$entries
  # Compute the winning position for each board 
  rank_bingo <- pos_board_complete(draws, entries)
  # Extract the losing board (i.e. the board with the highest winning position)
  board_nb <- which.max(rank_bingo)
  losing_board <- as.numeric(entries[(board_nb - 1) *25 + 1:25])
  # Compute the value of draw corresponding to the losing board
  last_draw <- max(rank_bingo)
  last_number <- as.numeric(draws[last_draw])
  # Compute the sum of the uncalled numbers in "losing_board"
  sum_uncalled <- sum(losing_board[!is.element(losing_board, draws[seq_len(last_draw)])])
  return(sum_uncalled * last_number)
}

file.path <- "2021/Input/4/data.txt"
print(part1(file.path)) 
print(part2(file.path)) 
