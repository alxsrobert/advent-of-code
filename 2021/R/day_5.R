# Extract the dataset from the input file.
data_import <- function(file.path){
  # Read the data file
  data <- readLines(file.path)
  # Replace the characters "->" and "," by " 
  data <- gsub(pattern = "[->,]", " ", data)
  # Re-read data (which will now be read as a matrix with 4 columns)
  data_mat <- read.table(text = data)
  # Re-name the columns
  colnames(data_mat) <- c("x1", "y1", "x2", "y2")
  # Return data_mat
  return(data_mat)
}

part1 <- function(file.path){
  # Extract the dataset
  data <- data_import(file.path)
  # Select the vertical lines
  data_vert <- data[data$x1 == data$x2,]
  # Calculate the coordinates of each data point covered by any vertical line
  coord1 <- unlist(apply(data_vert, 1, function(X)
    return(paste(X["x1"], seq(X["y1"], X["y2"])))))
  # Select the horizontal lines
  data_horiz <- data[data$y1 == data$y2,]
  # Calculate the coordinates of each data point covered by any horizontal line
  coord2 <- unlist(apply(data_horiz, 1, function(X)
    return(paste(seq(X["x1"], X["x2"]), X["y1"]))))
  # Concatenate the  vectors of coordinates
  all_coords <- c(coord1, coord2)
  # Return the number of coordinates duplicated at least once
  return(length(unique(all_coords[duplicated(all_coords)])))
}

part2 <- function(file.path){
  # Extract the dataset
  data <- data_import(file.path)
  # Select the vertical lines
  data_vert <- data[data$x1 == data$x2,]
  # Calculate the coordinates of each data point covered by any vertical line
  coord1 <- unlist(apply(data_vert, 1, function(X)
    return(paste(X["x1"], seq(X["y1"], X["y2"])))))
  # Select the horizontal lines
  data_horiz <- data[data$y1 == data$y2,]
  # Calculate the coordinates of each data point covered by any horizontal line
  coord2 <- unlist(apply(data_horiz, 1, function(X)
    return(paste(seq(X["x1"], X["x2"]), X["y1"]))))
  # Select the diagonal lines
  data_diag <- data[data$x1 != data$x2 & data$y1 != data$y2,]
  # Calculate the coordinates of each data point covered by any diagonal line
  coord3 <- unlist(apply(data_diag, 1, function(X)
    return(paste(seq(X["x1"], X["x2"]), seq(X["y1"], X["y2"])))))
  # Concatenate the  vectors of coordinates
  all_coords <- c(coord1, coord2, coord3)
  # Return the number of coordinates duplicated at least once
  return(length(unique(all_coords[duplicated(all_coords)])))
}
file.path <- "2021/Input/5/data.txt"
part1(file.path) 
part2(file.path) 
