part1 <- function(file.path){
  data <- unlist(read.table(file.path, colClasses = "character"))
  data_mat <- matrix(as.numeric(unlist(strsplit(data, ""))), nrow = length(data), byrow = T)
  # Return the sum of positions that have a lower values than each neighbour
  return(sum(data_mat[data_mat <  rbind(data_mat[-1,], 100) & 
                        data_mat <  rbind(100, data_mat[-nrow(data_mat),]) &
                        data_mat <  cbind(data_mat[,-1], 100) & 
                        data_mat <  cbind(100, data_mat[, -ncol(data_mat)])] + 1))
}
part2 <- function(file.path){
  data <- unlist(read.table(file.path, colClasses = "character"))
  data_mat <- matrix(as.numeric(unlist(strsplit(data, ""))), nrow = length(data), byrow = T)
  
  ## Basins are separated by 9, so we have to find all entries within a "basin of 9s)
  # Find vertical neighbours (i.e entries separated by 9s)
  nei_verti <- cumsum((data_mat == 9)) + 
    rep(seq_len(ncol(data_mat)), each = nrow(data_mat))
  nei_verti[data_mat == 9] <- NA
  # Find horizontal neighbours
  nei_horiz <- cumsum(t(data_mat) == 9) + 
    rep(seq_len(nrow(data_mat)), each = ncol(data_mat))
  nei_horiz[t(data_mat) == 9] <- NA
  # Transpose nei_horiz (so that nei_horiz and nei_verti have the same order)
  nei_horiz <- c(matrix(nei_horiz, byrow = T, nrow = nrow(data_mat)))
  
  for(i in seq_len(max(nei_verti, na.rm = T))){
    # val_horiz: The values of horizontal neighbours when nei_verti == i
    val_horiz <- unique(nei_horiz[nei_verti == i & !is.na(nei_verti)])
    # val_horiz: The values of vertical neighbours when nei_horiz == val_horiz
    val_vert <- unique(nei_verti[nei_horiz %in% val_horiz & !is.na(nei_verti)])
    if(length(c(val_vert, val_horiz)) > 0){
      # Give a unique value to all the positions where nei_horiz == val_horiz 
      # and nei_vert == val_vert
      nei_verti[!is.na(nei_verti) & (nei_verti %in% val_vert | nei_horiz %in% val_horiz)] <- 
        max(val_vert)
      }
  }
  # Return the product of the three largest basins
  return(prod(sort(tabulate(nei_verti), decreasing = T)[1:3]))
  
}

file.path <- "2021/Input/9/data.txt"
print(part1(file.path))
print(part2(file.path))
