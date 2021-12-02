part1 <- function(dt){
  colnames(dt) <- c("command", "value")
  # Horizontal = sum of all values where the command is "forward"
  horizontal <- sum(dt$value[dt$command == "forward"]) 
  # Depth = sum of all values where the command is "down" - 
  #         sum of all values where the command is "up"
  depth <- sum(dt$value[dt$command == "down"]) - sum(dt$value[dt$command == "up"])
  # Return the product of horizontal and depth
  return(horizontal * depth) 
}

part2 <- function(dt){
  colnames(dt) <- c("command", "value")
  # First we compute the changes in aim at each row:
  # - If the command is "down", the aim increases by the value
  # - If the command is "up", the aim decreases by the value
  # - Otherwise, no changes in aim
  dt$aim_entries <- 0
  dt[dt$command == "down", "aim_entries"] <- dt[dt$command == "down", "value"]
  dt[dt$command == "up", "aim_entries"] <- - dt[dt$command == "up", "value"]
  # Compute the value of "aim" at each row
  dt$aim <- cumsum(dt$aim_entries)
  # horizontal = sum of all values where the command is "forward"
  horizontal <- sum(dt$value[dt$command == "forward"]) 
  # depth = sum of the product between "value" and "aim" when the command is "forward"
  depth <- sum(dt$value[dt$command == "forward"] * dt$aim[dt$command == "forward"])
  # Return the product of horizontal and depth
  return(horizontal * depth) 
}

# Import data and run both functions
input <- read.table("2021/Input/2/data.txt")
print(part1(dt = input))
print(part2(dt = input))

