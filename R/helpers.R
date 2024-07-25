to_numeric <- function(x) {
  i <- which(x == "")
  x <- as.numeric(x)
  x[i] <- 0
  return(x)
}


