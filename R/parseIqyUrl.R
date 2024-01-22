parseIqyUrl <- function(path) {
  q <- readLines(path)
  qurl <- paste(q[3], q[4], sep = "?")
  return(qurl)
}