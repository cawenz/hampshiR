

dateFix <- function(date){
  if(!is.character(date)){
    stop("date must be a character vector")
  } else {
    d <- as.Date(date, tryFormats=c(
  "%b %d, %Y %I:%M:%S %p",
  "%b %d, %Y",
  "%Y-%m-%d",
  "%Y-%m-%d %I:%M:%S %p")
    )
    d
  }
}


