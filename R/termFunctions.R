#' Get a term label from integer.
#' @description
#' Takes a positive integer and returns the corresponding term label.
#'
#'
#' @param x a positive integer representing a Hampshire College term.
#'
#' @return A character string of the term (in the format 2023F
#' where 2023 is the year and F is the season).
#' @export
#'
#' @examples
#' x <- 106
#' termFromNum(x)
termFromNum <- function(x){
  x <- as.numeric(x)
  rem <- x %% 2

  subt <- ifelse(rem==1.5, -0.5,
                 ifelse(rem==0.5, 0.5,
                        ifelse(rem==0, 0,
                               1
                        )))

  season <- ifelse(rem==1.5, "J",
                   ifelse(rem==0.5, "A",
                          ifelse(rem==0, "S", "F"
                          )))

  year <- ((x-subt)/2)+1970
  t <- paste0(year,season)
  t
}




#' Get the number of of Hampshire College term label
#' @description
#' Useful for sequencing terms over time.
#'
#' @param x a character vector of term labels in the format "2023F" where 2023 is the
#' year and F is the season.
#'
#' @return a numeric vector.
#' @export
#'
#' @examples
#' terms <- sort(paste0(rep(2010:2020,2), c("S","F")))
#' getTermNum(terms)
getTermNum <- function(x) {
  addsub = ifelse(grepl("F|R",x),1,
                  ifelse(grepl("J", x),-0.5,
                         ifelse(grepl("A", x),0.5, 0
                         )))

  xnum = as.numeric(substr(x, 1, 4))

  y= (((xnum+addsub)-1970)*2)-addsub
  y
}


#' Get the term from a date
#' @description
#' A simple function to get the term (i.e. "2023F") from a date or datetime.
#' Relies on lubridate::semester to place a date in either the fall or spring term.
#'
#' @param date a vector with class date or datetime.
#'
#' @return a character vector of term labels.
#' @export
#'
#' @examples
#' date <- as.Date("2023-01-01")
#' getTermFromDate(date)
getTermFromDate <- function(date){
  if(!lubridate::is.Date(date)){
    stop("input needs to be a date")
  } else{
  sem <- lubridate::semester(date)

  season <-
    ifelse(sem==1, "S", "F")

  year <- lubridate::year(date)
  t <- paste0(year, season)

  r <- ifelse(t=="NANA", NA, t)
  r
  }
}
