#' Title
#'
#' @param birthDate A character string in the format %Y-%m-%d or a date object
#' @param refDate A character string in the format %Y-%m-%d or a date object
#' @param unit A character string indicating the desired unit. Must be one of 'year', 'month', 'week', or 'day'
#'
#' @return A number indicating age in the unit set in the unit argument
#' @export
#'
#' @examples
#' birthDate <- "2000-01-01"
#' refDate <- Sys.Date()
#' age <- getAge(birthDate, refDate, unit="year")
getAge <- function(birthDate, refDate = Sys.Date(), unit = "year") {
  if(grepl(x = unit, pattern = "year")) {
    lubridate::as.period(lubridate::interval(birthDate, refDate), unit = 'year')$year
  } else if(grepl(x = unit, pattern = "month")) {
    lubridate::as.period(lubridate::interval(birthDate, refDate), unit = 'month')$month
  } else if(grepl(x = unit, pattern = "week")) {
    floor(lubridate::as.period(lubridate::interval(birthDate, refDate), unit = 'day')$day / 7)
  } else if(grepl(x = unit, pattern = "day")) {
    lubridate::as.period(lubridate::interval(birthDate, refDate), unit = 'day')$day
  } else {
    print("Argument 'unit' must be one of 'year', 'month', 'week', or 'day'")
    NA
  }

}
