

#' Title
#'
#' @param id a character or numeric value
#'
#' @return a character value with the appropriate number of leading zeros
#' @export
#'
#' @examples
#' ids <- c(NA, 1, 12, 234, 4567, 7899, 87357, 376256, 8765676)
#' fixID(ids)
fixID <- function(id){
  lgth <-
    ifelse(is.na(id),NA,nchar(id))

  i <-
        ifelse(is.na(lgth),NA,
        ifelse(lgth==1, paste0("000000", id),
        ifelse(lgth==2, paste0("00000", id),
        ifelse(lgth==3, paste0("0000", id),
        ifelse(lgth==4, paste0("000", id),
        ifelse(lgth==5, paste0("00", id),
        ifelse(lgth==6, paste0("0", id),
        ifelse(lgth==7,as.character(id), NA
        ))))))))
  i
}
# fixID(ids)
#
# ids <- c(NA, 1, 12, 234, 4567, 7899, 87357, 376256, 8765676)
