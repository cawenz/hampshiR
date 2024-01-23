#' Rounding while preserving sum of vector.
#' @description
#' Rounds a vector of numbers while preserving the sum of the vector.
#' Particularly useful when rounding percentages that sum to 100%.
#' @param x Numeric vector; often a grouped dataframe.
#' @param digits Integer indicating the number of decimal places to be used while rounding.
#'
#' @return Numeric vector with rounded values that preserves the sum total.
#' @export
#'
#' @examples
#' x <- c(.33389,0.313,0.14593, 0.20217 )
#' round_sum(x, digits=1)
round_sum <- function (x, digits = 0)
{
    up <- 10^digits
    x <- x * up
    y <- floor(x)
    indices <- utils::tail(order(x - y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    return(y/up)
}
