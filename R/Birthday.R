#' Title: birthday
#'
#' @param x The number of people in the group
#'
#' @returns A vector of probabilities corresponding to the input
#' @export
#'
#' @examples
#' # Calculate probabilities for default class sizes 20-25 birthday()
birthday <- function(x){
  + 1 - exp(lchoose(365, x) + lfactorial(x) - x*log(365))
  }
