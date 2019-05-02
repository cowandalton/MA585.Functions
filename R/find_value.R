#' Parameter Estimator
#' 
#' This function runs either find_first_value or find_later_value depending on whether a list of prams is specified to find the value of a parameter for a function that gives a desired value.
#' @seealso \code{\link{find_later_value}} and \code{\link{find_first_value}}
#' @param f the function that is being used (note f should be a monotonic function for this function to work properly).
#' @param prams a list of parameter values for all parameters that are specified in a function call prior to the desired parameter (if the desired parameter is the first parameter in the function call, this should not be defined).
#' @param val the desired value for the function.
#' @param maximum the maximum value for the search parameter (default = 100000).
#' @param minimum the minimum value for the search parameter (default = 0).
#' @param only_integers logical indicating whether the function only takes integers for the search parameter (default = F, allowing decimals).
#' @param decimals the number of decimal places to use for the search parameter. 
#' @return A list will be returned containing the estimate for the desired parameter, the function value when the parameter estimate is used in f, and a warning if the parameter estimate results in a function value that is more than 1 away from val.
#' @export 
#' @examples 
#' find_value(log, val = 10, maximum = 1000000)
#' find_value(log, prams = list(32), val = 2, decimals = 6)

#this function runs either find_first_value or find_later_value depending on whether a list of prams is specified
find_value <- function(f, prams, val = 0, maximum = 100000, minimum = 0, only_integers = F, decimals = 4){
  if(missing(prams)){
    find_first_value(f = f, val = val, maximum = maximum, minimum = minimum, only_integers = only_integers, decimals = decimals)
  } else{
    find_later_value(f = f, prams = prams, val = val, maximum = maximum, minimum = minimum, only_integers = only_integers, decimals = decimals)
  }
}