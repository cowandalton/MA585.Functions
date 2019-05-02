#' Number of Non-decimal Digits
#' 
#' This function counts the number of non-decimal digits that a number has.
#' @param number input desired number to count digits.
#' @return count of non-decimal digits
#' @export
#' @examples 
#' count_digits(55.37)
#' count_digits(1001.2)

count_digits <- function(number){
  count <- 0
  number <- abs(number)
  #counting the number of times a number of times a number can be divided by 10 until it is less than 1.
  while(number >= 1){
    number <- number/10
    count <- count + 1
  }
  return(count)
}