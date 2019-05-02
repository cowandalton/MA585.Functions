#' Parameter Estimator (Besides First Parameter)
#' 
#' This function finds the value of a parameter other than the first for a function that gives a desired value.
#' @param f the function that is being used (note f should be a monotonic function for this function to work properly).
#' @param prams a list of parameter values for all parameters that are specified in a function call prior to the desired parameter.
#' @param val the desired value for the function.
#' @param maximum the maximum value for the search parameter (default = 100000).
#' @param minimum the minimum value for the search parameter (default = 0).
#' @param only_integers logical indicating whether the function only takes integers for the search parameter (default = F, allowing decimals).
#' @param decimals the number of decimal places to use for the search parameter. 
#' @return A list will be returned containing the estimate for the desired parameter, the function value when the parameter estimate is used in f, and a warning if the parameter estimate results in a function value that is more than 1 away from val.
#' @export 
#' @examples 
#' find_later_value(log, prams = list(32), val = 2, decimals = 6)

find_later_value <- function(f, prams, val = 0, maximum = 100000, minimum = 0, only_integers = F, decimals = 4){
  
  #checking that f is a valid function
  f <- match.fun(f)
  #counting the number of non-decimal digits for the specified maximum
  digmax <- count_digits(maximum)
  #counting the number of non-decimal digits for the specified minimum
  digmin <- count_digits(minimum)
  #using the larger number of digits from maximum or minimum
  digits <- max(digmin, digmax)
  #not allowing the number of decimals to be negative
  if(decimals < 0){
    decimals <- 0
  }
  #not allowing decimals if only want integers for the parameter
  if(only_integers == T){
    decimals <- 0
  }
  #calculating the sum of the number of non-decimal digits and the number of decimal digits to determine how many digits total to use for the parameter
  totdigits <- digits+decimals
  #starting the value of the parameter at zero
  value <- 0
  #setting the leading digit to zero originally
  leading.digit <- 0
  #checking if the minimum value is valid for the function
  min_check <- try(do.call(f, c(prams, minimum)), silent = T)
  if(class(min_check) != "numeric"){
    #stopping the function if the minimum value is not valid for the function
    stop("Error occurred when checking the minimum value. Please specify a new minimum.")
  }
  #checking if the maximum value is valid for the function
  max_check <- try(do.call(f, c(prams, maximum)), silent = T)
  if(class(max_check) != "numeric"){
    #stopping the function if the maximum value is not valid
    stop("Error occurred when checking the maximum value. Please specify a new maximum.")
  }
  #cycling through positive and negative values for the leading digit
  for(i in -10:9){
    #breaking the current for loop if the value plugging in for the parameter is greater than the maximum
    if(i*10^(digits-1) > maximum){
      break
    }
    #skipping to the next value of i if the next value value plugged in for the paramater is less than the minimum
    if((i+1)*10^(digits-1) < minimum){
      next
    }
    #if the current value plugged in for the parameter is less than the minimum, than the minimum is used instead. Otherwise, the current value is plugged into the function.
    if(i*10^(digits-1) < minimum){
      current_value <- do.call(f, c(prams, minimum))
    } else{
      current_value <- do.call(f, c(prams, (i*10^(digits-1))))
    }
    #checks if the next value plugged in for the parameter is greater than the maximum
    if((i+1)*10^(digits-1) > maximum){
      #assigns the maximum if the next value is greater than the maximum
      next_value <- do.call(f, c(prams, maximum))
    } else{
      #uses the next value if it is less than or equal to the maximum
      next_value <- do.call(f, c(prams, ((i+1)*10^(digits-1))))
    }
    #assigns the leading digit the current value of i if current function value is greater than or equal to the desired value and the next function value is less than or equal to the desired value
    if((current_value >= val) & (next_value <= val)){
      leading.digit <- i*10^(digits-1)
    }
    #assigns the leading digit the current value of i if current function value is less than or equal to the desired value and the next function value is greater than or equal to the desired value
    if((current_value <= val) & (next_value >= val)){
      leading.digit <- i*10^(digits-1)
    }
  }
  #adding the value for leading.digit to the current value of the parameter
  value <- value + leading.digit
  #this loop determines the second through second to last digit for the parameter
  for(j in 2:(totdigits-1)){
    #breaking the loop if there are two or fewer total digits wanted for the parameter
    if(totdigits <= 2){
      break
    }
    #setting the current digit to 0
    current.digit <- 0
    #this loop repeats what was done for the leading digit for other digits with the only change being that we add the current value for the parameter
    for(i in 0:9){
      if(value+i*10^(digits-j) > maximum){
        break
      }
      if(value+(i+1)*10^(digits-j) < minimum){
        next
      }
      if(value+i*10^(digits-j) < minimum){
        current_value <- do.call(f, c(prams, minimum))
      } else{
        current_value <- do.call(f, c(prams, value+i*10^(digits-j)))
      }
      if(value+(i+1)*10^(digits-j) > maximum){
        next_value <- do.call(f, c(prams, maximum))
      } else{
        next_value <- do.call(f, c(prams, (value+(i+1)*10^(digits-j))))
      }
      if((current_value >= val) & (next_value <= val)){
        current.digit <- i*10^(digits-j)
      }
      if((current_value <= val) & (next_value >= val)){
        current.digit <- i*10^(digits-j)
      }
    }
    #adding the current digit to the parameter value
    value <- value + current.digit
  }
  #setting the last digit to 0
  last.digit <- 0
  for(i in 0:9){
    #breaking the for loop if the parameter is supposed to be a one digit integer
    if((digits == 1) & (only_integers == T)){
      break
    }
    if(value+i*10^(digits-totdigits) > maximum){
      break
    }
    if(value+(i+1)*10^(digits-totdigits) < minimum){
      next
    }
    if(value+i*10^(digits-totdigits) < minimum){
      current_value <- do.call(f, c(prams, minimum))
    } else{
      current_value <- do.call(f, c(prams, value+i*10^(digits-totdigits)))
    }
    if(value+(i+1)*10^(digits-totdigits) > maximum){
      next_value <- do.call(f, c(prams, maximum))
    } else{
      next_value <- do.call(f, c(prams, (value+(i+1)*10^(digits-totdigits))))
    }
    #checking if the current value is greater than or equal to the desired value and the next value is less than or equal to the desired value
    if((current_value >= val) & (next_value <= val)){
      #checking if the current value is closer to the desired value than the next value
      if(abs(current_value-val) < abs(next_value-val)){
        #assigns the last digit based on the current value
        last.digit <- i*10^(digits-totdigits)
      } else{
        #assigns the last digit based on the next value
        last.digit <- (i+1)*10^(digits-totdigits)  
      }
    }
    #checking if the current value is less than or equal to the desired value and the next value is greater than or equal to the desired value
    if((current_value <= val) & (next_value >= val)){
      if(abs(current_value-val) < abs(next_value-val)){
        last.digit <- i*10^(digits-totdigits)
      } else{
        last.digit <- (i+1)*10^(digits-totdigits)  
      }
    }  
  }
  #adding the last digit to the parameter value
  value <- value + last.digit
  #throwing an error if the value is below the minimum or above the maximum
  if((value < minimum) || (value > maximum)){
    stop("Function does not take on desired value between the specified maximum and minimum")
  }
  #plugging in the parameter estimate into the function
  functionresult <- do.call(f, c(prams, value))
  #turning off warnings
  options(warn = -1)
  #trying to plug 1 less than the parameter estimate into the function
  lower_value <- try(do.call(f, c(prams, value-1)), silent = T)
  if(class(lower_value) != "numeric"){
    #if 1 less than the parameter estimate is not valid for the function, then use the minimum instead
    lower_value <- do.call(f, c(prams, minimum))
  }
  #gives an error message if the function value using the parameter estimate is farther from the desired value than a lower value of the parameter
  if((abs(functionresult-val) > (abs(lower_value-val)))){
    stop("Function does not take on desired value between the specified maximum and minimum.")  
  }
  #trying to plug 1 more than the parameter estimate into the function
  greater_value <- try(do.call(f, c(prams, value+1)), silent = T)
  if(class(greater_value) != "numeric"){
    #if 1 more than the parameter estimate is not valid for the function, then use the maximum instead
    greater_value <- do.call(f, c(prams, maximum))
  }
  #gives an error message if the function value using the parameter estimate is farther from the desired value than a greater value of the parameter
  if((abs(functionresult-val) > (abs(greater_value-val)))){
    stop("Function does not take on desired value between the specified maximum and minimum.")
  }
  #turning warnings back on
  options(warn = 0)
  #gives a warning if the function value is more than 1 away from the desired value
  if(abs(functionresult - val) > 1){
    warning <- c("Result is more than one different than value searching for. Try adjusting decimals or maximum and minimum.")
    #outputs a warning, the parameter estimate, and the function value when the parameter estimate is plugged into the function
    output <- list(warning = warning, parameter_estimate = value, function_value = functionresult)
  } else{
    #outputs the parameter estimate and the function value when the parameter estimate is plugged into the function
    output <- list(parameter_estimate = value, function_value = functionresult)
  }
  return(output)
}
