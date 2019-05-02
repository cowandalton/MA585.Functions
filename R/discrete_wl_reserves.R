#' Discrete Whole Life Insurance Reserve Calculator
#' 
#' This function calculates reserves for discrete whole life insurance that pays death benefits at the end of the year of death.
#' @param prem is the premium paid by each policyholder that is alive at the start of each year (default = 5000).
#' @param interest should be a vector of interest rates expressed as decimals.
#' @param mortality should be a vector of mortality rates given as decimals starting from birth.
#' @param number_alive is the number of policyholders alive when the life insurance first took effect.
#' @param benefit is the death benefit paid at the end of the year to each policyholder who dies.
#' @param seed_deaths is a seed for the normal distribution used when generating the number of policyholders who die each year (default = 462).
#' @param seed_rates is a seed for the normal distribution used when generating an interest rate for each year (default = 873).
#' @param start_age is the age of policyholders at when the life insurance first takes effect.
#' @return A dataframe will be returned containing the time, policyholders' age, the number of policyholders alive, the number of policyholders who died during that age, the insurance company's total premiums received for the year, the insurance company's claims for the year, and the interest rate applicable from time t to time t-1. 
#' @export 
#' @examples 
#' rates <- c(0.01, 0.02, 0.025, 0.021, 0.03)
#' mortality_probs_young <- seq(from = 0.0002, to = 0.01, length.out = 61)
#' mortality_probs_middle_age <- seq(from = 0.011, to = 0.06, length.out = 20)
#' mortality_probs_old <- seq(from = 0.065, to = 0.165, length.out = 10)
#' mortality_probs_older <- seq(from = 0.17, to = 0.85, length.out = 30)
#' mortality_probs <- c(mortality_probs_young, mortality_probs_middle_age, mortality_probs_old, mortality_probs_older)
#' results <- discrete_wl_reserves(prem = 10000, interest = rates, mortality = mortality_probs, start_age = 40)

discrete_wl_reserves <- function(prem = 5000, interest, mortality, number_alive = 10000, benefit = 400000, seed_deaths = 462, seed_rates = 873, start_age = 50){
  #prem is the premium paid by each policyholder that is alive at the start of each year
  #interest should be a vector of interest rates expressed as decimals
  #mortality should be a vector of mortality rates given as decimals starting from birth
  #number_alive is the number of policyholders alive when the life insurance first took effect
  #benefit is the death benefit paid at the end of the year to each policyholder who dies
  #seed_deaths is a seed for the normal distribution used when generating the number of policyholders who die each year
  #seed_rates is a seed for the normal distribution used when generating an interest rate for each year
  #start_age is the age of policyholders at when the life insurance first takes effect
  
  #creating a vector for the number of people alive from time 0 to time 120
  numalive <- rep(0,121)
  #setting the number of people alive at time 0 to the desired number
  numalive[1] <- number_alive
  #creating a vector for the number of people who die each year from time 0 to time 120
  numdeaths <- rep(0,121)
  t <- 0
  #setting a seed for generating the number of deaths each year
  set.seed(seed_deaths)
  #the loop stops when no more people are alive  
  while(numalive[t+1] > 0){
    #calculating how many people die each year using the binomial approximation to the normal distribution
    numdeaths[t+1] <- rnorm(1, mean = mortality[t+start_age+1]*numalive[t+1], sd = sqrt(mortality[t+start_age+1]*(1 - mortality[t+start_age+1])*numalive[t+1])) 
    #forcing the number of people who die each year to be at least 0
    if(numdeaths[t+1] < 0){
      numdeaths[t+1] <- 0
    }
    #calculating the number of people alive for each year by subtracting the number of people alive the previous year minus the number of pepole who died during that year 
    numalive[t+2] <- numalive[t+1] - numdeaths[t+1]
    t <- t + 1
  }
  #forcing the number of people who died during the last year to be exactly equal to the number alive at that time, so there are not more people who died than were alive
  numdeaths[t] <- numalive[t]
  #forcing the final number of people alive to 0
  numalive[t+1] <- 0
  #only keeping the number of people alive from time 0 to time t
  numalive <- head(numalive, t+1)
  #only keeping the number of deaths from time 0 to time t
  numdeaths <- head(numdeaths, t+1)
  time <- 0:t
  #creating a vector of ages
  age <- time + start_age
  #making a vector of premiums for time 0 to time t
  premiums <- rep(0, t+1)
  #calculating premium for time 0 by multiplying the annual premium by the number of people alive
  premiums[1] <- numalive[1]*prem
  #making a vector of reserves for time 0 to time t
  acreserves <- rep(0, t+1)
  #setting reserves at time 0 equal to the premium at time 0
  acreserves[1] <- premiums[1]
  #making a vector of claims for time 0 to time t
  claims <- rep(0, t+1)
  #making a vector of interest rates for time 0 to time t
  rates <- rep(0, t+1)
  #setting a seed for generating the interest rates for each year
  set.seed(seed_rates)
  #generating an interest rate for time 0 to time 1 using the normal distribution
  rates[1] <- rnorm(1, mean = mean(interest), sd = sd(interest))
  for(i in 2:(t+1)){
    #calculating the premium each year by multiplying the annual premium by the number of people alive
    premiums[i] <- numalive[i]*prem
    #calculating the claims for each year by multiplying the death benefit by the number of people who died the previous year 
    claims[i] <- numdeaths[i-1]*benefit
    #generating an interest rate for time t-1 to time t using the normal distribution
    rates[i] <- rnorm(1, mean = mean(interest), sd = sd(interest))
    #calculating reserves for each year by multiplying the previous year's reserves by one plus the interest rate, adding the current year's premium, and subtracting the current year's claims 
    acreserves[i] <- acreserves[i-1]*(1+rates[i-1])+premiums[i]-claims[i]
  }
  #creating a data frame of the results showing the age of policyholders, the number of policyholders alive, the number of policyholders who died, the reserves, the total premiums, the total claims, and the interest rate for each time
  finalresults <- data.frame(Time = time, Age = age, Number_alive = numalive, Deaths = numdeaths, Reserves = acreserves, Premiums = premiums, Claims = claims, Interest_Rates = rates)
  return(finalresults)
}
