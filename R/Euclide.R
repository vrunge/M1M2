##  GPL-3 License
## Copyright (c) 2021 Vincent Runge

#' Computing Euclidean Distance
#'
#' @description Euclidean Distance with the R function sum
#' @param v a vector of numeric data
#' @param w a vector of numeric data
#' @return the Euclidean distance between v and w
Euclide_R  <- function(v,w)
{
  return(sqrt(sum((v-w)^2)))
}

#' Computing Euclidean Distance with a for loop
#'
#' @description Computing Euclidean Distance with a for loop
#' @param v a vector of numeric data
#' @param w a vector of numeric data
#' @return the Euclidean distance between v and w
Euclide_R_for  <- function(v,w)
{
  n <- length(v)
  temp <- 0
  for(i in 1:n)
  {
    temp <- temp + (v[i]-w[i])^2
  }
  return(sqrt(temp))
}
