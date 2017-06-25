#' @name compute_inverse
#'
#' @export
#'
#' @title Compute inverse of a function
#'
#' @description Ask for a function and compute its inverse
#'
#' @param x A value between 0 and 1
#'
#' @author Som Bohora
#'
#' @examples
#' compute_inverse(0.6)

compute_inverse <- function(x){
  -log((1/x)-1)
}
