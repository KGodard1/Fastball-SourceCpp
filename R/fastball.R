#' Randomize a binary matrix using Fastball
#'
#' `fastball` randomizes a 0/1 matrix, preserving the row and column sums
#'
#' @param B matrix: a 0/1 matrix
#' @param R integer: number of rows in `B`
#' @param C integer: number of columns in `B`
#'
#' @return a 0/1 matrix randomly sampled from the space of all 0/1 matrices with the same row and column sums as B
#'
#' @details `fastball` is an optimized C++ implementation of the curveball algorithm for generating random
#'    0/1 matrices. If `fastball` is used in a loop to randomly sample from the space of all 0/1 matrices
#'    with the row and column sums as `B`, it is faster if `B` is supplied as an indexed list of the locations
#'    of the 1s, which can be generated using `apply(B==1, 1, which)`. This indexed list should be generated just
#'    once, outside the loop. When `B` is supplied as an indexed list, `R` and `C` must be specified.
#'
#' @details To maximize speed, `fastball` does not perform any parameter checks.
#'
#' @references curveball algorithm: {Strona, Giovanni, Domenico Nappo, Francesco Boccacci, Simone Fattorini, and Jesus San-Miguel-Ayanz. 2014. “A Fast and Unbiased Procedure to Randomize Ecological Binary Matrices with Fixed Row and Column Totals.” Nature Communications 5 (June). Nature Publishing Group: 4114. \doi{10.1038/ncomms5114}}
#'
#' @export
#'
#' @examples
fastball <- function(B, R = nrow(B), C = ncol(B)) {
  if (class(B)=="matrix" | class(B)==c("matrix", "array")) {B <- apply(B==1, 1, which)}  #If a matrix is provided, convert to an indexed list
  star <- fastball_cpp(B, c(R,C))  #Run fastball C++

  ### Fastball should always return a matrix
  ### Would it be faster to make this last step part of fastball_cpp
  Bstar <- matrix(0,R,C)  #Initialize randomized matrix
  for (row in 1:R) {Bstar[row,star[[row]]] <- 1}  #Convert list to matrix
  ###

  return(Bstar)
}
