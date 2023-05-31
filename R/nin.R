#' @title Determine Elements Not Present in Another Vector
#' @description This function returns a logical vector indicating which elements of `x` are not present in `y`.
#' @param x A vector of any type (e.g., numeric, character) whose elements are to be checked against the elements of `y`.
#' @param y A vector of any type (e.g., numeric, character) that serves as a reference for checking the presence of elements in `x`.
#' @return A logical vector with the same length as `x`, with each element set to TRUE if the corresponding element in `x` is not present in `y`, and FALSE otherwise.
#' @details This function uses the `%in%` operator to compare the elements of `x` and `y`, and then negates the result to obtain a logical vector indicating the absence of elements in `y`. It can be used for filtering, subset selection, or conditional operations on data.
#' @examples
#' if(interactive()){
#'  # Sample vectors
#'  x <- c(1, 2, 3, 4, 5)
#'  y <- c(3, 4, 5, 6, 7)
#'
#'  # Use `%nin%` to find elements of x not present in y
#'  result <- x %nin% y
#'  print(result) # Output: TRUE TRUE FALSE FALSE FALSE
#'  }
#' @export

##..................................................................................
## data manipulation and handling
##..................................................................................
## Returns a logical vector TRUE for elements of X not in Y
`%nin%` <- function(x, y) !(x %in% y)
