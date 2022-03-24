## An T F index for alle duplicated entries, not only the duplicated one
#' @title Show all duplicated values in a vector
#' @description Prints index or values of duplicates in a vector.
#' @param x Vector: The values to be checked for duplicates
#' @param index Logical: Return index of duplicated entries or the values
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname show_duplicated_hk
#' @export

show_duplicated_hk <- function(x, index = T) {

  # check for length of given vector
  if (length(x) == 0) {
    return(0)
  }

  # rename input
  my.vec <- x

  # create data.table with index
  dt <- data.table::data.table(my.vec, index = 1:length(my.vec))

  # get the duplicates from R's base duplicated function
  duplicated.values <- my.vec[duplicated(my.vec)]

  # get the unique duplicates
  unique.duplicated.values <- unique(duplicated.values)

  if(index == T) {

    # find all entries matching the duplicated values
    duplicated.entries <- dt[my.vec %in% unique.duplicated.values, ]

    # # key by duplicated.entries
    data.table::setkey(duplicated.entries, my.vec)

    # return index
    return(duplicated.entries$index)

    # return logical vector the length of the input
  } else if(index == F) {

    # find all elements matching the duplicated values
    duplicates.logical <- is.element(dt$my.vec, unique.duplicated.values)

    #return
    return(duplicates.logical)
  } else stop("Please set a valid index indicator: T/F")
}
