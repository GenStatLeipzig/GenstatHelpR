#' Change matching entries in the specified columns of a data.table
#'
#' Search a data.table 'dat' for a pattern ('from') and replace it with a given value ('to'). Changes can be made in the original data.table supplied to 'dat' directly or returned as a copy of 'dat' with the specified modifications, leaving the original 'dat' unchanged (default).
#'
#' @param dat Data.table or data.frame.
#'
#' @param columns Vector. Columns of data.table/data.frame to search and replace values in. Defaults to NA, which will result in all columns being selected. Can either be a vector of character strings matching the column names or an integer vector matching the column indexes.
#'
#' @param from String. Value or pattern to be replaced in the selected columns. Also accepts regular expressions.
#'
#' @param to Single string that the values in 'from' are to be changed to.
#'
#' @param stop_missing Logical. Whether to produce an error message when one of the columns given in 'columns' is not found in 'dat'.
#' Defaults to TRUE to avoid errors. If set to FALSE, the function will change values in 'from' to 'to' in the columns specified in 'columns' that were found in 'dat'.
#'
#' @param change_in_dat Logical. Defaults to TRUE. Whether to return the changed data.table as output to be assigned to a new variable (change_in_dat = FALSE) or whether to change dat directly without returning a new data.table with the specified changes, i.e. commit the changes in the data.table supplied to 'dat' directly (change_in_dat = TRUE).
#'
#' @details This functions simplifies applying identical changes of multiple (or all) columns of a data.table. Data.table is great for replacing filtered rows within a single column, but replacing values across columns can require the use of repetitive statements or the use of an 'lapply'-loop within the assignment in 'j' of the data.table. This function implements a use case of the latter for searching and replacing values in any combination of columns of a data.table. This works for single values or a regular expression pattern.
#'
#' @examples
#' \dontrun{
#'
#' # have a data.table
#' data(mtcars)
#' setDT(mtcars)
#'
#' # we can change several columns at once, for example change all entries of the number 3 to NA in the columns 9, 10 and 11
#' # this will return a data.table with the selected changes:
#' change_dt_cols_cb(dat = mtcars, columns = c(9,10,11), from = 3, to = NA)
#'
#' # we can also supply a character vector with the selected columns, resulting in the same changes
#' change_dt_cols_cb(dat = mtcars, columns = c("am", "gear", "carb"), from = 3, to = NA)
#'
#' # without supplying a 'columns'-vector, changes will be made in all columns
#' change_dt_cols_cb(dat = mtcars, from = 3, to = NA)
#'
#' # when using column indexes, selecting an unavailable index will result in an error by default
#' change_dt_cols_cb(dat = mtcars, columns = c(9, 10, 11, 12), from = 3, to = NA)
#'
#' # this behaviour can be disabled by setting 'stop_missing = FALSE'
#' change_dt_cols_cb(dat = mtcars, columns = c(9, 10, 11, 12), from = 3, to = NA, stop_missing = FALSE)
#'
#' # when dat should be changed directly, change_in_dat = TRUE can be used
#' mtcars #  original, unchanged data -> the above commands did not change it
#'
#' # this command will change mtcars without the need for an assignment statement
#' change_dt_cols_cb(dat = mtcars, from = 3, to = NA, change_in_dat = TRUE)
#'
#' # mtcars will have the changes applied by the function
#' mtcars
#'
#' }
#'
#'@import data.table
#'
#' @export
change_dt_cols_cb <- function(dat, columns = NA, from, to, stop_missing = TRUE, change_in_dat = FALSE) {

  # check 1
  if(missing(dat)){
    stop("Please assign a data.table, a list or data.frame to 'dat'.")
  } else if(!data.table::is.data.table(dat)) {
    if(!is.data.frame(dat) | !is.list(dat)) {
      stop("'dat' is not a data.frame and cannot be converted into a data.table. Please reconsider your input into 'dat'.")
    } else {
      try(data.table::setDT(dat))
    }
  }

  # check 2
  if(missing(to) | missing(from) | missing(dat)) {
    stop("Please assign a value to 'to' and 'from' and a data.table 'dat'.\nNo default was set (e. g. NA) to prohibit accidental deletions or entries.")
  } else if(length(to) != 1 | length(from) != 1) {
    stop("'to' and 'from' each need to be of length one.")
  } else if(identical(from, to)) {
    message("'from' has the same value as 'to'. No changes will be made")
    return(dat)
  }

  # check 3
  if(length(columns) == 1 & any(is.na(columns))) {
    # change in all columns of data.table by setting columns to names(dat)
    columns <- names(dat)
  } else if(is.numeric(columns)){
    # change numeric column entry into character vector of column names
    columns <- names(dat)[columns]
    if(any(is.na(columns)) & stop_missing == T) {
      stop("Some of your column indexes were not found in 'dat'.\nThis stop is was executed due to you setting stop_missing = T")
    } else if(any(is.na(columns)) & stop_missing == F) {
      message("Some or one of your entries into 'columns' was not found in 'dat'. However, I will continue with the columns I've found.")
      columns <- columns[!is.na(columns)]
    }
  } else if(any(!is.element(columns, names(dat)))) {
    stop("Some of your column names in 'columns' are not found in 'dat'")
  }

  # check 4
  if(change_in_dat == FALSE) {
    # get a copy of dat because data.table changes in source. I don't want that
    dat.dummy <- data.table::copy(dat)

    # actual computation happens here
    if(is.na(from)) {
      # use is.na(from) for finding elememts that are to be changed
      dat.dummy[ , (columns) := lapply(.SD, function(x){
        x[is.na(x)] <- to
        return(x)
      }), .SDcols = columns][]
    } else if(is.numeric(from)){
      dat.dummy[ , (columns) := lapply(.SD, function(x){
        x[x == (from)] <- to
        return(x)
      }), .SDcols = columns][]
    } else {
      dat.dummy[ , (columns) := lapply(.SD, function(x){
        x[grep(pattern = paste0("^", from, "$"), x = x)] <- to
        return(x)
      }), .SDcols = columns][]
    }
    return(dat.dummy)

    # other variant
  } else if(change_in_dat == TRUE) {

    # actual computation happens here
    if(is.na(from)) {
      # use is.na(from) for finding elememts that are to be changed
      dat[ , (columns) := lapply(.SD, function(x){
        x[is.na(x)] <- to
        return(x)
      }), .SDcols = columns][]
    } else if(is.numeric(from)){
      dat[ , (columns) := lapply(.SD, function(x){
        x[x == (from)] <- to
        return(x)
      }), .SDcols = columns][]
    } else {
      dat[ , (columns) := lapply(.SD, function(x){
        x[grep(pattern = paste0("^", from, "$"), x = x)] <- to
        return(x)
      }), .SDcols = columns][]
    }
  }
}

# future: add type of from: smaller, bigger, unequal, equal...
