% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_dt_cols_cb.R
\name{change_dt_cols_cb}
\alias{change_dt_cols_cb}
\title{Change matching entries in the specified columns of a data.table}
\usage{
change_dt_cols_cb(
  dat,
  columns = NA,
  from,
  to,
  stop_missing = TRUE,
  change_in_dat = FALSE
)
}
\arguments{
\item{dat}{Data.table or data.frame.}

\item{columns}{Vector. Columns of data.table/data.frame to search and replace values in. Defaults to NA, which will result in all columns being selected. Can either be a vector of character strings matching the column names or an integer vector matching the column indexes.}

\item{from}{String. Value or pattern to be replaced in the selected columns. Also accepts regular expressions.}

\item{to}{Single string that the values in 'from' are to be changed to.}

\item{stop_missing}{Logical. Whether to produce an error message when one of the columns given in 'columns' is not found in 'dat'.
Defaults to TRUE to avoid errors. If set to FALSE, the function will change values in 'from' to 'to' in the columns specified in 'columns' that were found in 'dat'.}

\item{change_in_dat}{Logical. Defaults to TRUE. Whether to return the changed data.table as output to be assigned to a new variable (change_in_dat = FALSE) or whether to change dat directly without returning a new data.table with the specified changes, i.e. commit the changes in the data.table supplied to 'dat' directly (change_in_dat = TRUE).}
}
\description{
Search a data.table 'dat' for a pattern ('from') and replace it with a given value ('to'). Changes can be made in the original data.table supplied to 'dat' directly or returned as a copy of 'dat' with the specified modifications, leaving the original 'dat' unchanged (default).
}
\details{
This functions simplifies applying identical changes of multiple (or all) columns of a data.table. Data.table is great for replacing filtered rows within a single column, but replacing values across columns can require the use of repetitive statements or the use of an 'lapply'-loop within the assignment in 'j' of the data.table. This function implements a use case of the latter for searching and replacing values in any combination of columns of a data.table. This works for single values or a regular expression pattern.
}
\examples{
\dontrun{

# have a data.table
data(mtcars)
setDT(mtcars)

# we can change several columns at once, for example change all entries of the number 3 to NA in the columns 9, 10 and 11
# this will return a data.table with the selected changes:
change_dt_cols_cb(dat = mtcars, columns = c(9,10,11), from = 3, to = NA)

# we can also supply a character vector with the selected columns, resulting in the same changes
change_dt_cols_cb(dat = mtcars, columns = c("am", "gear", "carb"), from = 3, to = NA)

# without supplying a 'columns'-vector, changes will be made in all columns
change_dt_cols_cb(dat = mtcars, from = 3, to = NA)

# when using column indexes, selecting an unavailable index will result in an error by default
change_dt_cols_cb(dat = mtcars, columns = c(9, 10, 11, 12), from = 3, to = NA)

# this behaviour can be disabled by setting 'stop_missing = FALSE'
change_dt_cols_cb(dat = mtcars, columns = c(9, 10, 11, 12), from = 3, to = NA, stop_missing = FALSE)

# when dat should be changed directly, change_in_dat = TRUE can be used
mtcars #  original, unchanged data -> the above commands did not change it

# this command will change mtcars without the need for an assignment statement
change_dt_cols_cb(dat = mtcars, from = 3, to = NA, change_in_dat = TRUE)

# mtcars will have the changes applied by the function
mtcars

}

}
