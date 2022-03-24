#' Paste Date and (optional) folder to file name
#'
#' For easier naming of files to be saved. Adds the current date in the form of YY-MM-DD_ as a prefix to a character string. Optionally adds a folder name, too. Can be used with function time_stamp_cb to save and retreive files with time stamps.
#'
#' @param suffix Single character string that is a file name that the date should be d
#'
#' @param folder Give the name of an optional folder name (including "/") that is pasted in front of the filename.
#'
#' @export

time_stamp_cb <- function(suffix, folder = NULL){
  paste0(ifelse(grepl(pattern = "/$",x = folder), folder, paste0(folder, "/")), format(Sys.Date(), "%g%m%d"),"_", suffix)
}
