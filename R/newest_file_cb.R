#' Find The Newest File.
#'
#' Look for a file in the working directory or a subfolder and return a character string with the newest version of the file matching the queried character string. Can be combined with time_stamp_cb() to create and retreive time-stamped files.
#'
#' @param look_for Character string with the name, or part of the name, of the file to look for. Should be the file name without the time stamp. Example: When the newest version of file YYMMDD_myFile.csv is searched, enter "myFile.csv", omitting the prefix (that was added by time_stamp_cb()).
#'
#' @param subfolder Specify the subfolder to look for the file for when location is the working directory
#'
#' @param directory when Files are not in the working directory put the full path to the directory to search through here. Does not work with parameter subfolder
#'
#' @param true_end Whether the file name entered in look_for ends exactly as specified. If set to 'true_end = FALSE', will match files with partial matches. For example, look_for = "myFile" with true_end = TRUE will not match "myFile2", but setting true_end = FALSE will.
#'
#' @param print_full If TRUE will print complete path to the file
#'
#' @import here
#'
#' @export

# newest file of given name
newest_file_cb <- function(look_for = NA, subfolder = NA, directory = NA, true_end = TRUE, print_full = TRUE) {

  if (is.na(look_for)) {
    stop("Please specifiy a character string to look_for")
  }

  if (is.na(directory)) {

      # check whether subfolder is given and define folder to scan through for files
    if (is.na(subfolder)) {
      designation <- here::here()
    } else {

      # remove any / in case I forgot that I don't need them
      subfolder <- gsub(x = subfolder, pattern = "\\|/", replacement = "")
      designation <- here::here(subfolder)
    }
  } else {

    # check if directory exists
    stopifnot(dir.exists(directory))

    # set the foreign directory as designation
    designation <- directory

  }

  # list files matching look_for
  files <- list.files(designation)

  # check if there are any files in the designated folder
  if(identical(files, character(0))) stop("No files found!")

  # search for files matching query
  files.wanted <- files[grep(look_for, files)]

  # get information about last modification of files
  files.detailed <- file.info(files.wanted)

  # sort by modification date
  files.ordered <- files.detailed[with(files.detailed, order(as.POSIXct(ctime))), ]

  # get the newest file from the list
  files.newest <- rownames(utils::tail(files.detailed, n = 1))

  if (print_full == T) {
    if(!is.na(subfolder) & is.na(directory)) {
      files.newest <- here::here(subfolder, files.newest)
    } else if (is.na(subfolder) & is.na(directory)) {
      files.newest <- here::here(files.newest)

      # if directory is given, add slash and remove redunant symbols afterwards
    } else if (!is.na(directory)) {

      # windows and linux directories differ in "/" or "\". change, which symbol will be inserted based on what is found in directory
      if (length(grep(x = directory, pattern = "/"))) {
        files.newest <- paste0(designation, "/", files.newest)
        files.newest <- gsub(pattern = "(/)\\1+", replacement = "/", x = files.newest)
      }
    }
  } else if (print_full != T & print_full != F) {
    stop("print_full must be TRUE or FALSE")
  }

  message("Newest File is: ", files.newest)

  # return the newest file
  return(files.newest)
}
