#' Extract strings from a vector of strings
#'
#' Using only base regex, return a matching string based on the posssibilites
#' entered in extract. Was intended to find a matching string when several
#' possibilites exist. For example, when searching str_extract(c("Case_Two",
#' "Case_One"), "One|Two"), the function will return the found regex in the
#' order they were found in the vector of strings, i.e. c("Two", "One").
#'
#' @param string character string or vector containing expressions to look for
#'
#' @param extract Regular expression to look for in string
#'
#' @export

str_extract_cb <- function(string, extract){
  str.extract <- substring(
    string,
    regexpr(extract, string),
    regexpr(extract, string) + attr(regexpr(extract, string), "match.length") - 1
  )
  return(str.extract)
}
