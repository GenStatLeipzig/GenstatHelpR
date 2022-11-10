#' @title advanced matching
#' @description customized version of match() function, checking for duplicate IDs and avoiding using NAs as matching-identifyer
#' @param x Vector of IDs from the target table that should be extended with look-up information, see match()
#' @param y Vector of IDs from the lookup table, see match()
#' @param testunique test that the same ID is not present in the look-up table serveral times, Default: T
#' @param makeunique when the same ID is present several times, check whether the information intended to be imported is unambigously linked to a single ID, requires specification of importcol, Default: F
#' @param importcol the vector from the lookup table that includes the information that should be matched to the target table, required when makeunique =T, Default: NULL
#' @param showMessages show some details , Default: T
#' @param ... additional parameters handed to match() function
#' @return vector of same length as x (target data) or NA if there is no match, see match()
#' @details see match() function
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  lookuptable = data.table::data.table(index = letters[1:3], lookupinfo = c(3,4,7))
#'  targettable = data.table::data.table(schluessel = rev(letters[1:3]))
#'  targettable[,lookupinfo := lookuptable[match_hk(targettable$schluessel, lookuptable$index), lookupinfo]]
#'  targettable
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{na.fail}}
#'  \code{\link[data.table]{data.table-package}}
#' @rdname match_hk
#' @export
#' @importFrom stats na.omit
#' @importFrom data.table data.table
#' @importFrom utils installed.packages
#' @author holger.kirsten@imise.uni-leipzig.de

match_hk = function(x, y, testunique =T, makeunique = F,importcol = NULL,showMessages = T, ...) {
  ##150122 makeunique = F statt T, na.omit bei duplicated y, fehlenden ok fall includiert
  ##160328 check auf gleiche laenge x und improtcol
  ##160616 match hk zeigt die duplikated zeilen statt mytabl falls ein Fehler kommt
  #   x = transkripte_eqtl$nuid
  #   y = ilmnAnnot013$nuid
  ##180530 data.table aware

  yname = deparse(substitute(y))

  # 150119 unique check auf schnelles duplicated umgestellt, auto makeuniuq
  if(testunique ==T){
    check = as.numeric(sum(duplicated(stats::na.omit(y))))
    if(identical(check, 0)) return(match(x, y, incomparables=c(NA, NaN),...))

    if(identical(check, 0)==F  & makeunique == F) {
      if(showMessages ==T) message("Duplicated entries:\n", paste(y[duplicated(y)], collapse = "\n"))
      stop(paste(yname ,"ist nicht unique"))
    }

    if(identical(check, 0)==F  & makeunique == T) {

      ## try to make it nunique
      if(is.null(importcol)) stop("When asking for make unique, please provide vector with values to be imported")
      if(length(importcol) != length(y)) stop("When asking for make unique, please provide vector with values to be imported")

      datatable_da = "data.table" %in%  rownames(installed.packages())
      datatable_da
      if(datatable_da) {
        matcher = unique(data.table::data.table(index = y, importcol = importcol))
        matcher = matcher[ index %in% x]
        matchercheck = matcher[,as.numeric(sum(duplicated(stats::na.omit(index))))]
        if(identical(matchercheck, 0)==F  ) {
          if(showMessages ==T) print(matcher[show_duplicated_hk(matcher$index)])
          stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
        }
      }

      if(datatable_da==F) {
        matcher = unique(data.frame(index = y, importcol = importcol))
        matcher = matcher[ matcher$index %in% x,]
        matchercheck = as.numeric(sum(duplicated(stats::na.omit(matcher$index))))
        if(identical(matchercheck, 0)==F  ) {
          if(showMessages ==T) print(matcher[show_duplicated_hk(matcher$index),])
          stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
        }
      }
      return(match(x, y, incomparables=c(NA, NaN),...))

    }

  }
  if(testunique ==F)  return(match(x, y, incomparables=c(NA, NaN),...))
}
