#' Represent a string using chemical elements
#'
#' Represent a string using chemical elements
#'
#' @param w Character.  A word to convert to chemical elements
#' @param sym Data Frame.  A data frame containing chemical information.  Defaults to elements
#'
#' @return Character vector of elements if successful or FALSE if not all characters can be assinged to elements
#'
#' @export
chemWord <- function(w, sym=elements) {
  symbolList <- chemWordRecurse(w, list(), sym$Symbol)
  if (length(symbolList) == 0) {
    return(FALSE)
  } else {
    ref <- which.min(lapply(symbolList, length))
    return(sym$Symbol[symbolList[[ref]]])
  }
}

#' recurse a string identifying chemical elements
#'
#' Recursive function to find chemical elements within a string
#'
#' @param w Character.  Text to search for chemical elements
#' @param t List.  Running list of partial matches for current search path
#' @param sym Data frame.  A data frame containing chemical information
#'
#' @return List of results.  Each list item refers to a positive result in representing the
#'   input string as a series of elements.  List items are vectors corresponding to row
#'   positions of the elements in \code{sym}.
chemWordRecurse <- function(w, t, sym=elements) {
  out <- list()

  chemWordRecurseInternal <- function(w, t, sym) {
    if (nchar(w) == 0) {
      out[[length(out) + 1]] <<- unlist(t)
    }
    for (i in 1:min(2, nchar(w))) {
      find_el <- match(toupper(substring(w, 1, i)),toupper(sym))
      if (!is.na(find_el)) {
        Recall(substring(w, i + 1), c(t, find_el), sym)
      }
    }
  }
  chemWordRecurseInternal(w, t, sym)
  return(out)
}

