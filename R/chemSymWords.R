##
## generate chem words
##

#' chemWord
#'
#' Generate word using chemical elements
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

