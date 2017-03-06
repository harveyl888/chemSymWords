#' Represent a string using chemical elements
#'
#' Represent a string using chemical elements
#'
#' @param w Character.  A word to convert to chemical elements
#' @param output Character.  Specify the output.  \code{min} returns the fewest elements,
#'   \code{max} returns the most elements, \code{list} returns a list of all hits
#' @param sym Data Frame.  A data frame containing chemical information.  Defaults to elements
#'
#' @return Character vector of elements if successful or FALSE if not all characters can be assinged to elements
#'
#' @export
chemWord <- function(w, output='min', sym=elements) {
  if (!output %in% c('min', 'max', 'list')) output <- 'list'
  symbolList <- chemWordRecurse(w, list(), sym$Symbol)
  if (length(symbolList) == 0) {
    return(FALSE)
  } else {
    if (output == 'min') {
      ref <- which.min(lapply(symbolList, length))
      return(sym$Symbol[symbolList[[ref]]])
    } else if (output == 'max') {
      ref <- which.max(lapply(symbolList, length))
      return(sym$Symbol[symbolList[[ref]]])
    } else {
      return(sapply(symbolList, function(x) sym$Symbol[x]))
    }
  }
}


#' recurse a string identifying chemical elements
#'
#' Recursive function to find chemical elements within a string
#'
#' @param w Character.  Text to search for chemical elements
#' @param t List.  Running list of partial matches for current search path
#' @param sym Character vector.  A vector of chemical symbols
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
    for (i in 1:min(max(nchar(sym)), nchar(w))) {
      find_el <- match(toupper(substring(w, 1, i)),toupper(sym))
      if (!is.na(find_el)) {
        Recall(substring(w, i + 1), c(t, find_el), sym)
      }
    }
  }
  chemWordRecurseInternal(w, t, sym)
  return(out)
}


#' Create a PNG representation using chemical elements
#'
#' Output a PNG file of chemical elements for a particular word
#'
#' @param w Character.  A word to convert to chemical elements
#' @param sym Data Frame.  A data frame containing chemical information.  Defaults to elements
#' @param f Character.  Filename for output file (including path)
#'
#' @return Boolean.  True if file generated, false if word could not be represented by chemical elements
#'
#' @import grid
#'
#' @export
chemWordPNG <- function(w, sym=elements, f='chemWord.png') {
  symbolList <- chemWordRecurse(w, list(), sym$Symbol)
  if (length(symbolList) == 0) {
    return(FALSE)
  } else {
    chemRef <- symbolList[[which.min(lapply(symbolList, length))]]
    nboxes <- length(chemRef)  ## Number of elements
    boxSize <- 100  ## Size for each symbol (in pixels)
    totalWidth <- nboxes * 1.06 * boxSize  ## Width of png output (in pixels)
    png(f, width = totalWidth, height = boxSize * 1.06, res = 72)
    vp = viewport(x = 0.5, y = 0.5, width = unit(totalWidth, 'points'), height = unit(boxSize, 'points'))
    grid.newpage()
    pushViewport(vp)

    for (i in 1:nboxes) {
      xC <- boxSize / 2 + (i-1)*boxSize + ((i-1)*2+1)*0.03*boxSize
      yC <- boxSize / 2
      grid.rect(x = xC, y = yC, width = boxSize, height = boxSize, default.units = "points")
      grid.text(label = sym$Symbol[chemRef[i]],
                x = xC,
                y = yC,
                just = c('center', 'center'),
                default.units = "points",
                gp=gpar(col='black', fontsize = 0.33 * boxSize))  ## Chemical Symbol
      xTop <- xC - boxSize * 0.45
      yTop <- boxSize * 0.9
      grid.text(label = sym$Atomic_Number[chemRef[i]],
                x = xTop,
                y = yTop,
                just = c('left', 'center'),
                default.units = 'points',
                gp = gpar(col = 'black', fontsize = 0.15 * boxSize))  ## top line - atomic number
      xBottom <- xC
      yBottom <- boxSize * 0.15
      grid.text(label = sym$Name[chemRef[i]],
                x = xBottom,
                y = yBottom,
                just = c('center', 'center'),
                default.units = 'points',
                gp = gpar(col = 'black', fontsize = 0.15 * boxSize))  ## bottom line - name
    }
    dev.off()
    return(TRUE)
  }
}
