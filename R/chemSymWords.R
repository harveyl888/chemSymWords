#' Represent a string using chemical elements
#'
#' Represent a string using chemical elements
#'
#' @param w Character.  A word to convert to chemical elements
#' @param output Character.  Specify the output.  \code{min} returns the fewest elements,
#'   \code{max} returns the most elements, \code{list} returns a list of all hits
#' @param sym Data Frame.  A data frame containing chemical information.  Defaults to elements
#' @param additional Data Frame.  Optional additional element table.  Data frame which must
#'   have a \code{Symbol} column containing the chemical symbol
#'
#' @return Character vector of elements if successful or FALSE if not all characters can be assinged to elements
#'
#' @importFrom dplyr bind_rows
#'
#' @export
chemWord <- function(w, output='min', sym=elements, additional=NULL) {
  if (!output %in% c('min', 'max', 'list')) output <- 'min'  ## default to min
  if (!is.null(additional)) sym <- bind_rows(additional, sym)  ## add additional elements
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
#' @param line1 Vector containing two elements.  Top row label description.  First element
#'   corresponds to a column ID in the \code{sym} data frame.  Second element corresponds
#'   to the horizontal positioning (left, right, center).  To remove this line from the PNG
#'   output set it to c(NA, NA)
#' @param line2 Vector containing two elements.  Second row label description.  See \code{line1} description
#' @param line3 Vector containing two elements.  Bottom row label description.  See \code{line1} description
#' @param additional Data Frame.  Optional additional element table.  Data frame which must
#'   have a \code{Symbol} column containing the chemical symbol and can also contain
#'   \code{Atomic_Number}, \code{Name} and \code{Atomic_Mass} for inclusion in the PNG
#' @param colorInvert Boolean.  Invert colors.  If true then elements will be white colored on a
#'   black background.  Default is false (black text on white background)
#' @param rounded Boolean.  Rounded box corners.  Default is false
#' @param f Character.  Filename for output file (including path)
#'
#' @return Boolean.  True if file generated, false if word could not be represented by chemical elements
#'
#' @import grid
#' @importFrom dplyr bind_rows
#'
#' @export
chemWordPNG <- function(w,
                        sym=elements,
                        line1=c('Name', 'center'),
                        line2=c('Atomic_Number', 'center'),
                        line3=c('Atomic_Mass', 'center'),
                        additional=NULL,
                        colorInvert = FALSE,
                        rounded = FALSE,
                        f='chemWord.png') {
  if (!is.null(additional)) sym <- bind_rows(additional, sym)  ## add additional elements

  l.text <- list(line1, line2, line3)
  l.text <- lapply(l.text, function(x) {
    if (!x[1] %in% names(sym)) x[1] <- NA
    if (!x[2] %in% c('left', 'right', 'centre', 'center')) x[2] <- 'center'
    x
  })

  if (colorInvert) {
    backCol <- 'black'
    foreCol <- 'white'
  } else {
    backCol <- 'white'
    foreCol <- 'black'
  }

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

    for (i in 1:nboxes) {  ## loop through each identified symbol
      xC <- boxSize / 2 + (i-1)*boxSize + ((i-1)*2+1)*0.03*boxSize
      yC <- boxSize / 2
      if (rounded) {
        grid.roundrect(x = xC,
                       y = yC,
                       width = boxSize,
                       height = boxSize,
                       default.units = "points",
                       gp=gpar(col=foreCol, fill=backCol))
      } else {
        grid.rect(x = xC,
                  y = yC,
                  width = boxSize,
                  height = boxSize,
                  default.units = "points",
                  gp=gpar(col=foreCol, fill=backCol))
      }
      grid.text(label = ifelse(is.na(sym$Symbol[chemRef[i]]), '', sym$Symbol[chemRef[i]]),
                x = xC,
                y = yC,
                just = c('center', 'center'),
                default.units = "points",
                gp=gpar(col=foreCol, fontsize = 0.33 * boxSize))  ## Chemical Symbol
      lapply(seq_along(l.text), function(x) {  ## Apply other labels
        if(l.text[[x]][2] == 'left') {
          xText <- xC - boxSize * 0.45
        } else if (l.text[[x]][2] == 'right') {
          xText <- xC + boxSize * 0.45
        } else {
          xText <- xC
        }
        if (x == 1) {
          yText <- boxSize * 0.9
        } else if (x == 2) {
          yText <- boxSize * 0.75
        } else {
          yText <- boxSize * 0.15
        }
        grid.text(label = ifelse(is.na(sym[[l.text[[x]][1]]][chemRef[i]]), '', sym[[l.text[[x]][1]]][chemRef[i]]),
                  x = xText,
                  y = yText,
                  just = c(l.text[[x]][2], 'center'),
                  default.units = 'points',
                  gp = gpar(col = foreCol, fontsize = 0.15 * boxSize))  ## top line - atomic number
      })
    }
    dev.off()
    return(TRUE)
  }
}
