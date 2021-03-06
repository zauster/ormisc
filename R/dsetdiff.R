#' Double setdiff
#'
#' Do a double setdiff and (default) throw an error.
#' @title Double setdiff
#' @param setA first set, typically a character vector
#' @param setB second set
#' @param throwError boolean, should an error be thrown if differences exist? Default is TRUE
#' @return list of differences and an error
#' @export
#' @author Oliver Reiter
dsetdiff <- function(setA, setB, throwError = TRUE) {
  nameA <- deparse(substitute(setA))
  nameB <- deparse(substitute(setB))

  diff1 <- setdiff(setA, setB)
  diff2 <- setdiff(setB, setA)

  if((throwError && length(diff1) > 0) || (throwError && length(diff2) > 0)) {
    mesg1 <- paste0(nameA, " contains elements that are missing in ", nameB,
                   ": ", paste0(diff1, collapse = ", "))
    mesg2 <- paste0(nameB, " contains elements that are missing in ", nameA,
                   ": ", paste0(diff2, collapse = ", "))
    if(length(diff1) > 0 && length(diff2) > 0) {
      mesg <- paste0(mesg1, mesg2, sep = "\n")
    } else if(length(diff1) > 0) {
      mesg <- mesg1
    } else if(length(diff2) > 0) {
      mesg <- mesg2
    }
    stop(mesg)
  }

  if(throwError == FALSE) {
    res <- list(diff1, diff2)
    return(res)
  } else {
    return(NULL)
  }
}
