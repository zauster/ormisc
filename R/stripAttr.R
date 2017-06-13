#' @title Remove all attributes from a column of a data.frame/data.table
#' @name stripAttr
#' @description Removes attributes of a column as they are often unnecessary. Found on stackoverflow.
#' @param data a columns of a data.frame or data.table
#' @return a column cleaned from all attributes except for
#' names, row.names, and class
#' @export
stripAttr <- function(col) {
  attr <- names(attributes(col))
  keepAttr <- c("names", "row.names", "class", ".internal.selfref")
  for(j in attr[!attr %in% keepAttr]) {
      attr(col, j) <- NULL
  }
  return(col)
}

#' @title Remove all attributes from all columns of a data.table
#' @name unsetAllAttr
#' @description Removes all attributes of all columns as they are often unnecessary. Found on stackoverflow and extendeed.
#' @param dt a data.table
#' @return a cleaned data.table
#' @export
unsetAllAttr <- function(dt) {
    dt[, (names(dt)) := lapply(.SD, stripAttr), .SDcols = names(dt)]
}
