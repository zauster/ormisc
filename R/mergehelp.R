#' Show corresponding (or not) factor levels
#'
#' Given two datasets, show what factor levels coincide, and which can
#' only be found in one of the two datasets
#' @param x first dataset
#' @param y two dataset
#' @param by.x factor in first dataset
#' @param by.y fatcor in second dataset
mergehelp <- function(x, y, by.x, by.y = by.x)
{
    if(is.data.table(x)) {
        xby <- x[, sort(unique(by.x)), with = FALSE][[by.x]]
    }
    else {
        xby <- sort(unique(x[, by.x]))
    }

    if(is.data.table(y)) {
        yby <- y[, sort(unique(by.y)), with = FALSE][[by.y]]
    }
    else {
        yby <- sort(unique(y[, by.y]))
    }

    cat("\n\nIn x, but NOT in y:\n")
    cat(setdiff(xby, yby), sep = ";  ")

    cat("\n\nIn y, but NOT in x:\n")
    cat(setdiff(yby, xby), sep = ";  ")

    cat("\n")
}
