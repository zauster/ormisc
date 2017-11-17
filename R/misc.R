#' Display a 'n' random rows of a data.frame
#'
#' Given a data.frame, display a random selection of it. To see more
#' than just the head or tail of the data
#' @param df the data.frame
#' @param n the number of rows to be displayed
#' @export
sampleview <- function(df, n = 6)
    {
        toshow <- sample(1:nrow(df), size = n)
        df[toshow, ]
    }

#' Test and convert to numeric
#'
#' Test if a given vector is numeric and if yes, then convert it to
#' numeric. Copied from Stackoverflow.
#' @param col a given vector
#' @return perhaps a numeric vector
#' @export
convert.numeric <- function(col) {
    test.col <- suppressWarnings(as.numeric(as.character(col[!is.na(col)])))
    if(all(!is.na(test.col))) {
        as.numeric(as.character(col))
    } else {
        col
    }
}


#' Convert a factor to character
#'
#' Given a factor vector, create a character vector from it
#' @param factor the factor vector
#' @export
fac2char <- function(factor)
    {
        as.character(levels(factor))[factor]
    }


#' Convert a factor to a number
#'
#' Given a factor vector, create a numeric vector from it
#' @param factor the factor vector
#' @export
fac2num <- function(factor)
    {
        as.numeric(levels(factor))[factor]
    }


#' Sort unique vector
#'
#' Given a vector, get the sorted unique values
#' @param x the vector
#' @export
sun <- function(x) sort(unique(x))


#' Difference in sets
#'
#' Given two vectors, get the element of the first vector that are NOT
#' in the second vector
#' @param x first vector
#' @param y second vector
#' @export
"%nin%" <- function(x, y) !(x %in% y)


#' Remove all
#'
#' Remove all objects from the environment
#' @export
rmall <- function() rm(list = ls(envir = .GlobalEnv),
                       envir = .GlobalEnv)


#' Extract a position in a vector
#'
#' Given a vector, return a vector with all zeros except at 'position'
#' @param x the vector
#' @param position the position
#' @export
getInd <- function(x, position)
    {
        res <- vector(length = length(x))
        res[position] <- x[position]
        res
    }

#' Code the time
#'
#' Turn the output of `Sys.time` into a long number
#' @return a 15 char long number
#' @export
codeTime <- function() {
    gsub("-", "", gsub(":", "", gsub(" ", "_", Sys.time())))
}
