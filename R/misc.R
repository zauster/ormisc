#' Finite data.frame
#'
#' Test if a data.frame only contains finite values
#' @param df the data.frame
#' @export
is.finite.data.frame <- function(df)
    {
        sapply(df, is.finite)
    }

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

#' Display a 'n' random rows of a data.frame
#'
#' Given a data.frame, display a random selection of it. To see more
                                        #than just the head or tail of the data
#' @param df the data.frame
#' @param n the number of rows to be displayed
#' @export
sampleview <- function(df, n = 6)
    {
        toshow <- sample(1:nrow(df), size = n)
        df[toshow, ]
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

#' Conert a factor to a number
#'
#' Given a factor vector, create a numeric vector from it
#' @param factor the factor vector
#' @export
fac2num <- function(factor)
    {
        as.numeric(levels(factor))[factor]
    }
