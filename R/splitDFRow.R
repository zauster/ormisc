#' Split a rows of a data.frame into several ones, using a character field
#'
#' From a character field with comma-separated codes, split the row in
#' several rows, one for each code in the character field
#' @param df the data.frame to be splitted
#' @param splitby a character sequence which will act as the separator
#' for the character field/column
#' @param column the column where the character codes are
#' @export
#' @examples
#' dta <- data.frame(a = 1:2, b = c("a, b, c", "d, e"), stringsAsFactors = FALSE)
#' splitDFRow(dta, ", ", "b")

splitDFRow <- function(df, splitby = ",", column)
    {
        ## split cells by "," and duplicate these lines:
        if(is.factor(df[, column]))
            stop("The specified column is not a character vector!")
        splitteddesc <- strsplit(df[, column] , splitby, fixed = TRUE)
        times <- sapply(splitteddesc, length)
        df.splitted <- df[rep(seq_len(nrow(df)), times), ]
        df.splitted[, column] <- unlist(splitteddesc)
        df.splitted[, column] <- gsub("^ ", "", df.splitted[, column])
        df.splitted[, column] <- gsub(" $", "", df.splitted[, column])

        return(df.splitted)
    }


#' Split a rows of a data.table into several ones, using a character field
#'
#' From a character field with comma-separated codes, split the row in
#' several rows, one for each code in the character field
#' @param dt the data.table to be splitted
#' @param splitby a character sequence which will act as the separator
#' for the character field/column
#' @param column the column where the character codes are
#' @export
#' @import data.table
#' @examples
#' dta <- data.table(a = 1:2, b = c("a, b, c", "d, e"), stringsAsFactors = FALSE)
#' dta2 <- splitDTRow(dta, ", ", "b")
splitDTRow <- function(dt, splitby = ",", column)
{
    setDT(dt)

    ## split cells by "," and duplicate these lines:
    if(is.factor(dt[, column, with = FALSE]))
        stop("The specified column is not a character vector!")

    ## split code-column by "," and duplicate these lines:
    splitteddesc <- strsplit(dt[, get(column)], splitby, fixed = TRUE)
    times <- sapply(splitteddesc, length)
    dt.splitted <- dt[rep(seq_len(nrow(dt)), times), ]
    setDT(dt.splitted)
    dt.splitted[, (column) := unlist(splitteddesc)]
    dt.splitted[, (column) := gsub("^ *", "", get(column))]
    dt.splitted[, (column) := gsub(" *$", "", get(column))]
    return(dt.splitted)
}
