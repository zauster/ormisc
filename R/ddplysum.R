#' Sum a column by group
#'
#' Take a data.frame, separate it by the variables given and calculate
                                        #the sum for the column given
#' @param data the data.frame
#' @param variables the variables to group by, use .(column1, column2)
#' @param tosum the column to be summed
#' @param naswitch logical, should NAs be ignored?
#' @export
ddplysum <- function(data, variables, tosum, naswitch = FALSE)
    {
        data <- ddply(data, variables, function(df) sum(df[, tosum], na.rm = naswitch))
        colnames(data)[colnames(data) == "V1"] <- as.character(tosum)
        data
    }

#' Calculate a growth rate
#'
#' Add a growth rate column for a given column in a data.frame
#' @param data the data.frame
#' @param variables the variables to group by
#' @param naswitch logical, should we add a NA at the beginning? If
#' TRUE, we add the growth rate to the original data.frame. If FALSE,
#' a new data.frame will be returned
#' @export
ddplygr <- function(data, variables, naswitch = TRUE)
    {
        data <- data[order(data$year), ]
        if(naswitch == TRUE)
            {
                res <- ddply(data, variables, transform,
                             gr = 100 * c(NA, exp(diff(log(value)) / abs(diff(year))) - 1))
            }
        else
            {
                res <- ddply(data, variables,
                             function(df) 100 * (exp(diff(log(df$value)) / abs(diff(df$year))) - 1))
                colnames(res)[colnames(res) == "V1"] <- "gr"
            }
        res
    }
