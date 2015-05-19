ddplysum <- function(data, variables, tosum, naswitch = FALSE)
    {
        data <- ddply(data, variables, function(df) sum(df[, tosum], na.rm = naswitch))
        colnames(data)[colnames(data) == "V1"] <- as.character(tosum)
        data
    }

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
