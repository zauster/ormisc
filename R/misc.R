is.finite.data.frame <- function(obj)
    {
        sapply(obj, is.finite)
    }

getInd <- function(x, position)
    {
        res <- vector(length = length(x))
        res[position] <- x[position]
        res
    }

sampleview <- function(df, n = 6)
    {
        toshow <- sample(1:nrow(df), size = n)
        df[toshow, ]
    }

fac2char <- function(factor)
    {
        as.character(levels(factor))[factor]
    }

fac2num <- function(factor)
    {
        as.numeric(levels(factor))[factor]
    }
