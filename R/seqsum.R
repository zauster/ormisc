
seqsum <- function(mat, n = 3)
    {
        if(is.matrix(mat))
            {
                if(dim(mat)[1] != 1 & dim(mat)[2] != 1)
                    {
                        grp <- seq(1, ncol(mat), n)
                        sapply(grp,
                               function(x) rowSums(mat[, x:(x + n - 1)], na.rm = TRUE))
            }
                else {
                    mat <- as.vector(mat)
                    grp <- seq(1, length(mat), n)
                    sapply(grp,
                           function(x) sum(mat[x:(x + n - 1)], na.rm = TRUE))

                }
            }
        else
            {
                grp <- seq(1, length(mat), n)
                sapply(grp,
                       function(x) sum(mat[x:(x + n - 1)], na.rm = TRUE))
            }
    }
