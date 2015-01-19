add2 <- function(x, y)
{
    x + y
}

above10 <- function(x)
{
    use <- x > 10
    x[use]
}




columnmean <- function(x, removeNA = TRUE)
{
    colnum <- ncol(x)
    means <- numeric(colnum)
    for (i in 1:colnum) 
    {
        means[i] <- mean(x[, i], na.rm = removeNA )
    }
    means
}