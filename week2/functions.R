add2 <- function(x, y) {
    x + y
}

higher_than <- function(v, n = 10) {
    v[v > n]
}

higher_than_10 <- function(v) {
    higher_than(v)
}

columnmean <- function(x, remove_na = TRUE) {
    means <- c()
    for (i in seq_len(ncol(x))) {
        column <- x[,i]
        if (remove_na) {
            column <- column[!is.na(column)]
        }
        m <- mean(column)
        means <- append(means, m)
    }
    means
}

withfree <- function(x) {
    x + z
}
