corr <- function(directory, threshold = 0) {
    complete_cases <- complete(directory)
    result <- numeric()

    for (i in seq_len(nrow(complete_cases))) {
        id <- complete_cases[i,]$id
        count <- complete_cases[i,]$nobs
        if (count > threshold) {
            filename <- str_pad(id, 3, pad='0')
            filename <- paste(filename, 'csv', sep='.')
            filename <- paste(directory, filename, sep='/')
            data <- read.csv(filename)

            data <- data[complete.cases(data),]
            value <- cor(data$sulfate, data$nitrate)
            result <- append(result, value)
        }
    }

    result
}
