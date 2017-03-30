complete <- function(directory, id = 1:332) {
    values <- c()

    for (i in id) {
        filename <- str_pad(i, 3, pad='0')
        filename <- paste(filename, 'csv', sep='.')
        filename <- paste(directory, filename, sep='/')
        data <- read.csv(filename)

        complete_count <- nrow(data[complete.cases(data),])
        values <- append(values, complete_count)
    }

    data.frame(id, 'nobs'=values)
}
