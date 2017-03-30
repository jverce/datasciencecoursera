library(stringr)


pollutantmean <- function(directory, pollutant, id = 1:332) {
    measurements <- c()

    for (i in id) {
        filename <- str_pad(i, 3, pad='0')
        filename <- paste(filename, 'csv', sep='.')
        filename <- paste(directory, filename, sep='/')
        data <- read.csv(filename)

        data <- data[pollutant]
        data <- data[!is.na(data)]
        measurements <- append(measurements, data)
    }

    mean(measurements)
}
