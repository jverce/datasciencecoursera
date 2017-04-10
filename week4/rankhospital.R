rankhospital <- function(state, outcome, num = 'best') {
    validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
    validOutcomeIndexes <- c(11, 17, 23)

    outcomeData <- read.csv(
        'outcome-of-care-measures.csv', colClasses='character')
    listOfStates <- unique(outcomeData[, 7])

    if (!state %in% listOfStates) {
        stop('invalid state')
    } else if (!outcome %in% validOutcomes) {
        stop('invalid outcome')
    }

    indexFilter <- !is.na(validOutcomeIndexes[match(validOutcomes, outcome)])
    outcomeIndex <- validOutcomeIndexes[indexFilter]

    hospitalsInState <- outcomeData[outcomeData[, 7] == state, ]
    hospitalsInState[, outcomeIndex] <- as.numeric(hospitalsInState[, outcomeIndex])
    hospitalsInState <- hospitalsInState[!is.na(hospitalsInState[, outcomeIndex]), ]

    if (num == 'best') {
        ordered <- hospitalsInState[order(hospitalsInState[, outcomeIndex], hospitalsInState[, 2]), ][1, 2]
    } else if (num == 'worst') {
        ordered <- hospitalsInState[order(-hospitalsInState[, outcomeIndex], hospitalsInState[, 2]), ][1, 2]
    } else {
        ordered <- hospitalsInState[order(hospitalsInState[, outcomeIndex], hospitalsInState[, 2]), ][num, 2]
    }

    ordered
}