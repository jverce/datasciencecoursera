rankall <- function(outcome, num = "best") {
    validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
    validOutcomeIndexes <- c(11, 17, 23)

    if (!outcome %in% validOutcomes) {
        stop('invalid outcome')
    }

    outcomeData <- read.csv(
        'outcome-of-care-measures.csv', colClasses='character')
    listOfStates <- unique(outcomeData[, 7])

    indexFilter <- !is.na(validOutcomeIndexes[match(validOutcomes, outcome)])
    outcomeIndex <- validOutcomeIndexes[indexFilter]

    outcomeData[, outcomeIndex] <- as.numeric(outcomeData[, outcomeIndex])
    outcomeData <- outcomeData[!is.na(outcomeData[, outcomeIndex]), ]

    numOfStates <- length(listOfStates)
    hospitalName <- character(numOfStates)
    stateName <- character(numOfStates)
    index <- 1

    for (state in listOfStates[order(listOfStates)]) {
        hospitalsInState <- outcomeData[outcomeData[, 7] == state, ]

        if (num == 'best') {
            selected <- hospitalsInState[order(hospitalsInState[, outcomeIndex], hospitalsInState[, 2]), ][1, 2]
        } else if (num == 'worst') {
            selected <- hospitalsInState[order(-hospitalsInState[, outcomeIndex], hospitalsInState[, 2]), ][1, 2]
        } else {
            selected <- hospitalsInState[order(hospitalsInState[, outcomeIndex], hospitalsInState[, 2]), ][num, 2]
        }

        hospitalName[index] <- selected
        stateName[index] <- state
        index <- index + 1
    }

    data.frame(hospital=hospitalName, state=stateName)
}