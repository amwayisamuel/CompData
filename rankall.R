rankall <- function(outcome, num = "best") {
## Read outcome data
	input <- read.csv("outcome-of-care-measures.csv")

## Check that state and outcome are valid
	if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) stop("invalid outcome") 

## For each state, find the hospital of the given rank
	state_list <- as.character(unique(input$State))

	#result_matrix <- matrix(nrow = length(state_list), ncol = 2)
	name_list <- c()


	for (state in seq.int(state_list)) {

		state_input <- subset(input, State == state_list[state])

		if (outcome == "heart attack") {
	    	outcome_input <- state_input[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack','Hospital.Name')]
			NA_removed <- outcome_input[outcome_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available',]
			numeric_outcome <- data.frame(as.numeric(levels(NA_removed$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[NA_removed$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack]), NA_removed$Hospital.Name)
			names(numeric_outcome)[1] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
			names(numeric_outcome)[2] <- "Hospital.Name"
			ordered_data <- numeric_outcome[order(numeric_outcome[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]], numeric_outcome$Hospital.Name), ]
    	}

    	if (outcome == "heart failure") {
	    	outcome_input <- state_input[,c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure','Hospital.Name')]
			NA_removed <- outcome_input[outcome_input$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available',]
			numeric_outcome <- data.frame(as.numeric(levels(NA_removed$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)[NA_removed$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure]), NA_removed$Hospital.Name)
			names(numeric_outcome)[1] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
			names(numeric_outcome)[2] <- "Hospital.Name"
			ordered_data <- numeric_outcome[order(numeric_outcome[["Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]], numeric_outcome$Hospital.Name), ]
		}

		if (outcome == "pneumonia") {
	    	outcome_input <- state_input[,c('Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia','Hospital.Name')]
			NA_removed <- outcome_input[outcome_input$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available',]
			numeric_outcome <- data.frame(as.numeric(levels(NA_removed$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)[NA_removed$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia]), NA_removed$Hospital.Name)
			names(numeric_outcome)[1] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
			names(numeric_outcome)[2] <- "Hospital.Name"
			ordered_data <- numeric_outcome[order(numeric_outcome[["Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]], numeric_outcome$Hospital.Name), ]
		}

			if (num == "best") {
				name_result <- as.character(ordered_data[1])
			}
			else if (num == "worst") {
				name_result <- as.character(ordered_data$Hospital.Name[length(ordered_data$Hospital.Name)])
			}
			else {
    			name_result <- as.character(ordered_data$Hospital.Name[num])
    		}

	name_list <- c(name_list, name_result)	

	}

## Return a data frame with the hospital names and the
## (abbreviated) state name

	result <- data.frame(cbind(name_list, state_list))
	names(result)[1] <- "hospital"
	names(result)[2] <- "state"

	return (result)

}