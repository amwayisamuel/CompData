rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
	input <- read.csv("outcome-of-care-measures.csv")

## Check that state and outcome are valid
	if (state %in% input$State == FALSE) stop("invalid state")
	if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) stop("invalid outcome") 


## Return hospital name in that state with the given rank
## 30-day death rate
	state_input <- subset(input, State == state)

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
		return(as.character(ordered_data[1]))
	}

	else if (num == "worst") {
		return(as.character(ordered_data$Hospital.Name[length(ordered_data$Hospital.Name)]))
	}

	else {
    	return(as.character(ordered_data$Hospital.Name[num]))
    }
    

}