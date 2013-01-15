getmonitor <- function(id, directory, summarize = FALSE) {
        
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here


	## Pad the id number to be in 3-decimal format
	paddedId <- formatC(id, width = 3, flag = 0)

	## Add .csv extension to the padded id number
	loadId <- paste(paddedId, "csv", sep = ".")
	
	## Load the datafile from the specified directory
    completePath <- paste(directory, loadId, sep = "/")
	input <- read.csv(completePath)
	
	## Show a summary of the data if 'summarize' parameter is set to True
	if (summarize == TRUE) {
		print(summary(input))
	}

	## Return the input (otherwise, the function would return NULL when called )
	return(input)
}