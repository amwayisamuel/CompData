corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations


	## Lists all the datafiles in the specified directory
	file_list <- list.files(directory)

	## Creates a complete path for the datafiles
	completePath <- paste(directory, file_list, sep = "/")

	## Initializes an empty vector for the correlation results
	cor_res <-c()
	
	## Initializes a for loop that iterates through the whole specified directory
	for (i in seq.int(completePath)) {

		## Loads in a .csv datafile in each iteration
		input <- read.csv(completePath[i])

		## Checks if the number of complete cases is larger than the specified threshold
		if (nrow(na.omit(input)) > threshold) {

			## Creates a vector of all the sulfate readings in the particular datafile
			sulf <- na.omit(input)$sulfate

			## Creates a vector of all the nitrate readings in the particular datafile
			nitr <- na.omit(input)$nitrate

			## Calculates the Pearson correlation coefficient for sulfate and nitrate
			## and appends the result to the initialized results vector
			cor_res <- c(cor_res, cor(sulf,nitr))
		}
	}

	## Returns the correlation results vector
    return (cor_res)
	
}