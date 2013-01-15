complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases


        ## Pad the ID number to be a .csv file in the correct format
        csvId <- paste(formatC(id, width = 3, flag = 0), "csv", sep = ".") 
        
        ## Set the directory and the complete path for loading .csv files
        completePath <- paste(directory, csvId, sep = "/")

        ## Initialize an empty vector for the number of observations
        nobs <- c()

        ## Start a for loop if there are multiple .csv files to be loaded
        for (i in seq.int(completePath)) {     

                ## Reads in the .csv files one by one           
                input <- read.csv(completePath[i])

                ## Creates a dataframe with only complete cases
                good <- na.omit(input)

                ## Counts the number of complete cases
                tempNobs <- nrow(good)

                ## Adds the number of complete cases to the already existing vector
                nobs <- c(nobs,tempNobs)
                                
        }

        ## Creates a dataframe of id numbers and numbers of complete cases for each .csv file
        result <- data.frame(id, nobs) 
        
        return (result)
}