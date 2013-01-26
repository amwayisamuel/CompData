agecount <- function(age = NULL) {
## Check that "age" is non-NULL; else throw error

## Read "homicides.txt" data file
	homicides <- readLines("homicides.txt")

## Extract ages of victims; ignore records where no age is
## given
	regex <- paste(age, " [Yy]ears", sep = "")
	result <- length(grep(expression, homicides))

## Return integer containing count of homicides for that age
	return(result)

}