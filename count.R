count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
	
## Check that specific "cause" is allowed; else throw error
	causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
	if (cause %in% causes == FALSE) stop("invalid cause of death")

## Read "homicides.txt" data file
	homicides <- readLines("homicides.txt")

## Extract causes of death
	if (cause == "asphyxiation") result <- length(grep("Cause: [Aa]sphyxiation", homicides))
	else if (cause == "blunt force") result <- length(grep("Cause: [Bb]lunt [Ff]orce", homicides))
	else if (cause == "other") result <- length(grep("Cause: [Oo]ther", homicides))
	else if (cause == "shooting") result <- length(grep("Cause: [Ss]hooting", homicides))
	else if (cause == "stabbing") result <- length(grep("Cause: [Ss]tabbing", homicides))
	else if (cause == "unknown") result <- length(grep("Cause: [Uu]nknown", homicides))
	
## Return integer containing count of homicides for that cause
	return(result)

}