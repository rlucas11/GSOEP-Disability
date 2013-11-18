# -------------------- define paths ------------------------------ #

pathOriginalData <- "~/Documents/Datasets/HILDA10/" # path of original panel data files, ending with "/"
pathWorking <- "../data/" # path where new files will be stored

# -------------------- study characteristics -------------------- #

firstYearOfStudy <- 2001
lastYearOfStudy <- 2010
startingWave <- 1

masterFile <- "Master_j100c.dta"
waveLabels <- letters[1:(lastYearOfStudy-firstYearOfStudy+1)] # Wave names as letters
yearsToInclude <- seq(firstYearOfStudy,lastYearOfStudy) # Years of Study
yearsToInclude2 <- substr(as.character(yearsToInclude), 3, 4) # Two-digit year for some variable names
originalDataFile <- "Rperson_$100c.dta"
idName <- "xwaveid"
hID <- "hhrhid" # Needs wave prefix
partnerID <- "hhpxid" # Needs wave prefix; this is equivalent to the partner's cross-wave ID.
charsToSub <- "\\$"

gsoep_prefixes <- c(letters[1:7],letters[7:26],paste("b",letters[1:2],sep=""))
gsoep_years <- c(1984:1990,1990:2011)
gsoep_ind_files <- paste(gsoep_prefixes,"p.dta",sep="")
gsoep_ind_files[8] <- "gpost.dta"
gsoep_h_files <- paste(gsoep_prefixes, "h.dta", sep="")
gsoep_h_files[8] <- "ghost.dta"
gsoep_eq_files <- paste(gsoep_prefixes, "pequiv.dta", sep="")
gsoep_eq_files[8] <- NA
gsoep_files <- as.data.frame(cbind(gsoep_years,gsoep_ind_files,gsoep_h_files,gsoep_eq_files), stringsAsFactors=FALSE)
gsoep_files$gsoep_years <- as.numeric(gsoep_files$gsoep_years)



## Function to get any longitudinal individual-level variable
## Provide a matrix with each line specifying a wave and a variable name
## Provides either a wide or long file with respondent ID, and all requested waves of a variable.
## Sample Year is provided with long files
get_variable <- function{
    
