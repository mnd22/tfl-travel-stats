# Functions for manipulating TfL Journey History

import_journey <- function(monthcode, prefix="data/", suffix="-journeys.csv")
{
    filename <- paste(prefix, monthcode, suffix, sep="")
    print(filename)
    read.csv(file=filename)
}

#' Imports files containing TfL journey history data in their standard CSV
#' format, returning the result as a data frame.  Allows input of multiple files
#' through pattern matching on filenames in a folder.
#' 
#' This function currently has no error handling, so worth checking that files
#' are in the correct format before importing.
#' 
#' @param folder Path to the folder containing the data files.
#' @param pattern Pattern to match to select files to import.
#' @return Data frame containing 
#' @examples
#' import_all_journeys()
#' import_all_journeys(folder="data", pattern="journeys")
import_all_journeys <- function(folder="data", pattern="journeys")
{
    # Build list of all files matching pattern and inform user.
    allfiles = list.files(path=folder, pattern="journeys*.csv", full.names=TRUE)
    print("Preparing to import:")
    print(allfiles)
    
    datavector = vector()
    
    # Read each file into memory assuming it is correctly formatted CSV file
    # with suitable header
    framelist <- lapply(allfiles, function(x){read.csv(file=x,
                                                       header=TRUE,
                                                       row.names=NULL)})
    
    # Join together list of data frames and return the result.
    Reduce(function(x,y){merge(x, y, all=TRUE)}, framelist)
}

#' Extracts only data related to tube and train journeys, identified by the
#' presence of the string " to " in the journey description, and then adds
#' columns showing the start and end stations to the data frame, as well as
#' properly formatted timestamps enableing time-based calculations.
#' 
#' NB: The current version has limited error handling, in particular it will
#'     include incomplete journeys with "[No touch-out]" shown.
#' 
#' @param alldata The input data, e.g. an output from import_all_journeys
#' @return Data frame containing only tube/train journeys with start/end cols.
#' @examples
#' get_tube_train_data(alldata)
get_tube_train_data <- function(alldata)
{
    # Extract all rows containing the string " to ".
    alldata[grepl(" to ", alldata$Journey.Action),] -> tubetraindata    
    
    # Split the journey description into start/end points, returning a vector of
    # pairs of strings.
    strsplit(as.vector(tubetraindata$Journey.Action), " to ") -> stationslist
    
    # Transform vector into an array and then transpose it to get a pair of vecs
    aperm(simplify2array(stationslist), c(2,1)) -> stationsarray
    
    # Add columns for start and end station to the new data frame.
    tubetraindata$Start.Station <- stationsarray[,1]
    tubetraindata$End.Station <- stationsarray[,2]

    # Add columns with properly rendered timestamps to enable time-based
    # analysis
    tubetraindata$Start.Timestamp <- strptime(paste(tubetraindata$Date, 
                                                    tubetraindata$Start.Time),
                                              "%d-%b-%Y %H:%M")
    tubetraindata$End.Timestamp <- strptime(paste(tubetraindata$Date,
                                                  tubetraindata$End.Time),
                                            "%d-%b-%Y %H:%M")
    tubetraindata$Duration <- difftime(tubetraindata$End.Timestamp,
                                       tubetraindata$Start.Timestamp)
    tubetraindata
}

#' Extracts only data related to a particular tube or train journeys, identified
#' pattern matching against start and end strings.  
#' 
#' NB: The current version has limited error handling, in particular it will
#'     include incomplete journeys with "[No touch-out]" shown, which may have
#'     unexpected durations.
#' 
#' @param startstation Pattern to match for starting station(s)
#' @param endstation Pattern to match for starting station(s)
#' @param inputdata    The input data, e.g. an output from get_tube_train_data
#'                     Must have be pre-selected to add Start/End station fields
#' @param quiet        Do not print interaction information to screen.
#' @return Data frame containing only tube/train journeys with start/end cols.
#' @examples
#' get_journeys(alldata)
get_journeys <- function(startstation, 
                         endstation, 
                         inputdata,
                         quiet = FALSE,
                         plotdurations = FALSE)
{
    outputdata <- tubetraindata[grepl(startstation, inputdata$Start.Station) &
                                grepl(endstation, inputdata$End.Station),]
    if (! quiet)
    {
        print("Results include all journeys starting at any of the following:")
        print(unique(outputdata$Start.Station))
        print("and ending at any of the following:")
        print(unique(outputdata$End.Station))
    }
    
    if (plotdurations)
    {
        journey_duration_plot(outputdata, plottitle = paste(startstation,
                                                            "to",
                                                            endstation))
    }
    
    outputdata
}

journey_duration_plot <- function(journeydata,
                                  plottitle = "Selected journeys")
{
    hist(as.integer(journeydata$Duration), 
         seq.int(from=0, to= max(journeydata$Duration)+2, by=2), 
         xlab="Minutes", 
         main=plottitle,
         col = "red")
}