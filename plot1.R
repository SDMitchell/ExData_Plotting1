library(tools)

# Default arguments
xresDefault <- 480
yresDefault <- 480
outputFilenameDefault <- "plot1.png"
listOfDaysToAnalyzeDefault <- list("1/2/2007", "2/2/2007")

# Invariable input parameters
targetFilename <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dataDescription <- "https://archive.ics.uci.edu/ml/datasets/Individual+household+electric+power+consumption"
tempFilename <- "household_power_consumption.zip"
inputFilename <- "household_power_consumption.txt"
dateFormat <- "%d/%m/%Y"
timeFormat <- "%H:%M:%S"
dateTimeFormat <- paste(dateFormat, timeFormat, sep=";")
inputFileColumnClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")

##
## A function to fetch the input data file if it does not currently exist locally.
## This function will download the zip file and write its MD5 sum to disk (with a ".MD5" extension). It
## will proceed to unpack the zip file and expect it to contain a file with the given destfile name. If it contains
## this file it will also write out its MD5 sum in the sam fashion as above. On error an exception is thrown; on success
## the input filename is returned
##
fetchDataFile <- function(targetURL = targetFilename, destfile = inputFilename) {

	# Try to download the given file if our input file does not already exist
	if(!file.exists(destfile))
	{
		download.file(url=targetURL, destfile=tempFilename, method="auto", mode="wb")
		if(file.exists(tempFilename))
		{
			# Record the downloaded file's MD5 sum; this file's creation date can serve as the downloaded date
			write(md5sum(tempFilename), paste(tempFilename, ".MD5", sep=""))

			# We retrieved the file, so try to unzip it
			unzip(tempFilename)

			# Don't go any further if the zip file unpacked correctly but we didn't get the contents we were expecting
			if(file.exists(destfile))
			{
				# Record the unzipped file's MD5 sum; this file's creation date can serve as the unpack date
				write(md5sum(destfile), paste(destfile, ".MD5", sep=""))
			}
			else
			{
				stop("There was a problem attempting to unzip the downloaded file from the given URL")
			}
		}
		else
		{
			stop("There was a problem attempting to download the file from the given URL")
		}
	}

	# Ok, we're all clear if we made it this far
	inputFilename
}

##
## This function will read the given data file and extract the samples representing the given list of days.
## The samples will be returned in an appropriately formatted data.frame
##
readData <- function(inputFile = inputFilename, listOfDaysToAnalyze = listOfDaysToAnalyzeDefault) {
	# Input file has to exist
	if(!file.exists(inputFile))
	{
		stop("The given input file does not exist")
	}

	data <- read.csv(inputFile, sep=";", na.strings="?", header=TRUE, colClasses=inputFileColumnClasses)

	# Make a new Timestamp column (this isn't used for this particular plot, but will be needed in future plots).
	data <- cbind(data, strptime(paste(data$Date, data$Time, sep=";"), dateTimeFormat))
	colnames(data)[ncol(data)] <- "Timestamp"

	# Caller has to ask for some days or they get all of them.
	# NOTE: There were no specifications given around
	# input date formats, which makes date conversions difficult to manage. Therefore for this plot we are
	# doing pure string matching on the dates, given that they are all digits and there are no alpha/case
	# issues to worry about.
	if(length(listOfDaysToAnalyze) > 0)
	{
		data <- data[data$Date %in% listOfDaysToAnalyze, ]
	}
	data
}

##
## This function simply creates the plot with the desired parameters. It is sent to the currently active device.
##
createPlot <- function(inputData) {
	hist(inputData$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
}

##
## The driver code to create a PNG file out of the default data set.
##
png(filename=outputFilenameDefault, width=xresDefault, height=yresDefault)
createPlot(readData(fetchDataFile()))
dev.off()
