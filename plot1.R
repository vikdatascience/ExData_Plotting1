## ********************************************************************************************
## This function generates the first plot of the assignment.
## Load the source
## call plot1() for default dates. 
## Pass in the start date and number of days since the start date as 
## parameters if you want the plot for a different set of dates.
## ********************************************************************************************
plot1 <- function(st = as.Date("2007-02-01"), n = 2 ) {
  ## Read the file into a data frame.
  
  print("Reading data from the file....")
  epc <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings="?")

  print("Read the data into a data frame. Now manipulating data classes.")
  
  ## Bind variables to the right classes
  ## Date and Time variables should be of date and time classes
  epc$Time  <- strptime(paste(epc$sDate, epc$Time), "%d/%m/%Y %H:%M:%S")
  epc$sDate <- as.Date(epc$sDate, "%d/%m/%Y")
  
  ## rest of the variables are numeric 
  for (i in 3:9) {
    epc[,i] <- as.numeric(epc[,i])
  }
  
  ## subset to the two dates of interest
  dr <- dr <- seq(as.Date(st), by="1 day", length.out=n)   
  epcf7 <- subset(epc, epc$sDate %in% dr)
  
  
  hist(epcf7$Global_active_power, 
       col="red", 
       xlab="Global Active Power (kilowatts)", ylab="Frequency", 
       main = "Global Active Power")
  
  dev.copy(png, file="plot1.png")
  dev.off()
  
  print("The plot saved to plot1.png")
}