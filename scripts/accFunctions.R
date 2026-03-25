require(doBy)
require(reshape2)
require(sp)
require(zoo)

########################################
# Function to filter out behaviours occuring for less than mintime
# Input:
# rawClassification - the vairable you want to filter
# mintime - the minimum number of sequential occurences of a behaviour required, anything less will be assigned to the previous behaviour
# Note: this was designed with murres in mind so transitions between behaviours labelled Diving and Swimming will not be filtered 

filterClass <- function(rawClassification, mintime = 2) {
  output <- rawClassification
  for (i in 2:(length(output) - mintime)) {
    temp <- output[i:(i + (mintime - 1))]
    temp <- ifelse(temp == "Diving", "Swimming", temp)
    tt <- which(names(table(temp)) == temp[1])
    if (table(temp)[tt] != mintime) output[i] <- output[i - 1]
  }
  output
}

########################################
# Function to assign a unique identifier to continous sessions of a behaviour
# Inputs: 
# behaviour - the variable you want to base sessions on
# maxSession - can be used to put an upper limit on the duration of a session, continuous behaviour longer than that will be split into multiple sessions

getSessions <- function(behaviour, maxSession = Inf) {
  output <- 1
  j <- 1
  k <- 0 
  for (i in 2:length(behaviour)) {
    j <- ifelse(behaviour[i] == behaviour[i - 1], j, j + 1)
    k <- ifelse(behaviour[i] == behaviour[i - 1], k + 1, 0)
    if (k >= maxSession) {
      j <- j + 1
      k <- 0
    }
    output[i] <- j
  }
  output
}

#######################################
# Functions to plot the density of a variable with labelled breaks and extract the value of a specific break point.

plotVar <- function(variable, adjust) {
  d <- density(na.omit(variable), adjust = adjust)
  tt <- findpeaks(d$y)
  
  d <- data.frame(x = d$x, y = d$y)
  ggplot(data = d, aes(x = x, y = y)) +
    geom_line() +
    geom_vline(xintercept = d$x[tt[,4]], linetype = 2) +
    annotate("text", y = max(d$y), x = d$x[tt[,4]], label = seq(1, nrow(tt)), hjust = 2)
}

getBreak <- function(variable, adjust, myBreak) {
  d <- density(na.omit(variable), adjust = adjust)
  tt <- findpeaks(d$y)
  d$x[tt[myBreak,4]]
}

#####################################
# Function to convert lat/lon to UTM

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

#####################################
# Function to get the mode of a variable

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

###############################
# A function to calculate basic accelerometry statistics, including:
# static acceleration, pitch, dynamic acceleration, and the SD of static 
# acceleration. A new data frame is returned with metrics and other existing
# variables specified by the user

getMetrics <- function(data, tagID, heave, sway, surge, 
                       time, window, frequency, 
                       keep = NA) {
  # Load the zoo package and give an error message if it is nor installed
  require(zoo)
  ptm <- proc.time()
  
  # Turn call names into strings for evaluation
  arguments <- as.list(match.call())
  
  # Get the number of rows to average over
  interval<-frequency*window
  
  # Make a new data frame with accelerometry and depth variables
  temp <- data.frame(FID = 1:nrow(data), # Add field id
                     tag = eval(arguments$tagID, data), # Add field id
                     time = eval(arguments$time, data), 
                     X = eval(arguments$surge, data),
                     Y = eval(arguments$sway, data),
                     Z = eval(arguments$heave, data))
  
  print("Calculating static accelerations and pitch")
  # Calculate mean acceleration over a moving window
  temp$staticX <- rollmean(temp$X, interval, fill = NA)
  temp$staticY <- rollmean(temp$Y, interval, fill = NA)
  temp$staticZ <- rollmean(temp$Z, interval, fill = NA)
  
  # Calculate pitch based on mean acceleration in each axis
  temp$pitch <- atan(temp$staticX/(sqrt((temp$staticY^2)+(temp$staticZ^2))))*(180/pi)

  # Calculate dynamic acceleration
  print("Calculating dynamic accleration")
  temp$dynamicX <- temp$X - temp$staticX
  temp$dynamicY <- temp$Y - temp$staticY
  temp$dynamicZ <- temp$Z - temp$staticZ
  temp$ODBA <- abs(temp$dynamicX) + abs(temp$dynamicY) + abs(temp$dynamicZ)
  
  if (is.vector(keep) == T) {
    temp <- cbind(temp, data[,keep])
    names(temp)[15:ncol(temp)] <- keep
    }
  
  prc.time <- round((proc.time() - ptm)[[3]], digits = 3)
  print(paste("Processing time:", prc.time, "sec"))
  return(temp)
}

###########################################
# Function to calculate peak frequency in a time series of data over a moving 
# window. If the heave axis is used, the ouput is the wing beat frequency.
# The data are output as a vector or frequencies. The code is designed to only 
# calculate peak frequency at regular sample intervals to reduce 
# computation time; however, peak frequency can be calculated for each row by 
# setting sample = 1.

getFrequency <- function(variable, WBFwindow, frequency, sample, threshold) {
  # Track processing time
  ptm <- proc.time()
  
  # Get the length of the variable and create output object
  axis <- variable
  lenVar <- length(variable)
  output <- rep(NA, lenVar)
  
  # Get values needed for the loop from user inputs
  sampInterval <- frequency * sample
  windowWidth <- frequency * WBFwindow
  midPoint <- floor(sampInterval/2)
  calcs <- seq(from = (1 + (windowWidth/2)), to = (lenVar - (windowWidth)/2), by = sampInterval)
  
  for (i in calcs) {
    # Prepare data for fft
    sampInt<- (i-floor(windowWidth/2)):((i-floor(windowWidth/2)) + windowWidth - 1)
    ddd <- axis[sampInt]
    ddd <- ts(data = ddd, start = 1, frequency = frequency)
    # Create indices for outputting the data
    halfwidth <- ceiling(length(sampInt)/2)
    freqs <- ((2:halfwidth/2)/(halfwidth))/(1/frequency)
    # calculate fft
    pows <- abs(fft(ddd)[2:halfwidth])^2
    # Select maximum frequency
    val <- freqs[which(pows == max(pows))[1]] 
    # Exclude frequencies with very low amplitudes
    if (IQR(ddd) < threshold) val <- 0
    # Write frequency value to output vector, filling all rows within the sample interval
    myInt <- (i - midPoint):((i - midPoint) + (sampInterval - 1))
    output[myInt] <- rep(val, sampInterval)
    # Print a progress message
    trackProg <- seq(from = 1, to = length(calcs), length.out = 11)[2:11]
    if (i %in% calcs[trackProg]) {
      print(paste("Finished processing:", round((i/lenVar) * 100), "% at", format(Sys.time(), "%T")))
      
    }
    
  }
  
  prc.time <- round((proc.time() - ptm)[[3]], digits = 3)
  print(paste("Processing time:", prc.time))
  
  return(output)
}
