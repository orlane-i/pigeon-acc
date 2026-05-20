## After clustering and subsetting the dataset to keep only cluster 1 corresponding to flight sequences, run the wingbeat frequency function
pigeon_path <- here::here()
output_activity_acc_data <- paste0(pigeon_path,"/outputs/activity/acc_data/")
load(paste0(output_activity_acc_data,"script3_subdatkmeans_cluster2.RData"))

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
  lenVar <- length(variable) # 59891
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

#####

# Set values for calculating all accelerometer derived measures
myFreq <- 20 # Sampling frequency of the accelerometers
myWindow <- 2 # Size of window that you want to average over - we used 2 sec, but a larger window may give more consistent results for general behaviours
mySubsample <- 1 # Rate to subsample during WBF calculations - it is too slow to do all rows
myWBFWindow <- 5 # Window size for calculating peak frequency - 2 seconds is too low for fft
myThreshold <- 0.5 # This is a threshold for calculating wing beat frequency, 
# and wing beats with amplitudes below this threshold will be given a WNF value of 0 

#####

# Run function to calculate wing beat frequency
datkmeans_cl$WBF <- getFrequency(variable = datkmeans_cl$heav.mu.3s, # Variable name for frequency calculation
                         WBFwindow = myWBFWindow, # Window to calculate peak frequency in
                         frequency = myFreq, # Sampling frequency of accelerometer
                         sample = mySubsample, # Subsampling interval for calculations
                         threshold = myThreshold # Minimum amplitude to be considered a wingbeat - you can start by setting this to 0 and try increasing values to see how it changes your results
)


#####
# add WBF calculated from raw data + metadata to clusterized df
datkmeans_cl2$Date <- format(datkmeans_cl2$Date, "%Y-%m-%d %H:%M:%OS1", tz = "Europe/Paris")
datr$time <- format(datr$time, "%Y-%m-%d %H:%M:%OS1", tz = "Europe/Paris")

# if local_timestamp is character
# remove "CEST" first
# datacc$local_timestamp <- gsub(" CEST", "", datacc$local_timestamp)

# # then convert to POSIXct
# datacc$local_timestamp <- as.POSIXct(
#   datacc$local_timestamp,
#   format = "%Y-%m-%d %H:%M:%OS4",
#   tz = "Europe/Paris"
# )

datkmeans_cl2 <- datkmeans_cl2 %>%
  select(-(27:29)) %>%
  select(-"time_clean")
save(datkmeans_cl2, file = paste0(output_activity_acc_data,"script3_subdatkmeans_cluster2.RData"))

datkmeans_cl2_WBF <- datkmeans_cl2_WBF[!is.na(datkmeans_cl2_WBF$WBF), ]

save(datkmeans_cl2_WBF, file= paste0(output_activity_acc_data,"script4_cluster2_WBF.RData"))
