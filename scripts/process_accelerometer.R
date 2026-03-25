## This script calls functions to calculate common movement metrics from 
## accelerometry data and correct the pitch based on average pitch during flight. Data
## are then subsampled to a 1 sec interval to make file sizes more manageable.

# load dat
load(paste0(output_activity_data,"dat_all_indiv_script1.RData"))

require(ggplot2)
options(digits.secs=4)

# remove GPS data to keep 20 data per sec for rolling functions
datacc <- subset(dat, !datatype == "GPS")
View(datacc)

# Set values for calculating all accelerometer derived measures
myFreq <- 20 # Sampling frequency of the accelerometers
myWindow <- 1 # Size of window that you want to average over - we used 2 sec, but a larger window may give more consistent results for general behaviours
mySubsample <- 1 # Rate to subsample during WBF calculations - it is too slow to do all rows
myWBFWindow <- 5 # Window size for calculating peak frequency - 2 seconds is too low for fft
myThreshold <- 0.5 # This is a threshold for calculating wing beat frequency, 
# and wing beats with amplitudes below this threshold will be given a WNF value of 0 

###########################################

# Name of folder in wd that contains the raw data files
inputFolder <- "raw_data"
# Name of folder in wd that will recieve output files
outputFolder <- "outputs/activity/acc_data"

###########################################
# Use this script to run each accelerometer file individually.
# Data files need to have variables for tag ID, time (as.POSIXct), and acceleration in the X, Y, and Z axes
# Additional variables of interest that are in your raw can be carried forward with the output.
# This code is written assuming your data files are all stored in a folder called Raw_Data 
# and output will be written to a folder called Acc_Data.
# The script also assumed data files have at least five variables: tag, time, X, Y, Z - you will probably neeed to modify lines 37:50 below to suit your files.
# Once you have the script working for your data, use the for loop to process all your files.

###########################################

print(paste("Getting basic metrics for", datacc$device_id[1] , "at", format(Sys.time(), "%T")))

# Make sure data are in chronological order before processing
#datacc <- dat[order(c(dat$device_id, dat$UTC_timestamp)),]

# Run function to calculate movement metrics
lets <- getMetrics(data = datacc, # Name of object holding data
                   tagID = device_id, # Tag identifier
                   time = UTC_timestamp, # Date field
                   heave = acc_z, # Axis of vertical movement
                   sway = acc_x, # Axis oflateral movement 
                   surge = acc_y, # Axis of horizontal movement
                   window = myWindow, # Interval for averaging metrics
                   frequency = myFreq, # Accelerometer sampling frequency
                   keep = c() # If there are other variables in the data you want to retain, include here as a list of variable names
)
View(lets)

# Run function to calculate wing beat frequency
lets$WBF <- getFrequency(variable = lets$acc_z, # Variable name for frequency calculation
                         WBFwindow = myWBFWindow, # Window to calculate peak frequency in
                         frequency = myFreq, # Sampling frequency of accelerometer
                         sample = mySubsample, # Subsampling interval for calculations
                         threshold = myThreshold # Minimum amplitude to be considered a wingbeat - you can start by setting this to 0 and try increasing values to see how it changes your results
)

# Create an index to select all data with a WBF between 3 and 9 - different species will need different values
WBFidx <- which(lets$WBF > 3 &lets$WBF < 9) # Values taken from De Meis et al. (2025), suitable for pigeons

# Determine the median value for each axis
standX <- mean(lets$X[WBFidx])
standY <- mean(lets$Y[WBFidx]) 
standZ <- mean(lets$Z[WBFidx]) + 1

# Use median values during flight to standardize the axes from the original data
datacc$X <- ((lets$X) - (standX)) 
datacc$Y <- (lets$Y) - (standY)
datacc$Z <- ((lets$Z) - (standZ)) 

# Run function to calculate movement metrics again using the standardized values of X, Y, Z
newData <- getMetrics(data = datacc, # Name of object holding data
                      tagID = device_id, # Tag identifier
                      time = UTC_timestamp, # Date field
                      heave = acc_z, # Axis of vertical movement
                      sway = acc_y, # Axis oflateral movement 
                      surge = acc_x, # Axis of horizontal movement
                      window = myWindow, # Interval for averaging metrics
                      frequency = myFreq, # Accelerometer sampling frequency
                      keep = c() # If there are other variables in the data you want to retain, include here as a list of variable names
)

# Sometimes tags are put on upside down, this can help identify those tags and fix it assumming your tag would mostly be at a positive angle
# However, you should plot pitch for each file and make sure it looks correct
if (median(newData$pitch, na.rm = T) < 0) newData$pitch <- newData$pitch * -1

# Calculate WBF again
newData$WBF <- getFrequency(variable = newData$Z, # Variable name for frequency calculation
                            WBFwindow = myWBFWindow, # Window to calculate peak frequency in
                            frequency = myFreq, # Sampling frequency of accelerometer
                            sample = mySubsample, # Subsampling interval for calculations
                            threshold = myThreshold # Minimum amplitude to be considered a wingbeat - you can start by setting this to 0 and try increasing values to see how it changes your results
)

# Calculate sdZ 
ptm <- proc.time()
print("Calculating sdZ")
newData$sdZ <- rollapply(newData$dynamicZ, myFreq * myWindow, sd, fill = 0)
prc.time <- round((proc.time() - ptm)[[3]], digits = 3)
print(paste("Processing time for sdZ:", prc.time, "sec"))

# Subsample the raw data to 1 second intervals
# Resulting files will be smaller and easier to work with
idx <- seq(from = 1, to = nrow(newData), by = myFreq)
subData <- newData[idx,]
subData <- na.omit(subData)

# Calculate mean pitch over a 240 sec window, this is slow but this variable is useful for plotting
ptm <- proc.time()
print("Calculating meanPitch240")
subData$meanPitch240 <- rollapply(subData$pitch, 240, mean, partial = T)
prc.time <- round((proc.time() - ptm)[[3]], digits = 3)
print(paste("Processing time for meanPitch240:", prc.time, "sec"))

# Plot main variables to see if everything looks correct
plotdat <- melt(subData[1:100000,], id.vars = c("time"), measure.vars = c("WBF","pitch","meanPitch240","sdZ"))
pp <- ggplot(data = plotdat, aes(x = time, y = value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y") +
  ggtitle(subData$tag[1])
print(pp)

#######################
# Write files to a new folder

dat_acc <- paste(outputFolder, "/" ,dat$tag[1], "dat_acc.csv", sep = "" )
write.csv(subData, dat_acc, row.names = F)


# Create path to save acc data
output_activity_accdata <- paste0(pigeon_path,"/outputs/activity/acc_data/")
dir.create(output_activity_accdata, showWarnings = FALSE, recursive = T)

# save subData
save(subData, file = paste0(output_activity_accdata, "dat_acc_process_accelerometer.RData"))
