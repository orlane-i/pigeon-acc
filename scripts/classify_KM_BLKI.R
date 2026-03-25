# This code implements the k-means (KM) method for black-legged kittiwakes
# described in the paper Patterson et al  - A comparison of techniques for classifying behaviour from accelerometers for two species of seabird
# Data from this paper can be downloaded from Dryad with this code.
# This code will implement this classification on those data, or could be modified to use your own data.

#########################

library(ggplot2)
library(doBy)
library(vegan)
library(reshape2)
options(digits.secs=4)

setwd("...")
source("accFunctions.R")

#########################

# Load data
myData <- read.csv("Accelerometer Data BLKI.csv")

# Format time
myData$time <- as.POSIXct(as.character(myData$time))

# Make sure data are in chronological order for each tag
myData <- myData[order(myData$tag, myData$time),]

################################
# Do initial classification of flying with a 2 class kmeans cluster
myData$KM_Flights <- kmeans(myData$WBF, 2)$cluster

# Examine results of flight classification
boxplot(WBF ~ KM_Flights, myData)

# Determine which class is flying
tt <- summaryBy(WBF ~ KM_Flights, myData, FUN = summary)
fly <- which(tt$WBF.Mean > 2)

# Filter any flights above that occur for less than 3 seconds
# This will take some time if you are working with all tracks
myData$KM_Flights <- filterClass(myData$KM_Flights, 3)

# Make a new variable identifying segments of continuous behaviour
myData$KM_Session <- getSessions(myData$KM_Flights, maxSession = 120)

################################
# Calculate mean values of predictor variables within behaviour segments

segData <- summaryBy(pitch + sdZ + WBF ~ tag + KM_Session + KM_Flights, myData, 
                     FUN = mean, keep.names = T)
head(segData,20)

# Standardize sdZ and pitch by their range
segData$stand_sdZ = decostand(log(segData[,"sdZ"]), method = "range")
segData$stand_pitch = decostand((segData[,"pitch"]) , method = "range")

# Run a 3 class k-mean classification on the non-flight segments
segData$KM[segData$KM_Flights != fly] <- kmeans(segData[segData$KM_Flights != fly,c("stand_sdZ","stand_pitch")], 3)$cluster
# Add a 4th class with the previously identified flight segments
segData$KM[segData$KM_Flights == fly] <- 4

################################
# Examine the basic results

table(segData$KM)
boxplot(WBF ~ KM, data = segData)
boxplot(sdZ ~ KM, data = segData)
boxplot(pitch ~ KM, data = segData)

#######################################################
# Label the KM classes with behaviours

# Label KM_Flights class previoulsy identified as flying to label flights
segData$KM_Class <- NA
segData$KM_Class[segData$KM_Flights == fly] <- "Flying"

# Classes with mean pitch > 10 should labelled as colony
(tt <- summaryBy(pitch ~ KM, data = segData))
(sit <- tt$KM[tt$pitch.mean > 10])
segData$KM_Class[segData$KM %in% sit] <- "Colony"

# Remaining classes should be swimming
segData$KM_Class[is.na(segData$KM_Class)] <- "Swimming"
table(segData$KM, segData$KM_Class)

#######################################################
# Merge labelled segments with original data, only retaining the KM_Session, KM, and KM_class variables

allData <- merge(myData[,!(names(myData) %in% c("KM","KM_Class","KM_Flights"))],
                 segData[c("tag","KM_Session","KM","KM_Class")],
                 all.x = T)
# Make sure data are in chronological order for each tag
allData <- allData[order(allData$tag, allData$time),]

#######################################################
# This will plot classification for each bird with all predictor variables
# It will take a bit of time for plots to run, then you can scroll backward to look at each bird
# meanPitch240 is a smoothed version of pitch which makes it easier to visualize pitch

birds <- unique(allData$tag)
for (j in birds[21:25]) {
  plotdat <- melt(allData[allData$tag == j,], id.vars = c("time","KM_Class"), measure.vars = c("WBF","meanPitch240","sdZ"))
  pp <- ggplot(data = plotdat, aes(x = time, y = value)) +
    geom_line() +
    geom_point(aes(col = KM_Class)) +
    facet_grid(variable ~ ., scales = "free_y") +
    ggtitle(j)
  print(pp)
}

#########################

#write.csv(allData, "...", row.names = F) 