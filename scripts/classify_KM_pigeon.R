# This code implements the k-means (KM) method for black-legged kittiwakes
# described in the paper Patterson et al  - A comparison of techniques for classifying behaviour from accelerometers for two species of seabird
# Data from this paper can be downloaded from Dryad with this code.
# This code will implement this classification on those data, or could be modified to use your own data.

#########################

library(ggplot2)
library(doBy)
library(vegan)
library(reshape2)
require(sp)
require(zoo)
options(digits.secs=4)

# source("accFunctions.R")

#########################

# Load data
pigeon_path <- here::here()
output_activity_accdata <- paste0(pigeon_path,"/outputs/activity/acc_data/")
load(paste0(output_activity_accdata,"dat_acc_process_accelerometer.RData"))

# Format time
subData$time <- as.POSIXct(as.character(subData$time))

# Make sure data are in chronological order for each tag
subData <- subData[order(subData$indiv, subData$time),]

################################
# Do initial classification of flying with a 2 class kmeans cluster
subData$KM_Flights <- kmeans(subData$WBF, 2)$cluster

# Examine results of flight classification
boxplot(WBF ~ KM_Flights, subData)

# Determine which class is flying
tt <- summaryBy(WBF ~ KM_Flights, subData, FUN = summary)
fly <- which(tt$WBF.Mean > 2)

########################################
# Function to filter out behaviours occuring for less than mintime
# Input:
# rawClassification - the variable you want to filter
# mintime - the minimum number of sequential occurences of a behaviour required, anything less will be assigned to the previous behaviour
# Note: this was designed with murres in mind so transitions between behaviours labelled Diving and Swimming will not be filtered 

filterClass <- function(rawClassification, mintime = 2) {
  output <- rawClassification
  for (i in 2:(length(output) - mintime)) {
    temp <- output[i:(i + (mintime - 1))]
    # temp <- ifelse(temp == "Diving", "Swimming", temp)
    tt <- which(names(table(temp)) == temp[1])
    if (table(temp)[tt] != mintime) output[i] <- output[i - 1]
  }
  output
}

# Filter any flights above that occur for less than 3 seconds
# This will take some time if you are working with all tracks
subData$KM_Flights <- filterClass(subData$KM_Flights, 3)

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

# Make a new variable identifying segments of continuous behaviour
subData$KM_Session <- getSessions(subData$KM_Flights, maxSession = 120)

################################
# Calculate mean values of predictor variables within behaviour segments

segData <- summaryBy(pitch + sdZ + WBF ~ indiv + KM_Session + KM_Flights, subData, 
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

# # Classes with mean pitch > 10 should labelled as colony
# (tt <- summaryBy(pitch ~ KM, data = segData))
# (sit <- tt$KM[tt$pitch.mean > 10])
# segData$KM_Class[segData$KM %in% sit] <- "Colony"
# 
# # Remaining classes should be swimming
# segData$KM_Class[is.na(segData$KM_Class)] <- "Swimming"
# table(segData$KM, segData$KM_Class)

#######################################################
# Merge labelled segments with original data, only retaining the KM_Session, KM, and KM_class variables

allData <- merge(subData[,!(names(subData) %in% c("KM","KM_Class","KM_Flights"))],
                 segData[c("indiv","KM_Session","KM","KM_Class")],
                 all.x = T)
# Make sure data are in chronological order for each tag
allData <- allData[order(allData$indiv, allData$time),]

#######################################################
# This will plot classification for each bird with all predictor variables
# It will take a bit of time for plots to run, then you can scroll backward to look at each bird
# meanPitch240 is a smoothed version of pitch which makes it easier to visualize pitch

# birds <- unique(allData$indiv)
# for (j in birds) {
#   plotdat <- melt(allData[allData$indiv == j,], id.vars = c("time","KM_Class"), measure.vars = c("WBF","meanPitch240","sdZ"))
#   pp <- ggplot(data = plotdat, aes(x = time, y = value)) +
#     geom_line() +
#     geom_point(aes(col = KM_Class)) +
#     facet_grid(variable ~ ., scales = "free_y") +
#     ggtitle(j)
#   print(pp)
# }


# for (j in birds) {
# 
#   # 1. subset des données
#   subdat <- allData[allData$indiv == j, ]
# 
#   # 2. créer un objet xts (important !)
#   xtsdat <- xts::xts(
#     subdat[, c("WBF", "meanPitch240", "sdZ")],
#     order.by = subdat$time
#   )
# 
#   # 3. rendre les index uniques si besoin
#   xtsdat <- xts::make.index.unique(xtsdat, eps = 0.001)
# 
#   # 4. plot dygraph
#   WBFplot <- dygraphs::dygraph(xtsdat, main = j) %>%
#     dygraphs::dyOptions(drawPoints = TRUE) %>%
#     dygraphs::dyRangeSelector()
#   WBFplot
# 
# ####
# # library(dplyr)
# # 
# # for (j in birds) {
# #   subdat <- allData %>% filter(indiv == j)
# #   
# #   # split par classe
# #   split_list <- split(subdat, subdat$KM_Class)
# #   
# #   xts_list <- lapply(split_list, function(df) {
# #     xts::xts(df$WBF, order.by = df$time)
# #   })
# #   
# #   merged <- do.call(merge, xts_list)
# #   colnames(merged) <- names(xts_list)
# #   
# #   WBFplot <- dygraphs::dygraph(merged, main = paste(j, "- WBF")) %>%
# #     dygraphs::dyOptions(drawPoints = TRUE) %>%
# #     dygraphs::dyRangeSelector() %>%
# #     print(WBFplot)
#   
#   # save Rdata
#   
#   # Create path to save plots
#   output_data_path_plot <- paste0(pigeon_path,"/outputs/by_indiv/",birds,"/accel_plots")
#   
#   htmlwidgets::saveWidget(WBFplot, file= paste(output_data_path_plot, "WBF_plot.html", sep = "/"), selfcontained = TRUE, background = "white", title = paste(class(WBFplot)[[1]], ": WBF"))
# }

# plot with 'dygraphs'

library(dplyr)
library(xts)
library(dygraphs)
library(htmlwidgets)

birds <- unique(allData$indiv)

for (j in birds) {
  
  # path
  output_data_path_plot <- paste0(pigeon_path,"/outputs/by_indiv/", j,"/accel_plots")
  
  # subset
  subdat <- allData %>% filter(indiv == j)
  
  # split by KM_Class
  split_list <- split(subdat, subdat$KM_Class)
  
  # create xts list by class
  xts_list <- lapply(split_list, function(df) {
    xts::xts(df$WBF, order.by = df$time)
  })
  
  # merge classes
  xts_merged <- do.call(merge, xts_list)
  
  # col names = classes
  colnames(xts_merged) <- paste0(names(xts_list))
  
  # 
  xts_merged <- xts::make.index.unique(xts_merged, eps = 0.001)
  
  # plot
  WBFplot <- dygraphs::dygraph(xts_merged, main = paste(j, "Wing Beat Frequency")) %>%
    dygraphs::dyAxis("y", label = "WBF (Hz)") %>%
    dygraphs::dyOptions(drawPoints = TRUE) %>%
    dygraphs::dyRangeSelector()
  
  # save
  htmlwidgets::saveWidget(
    WBFplot,
    file = paste(output_data_path_plot, "WBF_plot.html", sep = "/"),
    selfcontained = TRUE,
    background = "white",
    title = class(WBFplot)[[1]]
  )
}
######################################################
library(dplyr)
library(xts)
library(dygraphs)
library(htmlwidgets)

birds <- unique(allData$indiv)
vars <- c("WBF", "meanPitch240", "sdZ")

# for (j in birds) {
#   
#   # dossier de sortie
#   output_path <- file.path(pigeon_path, "outputs", "by_indiv", j, "accel_plots")
#   dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
#   
#   # subset individu
#   subdat <- allData %>% filter(indiv == j)
#   
#   for (v in vars) {
#     
#     # split par KM_Class
#     split_list <- split(subdat, subdat$KM_Class)
#     
#     # créer xts par classe
#     xts_list <- lapply(split_list, function(df) {
#       xts::xts(df[[v]], order.by = df$time)
#     })
#     
#     # merge
#     xts_merged <- do.call(merge, xts_list)
#     colnames(xts_merged) <- paste0("KM_", names(xts_list))
#     
#     # sécuriser index temps
#     xts_merged <- xts::make.index.unique(xts_merged, eps = 0.001)
#     
#     # label axe Y propre
#     y_label <- switch(v,
#                       "WBF" = "WBF (Hz)",
#                       "meanPitch240" = "Mean Pitch",
#                       "sdZ" = "sdZ",
#                       v)
#     
#     # plot
#     p <- dygraphs::dygraph(xts_merged, main = paste(j, "-", v, "by KM_Class")) %>%
#       dygraphs::dyAxis("y", label = y_label) %>%
#       dygraphs::dyOptions(drawPoints = TRUE) %>%
#       dygraphs::dyRangeSelector()
#     
#     # sauvegarde
#     htmlwidgets::saveWidget(
#       p,
#       file = file.path(output_path, paste0(v, "_by_KM_class.html")),
#       selfcontained = TRUE,
#       background = "white"
#     )
#   }
# }


#########################

#write.csv(allData, "...", row.names = F) 

# save allData
save(allData, file = paste0(output_activity_accdata, "dat_kmeans_clustering.RData"))