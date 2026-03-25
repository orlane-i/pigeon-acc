########################################
#   STEP 1 : IMPORT AND PREPARE DATA   #
########################################
rm(list = ls())

cat("\n# SCRIPT 1 : IMPORT AND CLEAN DATA #\n\n")

#######################################
#   1- Import and format accel data   #
#######################################

# set option to display decimal seconds up to 1 digit
options(digits.secs = 3)

# pigeon data path
pigeon_path <- here::here()

# load data
lf <- list.files(paste0(pigeon_path,"/data/")) # list all file names in DATA folder
lf <- lf[grep("csv", lf)] # grap csv file within lf list
lf

# combine all files within one data frame
dat <- NULL
for (i in 1:length(lf)){
  dat1 <- data.table::fread(paste0(pigeon_path, "/data/", lf[i]), data.table = FALSE)
  dat1$file <- lf[i] # create "file" column
  dat1$indiv_id <- strsplit(dat1$file, "_", "_")[[1]][2] # create "indiv_id" column by splitting file names
  dat <- rbind(dat,dat1)
}

# create a var with local timestamp
library(dplyr)
dat <- dat %>% dplyr::mutate(local_timestamp = as.POSIXct(UTC_timestamp,"%Y-%m-%d %H:%M:%OS",tz = "Europe/Paris"))

# clean tracks
dat <- subset(dat, UTC_date>'2023-06-25'&(UTC_date<'2023-06-29'|UTC_date>'2023-07-06')) # remove all rows before june 26 + between june 29 and july 7
View(dat)

cn <- names(dat)
head(dat)
tail(dat)
View(dat)

cat("\n# SCRIPT 1 :\n Data from file -",unique(dat$file),"\n indiv -",unique(dat$indiv_id),"\n have been loaded\n\n")

colnames(dat)
summary(dat)

##############################################################################################################################################################################

##############################################################################################################################################################################

# to work with all indiv data pre treated
#
# load selected indiv data
#file <- "dat_pigeon_selected_filtered.csv"
#dat <- data.table::fread(paste(pigeon_path, "outputs/data_exploration", file, sep = "/"), data.table = FALSE, drop = "V1")
#
## to work with all indiv data
#load(here::here("outputs/activity/data_exploration/script0_dat_all_indiv_pigeon.RData"))

# split data by indiv
dat_list <- split(dat, f = dat$indiv_id)

# create sample test 
ind <- head(dat, 10000)
View(ind)

#subsample first indiv of data list
ind <- dat_list[[1]]

library(zoo)
dat.xts <- lapply(dat_list, function(ind) {
  
  
  ### Plot with 'dygraphs' package ###
  
  ### -> To view and check data (no gap, no lack) and check the timestamp shifts during days (cf 3.3)
  
  col.pal4 <- RColorBrewer::brewer.pal(4, "Set2")
  
  # Plot red tag data
  acc.xts <- xts::xts(ind[, c("acc_x", "acc_y", "acc_z", "bat_soc_pct", "Latitude", "Longitude", "MSL_altitude_m", "speed_km/h", "int_temperature_C")], order.by = ind$UTC_timestamp) # cannot keep character var
  xts::tformat(acc.xts) <- "%Y-%m-%d %H:%M:%OS4"
  
  index(acc.xts)<- index(acc.xts) + 0.001 # add small constant to avoid rounding error of fractional seconds 
  head(format(index(acc.xts), "%Y-%m-%d %H:%M:%OS4"), 15) # check okay
  
  # check if any duplicates in acc.xts 
  table(duplicated(zoo::index(acc.xts)))
  duplicates <- acc.xts[duplicated(zoo::index(acc.xts))] # seems that duplicates are extra data (101 data in a burst instead of 100 -> delete duplicates)
  
  # add a small constant to duplicates 
  acc.xts = xts::make.index.unique(acc.xts, drop = F, eps = 0.0011 ,fromLast = F) # from last = F = the earliest observation with an identical timestamp is treated i.e. the GPS data ? (first duplicated record of the burst), drop = F to conserve GPS data or any data that have close timestamps (ones that have same timestamp than the first accel record would have been deleted)
  table(duplicated(zoo::index(acc.xts))) # check ok
  
  # plot raw accel
  plotdat<- dygraphs::dygraph(acc.xts[,c("acc_x", "acc_y", "acc_z")])
  plotdat<- dygraphs::dyOptions(plotdat, colors= col.pal4[1:3], drawPoints= F, drawAxesAtZero= T, drawGrid= F, useDataTimezone = T)
  plotdat<- dygraphs::dyRangeSelector(plotdat, height= 120)
  plotdat
  
  # save Rdata
  
  # Create path to save plots
  output_data_path_plot <- paste0(pigeon_path,"/outputs/by_indiv/",unique(ind$indiv_id),"/accel_plots")
  dir.create(output_data_path_plot ,showWarnings = FALSE, recursive = T)
  
  htmlwidgets::saveWidget(plotdat, file= paste(output_data_path_plot, "raw_accel_plot.html", sep = "/"), selfcontained = TRUE, background = "white", title = paste(class(plotdat)[[1]], ": Raw accelerometer signals"))
  
  return(acc.xts)
})


##### save data treated 
cat("\n# SCRIPT 1 : SAVE DATA #\n\n")

# Create path to save data
output_activity_data <- paste0(pigeon_path,"/outputs/activity/raw_data/")
dir.create(output_activity_data ,showWarnings = FALSE, recursive = T)

# save dat
save(dat, file = paste0(output_activity_data, "dat_all_indiv_script1.RData"))

# save dat_split
save(dat_list, file = paste0(output_activity_data, "dat_list_all_indiv_script1.RData"))

# save dat.xts
save(dat.xts, file = paste0(output_activity_data, "dat_xts_all_indiv_script1.RData"))
