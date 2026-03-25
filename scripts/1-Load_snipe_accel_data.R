########################################
#   STEP 1 : IMPORT AND PREPARE DATA   #
########################################
cat("\n# SCRIPT 1 : IMPORT AND CLEAN DATA #\n\n")

#######################################
#   1- Import and format accel data   #
#######################################

# # Import data following format -> (multiple species in the same csv or splitted in different csv files)
#
# if (!multiple_csv) {
#
# ###################### start with one csv file for all indiv data #####################
#
# # set option to display decimal seconds up to 1 digit
# options(digits.secs = 3)
#
# # snipe data path
# snipe_path <- here::here()
#
#
# # load data
#
# lf <- list.files(paste0(snipe_path,"/data/")) # list all file names in DATA folder
# lf <- lf[grep("csv", lf)] # grap csv file within lf list
# dat <- data.table::fread(paste(snipe_path, "data", lf[2], sep= "/"), data.table= FALSE)
#
# # set correspondance between device id and indiv name
# device_indiv_id <- data.frame(device_id = as.numeric(c("249351","249343","249349")), indiv_id = c("Snipe FR Drugeon JA752833 apr25", "Snipe FR Drugeon JA752836 avr25", "Snipe FR Drugeon JA752837 avr25"))
# device_indiv_id$indiv_id <- gsub(device_indiv_id$indiv_id, pattern = " ", replacement = "_")
# unique(device_indiv_id$indiv_id) # ckeck
#
# # add indiv name in data
# dat <- dplyr::left_join(dat, device_indiv_id)
#
# # select data for one indiv
# dat <- dat[dat$indiv_id == indiv,] # indiv is setted in make.R parameters
#
# cat(paste("\n# Processing for indiv", indiv,"# \n\n"))
#
# } else {
#
# ###################### start with one csv file per indiv ##########################
#
# # set option to display decimal seconds up to 1 digit
# options(digits.secs = 3)
#
# pigeon data path
pigeon_path <- here::here()
# load data

lf <- list.files(paste0(pigeon_path,"/data/")) # list all file names in DATA folder
lf <- lf[grep("csv", lf)] # grap csv file within lf list
lf
dat <- data.table::fread(paste0(pigeon_path, "/data/", lf[1]), data.table= FALSE)

cn <- names(dat)

# add information on origin file
dat$file<- lf[1]
print(lf[1])

for(i in lf[-1]){
## Read file
tmp <- data.table::fread(paste0(pigeon_path, "/data/", i), data.table= FALSE, col.names= cn) # read data file 'i'
## combine file lf[i] with previous data
try(dat<- rbind(dat, cbind(tmp, file= i))) # wrap in 'try' because some files may be empty and generate an error
print(i)#print the lf[i] added to first file : lf[1]
}

lf
dat <- dat[dat$file == lf[3],]

# set indiv id
dat$indiv_id <- substr(dat$file, 1, 25) # done in the cleaning function here after

cat("\n# SCRIPT 1 :\n Data from file -",unique(dat$file),"\n indiv -",unique(dat$indiv_id),"\n have been loaded\n\n")

# #### clean data

colnames(dat)
View(dat)
#
#
#
# ##############################################################################################################################################################################
#
# ##############################################################################################################################################################################
#
# # to work with all indiv data pre treated
#
#
# # load selected indiv data
# file <- "dat_snipe_selected_filtered.csv"
# dat <- data.table::fread(paste(snipe_path, "outputs/data_exploration", file, sep= "/"), data.table= FALSE, drop = "V1")

# to work with all indiv data
load(here::here("outputs/activity/data_exploration/script0_dat_all_indiv_snipe.RData"))

# split data by indiv
dat_list <- split(dat, f = dat$indiv_id)

# create sample test 
#ind <- head(dat, 10000)

dat.xts <- lapply(dat_list, function(ind) {
  
  
### Plot with 'dygraphs' package ###

### -> To view and check data (no gap, no lack) and check the timestamp shifts during days (cf 3.3)

col.pal4<- RColorBrewer::brewer.pal(4, "Set2")

# Plot red tag data
acc.xts <- xts::xts(ind[, c("acc_x", "acc_y", "acc_z", "bat_soc_pct", "Latitude", "Longitude", "MSL_altitude_m", "speed_km/h", "int_temperature_C")], order.by= ind$UTC_timestamp) # cannot keep character var
xts::tformat(acc.xts) <- "%Y-%m-%d %H:%M:%OS4"

#index(acc.xts)<- index(acc.xts) + 0.001 # add small constant to avoid rounding error of fractional seconds 
head(format(index(acc.xts), "%Y-%m-%d %H:%M:%OS4"), 15) # check okay

# check if any duplicates in acc.xts 
table(duplicated(zoo::index(acc.xts)))
duplicates <- acc.xts[duplicated(zoo::index(acc.xts))] # seems that duplicates are extra data (101 data in a burst instead of 100 -> delete duplicates)

# add a small constant to duplicates 
acc.xts = xts::make.index.unique(acc.xts, drop = F, eps = 0.0011 ,fromLast = F) # from last = F = the earliest observation with an identical timestamp is treated i.e. the GPS data ? (first duplicated record of the burst), drop = F to conserve GPS data or any data that have close timestamps (ones that have same timestamp than the first accel record would have been deleted)
table(duplicated(zoo::index(acc.xts))) # check ok

# plot raw accel
plotdat<- dygraphs::dygraph(acc.xts[,c("acc_x", "acc_y", "acc_z")]) #"2025-04-11 20:58:47/2025-04-25 21:00:00"
plotdat<- dygraphs::dyOptions(plotdat, colors= col.pal4[1:3], drawPoints= F, drawAxesAtZero= T, drawGrid= F, useDataTimezone = T)
plotdat<- dygraphs::dyRangeSelector(plotdat, height= 120)
plotdat

# save Rdata

# Create path to save plots
output_data_path_plot <- paste0(snipe_path,"/outputs/by_indiv/",unique(ind$indiv_id),"/accel_plots")
dir.create(output_data_path_plot ,showWarnings = FALSE, recursive = T)

htmlwidgets::saveWidget(plotdat, file= paste(output_data_path_plot, "raw_accel_plot.html", sep = "/"), selfcontained = TRUE, background = "white", title = paste(class(plotdat)[[1]], ": Raw accelerometer signals"))

return(acc.xts)
})



##### save data treated 
cat("\n# SCRIPT 1 : SAVE DATA #\n\n")

# Create path to save data
output_activity_data <- paste0(snipe_path,"/outputs/activity/data/")
dir.create(output_activity_data ,showWarnings = FALSE, recursive = T)

# save dat
save(dat,file= paste0(output_activity_data,"dat_all_indiv_script1.RData"))

# save dat_split
save(dat_list,file= paste0(output_activity_data,"dat_list_all_indiv_script1.RData"))

# save dat.xts
save(dat.xts,file= paste0(output_activity_data,"dat_xts_all_indiv_script1.RData"))
