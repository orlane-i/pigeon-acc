########################################
#   STEP 1 : IMPORT AND PREPARE DATA   #
########################################

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

# combine all files within one data frame
dat <- NULL
for (i in 1:length(lf)){
  dat1 <- data.table::fread(paste0(pigeon_path, "/data/", lf[i]), data.table = FALSE)
  dat1$file <- lf[i] # create "file" column
  dat <- rbind(dat,dat1)
}

dat$device_id<-factor(dat$device_id)

# set correspondance between device id and indiv name
library(dplyr)
mapping <- tibble(
  device_id = c(
    "233087","233087",
    "233090","233121",
    "233088","233127",
    "233089",
    "233119",
    "233121","231900",
    "233122","233119",
    "233120","231899",
    "231900","233090",
    "231897",
    "231898","233088",
    "231902","231901",
    "231898",
    "233120",
    "231902",
    "233091",
    "233122",
    "233092"
  ),
  
  date_debut = as.Date(c(
    "2023-06-26","2023-07-07",
    "2023-06-26","2023-07-07",
    "2023-06-26","2023-07-07",
    "2023-06-26",
    "2023-06-26",
    "2023-06-26","2023-07-07",
    "2023-06-26","2023-07-07",
    "2023-06-26","2023-07-07",
    "2023-06-26","2023-07-07",
    "2023-06-26",
    "2023-06-26","2023-07-07",
    "2023-06-26","2023-07-07",
    "2023-07-07",
    "2023-07-07",
    "2023-07-07",
    "2023-07-07",
    "2023-07-07",
    "2023-07-07"
  )),
  
  date_fin = as.Date(c(
    "2023-07-06","2023-07-31",
    "2023-07-06","2023-07-31",
    "2023-07-06","2023-07-31",
    "2023-07-06",
    "2023-07-06",
    "2023-07-06","2023-07-31",
    "2023-07-06","2023-07-31",
    "2023-07-06","2023-07-31",
    "2023-07-06","2023-07-31",
    "2023-07-06",
    "2023-07-06","2023-07-31",
    "2023-07-06","2023-07-31",
    "2023-07-31",
    "2023-07-31",
    "2023-07-31",
    "2023-07-31",
    "2023-07-31",
    "2023-07-31"
  )),
  
  indiv_id = c(
    "FR2021284043","FR2021239893",
    "FR2018394142","FR2018394142",
    "FR2022160870","FR2022160870",
    "FR2017193558",
    "FR2018394114",
    "FR2021284021","FR2021284021",
    "FR2021284035","FR2021284035",
    "FR2022013358","FR2022013358",
    "FR2017193529","FR2017193529",
    "FR2021284034",
    "FR2022160883","FR2022160883",
    "FR2017193535","FR2017193535",
    "FR2020355491",
    "FR2022160852",
    "FR2018304114",
    "FR2022160868",
    "FR2021284046",
    "FR2020005213"
  )
)

# add indiv name in data
dat$UTC_date <- as.Date(dat$UTC_date)

dat <- dat %>%
  mutate(UTC_date = as.Date(UTC_date)) %>%
  left_join(mapping, by = "device_id") %>%
  filter(UTC_date >= date_debut & UTC_date <= date_fin) %>% # clean tracks (delete rows before 2023-06-26)
  select(-date_debut, -date_fin)

# create a var with local timestamp
dat <- dat %>% dplyr::mutate(local_timestamp = as.POSIXct(UTC_timestamp,"%Y-%m-%d %H:%M:%OS",tz = "Europe/Paris"))

cn <- names(dat)

cat("\n# SCRIPT 1 :\n Data from file -",unique(dat$file),"\n indiv -",unique(dat$indiv_id),"\n have been loaded\n\n")

# split data by indiv
dat_list <- split(dat, f = dat$indiv_id)

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
