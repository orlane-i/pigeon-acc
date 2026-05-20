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
lf <- lf[grep("0.csv", lf)] # grap csv file within lf list

# combine all files within one data frame
dat <- NULL
for (i in 1:length(lf)){
  dat1 <- data.table::fread(paste0(pigeon_path, "/data/", lf[i]), data.table = FALSE)
  dat1$file <- lf[i] # create "file" column
  dat <- rbind(dat,dat1)
}

dat <- dat[-c(1:12), ] # remove first GPS data = errors

dat$device_id<-factor(dat$device_id)

# load metadata
metadata <- read.csv("data/metadata.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)
head(metadata)
summary(metadata)
metadata$device_id_june26 <- factor(metadata$device_id_june26)
metadata$device_id_july7 <- factor(metadata$device_id_july7)

library(tidyverse)

metadata_long <- metadata %>%
  # pivot devices
  pivot_longer(
    cols = matches("^device_(id|type)_"),
    names_to = c(".value", "UTC_date"),
    names_pattern = "(device_id|device_type)_(.*)"
  ) %>%
  
  # pivot masses
  pivot_longer(
    cols = starts_with("mass_"),
    names_to = "date_mass",
    names_prefix = "mass_",
    values_to = "mass"
  ) %>%
  
  # keep only rows where dates match
  filter(UTC_date == date_mass) %>%
  
  # cleaning
  select(-date_mass, -X) %>%
  
  # convert date to right format
  mutate(
    UTC_date = recode(UTC_date,
                  "june26" = "2023-06-26",
                  "july7"  = "2023-07-07"),
    UTC_date = as.Date(UTC_date)
  )

# add metadata to main data frame (add indiv names)
dat <- dat %>%
  mutate(UTC_date = as.Date(UTC_date)) %>%
  left_join(metadata_long, by = NULL) %>% # by = NULL : join using all variables in common across dat and metadata_long
  filter(UTC_date >= "2023-06-26") %>% # clean  irrelevant tracks (delete rows before 2023-06-26)
  mutate(local_timestamp = as.POSIXct(UTC_timestamp,"%Y-%m-%d %H:%M:%OS",tz = "Europe/Paris")) # add col with local time

# create body condition metrics = ratio mass/LT
dat$body_condition <- dat$mass/dat$LT

# create accoutumance metrics ("habituation") UTC_date < 2023-07-07 = NO / UTC_date >= 2023-07-07 = YES
dat$habituation <- ifelse(dat$UTC_date < as.Date("2023-07-07"),
                          "NO",
                          "YES")
dat$habituation <- factor(dat$habituation, levels = c("NO", "YES"))

# create duration metrics (duration of tracking by indiv)
dat <- dat %>%
  group_by(indiv_id) %>%
  mutate(duration_hours = as.numeric(max(local_timestamp) - min(local_timestamp), units = "hours")) %>%
  ungroup()
# min duration = 3.26h, max duration = 277h

# get mean duration without NA's
dat %>%
  group_by(indiv_id) %>%
  summarise(duration_hours = first(duration_hours)) %>%
  summarise(mean_duration = mean(duration_hours, na.rm = TRUE)) # mean_duration = 142h

# create session variable and calculate duration by indiv and by session
dat <- dat %>%
  mutate(
    session = ifelse(as.Date(local_timestamp) < as.Date("2023-07-07"),
                     "session1",
                     "session2"),
    session = factor(session)
  ) %>%
  group_by(indiv_id, session) %>%
  mutate(duration_session = as.numeric(max(local_timestamp) - min(local_timestamp),
                                             units = "hours")) %>%
  ungroup()

### visualize duration
library(ggplot2)
plot_data <- dat %>%
  filter(!is.na(indiv_id)) %>%
  group_by(indiv_id, session) %>%
  summarise(duration_session = first(duration_session), .groups = "drop")

ggplot(plot_data, aes(x = indiv_id, y = duration_session, fill = session)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # get summary duration without NA's
# dat %>%
#   group_by(session, indiv_id) %>%
#   summarise(duration_session = first(duration_session)) %>%
#   summarise(mean_duration = mean(duration_session, na.rm = TRUE),
#             min_duration = min(duration_session, na.rm = TRUE),
#             max_duration = max(duration_session, na.rm = TRUE), 
#             median_duration = median(duration_session, na.rm = TRUE)
#             )
# # session  mean_duration min_duration max_duration median_duration
# # session1          27.2         1.50         235.            12.6
# # session2          26.3         2.32         295.            10.9
# 
# # calculate duration by indiv and by session
# duration_session <- dat %>%
#   group_by(indiv_id, session) %>%
#   summarise(
#     duration_hours = as.numeric(max(local_timestamp) - min(local_timestamp), units = "hours"),
#     .groups = "drop"
#   )

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
  acc.xts <- xts::xts(ind[, c("acc_x", "acc_y", "acc_z", "bat_soc_pct", "Latitude", "Longitude", "MSL_altitude_m", "speed_km/h", "int_temperature_C")], order.by = ind$local_timestamp) # cannot keep character var
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
