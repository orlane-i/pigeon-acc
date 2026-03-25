######################################
#   STEP 2 : SUMMARY STATISTICS     #
#####################################
cat("\n# SCRIPT 2 : COMPUTING SUMMARY STATISTICS #\n\n")

##################################
#   1- Summary statistics       #
#################################

#dat.xts <- lapply(dat.xts, function(acc.xts) {

#### load data from script 1 if script launched alone

# load dat xts
load(paste0(output_activity_data,"dat_xts_all_indiv_script1.RData"))
#load dat list
load(paste0(output_activity_data,"dat_list_all_indiv_script1.RData"))

# sample 2 indivs to test script
# save_dat_list <- dat_list
# save_dat_xts <- dat.xts
# 
# dat_list <- list(dat_list[[1]],dat_list[[5]])
# names(dat_list) <- names(save_dat_list)[c(1,5)]
# 
# dat.xts <- list(dat.xts[[1]],dat.xts[[5]])
# names(dat.xts) <- names(save_dat_xts)[c(1,5)]

# initiate dat.1s.xts list
dat.1s.xts <- list()

for (indiv in names(dat.xts)) {
  
  acc.xts <- dat.xts[[indiv]]
  

  cat("\n# SCRIPT 2 : COMPUTING SUMMARY STATISTICS for indiv #\n", indiv,"\n")
  
  
##### Insert NAs in gaps  #####
  
  # remove GPS data to keep 10data per sec for rolling functions
  GPSindexes <- dat_list[[indiv]]$UTC_timestamp[dat_list[[indiv]]$datatype == "GPS"]
  GPSxts <- acc.xts[GPSindexes,]
  acc.xts <- acc.xts[!index(acc.xts) %in% GPSindexes, ]

  ### Insert 3s (i.e. 3*samp.Hz) of NA values between each accelerometry burst and between sequence -> (to avoid overlaps of moving window between bursts or different obs sequences i.e. avoid compute mean including accel data with large time gaps)
  samp.Hz<- 10 # sampling frequency for accelerometry bursts (in Hertz)
  acc.burst.endtimes <- dat_list[[indiv]]$UTC_timestamp[dat_list[[indiv]]$datatype == "SEN_ACC_10Hz_END"]
  if(length(acc.burst.endtimes) > 0){
    times.to.insert<- do.call(c, lapply(acc.burst.endtimes, FUN= function(x) seq(from= x + 1/samp.Hz, to= x + 3 , by=  1/samp.Hz))) # generate sequences of time indices starting after each burst endtime
    augmented.xts<- xts::xts(order.by= c(zoo::index(acc.xts), times.to.insert), tzone ="UTC") # create an empty xts object containing combined time indexes of acc.xts and times.to.insert i.e acc.xts times + gaps of 3s between bursts
    acc.xts <- merge(augmented.xts, acc.xts, fill= NA)# insert empty data (-> 30 NAs) at each gap greater than 3s in acc.xts
  }
  

###### 1. Summary statistics ######

acc.zoo= zoo::as.zoo(acc.xts)# zoo::rollapply on xts do not support argument needed : convert xts object in zoo to use the zoo::zoo::rollapply function 

#table(duplicated(index(acc.xts)))  # duplicated timestamps after rounded up -> some behaviors < 1s 

# compute rolling mean on a 1s window (step : by 1) (zoo::rollapply is faster with one columnat a time) 
# => (smoothed) = static acceleration 

acc.xts$surg.mu.1s <- zoo::rollapply(data= acc.zoo$acc_y, width= 1*samp.Hz, by= 1, FUN= mean, partial= T, fill= NA, align= "right", na.rm = T) # na.rm = T -> consequence is some means are computed on less than 10 data and even on only one data but negligible considering the amount of data 
acc.xts$sway.mu.1s <- zoo::rollapply(data= acc.zoo$acc_x, width= 1*samp.Hz, by= 1, FUN= mean, partial= T, fill= NA, align= "right", na.rm = T) # nb : "left" means with the 9 following data (last mean = last acc data because mean of an acc with 9 NA), "right" means with the 9 data before (i.e first mean = first acc), "cenetered" mean with the 5 data before and 4 after 
acc.xts$heav.mu.1s <- zoo::rollapply(data= acc.zoo$acc_z, width= 1*samp.Hz, by= 1, FUN= mean, partial= T, fill= NA, align= "right", na.rm = T)

# compute dynamic acceleration (detrended raw data)
acc.xts$surg.dyn<- acc.xts$acc_y - acc.xts$surg.mu.1s  # dyn accel = raw data - rolling mean(en 10hz) == raw data - static accel(10hz)
acc.xts$sway.dyn<- acc.xts$acc_x - acc.xts$sway.mu.1s
acc.xts$heav.dyn<- acc.xts$acc_z - acc.xts$heav.mu.1s

# Vectorial dynamic body acceleration   
acc.xts$VeDBA<- sqrt(acc.xts$surg.dyn^2 + acc.xts$sway.dyn^2 + acc.xts$heav.dyn^2)
# Sectional dynamic body acceleration -> could be useful instead of "heave" if collar rotates around the neck - this does lose the sign of the acceleration
acc.xts$SDBA<- sqrt(acc.xts$sway.dyn^2 + acc.xts$heav.dyn^2)

# plot correlation
#plot(as.vector(acc.xts$SDBA), as.vector(abs(acc.xts$heav.dyn)), pch= ".", col= rgb(0, 0, 0, 0.6))

# Sub-sample data at 1Hz frequency

## sub-sample data at 1s interval => @ 1Hz accel data (step: by 10 i.e gives a results every 10 data)
# Static acceleration on a 1s window
acc.1s.xts<- xts::xts(x= zoo::rollapply(data= acc.zoo$acc_y, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T))
names(acc.1s.xts)<- "surg.mu.1s"
acc.1s.xts$sway.mu.1s<- zoo::rollapply(data= acc.zoo$acc_x, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$heav.mu.1s<- zoo::rollapply(data= acc.zoo$acc_z, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)

# compute rolling mean on a 3s window on 1Hz accel data (step: by 10) 
acc.1s.xts$surg.mu.3s<- zoo::rollapply(data= acc.zoo$acc_y, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$sway.mu.3s<- zoo::rollapply(data= acc.zoo$acc_x, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$heav.mu.3s<- zoo::rollapply(data= acc.zoo$acc_z, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)


# compute body pitch @ 1 Hz and 3 Hz and centre on -1.5 to help visualisation
acc.1s.xts$Pitch.1s<- atan2(acc.1s.xts$surg.mu.1s, sqrt(acc.1s.xts$sway.mu.1s^2 + acc.1s.xts$heav.mu.1s^2)) * 180 / pi / 100 - 1.5   # 1 Hz nb : atan2(y, x) == atan(y/x)
acc.1s.xts$Pitch.3s<- atan2(acc.1s.xts$surg.mu.3s, sqrt(acc.1s.xts$sway.mu.3s^2 + acc.1s.xts$heav.mu.3s^2)) * 180 / pi / 100 - 1.5   # 3Hz 

# compute body roll @ 1 and 3 Hz and centre on -1.5 to help visualisation
acc.1s.xts$Roll.1s <- atan2(acc.1s.xts$sway.mu.1s, sqrt(acc.1s.xts$surg.mu.1s^2 + acc.1s.xts$heav.mu.1s^2)) * 180 / pi / 100 - 1.5 # 1Hz
acc.1s.xts$Roll.3s <- atan2(acc.1s.xts$sway.mu.3s, sqrt(acc.1s.xts$surg.mu.3s^2 + acc.1s.xts$heav.mu.3s^2)) * 180 / pi / 100 - 1.5 # 3Hz

acc.zoo= zoo::as.zoo(acc.xts)# again need to convert acc.xts$accel.dyn +  acc.xts$VeDBA + SDBA  as.zoo for zoo::rollapply function


# Compute acc.dyn mean on 3s
acc.1s.xts$surg.dyn.3s<- zoo::rollapply(data= acc.zoo$surg.dyn, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$sway.dyn.3s<- zoo::rollapply(data= acc.zoo$sway.dyn, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$heav.dyn.3s<- zoo::rollapply(data= acc.zoo$heav.dyn, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)


# compute rolling SD on a 1s window on 1Hz accel data (step: by 10)  #inutile ?
acc.1s.xts$surg.dyn.sd.1s<- zoo::rollapply(data= acc.zoo$surg.dyn, width= 1*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$sway.dyn.sd.1s<- zoo::rollapply(data= acc.zoo$sway.dyn, width= 1*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$heav.dyn.sd.1s<- zoo::rollapply(data= acc.zoo$heav.dyn, width= 1*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)


# compute rolling SD on a 3s window on 1Hz accel data (step: by 10)  
acc.1s.xts$surg.dyn.sd.3s<- zoo::rollapply(data= acc.zoo$surg.dyn, width= 3*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$sway.dyn.sd.3s<- zoo::rollapply(data= acc.zoo$sway.dyn, width= 3*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$heav.dyn.sd.3s<- zoo::rollapply(data= acc.zoo$heav.dyn, width= 3*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)


# Vectorial dynamic body acceleration @ 1 Hz
acc.1s.xts$VeDBA.1s<- zoo::rollapply(data= acc.zoo$VeDBA, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$VeDBA.3s<- zoo::rollapply(data= acc.zoo$VeDBA, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
#sd
acc.1s.xts$VeDBA.sd.1s<- zoo::rollapply(data= acc.zoo$VeDBA, width= 1*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$VeDBA.sd.3s<- zoo::rollapply(data= acc.zoo$VeDBA, width= 3*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)

# sectional dynamic body acceleration @ 1 Hz
acc.1s.xts$SDBA.1s<- zoo::rollapply(data= acc.zoo$SDBA, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$SDBA.3s<- zoo::rollapply(data= acc.zoo$SDBA, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
#sd
acc.1s.xts$SDBA.sd.1s<- zoo::rollapply(data= acc.zoo$SDBA, width= 1*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$SDBA.sd.3s<- zoo::rollapply(data= acc.zoo$SDBA, width= 3*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)

# data transformations
# no zeros in data, but use a small offset to prevent a long tail to the left
acc.1s.xts$log.VeDBA.sd.1s<- log(acc.1s.xts$VeDBA.sd.1s + 0.1)
acc.1s.xts$log.SDBA.sd.1s<- log(acc.1s.xts$SDBA.sd.1s + 0.1)
acc.1s.xts$log.VeDBA.sd.3s<- log(acc.1s.xts$VeDBA.sd.3s + 0.1)
acc.1s.xts$log.SDBA.sd.3s<- log(acc.1s.xts$SDBA.sd.3s + 0.1)

# compute amplitude mean @ 1 & 3 HZ
acc.xts$PDBAX <- abs(acc.xts$surg.dyn)
acc.xts$PDBAY <- abs(acc.xts$sway.dyn)
acc.xts$PDBAZ <- abs(acc.xts$heav.dyn)

acc.zoo= zoo::as.zoo(acc.xts)# again need to convert acc.xts$PDBA as.zoo for zoo::rollapply function

acc.1s.xts$PDBAX.mu.1s<- zoo::rollapply(data= acc.zoo$PDBAX, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$PDBAY.mu.1s<- zoo::rollapply(data= acc.zoo$PDBAY, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$PDBAZ.mu.1s<- zoo::rollapply(data= acc.zoo$PDBAZ, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)

acc.1s.xts$PDBAX.mu.3s<- zoo::rollapply(data= acc.zoo$PDBAX, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$PDBAY.mu.3s<- zoo::rollapply(data= acc.zoo$PDBAY, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$PDBAZ.mu.3s<- zoo::rollapply(data= acc.zoo$PDBAZ, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)

# Overall dynamic body acceleration @ 1 & 3 HZ
acc.xts$ODBA <- acc.xts$PDBAY + acc.xts$PDBAX +acc.xts$PDBAZ # 10Hz

acc.1s.xts$ODBA.mu.1s <- acc.1s.xts$PDBAY.mu.1s + acc.1s.xts$PDBAX.mu.1s +acc.1s.xts$PDBAZ.mu.1s # 1Hz
acc.1s.xts$ODBA.mu.3s <- acc.1s.xts$PDBAY.mu.3s + acc.1s.xts$PDBAX.mu.3s +acc.1s.xts$PDBAZ.mu.3s 
# or same as
#acc.1s.xts$ODBA.mu.1s<- zoo::rollapply(data= acc.zoo$ODBA, width= 1*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)
#acc.1s.xts$ODBA.mu.3s<- zoo::rollapply(data= acc.zoo$ODBA, width= 3*samp.Hz, by= 1*samp.Hz, FUN= mean, partial= T, fill= NA, align= "right", na.rm= T)

acc.zoo= zoo::as.zoo(acc.xts)# again need to convert acc.xts$PDBA as.zoo for zoo::rollapply function

# sd
acc.1s.xts$ODBA.sd.1s<- zoo::rollapply(data= acc.zoo$ODBA, width= 1*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)
acc.1s.xts$ODBA.sd.3s<- zoo::rollapply(data= acc.zoo$ODBA, width= 3*samp.Hz, by= 1*samp.Hz, FUN= sd, partial= T, fill= NA, align= "right", na.rm= T)

# data transformations
# no zeros in data, but use a small offset to prevent a long tail to the left
acc.1s.xts$log.ODBA.sd.1s<- log(acc.1s.xts$ODBA.sd.1s + 0.1)
acc.1s.xts$log.ODBA.sd.3s<- log(acc.1s.xts$ODBA.sd.3s + 0.1)


save.acc.xts <- acc.xts
save.acc.1s.xts <- acc.1s.xts



#Exclude rows of acc.xts that only contain NA values -> keeps rows with acc.mu extended my rolling mean
to.keep<- apply(acc.xts, 1, function(x){!all(is.na(x))})
table(to.keep)
acc.xts <- acc.xts[to.keep, ] # delete rows that are full of NA
nrow(acc.xts)

# to.keep<- apply(acc.1s.xts, 1, function(x){!all(is.na(x))})
# table(to.keep)
# acc.1s.xts <- acc.1s.xts[to.keep, ] # delete rows that are full of NA
# nrow(acc.1s.xts) #  # leaves NA -> use completecases instead

# can deleate all remaining NAs, instead using "complete.cases()"
to.keep <- complete.cases(acc.1s.xts) #False if NA in one column
table(to.keep)
acc.1s.xts <- acc.1s.xts[to.keep, ] # delete rows with at least one NA var




#### re-add datatype and gps data

# transform in df to rbind data
acc.df <- data.frame(acc.xts) %>% dplyr::mutate(index = index(acc.xts),
                                                datatype = NA) # ifelse(index %in% acc.burst.endtimes, "SEN_ACC_10Hz_END", NA) # do not add end bursts as they are shifted with rolling functions (ends up in the middle of meaned estimates)

acc.1s.df <- data.frame(acc.1s.xts) %>% dplyr::mutate(index = index(acc.1s.xts),
                                                datatype = NA) # ifelse(index %in% acc.burst.endtimes, "SEN_ACC_10Hz_END", NA) # do not add end bursts as they are shifted with rolling functions (ends up in the middle of meaned estimates)

gps.df <- data.frame(GPSxts) %>% dplyr::mutate(index = index(GPSxts),
                                                datatype = "GPS")
# create a copie to merge with acc.1s
gps.1s.df <- gps.df

# add new columns of acc.xts to GPSxts
for (col in setdiff(names(acc.df), names(gps.df))) { gps.df[[col]] <- NA } # extract new columns in acc.xts, add to gps data with NA

for (col in setdiff(names(acc.1s.df), names(gps.1s.df))) { gps.1s.df[[col]] <- NA } # extract new columns in acc.1s.xts, add to gps data with NA
for (col in setdiff(names(gps.1s.df), names(acc.1s.df))) { acc.1s.df[[col]] <- NA } # extract new columns in gps, add to acc data with NA

#reorder columns as in acc.1s to check
gps.1s.df <- gps.1s.df[ , names(acc.1s.df)]

# rbind gps data
acc.df <- rbind(acc.df, gps.df)
acc.xts <- xts(acc.df[ , setdiff(names(acc.df), "index")], # remove index column
               order.by = acc.df$index, tzone = "UTC")

acc.1s.df <- rbind(acc.1s.df, gps.1s.df)
acc.1s.xts <- xts(acc.1s.df[ , setdiff(names(acc.1s.df), "index")], # remove index column
               order.by = acc.1s.df$index, tzone = "UTC")

# reorder by index in case
acc.xts <- acc.xts[order(index(acc.xts))]
acc.1s.xts <- acc.1s.xts[order(index(acc.1s.xts))]

#rm(acc.1s.df   ,acc.1s.xts ,acc.burst.endtimes , acc.df, acc.xts , acc.zoo ,  augmented.xts, dat.1s.xts , dat.xts, gps.1s.df ,gps.df  , GPSindexes  , GPSxts , i ,  indiv, multiple_csv, na_col,    newplot, output_data_path_indiv, output_data_path_plot,  result.dat.1s.xts,     result.dat.xts ,    save.acc.1s.xts,      save.acc.xts ,     times.to.insert)


#######################################
#   2- Make dygraphs of variables     #
#######################################

cat("\n# SCRIPT 2 : PLOTING SUMMARY STATISTICS #\n\n")

# Create path to save data
output_data_path_indiv <- paste0(snipe_path,"/outputs/by_indiv/",indiv,"/data") # indiv = indiv_id
dir.create(output_data_path_indiv ,showWarnings = FALSE, recursive = T)

# save data in rdata files
save(acc.xts,file= paste0(output_data_path_indiv, "/acc.xts_script2.RData"))
save(acc.1s.xts,file= paste0(output_data_path_indiv, "/acc.1s.xts_script2.RData"))




##### Plot summary statistics  #####

# Create path to save plots in indiv files
output_data_path_plot <- paste0(snipe_path,"/outputs/by_indiv/",indiv,"/accel_plots")
dir.create(output_data_path_plot ,showWarnings = FALSE, recursive = T)



# Analysis of accelerometer data
col.pal3 <- RColorBrewer::brewer.pal(3, "Set1")
col.pal8 <- RColorBrewer::brewer.pal(8, "Set2")
col.pal4 <- RColorBrewer::brewer.pal(4, "Set2")
col.pal9 <- RColorBrewer::brewer.pal(9, "Set3")

# visualise a subset
newplot<- dygraphs::dygraph(acc.1s.xts[, c("surg.mu.1s", "sway.mu.1s", "heav.mu.1s")])
newplot<- dygraphs::dyOptions(newplot, colors= col.pal4[1:3], drawPoints= F, connectSeparatedPoints = F, drawAxesAtZero= T, drawGrid= F, useDataTimezone = T) #  pointSize= 1.5, pointShape= "circle",
newplot<- dygraphs::dyRangeSelector(newplot, height= 120)
newplot

htmlwidgets::saveWidget(newplot, file= paste(output_data_path_plot, "static_1s_accel_plot.html", sep = "/"), background = "white", title = paste(class(newplot)[[1]], ": static acceleration 1s"))


# view data with feeding behav to estimate periodicy of interest for cwt
newplot<- dygraphs::dygraph(acc.xts[,c("surg.dyn", "sway.dyn", "heav.dyn")])
newplot<- dygraphs::dyOptions(newplot, colors= col.pal8, drawPoints= F, pointSize= 1.5, pointShape= "circle", drawAxesAtZero= T, drawGrid= F, useDataTimezone = T)
newplot<- dygraphs::dyRangeSelector(newplot, height= 120)
newplot

htmlwidgets::saveWidget(newplot, file= paste(output_data_path_plot, "dynamic_accel_plot.html", sep = "/"), background = "white", title = paste(class(newplot)[[1]], ": dynamic acceleration"))


## plot reduced data
newplot<- dygraphs::dygraph(acc.1s.xts[, c("surg.mu.3s", "sway.mu.3s", "heav.mu.3s")])
newplot<- dygraphs::dyOptions(newplot, colors= col.pal8, drawPoints= F, connectSeparatedPoints = F, pointSize= 1.5, pointShape= "circle", drawAxesAtZero= T, drawGrid= F,useDataTimezone = T)
newplot<- dygraphs::dyRangeSelector(newplot, height= 120)
newplot

htmlwidgets::saveWidget(newplot, file= paste(output_data_path_plot, "static_3s_accel_plot.html", sep = "/"), background = "white", title = paste(class(newplot)[[1]], ": static acceleration 3s"))


# save df in dat.xts list
dat.xts[[indiv]] <- acc.xts 

dat.1s.xts[[indiv]] <- acc.1s.xts 


}
#return(acc.xts)
#})


##### save data treated 

cat("\n# SCRIPT 2 : SAVE DATA #\n\n")

# save dat.xts
save(dat.xts,file= paste0(output_activity_data,"script2_dat_xts_by_indiv.RData"))

# save dat.1s.xts
save(dat.1s.xts,file= paste0(output_activity_data,"script2_dat_1s_xts_by_indiv.RData"))
