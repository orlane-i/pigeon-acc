####################################################
#   STEP 3 : MAKE UNSUPERVISED CLASSIFICATION      #
####################################################
cat("\n# SCRIPT 3 : CLUSTERING ACCEL DATA #\n\n")

#########################################
#   1- Select variables for kmeans      #
#########################################
library(tidyverse)
library(zoo)
library(ggplot2)

# If you want to start from script 2, load data saved on previous sessions : 
pigeon_path <- here::here()
output_activity_data <- paste0(pigeon_path,"/outputs/activity/raw_data/")
output_activity_acc_data <- paste0(pigeon_path,"/outputs/activity/acc_data/")

# load dat xts
# load(paste0(output_activity_data,"script2_dat_xts_by_indiv.RData")) # no need in this script (classif with 1s dat) but will be used in script 4 to create xts data with clusters and plots
#load dat xts 1s
load(paste0(output_activity_data,"script2_dat_1s_xts_by_indiv.RData"))
#load dat list
# load(paste0(output_activity_data,"dat_list_all_indiv_script1.RData"))

ind <- dat.1s.xts

# to run over all indiv 
acc.1s.xts <- do.call(rbind, dat.1s.xts)

# and keep indiv
indiv_vec <- rep(names(dat.1s.xts), sapply(dat.1s.xts, NROW))

# attribut
attr(acc.1s.xts, "indiv") <- indiv_vec

save(acc.1s.xts, indiv, file = paste0(output_activity_acc_data, "acc_1s_xts_all_indiv_script3.RData"))

# indiv object
indiv <- attr(acc.1s.xts, "indiv")

#####
#res_kmeans <- lapply(names(ind), function(indiv)  { # run the function over list names instead of list objetcs to be able to extrac indiv name
  
  # acc.1s.xts = ind[[indiv]]
  # acc.xts = ind.xts[[8]] # no need in this script (classif with 1s dat) but will be used in script 4 to create xts data with clusters and plots
  
  
  
  # acc.xts = do.call(rbind, dat.xts)
  # load(paste0(output_activity_data,"script2_dat_xts_all_indiv.RData"))
  # load(paste0(output_activity_data,"script2_dat_1s_xts_all_indiv.RData"))
  # 
  #save(acc.xts,file= paste0(output_activity_data,"script2_dat_xts_all_indiv.RData"))
  #save(acc.1s.xts,file= paste0(output_activity_data,"script2_dat_1s_xts_all_indiv.RData"))
  

#acc.1s.xts <- dat.1s.xts[["FR_[FRP_JA752833]_April_Drugeon"]]
#acc.1s.xts <- do.call(rbind, dat.1s.xts)

# fill nas
  #fill gps data
acc.1s.xts[,44:53] <- na.locf(acc.1s.xts[,44:53], na.rm = FALSE, fromLast = FALSE) # from last false to complete 
# fill summ stat data
acc.1s.xts<- na.locf(acc.1s.xts, na.rm = FALSE, fromLast = TRUE) #  # or acc.1s.xts <- acc.1s.xts[!is.na(acc.1s.xts$surg.mu.1s),] if we want to remove gps data

  
# Select predictors for kmeans classification

# change xts for dataframe 
pred <- as.data.frame(acc.1s.xts) %>% # data is 1Hz frequency 
  dplyr::select(-datatype) %>% #remove datatype which is not a predictor and is fausly full "GPS"
  dplyr::mutate(dplyr::across(everything(), as.numeric)) # backtransform as numeric --> has been mutated in script 2

# réinjecter l'ID
pred$indiv <- indiv

# Select var
str(pred)

# select all var that are mean of data in a sec
pred.1s <- colnames(acc.1s.xts)[grep(x = colnames(acc.1s.xts), pattern =".1s")]
# select all var that are mean of data in 3 sec
pred.3s <- colnames(acc.1s.xts)[grep(x = colnames(acc.1s.xts), pattern =".3s")]

# pred.3s <- pred.3s[1:20]

# keep some var in a reduce pred object   
reduce.pred <- pred %>% 
  dplyr::select(dplyr::all_of(pred.3s))

# long and unreadible if many variables, only do for a few var
#pdf("test_all_clusters.pdf", width= 24, height= 24)
#reduce.pred %>% 
#  ggpairs()
#dev.off()


# scale var in a new objetc for clustering
scaled_pred <- reduce.pred %>% dplyr::mutate_if(.predicate = is.numeric, .funs = scale) 


##############################################
#   1bis- - Chose optimal k - Run kmeans     #
##############################################

# Create path to save kmeans outputs
# output_kmeans_path_indiv <- paste0(pigeon_path,"/outputs/by_indiv/",indiv,"/kmeans/")
# dir.create(output_kmeans_path_indiv ,showWarnings = FALSE, recursive = T)

#if (is.null(k_opti)) {
  
  # Set range of k 
  k_list <- 1:20
  
  #### Run kmeans with various  k
  
  cat("\n# SCRIPT 3 : CHOSING OPTIMAL K #\n\n")
  
  
  # k_opti <- find_optimal_k(k_list = k_list, predictors = scaled_pred , kmean_path = output_activity_acc_data)
  
#}

cat(paste("\n# Kmeans is set to classify in",k_opti,"clusters #\n\n"))



########################
#   2- Run kmeans     #
#######################

cat("\n# SCRIPT 3 : RUNNING KMEANS #\n\n")

########## Run kmeans

set.seed(223)

# Run for k = 3

system.time({ kmeans_3groupes <- scaled_pred %>% kmeans(centers = 3, nstart = 100, algorithm="MacQueen", iter.max = 100) }) # also exists Lloyd algorithm but takes doble time : 43s # nstart = Nombre de points de départ

#save(kmeans_5groupes,file= paste0("/outputs/kmeans_all_indivs/kmeans_mod_all_indiv.RData"))

kmeans_3groupes$tot.withinss

# Extract cluster numbers
clusters_3groupes <- kmeans_3groupes$cluster 

# See cluster center coordinates in each dimension
centers <- kmeans_3groupes$centers # Warning : variables are scaled

# See counts
table(clusters_3groupes)

# create a dataframe with variables and clusters
dat_kmeans <- reduce.pred %>% 
  dplyr::mutate(
    kmeans_3 = factor(clusters_3groupes),
                indiv = indiv
                )

# Summary of each statistic variable mean and sd per cluster

summary_acc_by_clust <- dat_kmeans %>%
  dplyr::group_by(kmeans_3) %>% 
  dplyr::summarise_if(is.numeric, .funs = list(Moyen = mean, 
                                               Ecart_type = sd,
                                               Median = median))

# save data
save(dat_kmeans, file= paste0(output_activity_acc_data,"script3_dat_kmeans.RData"))
save(summary_acc_by_clust, file= paste0(output_activity_acc_data,"script3_summary_acc_by_clust.RData"))

##############################################
#   3- Make box plot of kmeans variables     #
##############################################

# extract summary statistic names
# var <- colnames(dat_kmeans)[colnames(dat_kmeans) != "kmeans_3"]
# 
# # create and save boxplots of each cluster dimension (variables in kmean)
# cat(paste0("\n# Saving box plots of all var used in kmeans \n\n"))
 
# box_plot_summary_stat(var = var, data = dat_kmeans,  kmean_path = output_kmeans_path_indiv) # first created a function but non-necessary

#})

# write.csv(dat_kmeans, file = paste0(output_activity_acc_data,"script3_dat_kmeans.csv"))

## Plot with dygraphs

pigeon_path <- here::here()
output_activity_data <- paste0(pigeon_path,"/outputs/activity/raw_data/")
output_activity_acc_data <- paste0(pigeon_path,"/outputs/activity/acc_data/")
load(paste0(output_activity_acc_data,"script3_dat_kmeans.RData"))

# Load libraries
library(dygraphs)
library(xts)
library(dplyr)
library(stringr)

# Convert rownames in time column
# get rownames
dat_kmeans <- dat_kmeans %>%
  mutate(time_raw = rownames(.))

# convert in datetime
dat_kmeans <- dat_kmeans %>%
  mutate(time_clean = str_remove(time_raw, "^X"),
         
         # replace the first 3 "-" after the date by space + ":"
         time_clean = str_replace(time_clean,
                                  "(\\d{4})\\.(\\d{2})\\.(\\d{2})\\.(\\d{2})\\.(\\d{2})\\.(\\d{2})\\.(\\d+)",
                                  "\\1-\\2-\\3 \\4:\\5:\\6.\\7"),
         
         Date = as.POSIXct(time_clean, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Paris")
  )
# check
head(dat_kmeans$Date)

dat_kmeans <- dat_kmeans %>% select(-time_raw)

# remove nas
dat_kmeans <- dat_kmeans %>%
  filter(!is.na(Date))

# dygraph by cluster

clusters <- unique(dat_kmeans$kmeans_3)

plots <- list()

for (cl in clusters) {

  data_cl <- dat_kmeans %>%
    filter(kmeans_3 == cl) %>%
    arrange(Date)

  data_xts <- xts(
    data_cl %>% select(where(is.numeric), -kmeans_3),
    order.by = data_cl$Date
  )

  plots[[paste0("cluster_", cl)]] <- dygraph(data_xts) %>%
    dyRangeSelector()
}

plots$cluster_1
plots$cluster_2
plots$cluster_3

htmlwidgets::saveWidget(plots$cluster_1, file= paste(pigeon_path, "/outputs/activity/kmeans/cluster1_plots.html", sep = "/"), background = "white")
htmlwidgets::saveWidget(plots$cluster_2, file= paste(pigeon_path, "/outputs/activity/kmeans/cluster2_plots.html", sep = "/"), background = "white")
htmlwidgets::saveWidget(plots$cluster_3, file= paste(pigeon_path, "/outputs/activity/kmeans/cluster3_plots.html", sep = "/"), background = "white")

# dygraph by cluster only ODBA and heave
for (cl in clusters) {

  data_cl <- dat_kmeans %>%
    filter(kmeans_3 == cl) %>%
    arrange(Date)

  data_xts <- xts(
    data_cl %>% select(heav.dyn.sd.3s, heav.mu.3s, ODBA.mu.3s, VeDBA.3s),
    order.by = data_cl$Date
  )

  plots[[paste0("cluster_", cl)]] <- dygraph(data_xts) %>%
    dyRangeSelector()
}
#

# }

plots$cluster_1
plots$cluster_2
plots$cluster_3

htmlwidgets::saveWidget(plots$cluster_1, file= paste(output_kmeans_data, "cluster1_heave_plots.html", sep = "/"), background = "white")
htmlwidgets::saveWidget(plots$cluster_2, file= paste(output_kmeans_data, "cluster2_heave_plots.html", sep = "/"), background = "white")
htmlwidgets::saveWidget(plots$cluster_3, file= paste(output_kmeans_data, "cluster3_heave_plots.html", sep = "/"), background = "white")


datkmeans_cl2 <- datkmeans_cl2 %>% 
  select(-time_clean)

# subset data to keep cluster 2
datkmeans_cl2 <- dat_kmeans[dat_kmeans$kmeans_3 == 2, ]
datkmeans_cl2 <- datkmeans_cl2 %>% select(-kmeans_3)

datkmeans_cl1 <- dat_kmeans[dat_kmeans$kmeans_3 == 1, ]
datkmeans_cl1 <- datkmeans_cl1 %>% select(-kmeans_3)

save(datkmeans_cl2, file= paste0(output_activity_acc_data,"script3_subdatkmeans_cluster2.RData"))
save(datkmeans_cl1, file= paste0(output_activity_acc_data,"script3_subdatkmeans_cluster1.RData"))

# #### ACP
# 
# # scale var in a new objetc for clustering
# scaled_pred <- pred %>% dplyr::mutate_if(.predicate = is.numeric, .funs = scale) 
# 
# cat(paste0("\n# Running ACP \n\n"))
# 
# result_acp <- scaled_pred %>% 
#   FactoMineR::PCA(graph = FALSE)
# 
# # see part of variance in each acp comp
# result_acp$eig
# 
# # visualise acp
# graph_acp <- factoextra::fviz_pca_biplot(result_acp, col.ind = factor(clusters_5groupes)) + 
#   theme_bw()
#   
# graph_acp
# 
# # save acp graph
# ggsave(graph_acp, file = paste0(output_kmeans_path_indiv,"/ACP_of",k_opti,"clusterisation_dimensions.png"))
# 
# 
# 
# # extract ACP dimensions to test kmeans
# 
# #extract acp coord on principal dimensions
# acp_coords <- result_acp$ind$coord
# 
# # keep 5 dimensions = 80% of variance
# acp_data <- data.frame(acp_coords[, 1:5])
# 
# 
# ### k_opti
# # Set range of k 
# k_list <- 1:20
# 
# #### Run kmeans with various  k
# 
# cat("\n# SCRIPT 3 : CHOSING OPTIMAL K FOR KMEAN WITH ACP DIMENSIONS #\n\n")
# 
# 
# #k_opti <- find_optimal_k(k_list = k_list, predictors = acp_data , kmean_path = output_kmeans_path_indiv)
# 
# 
# 
# 
# ###
# set.seed(223)
# 
# # Run for k = 5
# 
# system.time({ kmeans_ACP_5groupes <- acp_data %>% kmeans(centers = k_opti, nstart = 100, algorithm="MacQueen", iter.max = 100) }) # also exists Lloyd algorithm but takes doble time : 43s # nstart = Nombre de points de départ
# 
# kmeans_ACP_5groupes$tot.withinss
# 
# clusters_ACP_5groupes <- kmeans_ACP_5groupes$cluster # Extract cluster numbers
# 
# cluster_ACP_5centers <- kmeans_ACP_5groupes$centers
# 
# table(clusters_ACP_5groupes)
# 
# dat_kmeans_ACP <- reduce.pred %>% 
#   dplyr::mutate(kmeans_5 = factor(clusters_ACP_5groupes))
# 
# # Summary of each statistic variable mean and sd
# moy_std_5k_ACP = dat_kmeans_ACP %>% 
#   dplyr::group_by(kmeans_5) %>% 
#   dplyr::summarise_if(is.numeric, .funs = list(Moyen = mean, 
#                                                Ecart_type = sd))
# 
# 
# # See center values of each cluster in each dimension
# centers <- kmeans_ACP_5groupes$centers # Warning : variables are scaled
