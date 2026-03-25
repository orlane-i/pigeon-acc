####################################################
#   STEP 3 : MAKE UNSUPERVISED CLASSIFICATION      #
####################################################
cat("\n# SCRIPT 3 : CLUSTERING ACCEL DATA #\n\n")

#########################################
#   1- Select variables for kmeans      #
#########################################


# If you want to start from script 2, load data saved on previous sessions : 

# load("/home/lise/Documents/mnhn/Thèse/Code/snipe/outputs/Snipe_FR_Drugeon_JA752833_apr25/data/acc.xts_script2.RData")
# load("/home/lise/Documents/mnhn/Thèse/Code/snipe/outputs/Snipe_FR_Drugeon_JA752833_apr25/data/acc.1s.xts_script2.RData")

# load dat xts
load(paste0(output_activity_data,"script2_dat_xts_by_indiv.RData")) # no need in this script (classif with 1s dat) but will be used in script 4 to create xts data with clusters and plots
#load dat xts 1s
load(paste0(output_activity_data,"script2_dat_1s_xts_by_indiv.RData"))
#load dat list
load(paste0(output_activity_data,"dat_list_all_indiv_script1.RData"))

sample_ind <- dat.1s.xts[c("BE_99Z46835_winter_Semois", "FR_[FRP_JA752826]_winter_Jura", "FR_[FRP_JA752843]_July_Drugeon", "FI_[AT197514]_juv_Espoo", "FI_[AT071252]_ad_Joensuu", "FR_[FRP_JA752846]_Drugeon_oct25")]
sample_ind.xts <- dat.xts[c("BE_99Z46835_winter_Semois", "FR_[FRP_JA752826]_winter_Jura", "FR_[FRP_JA752843]_July_Drugeon", "FI_[AT197514]_juv_Espoo", "FI_[AT071252]_ad_Joensuu", "FR_[FRP_JA752846]_Drugeon_oct25")]

res_kmeans <- lapply(names(sample_ind), function(indiv)  { # run the function over list names instead of list objetcs to be able to extrac indiv name
  
  acc.1s.xts = sample_ind[[indiv]]
  acc.xts = sample_ind.xts[[indiv]] # no need in this script (classif with 1s dat) but will be used in script 4 to create xts data with clusters and plots
  
  
  # to run over all indiv : 
  acc.1s.xts = do.call(rbind, dat.1s.xts)
  acc.xts = do.call(rbind, dat.xts)
  load(paste0(output_activity_data,"script2_dat_xts_all_indiv.RData"))
  load(paste0(output_activity_data,"script2_dat_1s_xts_all_indiv.RData"))
  
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

# Select var
str(pred)

# select all var that are mean of data in a sec
pred.1s <- colnames(acc.1s.xts)[grep(x = colnames(acc.1s.xts), pattern =".1s")]
# select all var that are mean of data in 3 sec
pred.3s <- colnames(acc.1s.xts)[grep(x = colnames(acc.1s.xts), pattern =".3s")]

pred.3s <- pred.3s[1:20]

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
output_kmeans_path_indiv <- paste0(snipe_path,"/outputs/by_indiv/",indiv,"/kmeans")
dir.create(output_kmeans_path_indiv ,showWarnings = FALSE, recursive = T)

if (is.null(k_opti)) {
  
  # Set range of k 
  k_list <- 1:20
  
  #### Run kmeans with various  k
  
  cat("\n# SCRIPT 3 : CHOSING OPTIMAL K #\n\n")
  
  
  k_opti <- find_optimal_k(k_list = k_list, predictors = scaled_pred , kmean_path = output_kmeans_path_indiv)
  
}

cat(paste("\n# Kmeans is set to classify in",k_opti,"clusters #\n\n"))



########################
#   2- Run kmeans     #
#######################

cat("\n# SCRIPT 3 : RUNNING KMEANS #\n\n")

########## Run kmeans

set.seed(223)

# Run for k = 5

system.time({ kmeans_5groupes <- scaled_pred %>% kmeans(centers = k_opti, nstart = 100, algorithm="MacQueen", iter.max = 100) }) # also exists Lloyd algorithm but takes doble time : 43s # nstart = Nombre de points de départ

#save(kmeans_5groupes,file= paste0("/outputs/kmeans_all_indivs/kmeans_mod_all_indiv.RData"))

kmeans_5groupes$tot.withinss

# Extract cluster numbers
clusters_5groupes <- kmeans_5groupes$cluster 

# See cluster center coordinates in each dimension
centers <- kmeans_5groupes$centers # Warning : variables are scaled

# See counts
table(clusters_5groupes)

# create a dataframe with variables and clusters
dat_kmeans <- reduce.pred %>% 
  dplyr::mutate(kmeans_5 = factor(clusters_5groupes))

# Summary of each statistic variable mean and sd per cluster

summary_acc_by_clust <- dat_kmeans %>%
  dplyr::group_by(kmeans_5) %>% 
  dplyr::summarise_if(is.numeric, .funs = list(Moyen = mean, 
                                               Ecart_type = sd,
                                               Median = median))

##############################################
#   3- Make box plot of kmeans variables     #
##############################################

# extract summary statistic names
var <- colnames(dat_kmeans)[colnames(dat_kmeans) != "kmeans_5"]

# create and save boxplots of each cluster dimension (variables in kmean)
cat(paste0("\n# Saving box plots of all var used in kmeans \n\n"))

box_plot_summary_stat(var = var, data = dat_kmeans,  kmean_path = output_kmeans_path_indiv) # first created a function but non-necessary

})

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
