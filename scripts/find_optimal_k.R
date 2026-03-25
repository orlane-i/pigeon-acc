#' find_optimal_k
#' 
#' @description
#' This function run multiple kmeans with various k and identify the optimal k, i.e. the k reducing number of clusters and the total inertia of the resulting kmean 
#' 
#' @param k_list a `list` range of k for which to run comparativ kmeans
#' @param predictors a `data.frame` data containing scaled predictors 
#' @param kmean_path a `characters` defining the path to which save kmeans outputs
#' 
#' @import 
#'
#' @return `k_opti`
#'

find_optimal_k <-  function(k_list = 1:20, predictors, kmean_path  ){
  
  # apply kmeans to a list of k
  inertia_kmeans_table <- purrr::map_dfr(.x = k_list, # map_dfr = function to row bind outputs
                                         .f = function(k){
                                           
                                           res_kmeans <- dplyr::select_if(predictors, is.numeric) %>% # in case but all predictors are supposed to be numeric
                                             kmeans(centers = k, nstart = 100, algorithm="MacQueen", iter.max = 100)
                                           
                                           result <- tibble::tibble(K = k,  
                                                                    cluster_inertia = res_kmeans$tot.withinss)
                                           
                                           return(result)
                                         })
  
  # see inertia table
  
  save(x = inertia_kmeans_table, file = paste(kmean_path, "inertia_kmeans_table.Rdata", sep = "/"))
  write.csv(x = data.frame(inertia_kmeans_table), file = paste(kmean_path, "inertia_kmeans_table.csv", sep = "/"), row.names = FALSE)
  
  # see inertia evolution with K
  ggplot(inertia_kmeans_table) + 
    aes(x = K, y = cluster_inertia) + 
    geom_point() + 
    labs(title = paste("Graph of decrease in cluster inertia as k increases"))
  
  
  ggsave(paste(kmean_path, "inertia_decrease_graph.png", sep = "/"))
  
  
  #### automatic graphical detection : elbow method
  
  # -> Compute the distance between each points x and the line (hypothenuse) between p1 and p2 (considering a perpendicular distance between hypothenuse and point x)
  
  # Need to normalize data as there is a big scale difference between inertia and k trials
  df_norm <- inertia_kmeans_table %>%
    dplyr::mutate(K_norm = (K - min(K)) / (max(K) - min(K)),
                  inertia_norm = (cluster_inertia - min(cluster_inertia)) / (max(cluster_inertia) - min(cluster_inertia)))
  
  # define p1 and p2 (first and last K points) to define the line extremities
  p1 <- df_norm[1, c("K_norm", "inertia_norm")]
  p2 <- df_norm[nrow(df_norm), c("K_norm", "inertia_norm")]
  
  # Compute distance from each point to the line
  df_dist <- df_norm %>%
    dplyr::rowwise() %>%
    dplyr::mutate(distance = abs((p2$K_norm - p1$K_norm)*(p1$inertia_norm - inertia_norm) -
                                   (p1$K_norm - K_norm)*(p2$inertia_norm - p1$inertia_norm)) /
                    sqrt((p2$K_norm - p1$K_norm)^2 + (p2$inertia_norm - p1$inertia_norm)^2)) %>%
    dplyr::ungroup()
  
  # Find the point with the max distance -> break point = elbow
  k_opti <- df_dist %>% dplyr::filter(distance == max(distance)) %>% dplyr::pull(K)
  
  # plot optimal k in red
  ggplot(inertia_kmeans_table, aes(x = K, y = cluster_inertia)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = k_opti, linetype = "dashed", color = "red") +
    labs(title = paste("Graph of inertia decrease and optimal K :", k_opti))
  
  ggsave(paste(kmean_path, "optimal_k_graph.png", sep = "/"))
  
  return(k_opti)
}
  