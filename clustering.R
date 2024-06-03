library("pbapply")
library("dtwclust")


perform_clustering <- function(time_series_path, num_clusters, num_samples, save_path, sample = FALSE) {
  # Load time series data
  result_list <- get(load(time_series_path))
  # Subsample time series because of memory constraints
  
  if (sample){ result_list <- sample(result_list, num_samples)}

  # Using different number of clusters
  cluster_sizes = 2L:num_clusters
  n_rep = 10L
  control = partitional_control(
    distmat = proxy::dist(result_list, method = "dtw_basic"),
    nrep = n_rep
    )
  


  clustering_result_files <- pblapply(cluster_sizes, function(k) {

      # Repeat n_rep times select the one with min_cvi score 
      clustering_result <- tsclust(series = result_list, k = k, centroid = "pam", control = control, seed = 3)

      # Calculate cvi score and choose clustering with minimum DB

      cvi_scores <- lapply(clustering_result, function(x) cvi(x, type = "D"))
      index <- which.max(cvi_scores)
      clustering_result <- clustering_result[[index]]

      file_path <- paste0(save_path, "/clustering_result_", k, ".RData")
      save(clustering_result, file = file_path)
    
    return(file_path)
  })
  
  return(clustering_result_files)
}



n_clusters = 9
n_samples = 5000 

clustering_result_files <- perform_clustering(
  time_series_path = "Data/result_list.RData", 
  num_clusters = n_clusters, 
  num_samples = n_samples, 
  save_path = "Data/ClusteringResults")

# Clustering Total Population Time Series

total_pop_clustering_result_files <- perform_clustering(
  time_series_path = "Data/total_pop.RData", 
  num_clusters = n_clusters, 
  num_samples = n_samples, 
  save_path = "Data/TotalPopClusteringResults")


# Calculate internal validation
calculate_internal_validation <- function(clustering_result_list, num_clusters, save_path) {
  # Calculate internal validation

  internal_validation <- lapply(clustering_result_list, function(clustering_result_file_path){
    clustering_result <- get(load(clustering_result_file_path))
    internal_validation <- cvi(clustering_result, type = "internal")
    return(internal_validation)
  })

  internal_validation_df <- do.call(rbind, internal_validation)
  internal_validation_df <- cbind(internal_validation_df, k = 2L:num_clusters)

  save(internal_validation_df, file = save_path)
  
  return(internal_validation_df)
}

internal_validation_df <- calculate_internal_validation(clustering_result_files, n_clusters, "Data/internal_validation_df.RData")
total_pop_internal_validation_df <- calculate_internal_validation(total_pop_clustering_result_files, n_clusters, "Data/total_pop_internal_validation_df.RData")