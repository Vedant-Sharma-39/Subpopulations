# Make a csv file of parameters and assigned cluster


cluster_results <- lapply(
  list.files("Data/ClusteringResults", full.names = TRUE), 
  function (file_path) {
    clust  <- get(load(file_path))
    return(clust@cluster)
  })


param_sets <-  get(load("Data/param_sets.RData"))

# For each clustering save a csv file with params and assigned cluster

for (i in seq_along(cluster_results)) {
  cluster_result <- cluster_results[[i]]
  cluster_result_df <- data.frame(param_sets, cluster = cluster_result)
  write.csv(cluster_result_df, paste0("Data/ClusteringResults/cluster_", i+1, ".csv"))
}