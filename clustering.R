require("dtwclust")

# Get time series data
time_series_path <- "Data/result_list.RData"
load(time_series_path)

# Subsample time series because of memory constraints

result_list <- sample(result_list, 10*1000)


# Using different number of clusters
cluster_sizes = 2L:10L
clustering_result_list <- lapply(
    cluster_sizes, 
    function(k) tsclust(series = result_list, k = k, seed = 42))

save(clustering_result_list, file = "Data/clustering_result_list.RData")

# Calculate internal validation
internal_validation <- lapply(
    clustering_result_list, 
    function(clustering_result) cvi(clustering_result, type = "internal"))


internal_validation_df <- do.call(rbind, internal_validation)
internal_validation_df <- cbind(internal_validation_df, k = cluster_sizes)

save(internal_validation_df, file = "Data/internal_validation_df.RData")
