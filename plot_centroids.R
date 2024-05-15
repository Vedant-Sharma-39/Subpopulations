require("ggplot2")

# Get clustering result
clustering_result_path <- "Data/clustering_result_list.RData"
load(clustering_result_path)

# Get centroids
centroids = lapply(
    clustering_result_list, 
    function(clustering_result) clustering_result@centroids)

two_clusters = centroids[[1]]


