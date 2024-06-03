library("ggplot2")

# Get centroids
total_pop_centroids = lapply(
    total_pop_clustering_result_files, 
    function (file_path) {
        clust  <- get(load(file_path))
        return(clust@centroids)
    })

centroids = lapply(
    clustering_result_files, 
    function (file_path) {
        clust  <- get(load(file_path))
        return(clust@centroids)
    })

# Convert two_clusters to a data frame
plot_centroids_2d <- function(centroids, savepath) {
  # Iterate over each set of clusters
  for (i in seq_along(centroids)) {
    # Convert clusters to a data frame
    clusters_df <- do.call(rbind, lapply(seq_along(centroids[[i]]), function(j) {
      df <- data.frame((centroids[[i]][[j]]))
      names(df) <- c("x", "y")
      df$Cluster <- paste("Cluster", j)
      df$Time <- seq_len(nrow(df))
      return(df)
    }))
    
    # Plot clusters
    pl <- ggplot(clusters_df) +
      geom_line(aes(x = Time, y = x, colour = "X")) +
      geom_line(aes(x = Time, y = y, colour = "Y")) +
      ylab("Population") +
      xlab("Time") +
      facet_wrap(~Cluster)
    
    # Save plot
    ggsave(paste0(savepath,"plot_centroids_", i+1, ".png"), pl, width = 10, height = 10, units = "cm")
  }
}

plot_centroids_1d <- function(centroids, savepath) {
  # Iterate over each set of clusters
  for (i in seq_along(centroids)) {
    # Convert clusters to a data frame
    clusters_df <- do.call(rbind, lapply(seq_along(centroids[[i]]), function(j) {
      df <- data.frame(centroids[[i]][[j]])
      names(df) <- c("x")
      df$Cluster <- paste("Cluster", j)
      df$Time <- seq_len(nrow(df))
      return(df)
    }))
    
    # Plot clusters
    pl <- ggplot(clusters_df) +
      geom_line(aes(x = Time, y = x)) +
      ylab("Population") +
      xlab("Time") +
      facet_wrap(~Cluster)
    
    # Save plot
    ggsave(paste0(savepath, "/plot_centroids_", i+1, ".png"), pl, width = 10, height = 10, units = "cm")
  }
}

plot_centroids_1d(total_pop_centroids, "Plots/TotalPopClusteringResults/")
plot_centroids_2d(centroids, "Plots/PopClusteringResults/")
