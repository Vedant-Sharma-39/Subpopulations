require("ggplot2")

# Load validation data
validation_data_path <- "Data/total_pop_internal_validation_df.RData"
internal_validation_df  <- get(load(validation_data_path))

# Plotting CVI DB, DI, Sil as function of number of clusters facetted by CVI

Indices = c("Sil", "D", "DB")

for (i in Indices){
  pl <- ggplot(internal_validation_df) +
    geom_line(aes(x = k, y = !! sym(i))) +
    ylab(i) +
    xlab("Number of clusters")
  
  ggsave(paste0("Plots/total_pop_plot_cvi_", i, ".png"), pl, width = 10, height = 10, units = "cm")
}

# Load validation data
validation_data_path <- "Data/internal_validation_df.RData"
internal_validation_df  <- get(load(validation_data_path))

# Plotting CVI DB, CH, Sil as function of number of clusters facetted by CVI

Indices = c("Sil", "D", "DB")

for (i in Indices){
  pl <- ggplot(internal_validation_df) +
    geom_line(aes(x = k, y = !! sym(i))) +
    ylab(i) +
    xlab("Number of clusters")
  
  ggsave(paste0("Plots/plot_cvi_", i, ".png"), pl, width = 10, height = 10, units = "cm")
}



internal_validation_df
