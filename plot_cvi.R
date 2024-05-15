require("ggplot2")

# Load validation data
validation_data_path <- "Data/internal_validation_df.RData"
load(validation_data_path)

# Plotting CVI Sil, SF, CH as function of number of clusters facetted by CVI

ggplot(internal_validation_df, aes(x = k)) +
geom_line(aes(y = COP)) +
theme_dark()

