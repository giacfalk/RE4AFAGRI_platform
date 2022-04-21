filter_columns <- read.csv("filter_columns.csv")

clusters <- dplyr::select(clusters, filter_columns$columns)

