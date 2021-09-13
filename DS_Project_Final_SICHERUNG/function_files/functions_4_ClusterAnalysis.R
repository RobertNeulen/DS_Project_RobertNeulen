



#############################################################################################################################################
cluster_determination <- function(df, n_cluster, features, df_feat_info){
  
  # Check which variables to include
  columns_selected <- df_feat_info[df_feat_info$category %in% features,"feature"]
  df <- df[,c("kreis", "kr_ew_19", columns_selected)]
  
  # Summarize
  df <- df[complete.cases(df), ]
  df <- df %>% dplyr::group_by(kreis) %>% summarise_all(list(mean), na.rm=T)
  
  # Normalize
  normalize <- function(x) {
    return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
  }
  df_km <- as.data.frame(lapply(df[2:ncol(df)], normalize))
  
  
  # K-Means
  set.seed(42)
  
  k <- n_cluster
  
  df_final <- data.frame(cluster=numeric(k), inertia=numeric(k))
  
  for(i in 1:k){
    
    km <- kmeans(df_km, i)
    
    df_final[i,1] <- i
    df_final[i,2] <- km$tot.withinss
  }
  
  # Plot ellbow graph
  ggplot(df_final, aes(x=cluster, y=inertia)) + 
    geom_line(size=0.5, color="deepskyblue4") +
    geom_point(size=2, color="deepskyblue4") + 
    scale_x_continuous(breaks=seq(1, k)) +
    labs(y="Total within-cluster variation", x="Number of clusters") +
    theme(plot.title =element_text(size=16), 
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size=14),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=14))


}



#############################################################################################################################################
cluster_analysis <- function(df, cluster_opt, features, df_feat_info, df_geo_info){
  
  # Check which variables to include
  columns_selected <- df_feat_info[df_feat_info$category %in% features,"feature"]
  df_adj <- df[,c("kreis", "ags5", "kr_ew_19", columns_selected)]

  # Summarize
  df_adj <- df_adj[complete.cases(df_adj), ]
  df_adj <- df_adj %>% dplyr::group_by(kreis, ags5) %>% summarise_all(list(mean), na.rm=T)
  
  # Normalize
  normalize <- function(x) {
    return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
  }
  df_km <- as.data.frame(lapply(df_adj[2:ncol(df_adj)], normalize))
  
    # K-Means
  set.seed(42)
  km <- kmeans(df_km, cluster_opt)
  df_adj$cluster <- km$cluster
  
  # Just keep necessary columns
  df_func_adj <- subset(df_adj, select=c(ags5, cluster))
  
  # Get infection and death cases data
  df_cluster_analysis <- df[,c("ags5","kreis","kr_ew_19","kr_inf_md_kum","kr_tod_md_kum")]
  df_cluster_analysis <- df_cluster_analysis %>% dplyr::group_by(ags5, kreis) %>% summarise_all(list(max))
  df_cluster_analysis[,c("kr_inf_md_kum","kr_tod_md_kum")] <- (df_cluster_analysis[,c("kr_inf_md_kum","kr_tod_md_kum")]/df_cluster_analysis$kr_ew_19)*10000
  
  # Add clusters to data frame
  df_cluster_analysis <- merge(df_cluster_analysis, df_func_adj, by="ags5", all.x=TRUE)
  
  # Get geo data
  df_geo_info <- subset(df_geo_info, select=c(ags5, geometry_kr))
  df_cluster_analysis <- merge(df_cluster_analysis, df_geo_info, by="ags5", all.x=TRUE)
  
   # Cluster as factor
  df_cluster_analysis$cluster <- as.factor(df_cluster_analysis$cluster)
  
  # Adjust geometry
  df_cluster_analysis$geometry_kr <- sf::st_cast(df_cluster_analysis$geometry_kr, "MULTIPOLYGON")
  
  # Plot results
  plot_cases <- ggplot(df_cluster_analysis) + 
    geom_boxplot(aes(x=cluster, y=kr_inf_md_kum, group=cluster, fill=cluster)) +
    scale_fill_viridis(discrete = TRUE)  +
    labs(y="Cumulated cases per 10,000", x="Cluster", title="Infection cases") +
    theme(legend.position="none",
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          plot.title = element_text(size=16))
  
  plot_deaths <- ggplot(df_cluster_analysis) + 
    geom_boxplot(aes(x=cluster, y=kr_tod_md_kum, group=cluster, fill=cluster)) +
    scale_fill_viridis(discrete = TRUE)  +
    labs(y="Cumulated cases per 10,000", x="Cluster", title="Deaths") +
    theme(legend.position="none",
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          plot.title = element_text(size=16))

  plots_death_inf <- ggarrange(plot_cases, plot_deaths, nrow=2)
  
  # Cluster
  plot_cluster <- ggplot(df_cluster_analysis) +
    geom_sf(aes(fill=cluster, geometry=geometry_kr, group=kreis)) +
    coord_sf() +
    scale_fill_viridis(discrete = TRUE) +
    labs(fill = "Cluster") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position="right",
          legend.title=element_text(size=20),
          legend.text=element_text(size=18),
          legend.background = element_rect(fill="transparent", colour=NA),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank())

  # Aggregate clustering feature data
  df_agg_return <- subset(df_adj, select=-c(ags5, kreis, kr_ew_19))
  df_agg_return <- df_agg_return %>% dplyr::group_by(cluster) %>% summarise_all(list(mean), na.rm=T)
  df_agg_return <- df_agg_return %>% pivot_longer(!cluster, names_to="Feature", values_to="value")
  df_agg_return <- df_agg_return %>% pivot_wider(names_from="cluster", values_from="value", names_prefix="Cluster ")
  
  # Merge with data information
  df_agg_return <- merge(df_agg_return, df_feat_info, by.x="Feature", by.y="feature", all.x=TRUE)
  df_agg_return <- subset(df_agg_return, select=-c(Feature))
  
  # Rename columns
  colnames(df_agg_return)[which(names(df_agg_return) == "category")] <- "Category"
  colnames(df_agg_return)[which(names(df_agg_return) == "name")] <- "Feature"
  
  # Re-order
  df_agg_return <- df_agg_return %>% relocate(Category, Feature)
  
  return(list("plots_cases"=plots_death_inf, "plot_cluster"=plot_cluster, "table"=df_agg_return))
  
}



#############################################################################################################################################
cluster_feat_table <- function(table_result, selected_categories){
  
  # Round values
  numeric_columns <- sapply(table_result, mode) == 'numeric'
  table_result[,numeric_columns] <- round(table_result[,numeric_columns], 2)
  
  # Select categories
  table_result <- table_result[which(table_result$Category %in% selected_categories),]
  

  
}
