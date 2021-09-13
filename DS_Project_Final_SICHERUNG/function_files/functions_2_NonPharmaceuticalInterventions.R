#############################################################################################################################################
# Intervention plot function
plot_interventions <- function(df, df_date, var, reg){
  
  # Select columns
  df <- df[,c("kreis", "date", "bundesland", var)]
  
  # Drop after latest date available for NPI
    ## Otherwise the missing value will be marked with red, no wanted
    ## Red marked just for the missing data, not for dates which are not available
  end_date <- df_date[df_date$df== "df_interventions", "max"]
  df <- df[df$date <= unlist(end_date),]
  
  # Subset data frame depending on chosen locations
  if("All" %in% reg){
    df <- df
  } else {
    df <- df[df$kreis %in% reg,]
  }
  
  # Create plotting variable
  df$plot_value <- as.factor(df[, var])
  
  # Create plot
  ggplot(df, aes(x=date, y=kreis)) +
    geom_tile(aes(fill=plot_value), color=NA) +
    scale_fill_manual(values=c("grey85","cornflowerblue")) +
    scale_y_discrete(expand=c(0,0)) +
    facet_grid(bundesland ~ .,
               scales = "free",
               space = "free") +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    theme(legend.position="none",
          axis.title=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          strip.text.y = element_text(angle = 0, color="white", size=14),
          strip.background = element_rect(fill="slategray4"),
          panel.background = element_rect(alpha("firebrick", 0.7)))
  
}
# plot_interventions(df_npi,df_date_coverage, "M01a","Tuebingen")

#############################################################################################################################################
# Intervention description function

table_interventions <- function(selection){
  
  # Check whether aggregated or non-aggregated should be used
  if(selection ==TRUE){
    df <- df_download
  } else {
    df <- df_download_sub
  }
  
  return(df)
}

#intervention_description(TRUE)
