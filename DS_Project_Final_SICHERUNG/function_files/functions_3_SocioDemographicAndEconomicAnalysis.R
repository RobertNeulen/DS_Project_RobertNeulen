
#############################################################################################################################################
# Housing Situation SQM

plot_housing_sqm <- function(df, reg){
  
  # Select region
  df <- df[df$kreis %in% reg,]

  # Adjust classes
  df$label <- factor(df$label, levels = c("<70 sqm","70 - 85 sqm","85 - 115 sqm","115 - 140 sqm",">140 sqm"))

  # Plot
  ggplot(df, aes(x=label, y=percent, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(df$percent)*1.1, na.rm=T), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

  
}

#plot_housing_sqm(df_housing_sqm, c("Tuebingen","Germany","Esslingen"))


#############################################################################################################################################
# Housing Situation Class

plot_housing_class <- function(df, reg){
  
  # Select region
  df <- df[df$kreis %in% reg,]
  
  # Reorder classes (for better labeling in the plot)
  df$label <- factor(df$label, levels = c("Terraced house", "Line building", "Free-standing villa", "Classic semi-detached house",
                                          "Multi-party complex", "Multi-party house en block", "Semi-detached multi-party house", "Detached multi-party house", "Multi-party house with neighbour", 
                                          "Detached one- to two-apartment house", "One- to two-apartment house with neighbour",
                                          "High-rise building", "Special form", "Commercial building", "Not classified"))
  
  # Plot
  ggplot(df, aes(x=label, y=percent, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.5,max(df$percent, na.rm=T)*1.1)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

}

#plot_housing_class(df_housing_class, c("Tuebingen","Germany","Esslingen"))





#############################################################################################################################################
# Education - Current Students 


plot_education_cs <- function(df, reg){

  # Select region and education type
  df <- df[(df$indicator == 1) & (df$kreis %in% reg),]
  
  # Adjust classes
  df$label <- factor(df$label, levels = c("Pre-school","Elementary school","Hauptschule",
                                          "Realschule","Gymnasium","Waldorfschule",
                                          "Integrated comprehensive school","Evening school","Special school",
                                          "Orientation level","Comprehensive school"))

  # Plot
  ggplot(df, aes(x=label, y=percent, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.5,max(df$percent, na.rm=T)*1.1)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
}

# plot_education_cs(df_education_profession, c("Germany","Esslingen"))



#############################################################################################################################################
# Education - Current Students

plot_education_gel <- function(df, reg){
  
  # Select region and education type
  df <- df[(df$indicator %in% c(2,3)) & (df$kreis %in% reg),]
  
  # Rename labels
  df$indicator <- factor(df$indicator, 
                         levels = c(2,3), 
                         labels = c("School Education","Higher/Professional Education"))
  
  
  # Plot
  ggplot(df, aes(x=label, y=percent, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    facet_wrap(~indicator, scale="free") +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.5,max(df$percent, na.rm=T)*1.1)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20),
          panel.spacing = unit(2, "lines"),
          plot.margin = margin(10,10,10,20),
          strip.text = element_text(size=16))

}
# plot_education_gel(df_education_profession, c("Germany","Esslingen"))


#############################################################################################################################################
# Day Care Coverage

plot_education_dcc <- function(df, reg){
  
  # Select region and education type
  df <- df[df$kreis %in% reg,]
  
  # Rename labels
  df$ind_daycare_fulltime  <- factor(df$ind_daycare_fulltime, 
                                     levels = c(0,1), 
                                     labels = c("General","Full-Time"))

  # Plot
  ggplot(df, aes(x=label, y=values, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.5,max(df$values, na.rm=T)*1.1)), expand=c(0,0)) +
    facet_wrap(~ind_daycare_fulltime, scale="free") +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20),
          panel.spacing = unit(2, "lines"),
          plot.margin = margin(10,10,10,20),
          strip.text = element_text(size=16))

}

# plot_education_dcc(df_daycare, c("Germany","Esslingen"))


#############################################################################################################################################
# Social Indicator

plot_social_indicator <- function(df, reg){
  
  # Select region and education type
  df <- df[df$kreis %in% reg,]
  
  # Plot
  ggplot(df, aes(x=label, y=values, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.20,max(df$values, na.rm=T)*1.5)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20))

}

# plot_social_indicator(df_social_indicator, c("Germany","Esslingen"))



#############################################################################################################################################
# Economic sectors

plot_economy_sector <- function(df, reg){

  # Select regions
  df <- df[df$kreis %in% reg,]
  
  # Plot
  ggplot(df, aes(x=label, y=percent, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.7,max(df$percent, na.rm=T)*1.1)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20))

}

#plot_economy_sector(df_econ_sector,c("Germany","Esslingen"))




#############################################################################################################################################
# Unemployment quota

plot_unemplyoment <- function(df, reg){

  # Select regions
  df <- df[df$kreis %in% reg,]

  # Plot
  ggplot(df, aes(x=date, y=kr_alq, group=kreis, color=kreis)) + 
    geom_point(size=3) + 
    geom_line(size=1) +
    labs(y="%", x="Date") +
    scale_color_viridis(discrete=T) +
    scale_x_date(date_breaks = "1 months", date_minor_breaks = "1 months", expand=c(0,0)) +
    scale_y_continuous(limits = c(0,max(0.7,max(df$kr_alq, na.rm=T)*1.1)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 90),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=14, angle=0, vjust=0.5),
          plot.title = element_text(size=20))

}

#plot_unemplyoment(df_unemployment,c("Tuebingen","Germany","Esslingen"))



#############################################################################################################################################
# GDP per capita in Germany


plot_germany_gdp <- function(df, reg, geo){
  
  # Set max. value at 90% quantile (prevent to heavy outliers skeweing the color palette)
  q <- unname(quantile(df$kr_bip_ew,  probs = c(90)/100))
  df$kr_bip_ew <- ifelse(df$kr_bip_ew >= q, q, df$kr_bip_ew)
  
  # Check if entire map needed or just regions
  if("Germany" %in% reg){
    df <- df
  }else{
    df$kr_bip_ew <- ifelse(df$kreis %in% reg, df$kr_bip_ew, NA)
  }
  
  # Add geo data
  df <- merge(df, geo, by="ags5", all.x=TRUE)
  
  # Plot
  ggplot(df) +
    geom_sf(aes(fill=kr_bip_ew, geometry=geometry_kr)) +
    scale_fill_gradientn(colors=c("red4","red1","orange1","yellow3","yellow2","green2","green3"),
                         name=NULL, limits=c(10,q), na.value="gray95") +
    coord_sf() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=16),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key.size = unit(40, 'pt'),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank())


  
}

# plot_germany_gdp(df_gdp_capita,c("Tuebingen","Esslingen"), df_geo)


#############################################################################################################################################
# Hospital

plot_medical <- function(df, reg){

  # Select regions
  df <- df[df$kreis %in% reg,]
  
  # Plot
  ggplot(df, aes(x=label, y=values, group=kreis, fill=kreis)) + 
    geom_bar(stat = "identity", position = 'dodge') +
    scale_fill_viridis(discrete = TRUE) +
    scale_y_continuous(limits = c(0,max(0.20,max(df$values, na.rm=T)*1.5)), expand=c(0,0)) +
    theme(legend.position="right",
          legend.title=element_blank(),
          legend.text=element_text(size=18),
          axis.text.x = element_text(size=14, angle = 45, hjust = 1),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size=20))

}

#plot_medical(df_medical,c("Tuebingen","Germany","Esslingen"))





















