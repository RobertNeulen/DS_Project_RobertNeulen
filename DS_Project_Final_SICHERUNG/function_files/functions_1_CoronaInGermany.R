
#############################################################################################################################################
# Incidence over time in Germany


plot_germany_incidence <- function(df, date, geo){
  
  # Select time
  df <- df[df$date == date,]
  
  # Add geo data
  df <- merge(df, geo, by="ags5", all.x=TRUE)
  
  # Plot
  ggplot(df) +
    geom_sf(aes(fill=kr_inz_rate, geometry=geometry_kr, group=kreis)) +
    scale_fill_gradientn(colors=c("yellow","yellow3","orange","red1","red2","red4","purple2"), name=NULL,
                         values=c(0,0.01,0.05,0.0875,0.125,0.25,0.5,1), limits=c(0,400), na.value="purple4") +     # Scaling inspired by "Tagesschau" map
    coord_sf() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          legend.position="right",
          legend.title=element_text(size=12),
          legend.text=element_text(size=16),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key.size = unit(40, 'pt'),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank())


  
}

#plot_germany_incidence(df_incidence,"2020-03-07", df_geo)



#############################################################################################################################################
# Infections/Death over time in Germany

plot_germany_cases <- function(df, var, reg, scaling){
  
  # Select region
  df <- df[df$kreis == reg,]
  
  # Check if scaling should be applied
  if(scaling==TRUE){
    df$value <- (df[,var]/df$kr_ew_19)*100000
  } else {
    df$value <- df[,var]
  }
  
  # Plot
  ggplot(df, aes(x=date, y=value)) + 
    geom_bar(stat = "identity", position = 'dodge', fill="#2D708EFF", color="#2D708EFF") +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) + 
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
}

#plot_germany_cases(df_infection_death, "kr_inf_md", "All", FALSE)

#############################################################################################################################################
# Cases/Deaths per age group

plot_cases_death_age_germany <- function(df, var, reg){
  
  # Select whether death or infection cases
  if(var == "kr_inf_md"){
    df <- df[df$dummy_infection == 1,]
  }else{
    df <- df[df$dummy_infection == 0,]
  }
  
  # Select region
  df <- df[df$kreis == reg,]
  
  # Split data
  df_cols_age <- df[df$dummy_age == 1,]
  df_cols_gender <- df[df$dummy_age == 0,]
  
  # Reorder factor levels
  df_cols_age$label <- factor(df_cols_age$label, levels = c("80+","60 - 79","35 - 59","15 - 34","5 - 14","up to 4","Unknown"))
  df_cols_gender$label <- factor(df_cols_gender$label, levels = c("Male","Female","Unknown"))
  
  # Create plots
  plot_age <- ggplot(df_cols_age, aes(x=date, y=count, fill=label)) +
    geom_area(position='fill') +
    labs(fill = "Age Group") +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_viridis(discrete = TRUE)  +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.position="top",
          legend.box="horizontal",
          legend.title=element_text(size=18),
          legend.text=element_text(size=16),
          plot.margin=unit(c(1,0,2,0),"cm")) +
    guides(fill = guide_legend(nrow = 1))

  
  plot_gender <- ggplot(df_cols_gender, aes(x=date, y=count, fill=label)) +
    geom_area(position='fill') +
    labs(fill = "Gender") +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_viridis(discrete = TRUE)  +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.position="top",
          legend.box="horizontal",
          legend.title=element_text(size=18),
          legend.text=element_text(size=16)) 
  
  # Put plots together
  grid.arrange(plot_age, plot_gender)
  
  
}


#plot_cases_death_age_germany(df_infection_death_group, "kr_inf_rd", "All")


#############################################################################################################################################
# ICU

plot_icu_spots <- function(df, reg){
  
  
  # Select region
  df <- df[df$kreis == reg,]
  
  # Split data
  df_icu_1 <- df[df$label == "ICU beds per 100,000",]
  df_icu_2 <- df[df$label %in% c("Free ICU Beds","Blocked ICU Beds"),]
  
  
  # Plots
  plot_total <- ggplot(df_icu_1, aes(x=date, y=count, color="ICU Beds per 100,000")) +
    geom_line(size=0.8) +
    scale_x_date(date_breaks = "1 weeks", date_minor_breaks = "1 weeks", expand=c(0,0)) +
    scale_y_continuous(limits=c(0,max(75,max(df_icu_1$count, na.rm=TRUE)*1.1)), expand = c(0,0)) + 
    scale_color_manual(values = c("ICU Beds per 100,000" = "#440154FF")) +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.position="top",
          legend.box="horizontal",
          legend.title=element_blank(),
          legend.text=element_text(size=16),
          plot.margin=unit(c(1,0,2,0),"cm"))
  
  plot_distr <- ggplot(df_icu_2, aes(x=date, y=count, fill=label)) +
    geom_area(position='fill') +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_viridis(discrete = TRUE)  +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.position="top",
          legend.box="horizontal",
          legend.title=element_blank(),
          legend.text=element_text(size=16),
          plot.margin=unit(c(1,0,2,0),"cm")) +
    guides(fill = guide_legend(nrow = 1))
  
  # Put plots together
  grid.arrange(plot_total, plot_distr)
}


#plot_icu_spots(df_icu, "All")



#############################################################################################################################################
# Ventilator

plot_ventilator <- function(df, reg){
  
  # Select region
  df <- df[df$kreis == reg,]
  
  # Split data
  df_vent_1 <- df[df$label %in% c("ICU Beds COVID Patients", "ICU Beds other Patients"),]
  df_vent_2 <- df[df$label %in% c("ICU COVID Patients with Ventilator","ICU COVID Patients without Ventilator"),]
  

  # Create plots
  plot_beds_corona <- ggplot(df_vent_1, aes(x=date, y=values, fill=label)) +
    geom_area(position='fill') +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_viridis(discrete = TRUE)  +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.position="top",
          legend.box="horizontal",
          legend.title=element_blank(),
          legend.text=element_text(size=16),
          plot.margin=unit(c(1,0,2,0),"cm")) +
    guides(fill = guide_legend(nrow = 1))
  
  
  plot_corna_ventilator <- ggplot(df_vent_2, aes(x=date, y=values, fill=label)) +
    geom_area(position='fill') +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_viridis(discrete = TRUE)  +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.position="top",
          legend.box="horizontal",
          legend.title=element_blank(),
          legend.text=element_text(size=16)) 
  
  # Put plots together
  grid.arrange(plot_beds_corona, plot_corna_ventilator)
  
  
}


#############################################################################################################################################
# Vaccination daily 

plot_vaccination <- function(df, reg){
  
  # Select region
  df <- df[df$bundesland == reg,]
  
  # Plot 
  ggplot(df, aes(fill=label, y=count, x=date)) + 
    geom_bar(position="stack", stat="identity") +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(labels = comma,
                       expand = c(0,1)) +
    scale_fill_manual(name = NULL,
                      values =c('First vaccination'='#2D708EFF','Second vaccination'='#29AF7FFF'),
                      labels = c('First Vaccination','Second Vaccination')) +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.title=element_text(size=18),
          legend.text=element_text(size=16))
  

}

#plot_vaccination(df_impfung, "All")

#############################################################################################################################################
# Vaccination total 

plot_vaccination_cumulated <- function(df, reg){
  
  # Select region
  df <- df[df$bundesland == reg,]

  # Plot
  ggplot(df, aes(x=date)) +
    geom_area(aes(y=bl_erstimpf, fill="first")) +
    geom_area(aes(y=bl_zweitimpf, fill="second")) +
    scale_x_date(date_breaks = "1 weeks",
                 date_minor_breaks = "1 weeks",
                 expand=c(0,0)) +
    scale_y_continuous(labels = comma,
                       expand = c(0,1)) +
    scale_fill_manual(name = NULL,
                      values =c('first'='#2D708EFF','second'='#29AF7FFF'),
                      labels = c('First Vaccination','Second Vaccination')) +
    theme(axis.title.y= element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=14),
          axis.text.y = element_text(margin = margin(1,1,1,1), size=14),
          legend.title=element_text(size=18),
          legend.text=element_text(size=16))
  
  
}

#plot_vaccination_cumulated(df_impung_cumulated, "All")


