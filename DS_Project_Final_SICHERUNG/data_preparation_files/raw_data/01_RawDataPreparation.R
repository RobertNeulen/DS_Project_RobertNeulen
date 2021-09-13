#############################################################################################################################################
#############################################################################################################################################
# Prepare session
  ## Clear session
  ## Install (if necessary) and load packages
#############################################################################################################################################

# Reset session
rm(list = ls())

# Set memory limit
memory.limit(size=30000)

# Necessary to display "Umlaute" correctly
#Sys.setlocale("LC_CTYPE","German")
Sys.setlocale(category = "LC_ALL", locale = "German")

# Necessary packages
packages <- c("tidyverse","reshape","data.table","writexl","lubridate","bsts")

# Install packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))



#############################################################################################################################################
#############################################################################################################################################
# This part reads in all ".csv" file sin the "data" folder:
  ## This is done with this extensive way to ensure that all stores data files get read in without having to apply any adjustments in case
  ## a new data file gets included.
#############################################################################################################################################

# Load all elements in sub-folder "data"
list_paths <- list.files("data/raw_data/")

# Exclude all ".xlsx" and ".csv" files
list_paths <- list_paths[!grepl(".xlsx", list_paths)]
list_paths <- list_paths[!grepl("csv", list_paths)]

# Exclude geo-data and commuting folder
list_paths <- list_paths[!grepl("geo_data", list_paths)]
list_paths <- list_paths[!grepl("commuting", list_paths)]

# Loop through all folders in paths
for (i in 1:length(list_paths)) {
  
  # Get element in path
  path_name <- list_paths[i]
  
  # Adjust path name
  path <- paste0("data/raw_data/", path_name, "/")
  
  # Get list of files in path
  list_files <- list.files(path)
  
  # Check whether folder is empty
    ## If YES, then skip folder
  if (identical(list_files, character(0))){
    
    next
    
    ## If NO, then read in all files from folder
  } else {
    
    # Loop through all files in path
    for (j in 1:length(list_files)) {
      
      # Get name of file
      file_name <- list_files[j]
      
      # Read in file ("kr_massnahmen_unterkategorien.csv" has to be considered separately)
      if(file_name == "kr_massnahmen_unterkategorien.csv"){
        
        next
        
      } else {
        
        df_loop <- read.csv(paste0(path, file_name), encoding="UTF-8")
        
        # Drop not needed column from the file ("X.U.DEDD._id")
        df_loop <- dplyr::select(df_loop, -"X.U.FEFF._id")
        
        # Drop perfect duplicates
          ## E.g. infection data contained perfect duplicates
        df_loop <- df_loop[!duplicated(df_loop), ]
        
        # Cut of the ".csv" part of the name
        df_name <- gsub("\\..*", "", file_name)
        
        # Adjust name of the file
        assign(paste0("df_", df_name), df_loop)
      }
      
      
    }
  }
}

# Remove from memory to save space
rm(df_loop)

# Read in "kr_massnahmen_unterkategorien.csv" separately
df_kr_massnahmen_unterkategorien <- read.csv("data/raw_data/political_interventions/kr_massnahmen_unterkategorien.csv", encoding="UTF-8", sep=";")



#############################################################################################################################################
#############################################################################################################################################
# Saving mind/max dates of dataframes
#############################################################################################################################################

# Open up data frame for saving dates
df_dates <- data.frame(df = character(), min = character(), max = character(), stringsAsFactors = FALSE)


#############################################################################################################################################
#############################################################################################################################################
# Adjustment of labor market data frame
#############################################################################################################################################


# Merge Economic data
df_point_of_interest <- subset(df_point_of_interest, select=-c(kreis,bundesland))
df_volkswirtschaftliche_gesamtrechnung <- subset(df_volkswirtschaftliche_gesamtrechnung, select=-c(kreis,bundesland))

df_final_economic <- merge(df_arbeitsmarktstruktur,df_point_of_interest, by=c("ags2","ags5"), all.x=TRUE)
df_final_economic <- merge(df_final_economic,df_volkswirtschaftliche_gesamtrechnung, by=c("ags2","ags5"), all.x=TRUE)

# Get economic development data
df_arbeitsmarktentwicklung_reshape <- pivot_longer(df_arbeitsmarktentwicklung, names_to="variable", values_to="value", cols = starts_with("kr_"))

# Get year and month
df_arbeitsmarktentwicklung_reshape$year <- str_sub(df_arbeitsmarktentwicklung_reshape$variable,-6,-3)
df_arbeitsmarktentwicklung_reshape$month <- str_sub(df_arbeitsmarktentwicklung_reshape$variable,-2,-1)
df_arbeitsmarktentwicklung_reshape$date <- paste0(df_arbeitsmarktentwicklung_reshape$year,"-",df_arbeitsmarktentwicklung_reshape$month,"-01")
df_arbeitsmarktentwicklung_reshape$date <- bsts::LastDayInMonth(df_arbeitsmarktentwicklung_reshape$date)

# Adjust variable name and reshape
df_arbeitsmarktentwicklung_reshape$variable <- str_sub(df_arbeitsmarktentwicklung_reshape$variable,1,-8)
df_arbeitsmarktentwicklung_reshape <- pivot_wider(df_arbeitsmarktentwicklung_reshape, names_from="variable", values_from="value")

# Fill missing values
list_vars <- colnames(df_arbeitsmarktentwicklung_reshape)[grepl("kr_", colnames(df_arbeitsmarktentwicklung_reshape))]
for(var in list_vars){
  var_loop <- as.name(var)
  
  # First apply backward fill
  df_arbeitsmarktentwicklung_reshape <- df_arbeitsmarktentwicklung_reshape %>% dplyr::group_by(kreis) %>% tidyr::fill(var_loop, .direction="up")
  
  # If still missing, than forward fill
  df_arbeitsmarktentwicklung_reshape <- df_arbeitsmarktentwicklung_reshape %>% dplyr::group_by(kreis) %>% tidyr::fill(var_loop, .direction="down")
}

# Drop unnecessary columns
df_arbeitsmarktentwicklung_reshape <- subset(df_arbeitsmarktentwicklung_reshape, select=-c(bundesland, kreis))

# Merge data
df_final_economic <- merge(df_final_economic, df_arbeitsmarktentwicklung_reshape, by=c("ags2","ags5"), all.x=TRUE)
df_final_economic <- subset(df_final_economic, select=-c(date))

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_arbeitsmarktstruktur",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_point_of_interest",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_volkswirtschaftliche_gesamtrechnung",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_arbeitsmarktentwicklung",as.character(min(getElement(df_arbeitsmarktentwicklung_reshape, "date"))),as.character(max(getElement(df_arbeitsmarktentwicklung_reshape, "date"))))

# Remove from memory to save space
rm(df_arbeitsmarktstruktur, df_point_of_interest, df_volkswirtschaftliche_gesamtrechnung, df_arbeitsmarktentwicklung, df_arbeitsmarktentwicklung_reshape, var_loop, list_vars)


#############################################################################################################################################
#############################################################################################################################################
# Adjustment of development structure data frame
#############################################################################################################################################

# Drop unnecessary columns
df_bebauung_adj <- subset(df_bebauung, select=-c(ags2, bundesland, kreis))

# Merge data frames
df_final_dev_structure <- merge(df_wohnsituation, df_bebauung_adj, by="ags5", all.x=TRUE)

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_bebauung",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_wohnsituation",NA,NA)

# Remove from memory to save space
rm(df_bebauung, df_bebauung_adj, df_wohnsituation)


#############################################################################################################################################
#############################################################################################################################################
# Adjustment of corona data (infections, vaccinations, etc.) data frame
#############################################################################################################################################


# Concatenate data frames
df_epidemic <- rbind(df_genesene, df_infektionen, df_todesfaelle)

# Reshape data frames
df_final_epidemic <- pivot_longer(df_epidemic, names_to="date", values_to="value", cols = starts_with("d"))
df_intensivstationen_reshape <- pivot_longer(df_intensivstationen, names_to="variable", values_to="value", cols = starts_with("kr_"))

# Adjust date column
df_final_epidemic$date <- gsub("d", "", df_final_epidemic$date)
df_final_epidemic$date <- as.Date(df_final_epidemic$date, format="%Y%m%d")

df_intensivstationen_reshape <- data.table::setnames(df_intensivstationen_reshape, old="datum", new="date")
df_intensivstationen_reshape$date <- as.Date(df_intensivstationen_reshape$date)

df_impfdaten_reshape <- data.table::setnames(df_impfdaten, old="datum", new="date")
df_impfdaten_reshape$date <- as.Date(df_impfdaten_reshape$date)

# Again concatenate data frames
  ## Just "df_intensivstationen_reshape"
  ## Data frame "df_impfdaten" is just on the level of the "Bundesland, hence has to be merged later
df_final_epidemic <- rbind(df_final_epidemic, df_intensivstationen_reshape)
  
# Turn the respective variable into columns
df_final_epidemic <- pivot_wider(df_final_epidemic, names_from="variable", values_from="value")

# Get vaccination data on "Bundesland" level
df_final_epidemic <- merge(df_final_epidemic, df_impfdaten_reshape, by=c("ags2","date"), all.x=TRUE)
  ## Adjust non-merging columns
df_final_epidemic <- subset(df_final_epidemic, select=-c(bundesland.y))
df_final_epidemic <- data.table::setnames(df_final_epidemic, old = c("bundesland.x"), new = c("bundesland"))

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_final_epidemic",as.character(min(getElement(df_final_epidemic, "date"))),as.character(max(getElement(df_final_epidemic, "date"))))
df_dates[nrow(df_dates)+1,] <- c("df_intensivstationen",as.character(min(getElement(df_intensivstationen_reshape, "date"))),as.character(max(getElement(df_intensivstationen_reshape, "date"))))
df_dates[nrow(df_dates)+1,] <- c("df_impfdaten",as.character(min(getElement(df_impfdaten_reshape, "date"))),as.character(max(getElement(df_impfdaten_reshape, "date"))))

# Remove from memory to save space
rm(df_epidemic, df_genesene, df_infektionen, df_todesfaelle, df_intensivstationen, df_intensivstationen_reshape, df_impfdaten, df_impfdaten_reshape)



#############################################################################################################################################
#############################################################################################################################################
# Adjustment of interventions data frame
#############################################################################################################################################


# Adjust "Oberkategorie" data
df_kr_massnahmen_oberkategorien <- data.table::setnames(df_kr_massnahmen_oberkategorien, old = c("m_code"), new = c("code"))

# Drop unnecessary columns
df_kr_massnahmen_unterkategorien <- subset(df_kr_massnahmen_unterkategorien, select=-c(id))

# Merge together with "Unterkategorie" data
df_massnahmen_reshaped <- rbind(df_kr_massnahmen_unterkategorien, df_kr_massnahmen_oberkategorien)

# Reshape data frame
df_massnahmen_reshaped <- pivot_longer(df_massnahmen_reshaped, names_to="date",values_to="intervention", cols = starts_with("d"))

# Turn the intervention variable into columns
df_massnahmen_reshaped <- pivot_wider(df_massnahmen_reshaped, names_from="code", values_from="intervention")

# Adjust date column
df_massnahmen_reshaped$date <- gsub("d", "", df_massnahmen_reshaped$date)
df_massnahmen_reshaped$date <- as.Date(df_massnahmen_reshaped$date, format="%Y%m%d")

# Sort data frame
df_final_interventions <- df_massnahmen_reshaped[with(df_massnahmen_reshaped, order(bundesland, kreis, date)), ]

# Recode variables (should be just 0-1 encoded)
for(i in 6:ncol(df_final_interventions)){
  df_final_interventions[,i] <- ifelse(df_final_interventions[, i] == 0, 0, 1)
}

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_interventions",as.character(min(getElement(df_final_interventions, "date"))),as.character(max(getElement(df_final_interventions, "date"))))

# Drop interventions which are not filled
df_final_interventions <- subset(df_final_interventions, select=-c(M22, M23, M24))

# Remove from memory to save space
rm(df_kr_massnahmen_oberkategorien, df_kr_massnahmen_unterkategorien, df_massnahmen_reshaped)



#############################################################################################################################################
#############################################################################################################################################
# Adjustment of population data frame
#############################################################################################################################################

# Get population data
df_final_population <- df_bevoelkerung

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_bevoelkerung",NA,NA)

# Remove from memory to save space
rm(df_bevoelkerung)



#############################################################################################################################################
#############################################################################################################################################
# Adjustment of education data frame
#############################################################################################################################################

# Drop unnecessary columns
df_kinderbetreuung_reshape <- subset(df_kinderbetreuung, select=-c(ags2,bundesland,kreis))
df_bildungsniveau_reshape <- subset(df_bildungsniveau, select=-c(ags2,bundesland,kreis))

# Merge data together
df_final_education <- merge(df_ausbildungssituation, df_kinderbetreuung_reshape, by="ags5", all.x=TRUE)
df_final_education <- merge(df_final_education, df_bildungsniveau_reshape, by="ags5", all.x=TRUE)

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_kinderbetreuung",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_bildungsniveau",NA,NA)

# Remove from memory to save space
rm(df_kinderbetreuung, df_bildungsniveau, df_kinderbetreuung_reshape, df_bildungsniveau_reshape, df_ausbildungssituation)



#############################################################################################################################################
#############################################################################################################################################
# Adjustment of settlement data frame
#############################################################################################################################################

# Drop unnecessary columns
df_besiedlung_reshape <- subset(df_besiedlung, select=-c(ags2,bundesland,kreis))
df_flaechennutzung_reshape <- subset(df_flaechennutzung, select=-c(ags2,bundesland,kreis))

# Merge data together
df_final_settlement <- merge(df_raumordnung_reshape, df_besiedlung_reshape, by="ags5", all.x=TRUE)
df_final_settlement <- merge(df_final_settlement, df_flaechennutzung_reshape, by="ags5", all.x=TRUE)

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_besiedlung",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_flaechennutzung",NA,NA)
df_dates[nrow(df_dates)+1,] <- c("df_raumordnung",NA,NA)

# Remove from memory to save space
rm(df_besiedlung, df_besiedlung_reshape, df_flaechennutzung, df_flaechennutzung_reshape, df_raumordnung, df_raumordnung_reshape)



#############################################################################################################################################
#############################################################################################################################################
# Adjustment of social indicator data frame
#############################################################################################################################################

# Store data
df_final_socialindicator <- df_sozialindikatoren

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_sozialindikatoren",NA,NA)

# Remove from memory to save space
rm(df_sozialindikatoren)


#############################################################################################################################################
#############################################################################################################################################
# Adjustment of traffic data frame
#############################################################################################################################################

# Take "pendler" data
df_traffic <- df_pendler[,c("ags5","kr_ep","kr_ap","kr_sp")]

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_pendler",NA,NA)

rm(df_verkehr, df_pendler)


#############################################################################################################################################
#############################################################################################################################################
# Adjustment of weather data frame
#############################################################################################################################################


# List of variables
list_vars <- colnames(df_wetterdaten[,grepl("kr_" , colnames(df_wetterdaten))])
list_vars <- gsub('.{7}$', '', list_vars) 
list_vars <- list_vars[!duplicated(list_vars)]

# Initial data frame
df_vars_initial <- colnames(df_wetterdaten[,grepl(list_vars[1], colnames(df_wetterdaten))])
df_wetterdaten_reshaped <- df_wetterdaten[,c("ags5",df_vars_initial)]
df_wetterdaten_reshaped <- pivot_longer(df_wetterdaten_reshaped, names_to="date", values_to=list_vars[1], cols = starts_with("kr_"))
df_wetterdaten_reshaped$date <- substr(df_wetterdaten_reshaped$date, nchar(df_wetterdaten_reshaped$date)-6+1, nchar(df_wetterdaten_reshaped$date))
df_wetterdaten_reshaped$date <- as.Date(paste(df_wetterdaten_reshaped$date,"01",sep=""), format="%Y%m%d")

# Skip first entry (as it is already used in the reshaped data frame)
list_vars <- list_vars[2:length(list_vars)]

for(group in list_vars){
  
  # Select variables of interest
  loop_vars <- colnames(df_wetterdaten[,grepl(group, colnames(df_wetterdaten))])
  df_loop <- df_wetterdaten[,c("ags5",loop_vars)]
  
  # Reshape data frame
  df_loop <- pivot_longer(df_loop, names_to="date", values_to=group, cols = starts_with("kr_"))
  
  # Adjust date column
  df_loop$date <- substr(df_loop$date, nchar(df_loop$date)-6+1, nchar(df_loop$date))
  
  # Adjust date column (to first of month)
  df_loop$date <- as.Date(paste(df_loop$date,"01",sep=""), format="%Y%m%d")
  
  # Merge to reshaped data frame
  df_wetterdaten_reshaped <- merge(df_wetterdaten_reshaped, df_loop, by=c("ags5","date"), all.x=TRUE)
}

# Get final data frame
df_final_weather <- df_wetterdaten_reshaped

# Save dates
  ## Adjust max date so that in this case shows end of month
date_max <- max(getElement(df_final_weather, "date"))
date_max <- ceiling_date(date_max, "month") - days(1)
df_dates[nrow(df_dates)+1,] <- c("df_wetterdaten",as.character(min(getElement(df_final_weather, "date"))),as.character(date_max))

# Remove from memory to save space
rm(df_wetterdaten_reshaped, df_wetterdaten, df_loop)



#############################################################################################################################################
#############################################################################################################################################
# Adjustment of hospital and nursing home data frame
#############################################################################################################################################


# Drop unnecessary columns
df_pflegedaten_reshape <- subset(df_pflegedaten, select=-c(ags2,bundesland,kreis))
df_krankenhausdaten_reshape <- subset(df_krankenhausdaten, select=-c(ags2,bundesland,kreis))
df_medizinische_versorgung_reshape <- subset(df_medizinische_versorgung, select=-c(ags2,bundesland,kreis))

# Merge data together
df_final_hospital <- merge(df_pflegedaten_reshape, df_krankenhausdaten_reshape, by="ags5", all.x=TRUE)
df_final_hospital <- merge(df_final_hospital, df_medizinische_versorgung_reshape, by="ags5", all.x=TRUE)

# Save dates
df_dates[nrow(df_dates)+1,] <- c("df_krankenhaus",NA,NA)

# Remove from memory to save space
rm(df_pflegedaten, df_krankenhausdaten, df_pflegedaten_reshape, df_krankenhausdaten_reshape, df_medizinische_versorgung, df_medizinische_versorgung_reshape)




#############################################################################################################################################
#############################################################################################################################################
# Adjust final data frame
#############################################################################################################################################


#############################################################################################################################################
# Merge data frames

# Preparation
df_final_epidemic$year <- format(df_final_epidemic$date, format="%Y")
df_final_epidemic$month <- format(df_final_epidemic$date, format="%m")


# 1st merge
df_final <- merge(df_final_epidemic, df_final_interventions, by=c("ags2","ags5","date"), all.x=TRUE)
rm(df_final_epidemic, df_final_interventions)
  ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(kreis.y,bundesland.y))
df_final <- data.table::setnames(df_final, old = c("bundesland.x","kreis.x"), new = c("bundesland","kreis"))


# 2nd merge
df_final <- merge(df_final, df_final_population, by=c("ags2","ags5"), all.x=TRUE)
rm(df_final_population)
  ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(kreis.y,bundesland.y))
df_final <- data.table::setnames(df_final, old = c("bundesland.x","kreis.x"), new = c("bundesland","kreis"))


# 3rd merge
df_final_economic <- subset(df_final_economic, select=-c(kreis, bundesland))
df_final <- merge(df_final, df_final_economic, by=c("ags2","ags5","year","month"), all.x=TRUE)
rm(df_final_economic)


# 4th merge
df_final <- merge(df_final, df_final_dev_structure, by=c("ags2","ags5"), all.x=TRUE)
rm(df_final_dev_structure)
  ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(kreis.y,bundesland.y))
df_final <- data.table::setnames(df_final, old = c("bundesland.x","kreis.x"), new = c("bundesland","kreis"))


# 5th merge
df_final <- merge(df_final, df_final_education, by=c("ags2","ags5"), all.x=TRUE)
rm(df_final_education)
  ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(kreis.y,bundesland.y))
df_final <- data.table::setnames(df_final, old = c("bundesland.x","kreis.x"), new = c("bundesland","kreis"))


# 6th merge
df_final <- merge(df_final, df_final_settlement, by=c("ags2","ags5"), all.x=TRUE)
rm(df_final_settlement)
 ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(kreis.y,bundesland.y))
df_final <- data.table::setnames(df_final, old = c("bundesland.x","kreis.x"), new = c("bundesland","kreis"))


# 7th merge
df_final <- merge(df_final, df_final_socialindicator, by=c("ags2","ags5"), all.x=TRUE)
rm(df_final_socialindicator)
  ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(kreis.y,bundesland.y))
df_final <- data.table::setnames(df_final, old = c("bundesland.x","kreis.x"), new = c("bundesland","kreis"))


# 8th merge
  ## Get "month" and "year"
df_final_weather$year <- format(df_final_weather$date, format="%Y")
df_final_weather$month <- format(df_final_weather$date, format="%m")
  ## Merge on "ags5" and "year" and "month"
df_final <- merge(df_final, df_final_weather, by=c("ags5","year","month"), all.x=TRUE)
rm(df_final_weather)
  ## Adjust non-merging columns
df_final <- subset(df_final, select=-c(date.y))
df_final <- data.table::setnames(df_final, old = c("date.x"), new = c("date"))


# 9th merge
df_final <- merge(df_final, df_final_hospital, by=c("ags5"), all.x=TRUE)
rm(df_final_hospital)


# 10th merge
df_final <- merge(df_final, df_traffic, by=c("ags5"), all.x=TRUE)
rm(df_traffic)


# Drop "month" and "year"
df_final <- subset(df_final, select=-c(month,year))



#############################################################################################################################################
# Adjust "Umlaute"
df_final$kreis <- gsub('ü', 'ue', df_final$kreis)
df_final$kreis <- gsub('ö', 'oe', df_final$kreis)
df_final$kreis <- gsub('ä', 'ae', df_final$kreis)
df_final$kreis <- gsub('ß', 'ss', df_final$kreis)

df_final$bundesland <- gsub('ü', 'ue', df_final$bundesland)
df_final$bundesland <- gsub('ö', 'oe', df_final$bundesland)
df_final$krebundeslandis <- gsub('ä', 'ae', df_final$bundesland)
df_final$krebundeslandis <- gsub('ß', 'ss', df_final$bundesland)


#############################################################################################################################################
# Replace -99 (replace with NA)

for (i in seq_along(df_final)) {
  df_final[[i]][df_final[[i]] %in% -99] <- NA
}


#############################################################################################################################################
# Drop other columns
df_final <- subset(df_final, select=-c(krebundeslandis))
 


#############################################################################################################################################
# Save final data frame as .csv
write.csv(df_final,"data/raw_data/final_dataframe.csv", row.names = FALSE)

# Save date data
write_xlsx(df_dates,"data/other/df_date_coverage.xlsx")



