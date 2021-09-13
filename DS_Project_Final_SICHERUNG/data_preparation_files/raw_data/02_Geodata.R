#############################################################################################################################################
#############################################################################################################################################
# Prepare session
  ## Clear session
  ## Install (if necessary) and load packages
#############################################################################################################################################

# Reset session
rm(list = ls())
gc()

# Set memory limit
memory.limit(size=30000)

# Necessary to display "Umlaute" correctly
Sys.setlocale(category = "LC_ALL", locale = "German")

# Necessary packages
packages <- c("sf","data.table","tidyverse","stringi","raster")

# Install packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))



#############################################################################################################################################
#############################################################################################################################################
# Load and prepare data
#############################################################################################################################################

# Load geodata for "Bundesland" and "Kreis"
geom_bundesland <- as.data.frame(readRDS("data/raw_data/geo_data/gadm36_DEU_1_sf.Rds"))
geom_kreis <- as.data.frame(readRDS("data/raw_data/geo_data/gadm36_DEU_2_sf.Rds"))

# Keep relevant columns and rename them
geom_bundesland <- subset(geom_bundesland, select=c(NAME_1, geometry))
geom_bundesland <- data.table::setnames(geom_bundesland, old=c("NAME_1","geometry"), new=c("bundesland","geometry_bl"))

geom_kreis <- subset(geom_kreis, select=c(NAME_2, geometry))
geom_kreis <- data.table::setnames(geom_kreis, old=c("NAME_2","geometry"), new=c("kreis","geometry_kr"))

# Additional encoding
Encoding(geom_bundesland$bundesland)  <- "UTF-8"
Encoding(geom_kreis$kreis)  <- "UTF-8"

# Load one of the files from main data source
df_orig <- read.csv("data/raw_data/infection/infektionen.csv", encoding="UTF-8")

# Prepare data for merging
df_orig <- subset(df_orig, select=c("bundesland","ags2","kreis","ags5"))
df_orig <- df_orig[!duplicated(df_orig),]






#############################################################################################################################################
#############################################################################################################################################
# Correct entries
#############################################################################################################################################

# Rename "bundesland"
geom_bundesland[geom_bundesland$bundesland== "Bayern","bundesland"] = "Freistaat Bayern"

# Adjust entries for "Goettingen" and "Osterode am Harz"
  ## Both should be the same "Kreis" ("Goettingen") but is an error within the geo data
geo_osterode <- geom_kreis[grepl("Osterode", geom_kreis$kreis),]
geo_goettingen <- geom_kreis[grepl("Göttingen", geom_kreis$kreis),]
geo_merge <- st_union(geo_osterode$geometry_kr, geo_goettingen$geometry_kr)

  ## Drop entry for "Osterode am Harz"
geom_kreis <- geom_kreis[!grepl("Osterode am Harz", geom_kreis$kreis),]
  ## Replace geo entry for "Goettignen"
geom_kreis[geom_kreis$kreis=="Göttingen","geometry_kr"] <- geo_merge


# Rename "kreis"
geom_kreis[geom_kreis$kreis== "Städteregion Aachen","kreis"] = "Aachen"
geom_kreis[geom_kreis$kreis== "Altenkirchen (Westerwald)","kreis"] = "Altenkirchen (Ww)"

geom_kreis[geom_kreis$kreis== "Ansbach","kreis"] = "Ansbach, Kreis"
geom_kreis[geom_kreis$kreis== "Ansbach (Kreisfreie Stadt)","kreis"] = "Ansbach, Stadt"
geom_kreis[geom_kreis$kreis== "Aschaffenburg","kreis"] = "Aschaffenburg, Kreis"
geom_kreis[geom_kreis$kreis== "Aschaffenburg (Kreisfreie Stadt)","kreis"] = "Aschaffenburg, Stadt"
geom_kreis[geom_kreis$kreis== "Augsburg","kreis"] = "Augsburg, Kreis"
geom_kreis[geom_kreis$kreis== "Augsburg (Kreisfreie Stadt)","kreis"] = "Augsburg, Stadt"
geom_kreis[geom_kreis$kreis== "Bamberg","kreis"] = "Bamberg, Kreis"
geom_kreis[geom_kreis$kreis== "Bamberg (Kreisfreie Stadt)","kreis"] = "Bamberg, Stadt"
geom_kreis[geom_kreis$kreis== "Bayreuth","kreis"] = "Bayreuth, Kreis"
geom_kreis[geom_kreis$kreis== "Bayreuth (Kreisfreie Stadt)","kreis"] = "Bayreuth, Stadt"
geom_kreis[geom_kreis$kreis== "Coburg","kreis"] = "Coburg, Kreis"
geom_kreis[geom_kreis$kreis== "Coburg (Kreisfreie Stadt)","kreis"] = "Coburg, Stadt"
geom_kreis[geom_kreis$kreis== "Dillingen an der Donau","kreis"] = "Dillingen a.d.Donau"
geom_kreis[geom_kreis$kreis== "Flensburg","kreis"] = "Flensburg, Stadt"
geom_kreis[geom_kreis$kreis== "Region Hannover","kreis"] = "Hannover"
geom_kreis[geom_kreis$kreis== "Heilbronn","kreis"] = "Heilbronn, Kreis"
geom_kreis[geom_kreis$kreis== "Heilbronn (Stadtkreis)","kreis"] = "Heilbronn, Stadt"
geom_kreis[geom_kreis$kreis== "Hochtaunuskreis","kreis"] = "Hochtaunus"
geom_kreis[geom_kreis$kreis== "Hof","kreis"] = "Hof, Kreis"
geom_kreis[geom_kreis$kreis== "Hof (Kreisfreie Stadt)","kreis"] = "Hof, Stadt"
geom_kreis[geom_kreis$kreis== "Karlsruhe (Stadtkreis)","kreis"] = "Karlsruhe, Kreis"
geom_kreis[geom_kreis$kreis== "Karlsruhe","kreis"] = "Karlsruhe, Stadt"
geom_kreis[geom_kreis$kreis== "Kiel","kreis"] = "Kiel, Landeshauptstadt"
geom_kreis[geom_kreis$kreis== "Darmstadt","kreis"] = "Kreisfreie Stadt Darmstadt"
geom_kreis[geom_kreis$kreis== "Frankfurt am Main","kreis"] = "Kreisfreie Stadt Frankfurt am Main"
geom_kreis[geom_kreis$kreis== "Kassel (Kreisfreie Stadt)","kreis"] = "Kreisfreie Stadt Kassel"
geom_kreis[geom_kreis$kreis== "Offenbach am Main","kreis"] = "Kreisfreie Stadt Offenbach am Main"
geom_kreis[geom_kreis$kreis== "Lahn-Dill-Kreis","kreis"] = "Lahn-Dill"
geom_kreis[geom_kreis$kreis== "Wiesbaden","kreis"] = "Landeshauptstadt Wiesbaden"
geom_kreis[geom_kreis$kreis== "Merzig-Wadern","kreis"] = "Landkreis Merzig-Wadern"
geom_kreis[geom_kreis$kreis== "Neunkirchen","kreis"] = "Landkreis Neunkirchen"
geom_kreis[geom_kreis$kreis== "Rostock","kreis"] = "Landkreis Rostock"
geom_kreis[geom_kreis$kreis== "Saarlouis","kreis"] = "Landkreis Saarlouis"
geom_kreis[geom_kreis$kreis== "St. Wendel","kreis"] = "Landkreis St. Wendel"
geom_kreis[geom_kreis$kreis== "Landshut","kreis"] = "Landshut, Kreis"
geom_kreis[geom_kreis$kreis== "Landshut (Kreisfreie Stadt)","kreis"] = "Landshut, Stadt"
geom_kreis[geom_kreis$kreis== "Leipzig","kreis"] = "Leipzig, Kreis"
geom_kreis[geom_kreis$kreis== "Leipzig (Kreisfreie Stadt)","kreis"] = "Leipzig, Stadt"
geom_kreis[geom_kreis$kreis== "Lübeck","kreis"] = "Lübeck, Hansestadt"
geom_kreis[geom_kreis$kreis== "Main-Kinzig-Kreis","kreis"] = "Main-Kinzig"
geom_kreis[geom_kreis$kreis== "Main-Taunus-Kreis","kreis"] = "Main-Taunus"
geom_kreis[geom_kreis$kreis== "Mühldorf am Inn","kreis"] = "Mühldorf a.Inn"
geom_kreis[geom_kreis$kreis== "Neumarkt in der Oberpfalz","kreis"] = "Neumarkt i.d.OPf."
geom_kreis[geom_kreis$kreis== "Neumünster","kreis"] = "Neumünster, Stadt"
geom_kreis[geom_kreis$kreis== "Neustadt an der Aisch-Bad Windsheim","kreis"] = "Neustadt a.d.Aisch-Bad Windsheim"
geom_kreis[geom_kreis$kreis== "Neustadt an der Waldnaab","kreis"] = "Neustadt a.d.Waldnaab"
geom_kreis[geom_kreis$kreis== "Nienburg (Weser)","kreis"] = "Nienburg/Weser"
geom_kreis[geom_kreis$kreis== "Oldenburg","kreis"] = "Oldenburg, Kreis"
geom_kreis[geom_kreis$kreis== "Oldenburg (Kreisfreie Stadt)","kreis"] = "Oldenburg, Stadt"
geom_kreis[geom_kreis$kreis== "Passau","kreis"] = "Passau, Kreis"
geom_kreis[geom_kreis$kreis== "Passau (Kreisfreie Stadt)","kreis"] = "Passau, Stadt"
geom_kreis[geom_kreis$kreis== "Pfaffenhofen an der Ilm","kreis"] = "Pfaffenhofen a.d.Ilm"
geom_kreis[geom_kreis$kreis== "Regensburg","kreis"] = "Regensburg, Kreis"
geom_kreis[geom_kreis$kreis== "Regensburg (Kreisfreie Stadt)","kreis"] = "Regensburg, Stadt"
geom_kreis[geom_kreis$kreis== "Rheingau-Taunus-Kreis","kreis"] = "Rheingau-Taunus"
geom_kreis[geom_kreis$kreis== "Rosenheim","kreis"] = "Rosenheim, Kreis"
geom_kreis[geom_kreis$kreis== "Rosenheim (Kreisfreie Stadt)","kreis"] = "Rosenheim, Stadt"
geom_kreis[geom_kreis$kreis== "Rostock (Kreisfreie Stadt)","kreis"] = "Rostock, Hansestadt"
geom_kreis[geom_kreis$kreis== "Schwalm-Eder-Kreis","kreis"] = "Schwalm-Eder"
geom_kreis[geom_kreis$kreis== "Schweinfurt","kreis"] = "Schweinfurt, Kreis"
geom_kreis[geom_kreis$kreis== "Schweinfurt (Kreisfreie Stadt)","kreis"] = "Schweinfurt, Stadt"
geom_kreis[geom_kreis$kreis== "Schwerin","kreis"] = "Schwerin, Landeshauptstadt"
geom_kreis[geom_kreis$kreis== "Frankenthal (Pfalz)","kreis"] = "Stadt Frankenthal (Pfalz)"
geom_kreis[geom_kreis$kreis== "Kaiserslautern (Kreisfreie Stadt)","kreis"] = "Stadt Kaiserslautern"
geom_kreis[geom_kreis$kreis== "Koblenz","kreis"] = "Stadt Koblenz"
geom_kreis[geom_kreis$kreis== "Landau in der Pfalz","kreis"] = "Stadt Landau in der Pfalz"
geom_kreis[geom_kreis$kreis== "Ludwigshafen am Rhein","kreis"] = "Stadt Ludwigshafen a. Rh."
geom_kreis[geom_kreis$kreis== "Mainz","kreis"] = "Stadt Mainz"
geom_kreis[geom_kreis$kreis== "Neustadt an der Weinstraße","kreis"] = "Stadt Neustadt a.d. W."
geom_kreis[geom_kreis$kreis== "Pirmasens","kreis"] = "Stadt Pirmasens"
geom_kreis[geom_kreis$kreis== "Speyer","kreis"] = "Stadt Speyer"
geom_kreis[geom_kreis$kreis== "Trier","kreis"] = "Stadt Trier"
geom_kreis[geom_kreis$kreis== "Worms","kreis"] = "Stadt Worms"
geom_kreis[geom_kreis$kreis== "Zweibrücken","kreis"] = "Stadt Zweibrücken"
geom_kreis[geom_kreis$kreis== "Vogelsberg","kreis"] = "Vogelsberg"
geom_kreis[geom_kreis$kreis== "Weiden in der Oberpfalz","kreis"] = "Weiden i.d.OPf."
geom_kreis[geom_kreis$kreis== "Werra-Meißner-Kreis","kreis"] = "Werra-Meißner"
geom_kreis[geom_kreis$kreis== "Wetteraukreis","kreis"] = "Wetterau"
geom_kreis[geom_kreis$kreis== "Wunsiedel im Fichtelgebirge","kreis"] = "Wunsiedel i.Fichtelgebirge"
geom_kreis[geom_kreis$kreis== "Vogelsbergkreis","kreis"] = "Vogelsberg"

# Additional problems regarding encoding
  ## UTF-8 encoding didn't work for all, still some undefined characters in some strings
  ## This has to be corrected before replacement can be done
geom_kreis$kreis <- str_replace(geom_kreis$kreis, "Osnabr.ck", "Osnabrück")
geom_kreis$kreis <- str_replace(geom_kreis$kreis, "M.nchen ", "München ")
geom_kreis$kreis <- str_replace(geom_kreis$kreis, "F.rth", "Fürth")
geom_kreis$kreis <- str_replace(geom_kreis$kreis, "W.rzburg", "Würzburg")

geom_kreis[geom_kreis$kreis== "Fürth","kreis"] = "Fürth, Kreis"
geom_kreis[geom_kreis$kreis== "Fürth (Kreisfreie Stadt)","kreis"] = "Fürth, Stadt"
geom_kreis[geom_kreis$kreis== "München","kreis"] = "München, Kreis"
geom_kreis[geom_kreis$kreis== "München (Kreisfreie Stadt)","kreis"] = "München, Landeshauptstadt"
geom_kreis[geom_kreis$kreis== "Osnabrück","kreis"] = "Osnabrück, Kreis"
geom_kreis[geom_kreis$kreis== "Osnabrück (Kreisfreie Stadt)","kreis"] = "Osnabrück, Stadt"
geom_kreis[geom_kreis$kreis== "Würzburg","kreis"] = "Würzburg, Kreis"
geom_kreis[geom_kreis$kreis== "Würzburg (Kreisfreie Stadt)","kreis"] = "Würzburg, Stadt"




#############################################################################################################################################
#############################################################################################################################################
# Merging
#############################################################################################################################################

# Merge geometry for "Bundesland" 
df_merged <- merge(df_orig, geom_bundesland, by="bundesland", all.x=TRUE)
df_merged <- merge(df_merged, geom_kreis, by="kreis", all.x=TRUE)

#############################################################################################################################################
# Adjust "Umlaute"
df_merged$kreis <- gsub('ü', 'ue', df_merged$kreis)
df_merged$kreis <- gsub('ö', 'oe', df_merged$kreis)
df_merged$kreis <- gsub('ä', 'ae', df_merged$kreis)
df_merged$kreis <- gsub('ß', 'ss', df_merged$kreis)

df_merged$bundesland <- gsub('ü', 'ue', df_merged$bundesland)
df_merged$bundesland <- gsub('ö', 'oe', df_merged$bundesland)
df_merged$bundesland <- gsub('ä', 'ae', df_merged$bundesland)
df_merged$bundesland <- gsub('ß', 'ss', df_merged$bundesland)

# Save as .rds
saveRDS(df_merged, "data/prepared_data/df_geoinformation.rds")










