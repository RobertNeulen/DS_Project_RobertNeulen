#############################################################################################################################################
#############################################################################################################################################
# Prepare session
## Clear session
## Install (if necessary) and load packages
#############################################################################################################################################

# Reset session
rm(list = ls())

# Necessary to display "Umlaute" correctly
#Sys.setlocale("LC_CTYPE","C")
Sys.setlocale(category = "LC_ALL", locale = "German")


# Necessary packages
packages <- c("tidyverse","readxl","data.table","writexl") 

# Install packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))



#############################################################################################################################################
# Read data

df <- read.csv("data/raw_data/final_dataframe.csv")
df_descr <- read_excel("data/other/datensatzbeschreibung_allgemein.xlsx")



#############################################################################################################################################
# Adjust data frames

# Extract column names
df_cols <- as.data.frame(colnames(df))
colnames(df_cols) <- "Variablenname"

# Delete some column names
df_cols <- subset(df_cols, Variablenname!="ags2" & Variablenname!="ags5" & Variablenname!="kreis" & Variablenname!="bundesland" & Variablenname!="date")

# Drop columns for interventions (already have their own list)
df_cols <- df_cols[-grep("M", df_cols$Variablenname),]
df_cols <- as.data.frame(df_cols)
colnames(df_cols) <- "Variablenname"

# Extract columns
df_descr <- df_descr[,c("Variablenname","Beschreibung","Indikator")]

# Cut of day date parts of variable names
df_descr$Variablenname <- gsub("_JJJJMM", "", df_descr$Variablenname)

# Transform to lower case
df_descr$Variablenname <- tolower(df_descr$Variablenname)

# Drop empty rows
df_descr <- df_descr[rowSums(is.na(df_descr)) != ncol(df_descr), ]

# Adjust some names (where not matched n the merge later on due to differences in documented and stored/adjusted name)
df_descr$Variablenname <- gsub("kr_inf_a99", "kr_inf_99", df_descr$Variablenname)
df_descr$Variablenname <- gsub("kr_inf_m_a99", "kr_inf_m_99", df_descr$Variablenname)
df_descr$Variablenname <- gsub("kr_inf_w_a99", "kr_inf_w_99", df_descr$Variablenname)
df_descr$Variablenname <- gsub("kr_pen$", "kr_pen_change", df_descr$Variablenname)
df_descr$Variablenname <- gsub("kr_tod_rate", "kr_sterbe_rate", df_descr$Variablenname)
df_descr$Variablenname <- gsub("kr_tod_a99", "kr_tod_99", df_descr$Variablenname)


#############################################################################################################################################
# Merge data frames

# Merge
df_merged <- merge(df_cols, df_descr, by="Variablenname", all.x=TRUE)

# If an indicator is missing (e.g. new variable is introduced into the data sets) --> use 1 (should get scaled to cases/observations per 100,000)
df_merged[["Indikator"]][is.na(df_merged[["Indikator"]])] <- 1


#############################################################################################################################################
# Store data
write_xlsx(df_merged,"data/other/df_variable_description.xlsx")






