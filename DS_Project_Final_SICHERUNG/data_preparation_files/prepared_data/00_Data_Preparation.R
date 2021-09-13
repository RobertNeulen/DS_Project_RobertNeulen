
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
packages <- c("tidyverse","shiny","ggplot2","readxl","data.table","shinythemes","zoo","DataCombine",
              "scales","writexl","DT","sf","gridExtra","viridis","shinycssloaders","caret","modelr",
              "rsconnect","shinyWidgets","shinydashboard","fastDummies","lubridate","ggExtra","stats",
              "fixest","RColorBrewer","plotly","ggpubr") 

# Install packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


#############################################################################################################################################
# Read in data
df <- read.csv("data/raw_data/final_dataframe.csv")

# Load description data
df_descr <- read_excel("data/other/datensatzbeschreibung_massnahmen.xlsx", sheet="Oberkategorien")
df_descr_sub <- read_excel("data/other/datensatzbeschreibung_massnahmen.xlsx", sheet="DSB KR")
df_descr_sub_V2 <- read_excel("data/other/datensatzbeschreibung_massnahmen_V2.xlsx", sheet="DSB KR")

# Adjust date column
df$date <- as.Date(df$date)

# Order data
df <- arrange(df, kreis, bundesland, date)

# Prepare data
source("data_preparation_files/prepared_data/01_CoronaInGermany.r")
source("data_preparation_files/prepared_data/02_NPIOverviewAndEvaluation.r")
source("data_preparation_files/prepared_data/03_SocioDemographicAndEconomicAnalysis.r")
source("data_preparation_files/prepared_data/04_ClusterAnalysis.r")
source("data_preparation_files/prepared_data/99_NPIDescription.r")

