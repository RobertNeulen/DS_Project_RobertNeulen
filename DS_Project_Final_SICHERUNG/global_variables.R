#############################################################################################################################################
#############################################################################################################################################
# Prepare session
## Clear session
## Install (if necessary) and load packages
#############################################################################################################################################

# Reset session
#rm(list = ls())

# Necessary to display "Umlaute" correctly
Sys.setlocale(category = "LC_ALL", locale = "German")


# Necessary packages
packages <- c("tidyverse","shiny","ggplot2","readxl","data.table","shinythemes","zoo","DataCombine",
              "scales","writexl","DT","sf","gridExtra","viridis","shinycssloaders","caret","modelr",
              "rsconnect","shinyWidgets","shinydashboard","fastDummies","lubridate","ggExtra","stats",
              "fixest","RColorBrewer","plotly","ggpubr") 

# Install packages
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


#############################################################################################################################################
# Load prepared data

# Data: Corona in Germany
df_incidence <- readRDS("data/prepared_data/df_CIG_incidence.rds")
df_infection_death <- readRDS("data/prepared_data/df_CIG_infection_death.rds")
df_infection_death_group <- readRDS("data/prepared_data/df_CIG_infection_death_group.rds")
df_icu <- readRDS("data/prepared_data/df_CIG_icu.rds")
df_ventilator <- readRDS("data/prepared_data/df_CIG_ventilator.rds")
df_impfung <- readRDS("data/prepared_data/df_CIG_impfung.rds")
df_impfung_cumulated <- readRDS("data/prepared_data/df_CIG_impfung_cumulated.rds")

# Data: NPI
df_npi <- readRDS("data/prepared_data/df_NPI.rds")

# Data: Socio Demographic and Economic Analysis
df_housing_sqm <- readRDS("data/prepared_data/df_SEA_housing_sqm.rds")
df_housing_class <- readRDS("data/prepared_data/df_SEA_housing_class.rds")
df_education_profession <- readRDS("data/prepared_data/df_SEA_education_profession.rds")
df_daycare <- readRDS("data/prepared_data/df_SEA_daycare.rds")
df_social_indicator <-readRDS("data/prepared_data/df_SEA_social_indicator.rds")
df_econ_sector <- readRDS("data/prepared_data/df_SEA_econ_sector.rds")
df_unemployment <- readRDS("data/prepared_data/df_SEA_unemployment.rds")
df_gdp_capita <- readRDS("data/prepared_data/df_SEA_gdp_capita.rds")
df_medical <- readRDS("data/prepared_data/df_SEA_medical.rds")

# Data: Cluster Analysis
df_cluster_feat <- readRDS("data/prepared_data/df_CA_feat_cluster.rds")
df_cluster_analysis <- readRDS("data/prepared_data/df_CA_cluster_analysis.rds")

# Load description data
df_descr <- readRDS("data/prepared_data/df_NPI_categories.rds")
df_descr_sub <- readRDS("data/prepared_data/df_NPI_subcategories.rds")

# Load geo data
df_geo <- as.data.frame(readRDS("data/prepared_data/df_geoinformation.rds"))
df_geo <- subset(df_geo, select=c(ags5, geometry_kr))

# Load date coverage data
df_date_coverage <- read_excel("data/other/df_date_coverage.xlsx")
df_date_coverage$min <- as.Date(df_date_coverage$min)
df_date_coverage$max <- as.Date(df_date_coverage$max)


#############################################################################################################################################
# Prepare lists

# Prepare download file
df_download <- df_descr
df_download_sub <- df_descr_sub

# Prepare unique values of "Kreis"
l_kreis <- as.character(df_incidence$kreis)
l_kreis <- str_sort(unique(l_kreis))

# Prepare unique values of "Bundesland"
l_bundesland <- as.character(df_impfung$bundesland)
l_bundesland <- str_sort(unique(l_bundesland))

# Select all columns starting with "M"
df_list <- select_if(df_npi, grepl("^M", names(df_npi)))
names_NPI <- colnames(df_list)

# If the columns have more than 4 characters --> not aggregated columns
names_notagg <- names_NPI[nchar(names_NPI) > 4]

# If the columns have less/equal than 4 characters --> aggregated columns
    ## Also sort the vector of column names
names_agg <- names_NPI[nchar(names_NPI) <= 4]
names_agg <- names_agg[order(-nchar(names_agg), names_agg)]



#############################################################################################################################################
# Load all necessary functions
source("function_files/functions_1_CoronaInGermany.R")
source("function_files/functions_2_NonPharmaceuticalInterventions.R")
source("function_files/functions_3_SocioDemographicAndEconomicAnalysis.R")
source("function_files/functions_4_ClusterAnalysis.R")
source("function_files/functions_5_NPIEvaluation.R")


