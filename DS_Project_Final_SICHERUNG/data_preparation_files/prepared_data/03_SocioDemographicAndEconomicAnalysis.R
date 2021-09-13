

#############################################################################################################################################
# Housing Situation SQM

# Get columns of interest
cols_housing_sqm <- colnames(df[grepl("kr_wfl_", colnames(df))])

# Select data
df_housing_sqm <- df[,c("kreis", cols_housing_sqm)]

# Drop duplicates
df_housing_sqm <- df_housing_sqm[!duplicated(df_housing_sqm),]

# Aggregate for Germany
df_housing_sqm_germany <- subset(df_housing_sqm, select=-c(kreis))
df_housing_sqm_germany <- df_housing_sqm_germany %>% summarise_all(.funs=list(sum), na.rm=T)
df_housing_sqm_germany$kreis <- "Germany"

# Add to dataframe
df_housing_sqm <- rbind(df_housing_sqm, df_housing_sqm_germany)

# Reshape data frame
df_housing_sqm <- pivot_longer(df_housing_sqm, names_to="class", values_to="values", cols = starts_with("kr_"))

# Get class labels
df_housing_sqm <- df_housing_sqm %>% mutate(label = case_when(class == "kr_wfl_kl1" ~ "<70 sqm",
                                                              class == "kr_wfl_kl2" ~ "70 - 85 sqm",
                                                              class == "kr_wfl_kl3" ~ "85 - 115 sqm",
                                                              class == "kr_wfl_kl4" ~ "115 - 140 sqm",
                                                              class == "kr_wfl_kl5" ~ ">140 sqm"))

# Percentage share
df_housing_sqm <- df_housing_sqm %>% dplyr::group_by(kreis) %>% mutate(percent = values/sum(values, na.rm=T))



#############################################################################################################################################
# Housing Situation Class

# Get columns of interest
cols_housing_class <- colnames(df[grepl("kr_hh_gebtyp", colnames(df))])

# Select data
df_housing_class <- df[,c("kreis", cols_housing_class)]

# Drop duplicates
df_housing_class <- df_housing_class[!duplicated(df_housing_class),]

# Aggregate for Germany
df_housing_class_germany <- subset(df_housing_class, select=-c(kreis))
df_housing_class_germany <- df_housing_class_germany %>% summarise_all(.funs=list(sum), na.rm=T)
df_housing_class_germany$kreis <- "Germany"

# Add to dataframe
df_housing_class <- rbind(df_housing_class, df_housing_class_germany)


# Reshape data frame
df_housing_class <- pivot_longer(df_housing_class, names_to="class", values_to="values", cols = starts_with("kr_"))

# Get class labels
df_housing_class <- df_housing_class %>% mutate(label = case_when(class == "kr_hh_gebtyp1" ~ "Detached one- to two-apartment house",
                                                                  class == "kr_hh_gebtyp2" ~ "Free-standing villa",
                                                                  class == "kr_hh_gebtyp3" ~ "One- to two-apartment house with neighbour",
                                                                  class == "kr_hh_gebtyp4" ~ "Classic semi-detached house",
                                                                  class == "kr_hh_gebtyp5" ~ "Terraced house",
                                                                  class == "kr_hh_gebtyp6" ~ "Detached multi-party house",
                                                                  class == "kr_hh_gebtyp7" ~ "Semi-detached multi-party house",
                                                                  class == "kr_hh_gebtyp8" ~ "Multi-party house with neighbour",
                                                                  class == "kr_hh_gebtyp9" ~ "Multi-party house en block",
                                                                  class == "kr_hh_gebtyp10" ~ "Line building",
                                                                  class == "kr_hh_gebtyp11" ~ "Multi-party complex",
                                                                  class == "kr_hh_gebtyp12" ~ "High-rise building",
                                                                  class == "kr_hh_gebtyp97" ~ "Special form",
                                                                  class == "kr_hh_gebtyp98" ~ "Commercial building",
                                                                  class == "kr_hh_gebtyp99" ~ "Not classified"))


# Percentage share
df_housing_class <- df_housing_class %>% dplyr::group_by(kreis) %>% mutate(percent = values/sum(values, na.rm=T))



#############################################################################################################################################
# Education

# Get columns of interest
cols_students <- colnames(df[grepl("kr_schuel_", colnames(df))])
cols_education_school <- colnames(df[grepl("kr_schul_", colnames(df))])
cols_education_school_higher <- colnames(df[grepl("kr_beruf_", colnames(df))])

# Select data
df_education_profession <- df[,c("kreis", cols_students, cols_education_school, cols_education_school_higher)]

# Drop duplicates
df_education_profession <- df_education_profession[!duplicated(df_education_profession),]

# Aggregate for Germany
df_education_profession_germany <- subset(df_education_profession, select=-c(kreis))
df_education_profession_germany <- df_education_profession_germany %>% summarise_all(.funs=list(sum), na.rm=T)
df_education_profession_germany$kreis <- "Germany"

# Add to dataframe
df_education_profession <- rbind(df_education_profession, df_education_profession_germany)

# Reshape data frame
df_education_profession <- pivot_longer(df_education_profession, names_to="class", values_to="values", cols = starts_with("kr_"))

# Add indicator
df_education_profession$indicator <- ifelse(df_education_profession$class %in% cols_students, 1, 0)
df_education_profession$indicator <- ifelse(df_education_profession$class %in% cols_education_school, 2, df_education_profession$indicator)
df_education_profession$indicator <- ifelse(df_education_profession$class %in% cols_education_school_higher, 3, df_education_profession$indicator)

# Get class labels
df_education_profession <- df_education_profession %>% mutate(label = case_when(class == "kr_schuel_vor" ~ "Pre-school",
                                                                                class == "kr_schuel_grund" ~ "Elementary school",
                                                                                class == "kr_schuel_haupt" ~ "Hauptschule",
                                                                                class == "kr_schuel_real" ~ "Realschule",
                                                                                class == "kr_schuel_gym" ~ "Gymnasium",
                                                                                class == "kr_schuel_waldorf" ~ "Waldorfschule",
                                                                                class == "kr_schuel_geschul" ~ "Integrated comprehensive school",
                                                                                class == "kr_schuel_abend" ~ "Evening school",
                                                                                class == "kr_schuel_sonder" ~ "Special school",
                                                                                class == "kr_schuel_orient" ~ "Orientation level",
                                                                                class == "kr_schuel_mult" ~ "Comprehensive school",
                                                                                class == "kr_schul_kein" ~ "Nothing",
                                                                                class == "kr_schul_haupt" ~ "Hauptschule",
                                                                                class == "kr_schul_mittl" ~ "Realschule/Gymnasium",
                                                                                class == "kr_schul_hoch" ~ "College entrance qualification",
                                                                                class == "kr_beruf_kein" ~ "Nothing",
                                                                                class == "kr_beruf_lehre" ~ "Apprenticeship",
                                                                                class == "kr_beruf_fachschul" ~ "University of Applied Science",
                                                                                class == "kr_beruf_hochschul" ~ "University",
                                                                                class == "kr_beruf_prom" ~ "PhD"))

# Percentage share
df_education_profession <- df_education_profession %>% dplyr::group_by(kreis, indicator) %>% mutate(percent = values/sum(values, na.rm=T))



#############################################################################################################################################
# Day Care Coverage

# Get columns of interest
cols_daycare <- c("kr_tg_qu_kk","kr_tg_qu_vk")
cols_daycare_fulltime <- c("kr_tg_qu_gkk","kr_tg_qu_gvk")

# Select data
df_daycare <- df[,c("kreis", cols_daycare, cols_daycare_fulltime)]

# Drop duplicates
df_daycare <- df_daycare[!duplicated(df_daycare),]

# Aggregate for Germany
df_daycare_germany <- subset(df_daycare, select=-c(kreis))
df_daycare_germany <- df_daycare_germany %>% summarise_all(.funs=list(mean), na.rm=T)
df_daycare_germany$kreis <- "Germany"

# Add to dataframe
df_daycare <- rbind(df_daycare, df_daycare_germany)

# Reshape data frame
df_daycare <- pivot_longer(df_daycare, names_to="class", values_to="values", cols = starts_with("kr_"))

# Add indicator
df_daycare$ind_daycare_fulltime <- ifelse(df_daycare$class %in% cols_daycare_fulltime, 1, 0)

# Get class labels
df_daycare <- df_daycare %>% mutate(label = case_when(class == "kr_tg_qu_kk" ~ "Infant",
                                                      class == "kr_tg_qu_vk" ~ "Pre-schooler",
                                                      class == "kr_tg_qu_gkk" ~ "Infant",
                                                      class == "kr_tg_qu_gvk" ~ "Pre-schooler"))

# Replace NA with 0
df_daycare[is.na(df_daycare)] <- 0




#############################################################################################################################################
# Social Indicator

# Get columns of interest
cols_social_indicator <- c("kr_arm_alt","kr_sgb_qu","kr_arm_kind")

# Select data
df_social_indicator <- df[,c("kreis", cols_social_indicator)]

# Drop duplicates
df_social_indicator <- df_social_indicator[!duplicated(df_social_indicator),]

# Aggregate for Germany
df_social_indicator_germany <- subset(df_social_indicator, select=-c(kreis))
df_social_indicator_germany <- df_social_indicator_germany %>% summarise_all(.funs=list(mean), na.rm=T)
df_social_indicator_germany$kreis <- "Germany"

# Add to dataframe
df_social_indicator <- rbind(df_social_indicator, df_social_indicator_germany)

# Reshape data frame
df_social_indicator <- pivot_longer(df_social_indicator, names_to="class", values_to="values", cols = starts_with("kr_"))

# Get class labels
df_social_indicator <- df_social_indicator %>% mutate(label = case_when(class == "kr_arm_alt" ~ "Poverty among the elderly",
                                                                        class == "kr_sgb_qu" ~ "Social welfare recipients",
                                                                        class == "kr_arm_kind" ~ "Poverty among children"))

# Adjust classes
df_social_indicator$label <- factor(df_social_indicator$label, levels = c("Poverty among the elderly","Poverty among children","Social welfare recipients"))

# Replace NA with 0
df_social_indicator[is.na(df_social_indicator)] <- 0
                                                      
                                                      
#############################################################################################################################################
# Economic sectors

# Get columns of interest
cols_econ_sector <- colnames(df[grepl("kr_erwt_ao_", colnames(df))])

# Select data
df_econ_sector <- df[,c("kreis", cols_econ_sector)]

# Drop duplicates
df_econ_sector <- df_econ_sector[!duplicated(df_econ_sector),]

# Aggregate for Germany
df_econ_sector_germany <- subset(df_econ_sector, select=-c(kreis))
df_econ_sector_germany <- df_econ_sector_germany %>% summarise_all(.funs=list(sum), na.rm=T)
df_econ_sector_germany$kreis <- "Germany"

# Add to dataframe
df_econ_sector <- rbind(df_econ_sector, df_econ_sector_germany)

# Reshape data frame
df_econ_sector <- pivot_longer(df_econ_sector, names_to="class", values_to="values", cols = starts_with("kr_"))

# Replace NA with 0
df_econ_sector[is.na(df_econ_sector)] <- 0

# Get class labels
df_econ_sector <- df_econ_sector %>% mutate(label = case_when(class == "kr_erwt_ao_b1" ~ "Agriculture",
                                                              class == "kr_erwt_ao_b2" ~ "Raw Material Extraction",
                                                              class == "kr_erwt_ao_b3" ~ "Construction",
                                                              class == "kr_erwt_ao_b4" ~ "Manufactoring",
                                                              class == "kr_erwt_ao_b5" ~ "Traffic and Energy",
                                                              class == "kr_erwt_ao_b6" ~ "Trade",
                                                              class == "kr_erwt_ao_b7" ~ "Finance and Insurance",
                                                              class == "kr_erwt_ao_b8" ~ "Services",
                                                              class == "kr_erwt_ao_b9" ~ "Public Administration",
                                                              class == "kr_erwt_ao_b99" ~ "Unknown"))

# Percentage share
df_econ_sector <- df_econ_sector %>% dplyr::group_by(kreis) %>% mutate(percent = values/sum(values, na.rm=T))



#############################################################################################################################################
# Unemployment quota

# Select data
df_unemployment <- df[,c("kreis", "date", "kr_alq")]

# Drop NA
df_unemployment <- df_unemployment[complete.cases(df_unemployment), ]

# Get month and year (for duplicate removement)
df_unemployment$year <- format(df_unemployment$date, format="%Y")
df_unemployment$month <- format(df_unemployment$date, format="%m")

# Duplicate removement
df_unemployment <- df_unemployment %>% distinct(kreis, year, month, .keep_all = TRUE)

# Drop columns
df_unemployment <- subset(df_unemployment, select=-c(year, month))

# Aggregate for Germany
df_unemployment_germany <- subset(df_unemployment, select=-c(kreis))
df_unemployment_germany <- df_unemployment_germany %>% dplyr::group_by(date) %>% summarise_all(.funs=list(mean), na.rm=T)
df_unemployment_germany$kreis <- "Germany"

# Add to dataframe
df_unemployment <- rbind(df_unemployment, df_unemployment_germany)


#############################################################################################################################################
# GDP per capita in Germany

# Choose variables of interest
df_gdp_capita <- df[, c("kr_bip_ew", "ags5", "kreis")]

# Drop missing values
df_gdp_capita <- df_gdp_capita[complete.cases(df_gdp_capita$kr_bip_ew),]

# Drop duplicates
df_gdp_capita <- df_gdp_capita[!duplicated(df_gdp_capita),]

# Aggregate for Germany
df_gdp_capita_germany <- subset(df_gdp_capita, select=-c(kreis))
df_gdp_capita_germany <- df_gdp_capita_germany %>% summarise_all(.funs=list(mean), na.rm=T)
df_gdp_capita_germany$kreis <- "Germany"

# Add to dataframe
df_gdp_capita <- rbind(df_gdp_capita, df_gdp_capita_germany)



#############################################################################################################################################
# Medical

# Choose variables of interest
df_medical <- df[,c("kreis", "kr_ar_ew", "kr_kh_bett_ew", "kr_apo")]

# Drop missing values
df_medical <- df_medical[!duplicated(df_medical),]

# Aggregate for Germany
df_medical_germany <- subset(df_medical, select=-c(kreis))
df_medical_germany <- df_medical_germany %>% summarise_all(.funs=list(mean), na.rm=T)
df_medical_germany$kreis <- "Germany"

# Add to dataframe
df_medical <- rbind(df_medical, df_medical_germany)

# Set all to per 10,000/100,000 (later for pharmacies for a better analysis) habitants
df_medical$kr_kh_bett_ew <- df_medical$kr_kh_bett_ew*10
df_medical$kr_apo <- df_medical$kr_apo

# Reshape data frame
df_medical <- pivot_longer(df_medical, names_to="class", values_to="values", cols = starts_with("kr_"))

# Get class labels
df_medical <- df_medical %>% mutate(label = case_when(class == "kr_ar_ew" ~ "Doctors per 10,000",
                                                      class == "kr_kh_bett_ew" ~ "Hospital Beds per 10,000",
                                                      class == "kr_apo" ~ "Pharmacies per 100,000"))



#############################################################################################################################################
# Store as RDS file

saveRDS(df_housing_sqm, "data/prepared_data/df_SEA_housing_sqm.rds")
saveRDS(df_housing_class, "data/prepared_data/df_SEA_housing_class.rds")
saveRDS(df_education_profession, "data/prepared_data/df_SEA_education_profession.rds")
saveRDS(df_daycare, "data/prepared_data/df_SEA_daycare.rds")
saveRDS(df_social_indicator, "data/prepared_data/df_SEA_social_indicator.rds")
saveRDS(df_econ_sector, "data/prepared_data/df_SEA_econ_sector.rds")
saveRDS(df_unemployment, "data/prepared_data/df_SEA_unemployment.rds")
saveRDS(df_gdp_capita, "data/prepared_data/df_SEA_gdp_capita.rds")
saveRDS(df_medical, "data/prepared_data/df_SEA_medical.rds")






