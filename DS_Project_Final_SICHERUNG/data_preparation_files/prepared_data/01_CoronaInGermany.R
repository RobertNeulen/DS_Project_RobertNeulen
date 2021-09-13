




################################################################################################################################################################
# Incidence Germany

# Get columns of interest
df_incidence <- df[, c("date", "kr_inz_rate", "ags5","kreis")]

# Drop NA
df_incidence <- df_incidence[complete.cases(df_incidence), ]



################################################################################################################################################################
# Infections/Death over time in Germany

# Select columns of interest
df_infection_death <- df[, c("kreis", "date", "kr_ew_19", "kr_inf_md", "kr_tod_md")]

# Aggregate for Germany
df_infection_death_germany <- df_infection_death %>% dplyr::group_by(date) %>% summarise(kr_ew_19 = sum(kr_ew_19, na.rm=T),
                                                                                         kr_inf_md = sum(kr_inf_md, na.rm=T),
                                                                                         kr_tod_md=sum(kr_tod_md, na.rm=T))
df_infection_death_germany$kreis <- "All"

# Add to dataframe
df_infection_death <- rbind(df_infection_death, df_infection_death_germany)




################################################################################################################################################################
# Cases per age group

# Get age and gender column for infections and death cases
cols_age_cases_1 <- colnames(df[grepl("kr_inf_a[0-9]", colnames(df))])
cols_age_cases_2 <- colnames(df[grepl("kr_tod_a[0-9]", colnames(df))])
cols_age_cases <- c(cols_age_cases_1, "kr_inf_99", cols_age_cases_2, "kr_tod_99")

cols_gender_cases_1 <- c("kr_inf_m","kr_inf_w","kr_inf_u")
cols_gender_cases_2 <- c("kr_tod_m","kr_tod_w","kr_tod_u")
cols_gender_cases <- c(cols_gender_cases_1, cols_gender_cases_2)

# Select data
df_infection_death_group <- df[,c("kreis", "date", cols_age_cases, cols_gender_cases)]

# Aggregate for Germany
df_cases_germany_group <- subset(df_infection_death_group, select=-c(kreis))
df_cases_germany_group <- df_cases_germany_group %>% dplyr::group_by(date) %>% summarise_all(.funs=list(sum), na.rm=T)
df_cases_germany_group$kreis <- "All"

# Add to dataframe
df_infection_death_group <- rbind(df_infection_death_group, df_cases_germany_group)

# Reshape and add indicator for infection or death cases
df_infection_death_group <- pivot_longer(df_infection_death_group, names_to="class", values_to="count", cols = starts_with("kr_"))

# Identifier for age/gender and infection/death
df_infection_death_group$dummy_age <- ifelse(df_infection_death_group$class %in% cols_age_cases, 1, 0)
df_infection_death_group$dummy_infection <- ifelse(grepl("_inf_", df_infection_death_group$class), 1, 0) 

# Get class labels
df_infection_death_group <- df_infection_death_group %>% mutate(label = case_when(grepl("a0004", class) ~ "up to 4",
                                                                                  grepl("a0514", class) ~ "5 - 14",
                                                                                  grepl("a1534", class) ~ "15 - 34",
                                                                                  grepl("a3559", class) ~ "35 - 59",
                                                                                  grepl("a6079", class) ~ "60 - 79",
                                                                                  grepl("a80", class) ~ "80+",
                                                                                  grepl("99", class) ~ "Unknown",
                                                                                  grepl("_m", class) ~ "Male",
                                                                                  grepl("_w", class) ~ "Female",
                                                                                  grepl("_u", class) ~ "Unknown"))



################################################################################################################################################################
# ICU capacities

# Select columns
df_icu <- df[,c("date", "kreis","kr_its_bett", "kr_its_bett_f","kr_its_bett_ew", "kr_its_bett_b")]

# Drop NA
df_icu <- df_icu[complete.cases(df_icu), ]

# Aggregate for Germany
df_icu_germany <- subset(df_icu, select=-c(kreis))
df_icu_germany <- df_icu_germany %>% dplyr::group_by(date) %>% summarise_all(.funs=list(sum), na.rm=T)
df_icu_germany$kreis <- "All"

# Add to dataframe
df_icu <- rbind(df_icu, df_icu_germany)

# Reshape dataframe
df_icu <- pivot_longer(df_icu, names_to="class", values_to="count", cols = starts_with("kr_its"))

# Get class labels
df_icu <- df_icu %>% mutate(label = case_when(class == "kr_its_bett" ~ "ICU Beds",
                                              class == "kr_its_bett_f" ~ "Free ICU Beds",
                                              class == "kr_its_bett_ew" ~ "ICU beds per 100,000",
                                              class == "kr_its_bett_b" ~ "Blocked ICU Beds"))



#############################################################################################################################################
# Ventilator

# Select columns of interest
df_ventilator <- df[,c("kreis", "date","kr_its_bett","kr_its_inf","kr_its_inf_b")]

# Drop NA
df_ventilator <- df_ventilator[complete.cases(df_ventilator), ]

# Create new variables
df_ventilator$kr_its_inf_other <- df_ventilator$kr_its_bett - df_ventilator$kr_its_inf
df_ventilator$kr_its_inf_wo_b <- df_ventilator$kr_its_inf - df_ventilator$kr_its_inf_b

# Drop variables
df_ventilator <- subset(df_ventilator, select=-c(kr_its_bett))

# Aggregate for Germany
df_ventilator_germany <- subset(df_ventilator, select=-c(kreis))
df_ventilator_germany <- df_ventilator_germany %>% dplyr::group_by(date) %>% summarise_all(.funs=list(sum), na.rm=T)
df_ventilator_germany$kreis <- "All"

# Add to dataframe
df_ventilator <- rbind(df_ventilator, df_ventilator_germany)

# Reshape data frames
df_ventilator <- pivot_longer(df_ventilator, names_to="class", values_to="values", cols = starts_with("kr_"))

# Get class labels
df_ventilator <- df_ventilator %>% mutate(label = case_when(class == "kr_its_inf" ~ "ICU Beds COVID Patients",
                                                            class == "kr_its_inf_other" ~ "ICU Beds other Patients",
                                                            class == "kr_its_inf_b" ~ "ICU COVID Patients with Ventilator",
                                                            class == "kr_its_inf_wo_b" ~ "ICU COVID Patients without Ventilator"))




#############################################################################################################################################
# Vaccination daily

# Select vaccination data
df_impfung <- df[,c("date","bundesland", "bl_erstimpf_vt", "bl_zweitimpf_vt")]

# Drop duplicates
df_impfung <- df_impfung[!duplicated(df_impfung[1:2]),]

# Drop if no vaccination data available
df_impfung <- df_impfung[complete.cases(df_impfung), ]

# Aggregate for Germany
df_impfung_germany <- subset(df_impfung, select=-c(bundesland))
df_impfung_germany <- df_impfung_germany %>% dplyr::group_by(date) %>% summarise_all(.funs=list(sum), na.rm=T)
df_impfung_germany$bundesland <- "All"

# Add to dataframe
df_impfung <- rbind(df_impfung, df_impfung_germany)

# Reshape data (not necessary for cumulated vaccinations)
df_impfung <- pivot_longer(df_impfung, cols=c("bl_erstimpf_vt", "bl_zweitimpf_vt"), names_to="vaccination", values_to="count")

# Get class labels
df_impfung <- df_impfung %>% mutate(label = case_when(vaccination == "bl_erstimpf_vt" ~ "First vaccination",
                                                      vaccination == "bl_zweitimpf_vt" ~ "Second vaccination"))



#############################################################################################################################################
# Vaccination cumulated

# Select vaccination data
df_impfung_cumulated <- df[,c("date","bundesland", "bl_erstimpf", "bl_zweitimpf")]

# Drop duplicates
df_impfung_cumulated <- df_impfung_cumulated[!duplicated(df_impfung_cumulated[1:2]),]

# Drop if no vaccination data available
df_impfung_cumulated <- df_impfung_cumulated[complete.cases(df_impfung_cumulated), ]

# Aggregate for Germany
df_impfung_cumulated_germany <- subset(df_impfung_cumulated, select=-c(bundesland))
df_impfung_cumulated_germany <- df_impfung_cumulated_germany %>% dplyr::group_by(date) %>% summarise_all(.funs=list(sum), na.rm=T)
df_impfung_cumulated_germany$bundesland <- "All"

# Add to dataframe
df_impfung_cumulated <- rbind(df_impfung_cumulated, df_impfung_cumulated_germany)



#############################################################################################################################################
# Store as RDS file

saveRDS(df_incidence, "data/prepared_data/df_CIG_incidence.rds")
saveRDS(df_infection_death, "data/prepared_data/df_CIG_infection_death.rds")
saveRDS(df_infection_death_group, "data/prepared_data/df_CIG_infection_death_group.rds")
saveRDS(df_icu, "data/prepared_data/df_CIG_icu.rds")
saveRDS(df_ventilator, "data/prepared_data/df_CIG_ventilator.rds")
saveRDS(df_impfung, "data/prepared_data/df_CIG_impfung.rds")
saveRDS(df_impfung_cumulated, "data/prepared_data/df_CIG_impfung_cumulated.rds")




