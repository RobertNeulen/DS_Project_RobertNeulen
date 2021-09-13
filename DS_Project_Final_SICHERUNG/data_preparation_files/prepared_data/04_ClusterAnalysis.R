

#############################################################################################################################################
# Define sets of variables

cols_age <- c("kr_ew_00u05", "kr_ew_05u15", "kr_ew_15u18", "kr_ew_18u20", "kr_ew_20u25", "kr_ew_25u30", "kr_ew_30u35", "kr_ew_35u60", "kr_ew_60u80", "kr_ew_80")
cols_econ_structure <- c(colnames(df)[grepl("kr_erwt_ao_", colnames(df))])
cols_econ <- c("kr_alq","kr_bip_ew")
cols_edu <- c("kr_schueler", "kr_student")
cols_pop <- c("kr_ew_dichte")
cols_nursing <- c("kr_pb")
cols_settle <- c(colnames(df)[grepl("kr_ew_gebtyp", colnames(df))])
cols_childcare <- c("kr_tg_qu_kk", "kr_tg_qu_vk")
cols_clubs <- c("kr_vereine")
cols_foreign <- c("kr_ausl_ant")
cols_commute <- c("kr_sp")

cols_feat <- c(cols_age, cols_econ_structure, cols_edu, cols_pop, cols_nursing, cols_settle, cols_childcare, cols_clubs, cols_foreign, cols_commute, cols_econ)

# Create data frame
df_feat_cluster <- data.frame(category=character(length(cols_feat)),
                              feature=character(length(cols_feat)),
                              name=character(length(cols_feat)),
                              stringsAsFactors=FALSE)

# Get features in columns
df_feat_cluster$feature <- cols_feat

# Get categories depending on feature
df_feat_cluster[which(df_feat_cluster$feature %in% cols_age), "category"] <- "Age structure"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_econ_structure), "category"] <- "Economic sector structure"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_econ), "category"] <- "Economic characteristics"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_edu), "category"] <- "School and university students"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_pop), "category"] <- "Population density"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_nursing), "category"] <- "Nursing home inhabitants"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_settle), "category"] <- "Settlement types"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_childcare), "category"] <- "Childcare"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_clubs), "category"] <- "Clubs and Societies"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_foreign), "category"] <- "Foreign habitants"
df_feat_cluster[which(df_feat_cluster$feature %in% cols_commute), "category"] <- "Commuting"


# Get variable names
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_00u05"), "name"] <- "Age group: 0 - 5"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_05u15"), "name"] <- "Age group: 5 - 15"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_15u18"), "name"] <- "Age group: 15 - 18"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_18u20"), "name"] <- "Age group: 18 - 20"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_20u25"), "name"] <- "Age group: 20 - 25"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_25u30"), "name"] <- "Age group: 25 - 30"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_30u35"), "name"] <- "Age group: 30 - 35"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_35u60"), "name"] <- "Age group: 35 - 60"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_60u80"), "name"] <- "Age group: 60 - 80"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_80"), "name"] <- "Age group: 80+"

df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b1"), "name"] <- "Economic sector: Agriculture"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b2"), "name"] <- "Economic sector: Raw Material Extraction"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b3"), "name"] <- "Economic sector: Construction"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b4"), "name"] <- "Economic sector: Manufacturing"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b5"), "name"] <- "Economic sector: Traffic and Energy"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b6"), "name"] <- "Economic sector: Trade"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b7"), "name"] <- "Economic sector: Finance and Insurance"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b8"), "name"] <- "Economic sector: Services"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b9"), "name"] <- "Economic sector: Public Administration"
df_feat_cluster[which(df_feat_cluster$feature == "kr_erwt_ao_b99"), "name"] <- "Economic sector: Unknown"

df_feat_cluster[which(df_feat_cluster$feature == "kr_alq"), "name"] <- "Unemployment Quota"
df_feat_cluster[which(df_feat_cluster$feature == "kr_bip_ew"), "name"] <- "GDP per Capita"

df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp1"), "name"] <- "Settlement type: Detached one- to two-apartment house"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp2"), "name"] <- "Settlement type: Free-standing villa"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp3"), "name"] <- "Settlement type: One- to two-apartment house with adjoining owner"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp4"), "name"] <- "Settlement type: Classic semi-detached house"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp5"), "name"] <- "Settlement type: Terraced house"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp6"), "name"] <- "Settlement type: Detached multi-party house"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp7"), "name"] <- "Settlement type: Semi-detached multi-party house"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp8"), "name"] <- "Settlement type: Multi-party house with neighbour"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp9"), "name"] <- "Settlement type: Multi-party house en block"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp10"), "name"] <- "Settlement type: Row building"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp11"), "name"] <- "Settlement type: Multifamily complex"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp12"), "name"] <- "Settlement type: High-rise building"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp97"), "name"] <- "Settlement type: Special form"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp98"), "name"] <- "Settlement type: Commerce"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_gebtyp99"), "name"] <- "Settlement type: Not classified"

df_feat_cluster[which(df_feat_cluster$feature == "kr_schueler"), "name"] <- "Number of school students"
df_feat_cluster[which(df_feat_cluster$feature == "kr_student"), "name"] <- "Number of university students"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ew_dichte"), "name"] <- "Population density"
df_feat_cluster[which(df_feat_cluster$feature == "kr_pb"), "name"] <- "Number of people with need of care"
df_feat_cluster[which(df_feat_cluster$feature == "kr_tg_qu_kk"), "name"] <- "Childcare: Toddlers"
df_feat_cluster[which(df_feat_cluster$feature == "kr_tg_qu_vk"), "name"] <- "Childcare: Pre-school kids"
df_feat_cluster[which(df_feat_cluster$feature == "kr_vereine"), "name"] <- "Number of clubs and societies"
df_feat_cluster[which(df_feat_cluster$feature == "kr_ausl_ant"), "name"] <- "Share of people with foreign nationality"
df_feat_cluster[which(df_feat_cluster$feature == "kr_sp"), "name"] <- "Total amount of commuters"


#############################################################################################################################################
# Select variables

df_cluster_analysis <- df[,c("kreis", "ags5", "date", "kr_ew_19", "kr_inf_md_kum","kr_tod_md_kum", cols_feat)]


#############################################################################################################################################
# Scale variables if necessary

# Per 10,000 if necessary
cols_adj <- c("kr_ew_00u05", "kr_ew_05u15", "kr_ew_15u18", "kr_ew_18u20", "kr_ew_20u25", "kr_ew_25u30", "kr_ew_30u35", "kr_ew_35u60", "kr_ew_60u80", "kr_ew_80", 
              colnames(df_cluster_analysis)[grepl("kr_erwt_ao_", colnames(df_cluster_analysis))], "kr_schueler", "kr_student", "kr_schueler", 
              colnames(df_cluster_analysis)[grepl("kr_ew_gebtyp", colnames(df_cluster_analysis))], "kr_vereine", "kr_sp")

df_cluster_analysis[, colnames(df_cluster_analysis) %in% cols_adj] <- (df_cluster_analysis[, colnames(df_cluster_analysis) %in% cols_adj]/df_cluster_analysis$kr_ew_19)*10000



#############################################################################################################################################
# Store as RDS file

saveRDS(df_feat_cluster, "data/prepared_data/df_CA_feat_cluster.rds")
saveRDS(df_cluster_analysis, "data/prepared_data/df_CA_cluster_analysis.rds")







