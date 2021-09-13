

#############################################################################################################################################
# Load NPI Variables

# Get variables of interest
cols_NPI <- c(colnames(df)[grepl("^M", colnames(df))])
cols_NPI_eval <- c("kr_neuinf_rate", "kr_inz_rate", "kr_sterbe_rate", "kr_tod_md_kum","kr_ew_19")

# Get data
df_NPI <- df[,c("kreis", "bundesland", "date", cols_NPI, cols_NPI_eval)]

# Calculate 7 day death incidence
df_NPI$kr_death_rate <- ((df_NPI$kr_tod_md_kum - lag(df_NPI$kr_tod_md_kum, 7))/df_NPI$kr_ew_19)*100000

# Now we have 4 different measures
  ## 7 days incidence rate infections: kr_inz_rate
  ## 7 days incidence rate deaths: kr_death_rate (self-calculated)
  ## New infections cases as share of cumulated cases: kr_neuinf_rate
  ## New death cases as share of cumulated cases: kr_sterbe_rate
# Calculation for "kr_death_rate" can be controlled with:
# test <- df[,c("kreis","date","kr_inz_rate","kr_inf_md_kum","kr_ew_19")]
# test$test <- ((test$kr_inf_md_kum - lag(test$kr_inf_md_kum, 7))/test$kr_ew_19)*100000


# Drop unnecessary columns
df_NPI <- subset(df_NPI, select=-c(kr_tod_md_kum, kr_ew_19))

# Get month and year variable
df_NPI$week <- week(df_NPI$date)
df_NPI$month <- month(df_NPI$date)
df_NPI$year <- year(df_NPI$date)


#############################################################################################################################################
# Store as RDS file

saveRDS(df_NPI, "data/prepared_data/df_NPI.rds")








