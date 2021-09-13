
#############################################################################################################################################

# Select and prepare data
df_descr <- subset(df_descr, select=c(Variablenname, Variable_Eng, Beschreibung))
df_descr <- data.table::setnames(df_descr, old = c("Variablenname","Variable_Eng","Beschreibung"), new = c("Code","Description","Application criterion"))
df_descr[,"Application criterion"] <- "-"

# These translation come from an older version (done by hand)
  ## Can be used for the newer version as well
  ## Changes just with respect to the new sub-groups
df_descr_sub <- subset(df_descr_sub, select=c(Variable, Variable_Eng))

# Drop duplicates
df_descr_sub <- df_descr_sub[!duplicated(df_descr_sub$Variable), ]

# Correct spelling errors/changes in new overview
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Abstantsregelung", "Abstandsregelung")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Groß-&Einzelhandel", "Groß- & Einzelhandel")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Kultur&Bildungseinr", "Kultur- & Bildungseinr")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "//Testbezogene", "// Testbezogene")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Kontakt-/Versammlungsbeschr. Privatpers. im priv Raum //", "Kontakt-/Versammlungsbeschr. Privatpers. im priv. Raum //")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Kontakt-/Versammlungsbeschr. Privatpers. im priv //", "Kontakt-/Versammlungsbeschr. Privatpers. im priv. Raum //")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Reisebeschr. Inland // Reisen nur unter bestimmten Voraussetzungen", "Reisebeschr. Inland // Reisen nur unter bestimmten Voraussetzungen (bspw. Bewegungsradius)")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Kontatk", "Kontakt")

df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Selektiv 1", "Selektiv 1: ")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Selektiv 2", "Selektiv 2: ")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Selektiv 3", "Selektiv 3: ")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Selektiv 4", "Selektiv 4: ")

df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Zeitversetzt 1", "Zeitversetzt 1: ")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Zeitversetzt 2", "Zeitversetzt 2: ")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Zeitversetzt 3", "Zeitversetzt 3: ")
df_descr_sub_V2$Variable <- str_replace(df_descr_sub_V2$Variable, "Zeitversetzt 4", "Zeitversetzt 4: ")

# Drop all "Platzhalter" interventions
df_descr_sub_V2 <- df_descr_sub_V2[!grepl("Platzhalter", df_descr_sub_V2$Variable),]

# Merge to new data
df_descr_sub_V2 <- merge(df_descr_sub_V2, df_descr_sub, by="Variable", all.x=TRUE)

# Define "Application criterion"
df_descr_sub_V2$Beschreibung_Eng <- NA
df_descr_sub_V2[grepl("Unabhängig von Neuinfektionen", df_descr_sub_V2$Beschreibung), "Beschreibung_short_Eng"] <- "Independent of new infections"
df_descr_sub_V2[grepl("Ab 0 Neuinfekt. pro 100K", df_descr_sub_V2$Beschreibung), "Beschreibung_short_Eng"] <- "From 0 new infections per 100k"
df_descr_sub_V2[grepl("Ab 10 Neuinfekt. pro 100K", df_descr_sub_V2$Beschreibung), "Beschreibung_short_Eng"] <- "From 10 new infections per 100k"
df_descr_sub_V2[grepl("Ab 35 Neuinfekt. pro 100K", df_descr_sub_V2$Beschreibung), "Beschreibung_short_Eng"] <- "From 35 new infections per 100k"
df_descr_sub_V2[grepl("Ab 50 Neuinfekt. pro 100K", df_descr_sub_V2$Beschreibung), "Beschreibung_short_Eng"] <- "From 50 new infections per 100k"
df_descr_sub_V2[grepl("Ab 100 Neuinfekt. pro 100K", df_descr_sub_V2$Beschreibung), "Beschreibung_short_Eng"] <- "From 100 new infections per 100k"

# Add new translations
df_descr_sub_V2[grepl("Krankenhäuser //  Schließung öffentlicher Bereiche", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Hospitals // Closure of public areas"
df_descr_sub_V2[grepl("Krankenhäuser // Besuche nur in Ausnahmefällen", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Hospitals // Visits only in exceptional cases"
df_descr_sub_V2[grepl("Krankenhäuser // Besuchsbegrenzung", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Hospitals // Visitor limit"
df_descr_sub_V2[grepl("Krankenhäuser // Besuchsverbot", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Hospitals // Visitor ban"
df_descr_sub_V2[grepl("Krankenhäuser // Isolation von Patienten", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Hospitals // Isolation of patients"
df_descr_sub_V2[grepl("Krankenhäuser // Verbot von Veranstaltungen", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Hospitals // Prohibition of events"
df_descr_sub_V2[grepl("Pflegeeinrichtungen //   Isolation bzw. Betretungsverbot bei Verdacht auf Infekt.", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Nursing homes // Isolation or prohibition of entry in case of suspected infection"
df_descr_sub_V2[grepl("Pflegeeinrichtungen //   Schließung öffentlicher Bereiche", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Nursing homes // Closure of public areas"
df_descr_sub_V2[grepl("Pflegeeinrichtungen //   Verbot von Veranstaltungen", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Nursing homes // Prohibition of events"
df_descr_sub_V2[grepl("Pflegeeinrichtungen //  Besuchsbegrenzung", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Nursing homes // Visitor limit"
df_descr_sub_V2[grepl("Pflegeeinrichtungen //  Kapazitätsbegrenzung & Notbetr.", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Nursing homes // Capacity limitation and emergency care (day care)"
df_descr_sub_V2[grepl("Pflegeeinrichtungen // Besuchsverbot", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Nursing homes // Visitor ban"
df_descr_sub_V2[grepl("Test-Maßnahmen // Test-Maßnahmen in Schulen", df_descr_sub_V2$Variable), "Variable_Eng"] <- "Testing measures // Test measures in schools"

# Rename columns
df_descr_sub_V2 <- subset(df_descr_sub_V2, select=c(Variablenname, Variable_Eng, Beschreibung_short_Eng))
df_descr_sub_V2 <- data.table::setnames(df_descr_sub_V2, old = c("Variablenname", "Variable_Eng", "Beschreibung_short_Eng"), new = c("Sub-Code", "Description", "Application criterion"))

# Re-order
df_descr_sub_V2 <- df_descr_sub_V2[order(df_descr_sub_V2$`Sub-Code`),]


#############################################################################################################################################
# Store as RDS file

saveRDS(df_descr, "data/prepared_data/df_NPI_categories.rds")
saveRDS(df_descr_sub_V2, "data/prepared_data/df_NPI_subcategories.rds")



