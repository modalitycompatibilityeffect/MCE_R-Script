####Vorarbeit####

#Pakete installieren
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!require("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
if (!require("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!require("car", quietly = TRUE)) {
  install.packages("car")
}
if (!require("ez", quietly = TRUE)) {
  install.packages("ez")
}
if (!require("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
if (!require("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
#working directory setzen
setwd ("~/Desktop/Masterarbeit/R/Daten")
#leeres Dataframe erstellen
all_means <- data.frame(VP = 31:60)






#### Lab Daten ####


#Lab Daten einlesen
LabDaten <- read.table("monali_lab_data_corrected.tsv", header = TRUE, sep = "\t")


#OA löschen
LabDaten_YA <- subset(LabDaten, subjectID < 61)
#Practice Durchgänge löschen
LabDaten_YA_Procedure <- subset(LabDaten_YA, timepoint.x != "Practice")


#ST, DT und TS in jeweils compatible und incompatible splitten 
LabDaten_YA_Procedure_DTc <- subset(LabDaten_YA_Procedure, STDT == "DT" & mapping == "compatible")
LabDaten_YA_Procedure_DTi <- subset(LabDaten_YA_Procedure, STDT == "DT" & mapping == "incompatible")

LabDaten_YA_Procedure_STc <- subset(LabDaten_YA_Procedure, STDT == "ST" & mapping == "compatible")
LabDaten_YA_Procedure_STi <- subset(LabDaten_YA_Procedure, STDT == "ST" & mapping == "incompatible")

LabDaten_YA_Procedure_TSc <- subset(LabDaten_YA_Procedure, STDT == "TS" & mapping == "compatible")
LabDaten_YA_Procedure_TSi <- subset(LabDaten_YA_Procedure, STDT == "TS" & mapping == "incompatible")





###Dual Task

##DTc
#Grenzwerte filtern
LabDaten_YA_Procedure_DTc_filtered <- subset(LabDaten_YA_Procedure_DTc, RT >=100 & RT <=4000)
#Interquartilsmethode
Q1 <- quantile(LabDaten_YA_Procedure_DTc_filtered$RT, 0.25, na.rm = TRUE)
Q3 <- quantile(LabDaten_YA_Procedure_DTc_filtered$RT, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
LabDaten_YA_Procedure_DTc_filtered_IQR <- LabDaten_YA_Procedure_DTc_filtered[!(LabDaten_YA_Procedure_DTc_filtered$RT < (Q1 - 1.5 * IQR) | LabDaten_YA_Procedure_DTc_filtered$RT > (Q3 + 1.5 * IQR)), ]
#Mittelwerte nach subjectID berechnen
mittelwerte <- aggregate(RT ~ subjectID, data = LabDaten_YA_Procedure_DTc_filtered_IQR, FUN = mean)
#Spalten umbenennen
colnames(mittelwerte) <- c("VP", "Lab_DTc")
# Kombiniere die Daten aus "mittelwerte" in "all_means" anhand der Spalte "VP"
all_means <- merge(all_means, mittelwerte, by = "VP", all.x = TRUE)


##DTi
#Grenzwerte filtern
LabDaten_YA_Procedure_DTi_filtered <- subset(LabDaten_YA_Procedure_DTi, RT >=100 & RT <=4000)
#Interquartilsmethode
Q1 <- quantile(LabDaten_YA_Procedure_DTi_filtered$RT, 0.25, na.rm = TRUE)
Q3 <- quantile(LabDaten_YA_Procedure_DTi_filtered$RT, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
LabDaten_YA_Procedure_DTi_filtered_IQR <- LabDaten_YA_Procedure_DTi_filtered[!(LabDaten_YA_Procedure_DTi_filtered$RT < (Q1 - 1.5 * IQR) | LabDaten_YA_Procedure_DTi_filtered$RT > (Q3 + 1.5 * IQR)), ]
#Mittelwerte nach subjectID berechnen
mittelwerte <- aggregate(RT ~ subjectID, data = LabDaten_YA_Procedure_DTi_filtered_IQR, FUN = mean)
#Spalten umbenennen
colnames(mittelwerte) <- c("VP", "Lab_DTi")
# Kombiniere die Daten aus "mittelwerte" in "all_means" anhand der Spalte "VP"
all_means <- merge(all_means, mittelwerte, by = "VP", all.x = TRUE)


##MCE berechnen und in neuer Spalte in all_means eintragen
all_means$Lab_DT_MCE <- all_means$Lab_DTi - all_means$Lab_DTc



###Single Task

##STc
#Grenzwerte filtern
LabDaten_YA_Procedure_STc_filtered <- subset(LabDaten_YA_Procedure_STc, is.na(rt_man) | (rt_man >= 100 & rt_man <= 4000))
LabDaten_YA_Procedure_STc_filtered <- subset(LabDaten_YA_Procedure_STc, is.na(rt_voc) | (rt_voc >= 100 & rt_voc <= 4000))
#neue Spalte mit rt_voc und rt_man kombiniert für Interquartilsmethode
LabDaten_YA_Procedure_STc_filtered$rt_voc_or_man <- ifelse(!is.na(LabDaten_YA_Procedure_STc_filtered$rt_man) & !is.na(LabDaten_YA_Procedure_STc_filtered$rt_voc), NA,
                                                           ifelse(!is.na(LabDaten_YA_Procedure_STc_filtered$rt_man), LabDaten_YA_Procedure_STc_filtered$rt_man,
                                                                  ifelse(!is.na(LabDaten_YA_Procedure_STc_filtered$rt_voc), LabDaten_YA_Procedure_STc_filtered$rt_voc, NA)))
#Interquartilsmethode 
Q1 <- quantile(LabDaten_YA_Procedure_STc_filtered$rt_voc_or_man, 0.25, na.rm = TRUE)
Q3 <- quantile(LabDaten_YA_Procedure_STc_filtered$rt_voc_or_man, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
LabDaten_YA_Procedure_STc_filtered_IQR <- LabDaten_YA_Procedure_STc_filtered[!(LabDaten_YA_Procedure_STc_filtered$rt_voc_or_man < (Q1 - 1.5 * IQR) | LabDaten_YA_Procedure_STc_filtered$rt_voc_or_man > (Q3 + 1.5 * IQR)), ]
#Mittelwerte nach subjectID berechnen
mittelwerte <- aggregate(rt_voc_or_man ~ subjectID, data = LabDaten_YA_Procedure_STc_filtered_IQR, FUN = mean)
#Spalten umbenennen
colnames(mittelwerte) <- c("VP", "Lab_STc")
# Kombiniere die Daten aus "mittelwerte" in "all_means" anhand der Spalte "VP"
all_means <- merge(all_means, mittelwerte, by = "VP", all.x = TRUE)


##STi
#Grenzwerte filtern
LabDaten_YA_Procedure_STi_filtered <- subset(LabDaten_YA_Procedure_STi, is.na(rt_man) | (rt_man >= 100 & rt_man <= 4000))
LabDaten_YA_Procedure_STi_filtered <- subset(LabDaten_YA_Procedure_STi, is.na(rt_voc) | (rt_voc >= 100 & rt_voc <= 4000))
#neue Spalte mit rt_voc und rt_man kombiniert für Interquartilsmethode
LabDaten_YA_Procedure_STi_filtered$rt_voc_or_man <- ifelse(!is.na(LabDaten_YA_Procedure_STi_filtered$rt_man) & !is.na(LabDaten_YA_Procedure_STi_filtered$rt_voc), NA,
                                                           ifelse(!is.na(LabDaten_YA_Procedure_STi_filtered$rt_man), LabDaten_YA_Procedure_STi_filtered$rt_man,
                                                                  ifelse(!is.na(LabDaten_YA_Procedure_STi_filtered$rt_voc), LabDaten_YA_Procedure_STi_filtered$rt_voc, NA)))
#Interquartilsmethode 
Q1 <- quantile(LabDaten_YA_Procedure_STi_filtered$rt_voc_or_man, 0.25, na.rm = TRUE)
Q3 <- quantile(LabDaten_YA_Procedure_STi_filtered$rt_voc_or_man, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
LabDaten_YA_Procedure_STi_filtered_IQR <- LabDaten_YA_Procedure_STi_filtered[!(LabDaten_YA_Procedure_STi_filtered$rt_voc_or_man < (Q1 - 1.5 * IQR) | LabDaten_YA_Procedure_STi_filtered$rt_voc_or_man > (Q3 + 1.5 * IQR)), ]
#Mittelwerte nach subjectID berechnen
mittelwerte <- aggregate(rt_voc_or_man ~ subjectID, data = LabDaten_YA_Procedure_STi_filtered_IQR, FUN = mean)
#Spalten umbenennen
colnames(mittelwerte) <- c("VP", "Lab_STi")
# Kombiniere die Daten aus "mittelwerte" in "all_means" anhand der Spalte "VP"
all_means <- merge(all_means, mittelwerte, by = "VP", all.x = TRUE)


##MCE berechnen und in neuer Spalte in all_means eintragen
all_means$Lab_ST_MCE <- all_means$Lab_STi - all_means$Lab_STc



###Task Switching

##TSc
#repetitions rausfiltern
LabDaten_YA_Procedure_TSc_filtered <- subset(LabDaten_YA_Procedure_TSc, trial_type != "rep")
#neue Spalte mit rt_voc und rt_man kombiniert für Interquartilsmethode
LabDaten_YA_Procedure_TSc_filtered$rt_voc_or_man <- ifelse(!is.na(LabDaten_YA_Procedure_TSc_filtered$rt_man) & !is.na(LabDaten_YA_Procedure_TSc_filtered$rt_voc), NA,
                                                           ifelse(!is.na(LabDaten_YA_Procedure_TSc_filtered$rt_man), LabDaten_YA_Procedure_TSc_filtered$rt_man,
                                                                  ifelse(!is.na(LabDaten_YA_Procedure_TSc_filtered$rt_voc), LabDaten_YA_Procedure_TSc_filtered$rt_voc, NA)))
#Grenzwerte filtern
LabDaten_YA_Procedure_TSc_filtered <- subset(LabDaten_YA_Procedure_TSc_filtered, is.na(rt_voc_or_man) | (rt_voc_or_man >= 100 & rt_voc_or_man <= 4000))
#Interquartilsmethode 
Q1 <- quantile(LabDaten_YA_Procedure_TSc_filtered$rt_voc_or_man, 0.25, na.rm = TRUE)
Q3 <- quantile(LabDaten_YA_Procedure_TSc_filtered$rt_voc_or_man, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
LabDaten_YA_Procedure_TSc_filtered_IQR <- LabDaten_YA_Procedure_TSc_filtered[!(LabDaten_YA_Procedure_TSc_filtered$rt_voc_or_man < (Q1 - 1.5 * IQR) | LabDaten_YA_Procedure_TSc_filtered$rt_voc_or_man > (Q3 + 1.5 * IQR)), ]
#Mittelwerte nach subjectID berechnen
mittelwerte <- aggregate(rt_voc_or_man ~ subjectID, data = LabDaten_YA_Procedure_TSc_filtered_IQR, FUN = mean)
#Spalten umbenennen
colnames(mittelwerte) <- c("VP", "Lab_TSc")
# Kombiniere die Daten aus "mittelwerte" in "all_means" anhand der Spalte "VP"
all_means <- merge(all_means, mittelwerte, by = "VP", all.x = TRUE)


##TSi
#repetitions rausfiltern
LabDaten_YA_Procedure_TSi_filtered <- subset(LabDaten_YA_Procedure_TSi, trial_type != "rep")
#neue Spalte mit rt_voc und rt_man kombiniert für Interquartilsmethode
LabDaten_YA_Procedure_TSi_filtered$rt_voc_or_man <- ifelse(!is.na(LabDaten_YA_Procedure_TSi_filtered$rt_man) & !is.na(LabDaten_YA_Procedure_TSi_filtered$rt_voc), NA,
                                                           ifelse(!is.na(LabDaten_YA_Procedure_TSi_filtered$rt_man), LabDaten_YA_Procedure_TSi_filtered$rt_man,
                                                                  ifelse(!is.na(LabDaten_YA_Procedure_TSi_filtered$rt_voc), LabDaten_YA_Procedure_TSi_filtered$rt_voc, NA)))
#Grenzwerte filtern
LabDaten_YA_Procedure_TSi_filtered <- subset(LabDaten_YA_Procedure_TSi_filtered, is.na(rt_voc_or_man) | (rt_voc_or_man >= 100 & rt_voc_or_man <= 4000))
#Interquartilsmethode 
Q1 <- quantile(LabDaten_YA_Procedure_TSi_filtered$rt_voc_or_man, 0.25, na.rm = TRUE)
Q3 <- quantile(LabDaten_YA_Procedure_TSi_filtered$rt_voc_or_man, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
LabDaten_YA_Procedure_TSi_filtered_IQR <- LabDaten_YA_Procedure_TSi_filtered[!(LabDaten_YA_Procedure_TSi_filtered$rt_voc_or_man < (Q1 - 1.5 * IQR) | LabDaten_YA_Procedure_TSi_filtered$rt_voc_or_man > (Q3 + 1.5 * IQR)), ]
#Mittelwerte nach subjectID berechnen
mittelwerte <- aggregate(rt_voc_or_man ~ subjectID, data = LabDaten_YA_Procedure_TSi_filtered_IQR, FUN = mean)
#Spalten umbenennen
colnames(mittelwerte) <- c("VP", "Lab_TSi")
# Kombiniere die Daten aus "mittelwerte" in "all_means" anhand der Spalte "VP"
all_means <- merge(all_means, mittelwerte, by = "VP", all.x = TRUE)


##MCE berechnen und in neuer Spalte in all_means eintragen
all_means$Lab_TS_MCE <- all_means$Lab_TSi - all_means$Lab_TSc




###Datenoutput

###all_means bearbeiten 
#Umbenennen
all_means_lab <- all_means
#Neue Spaltennamen definieren
new_colnames <- c(
  "VP",
  "DTc",
  "DTi",
  "DT_MCE",
  "STc",
  "STi",
  "ST_MCE",
  "TSc",
  "TSi",
  "TS_MCE"
)
#Spaltennamen in all_means_lab ändern
colnames(all_means_lab) <- new_colnames
#VP31 aufgrund fehlerhafter Daten entfernen
all_means_lab <- subset(all_means_lab, VP != 31)
#VPs ohne Daten löschen
all_means_lab <- all_means_lab[complete.cases(all_means_lab), ]



###Boxplots
#ggplot und tidyr laden
library(ggplot2)
library(tidyr)


##Datenfilter Showcase
#Boxplot für LabDaten_YA_Procedure_TSi_filtered erstellen (reps und Grenzwerte bereits gefiltert)
plot1 <- ggplot(data = LabDaten_YA_Procedure_TSi_filtered, aes(x = "", y = rt_voc_or_man)) +
  geom_boxplot() +
  geom_jitter(width = 0.25, height = 0, color = "blue", alpha = 0.4) + 
  labs(x = "vor IQR-Methode", y = "Reaktionszeit in ms") +
  ggtitle("Labor TSi Reaktionszeiten") +
  ylim(0, 4000)
#Boxplot für LabDaten_YA_Procedure_TSi_filtered_IQR erstellen (final gefiltert mit IQR Methode)
plot2 <- ggplot(data = LabDaten_YA_Procedure_TSi_filtered_IQR, aes(x = "", y = rt_voc_or_man)) +
  geom_boxplot() +
  geom_jitter(width = 0.25, height = 0, color = "blue", alpha = 0.4) + 
  labs(x = "nach IQR-Methode", y = "Reaktionszeit in ms") +
  ggtitle("Labor TSi Reaktionszeiten") +
  ylim(0, 4000)
#Die beiden Boxplots nebeneinander anordnen
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)


##Lab Boxplots

##Mittelwerte der VPs in den unterschiedlichen Aufgabentypen
#Daten in das long Format umwandeln
all_means_lab_long_mappings <- pivot_longer(all_means_lab, 
                                   cols = c("DTi", "DTc", "STi", "STc", "TSi", "TSc"), 
                                   names_to = "Gruppe", 
                                   values_to = "Wert")
#Boxplot mit allen Datenpunkten
ggplot(all_means_lab_long_mappings, aes(x = factor(Gruppe), y = Wert)) +
  geom_boxplot() +
  geom_jitter(data = all_means_lab_long_mappings, aes(x = factor(Gruppe), y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "Aufgabentyp", y = "Reaktionszeit in ms") +
  ggtitle("Mittelwerte der einzelnen VPs (Labor)")

##Lab MCE Boxplots
#Daten in das long Format umwandeln
all_means_lab_long_MCE <- pivot_longer(all_means_lab, 
                                   cols = c("DT_MCE", "ST_MCE", "TS_MCE"), 
                                   names_to = "Gruppe", 
                                   values_to = "Wert")
#Boxplot mit allen Datenpunkten
ggplot(all_means_lab_long_MCE, aes(x = factor(Gruppe), y = Wert)) +
  geom_boxplot() +
  geom_jitter(data = all_means_lab_long_MCE, aes(x = factor(Gruppe), y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "Aufgabentyp", y = "MCE Wert") +
  ggtitle("MCE Werte Labor")








#### Driving Simulator Daten auswerten ####


##leere Dataframes erstellen
all_means_DS <- data.frame(VP = 31:60)
all_means_DS_vocal <- data.frame(VP = 31:60)
all_means_DS_manual <- data.frame(VP = 31:60)




#### vocal DS Daten ####


###Dual-Task

###DTc
#einlesen
DS_Daten_DTc <- read.csv("reaction_times_DS_vocal_dtc.csv")
#Older adults rausfiltern
DS_Daten_YA_DTc <- DS_Daten_DTc[as.numeric(gsub("Sub", "", DS_Daten_DTc$Sub.ID)) < 61, ]
#Practice trials und instruction triggers rausfiltern 
DS_Daten_YA_Procedure_DTc <- DS_Daten_YA_DTc[, -c(2:21, 38:39, 55:56)]
#Subjects ohne Daten rausfiltern
DS_Daten_YA_Procedure_DTc_clean <- subset(DS_Daten_YA_Procedure_DTc, !apply(DS_Daten_YA_Procedure_DTc[, -1], 1, function(row) all(is.na(row))))
#Ein neues DataFrame erstellen
DS_Daten_YA_Procedure_DTc_clean_filtered <- DS_Daten_YA_Procedure_DTc_clean
#Spalten als numerisch definieren
DS_Daten_YA_Procedure_DTc_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_DTc_clean_filtered[, -1], as.numeric)
#Alle Werte unter 100 und über 4000 mit NA ersetzen
DS_Daten_YA_Procedure_DTc_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_DTc_clean_filtered[, -1], function(x) ifelse(x < 100 | x > 4000, NA, x))

##Interquartilsmethode
#Neues Dataframe für IQR Methode anlegen
DS_Daten_YA_Procedure_DTc_clean_filtered_IQR <- DS_Daten_YA_Procedure_DTc_clean_filtered
#Funktion zur Anwendung der Interquartilsmethode auf einen Vektor und Ersetzung der Ausreißer durch NA
apply_IQR <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[(x < lower_bound | x > upper_bound)] <- NA
  return(x)
}
#Anwendung der Funktion auf alle Spalten außer der ersten
DS_Daten_YA_Procedure_DTc_clean_filtered_IQR[, -1] <- lapply(DS_Daten_YA_Procedure_DTc_clean_filtered_IQR[, -1], apply_IQR)

##Mittelwerte berechnen und in all_means_DS_vocal eintragen anhand der VP
#Mittelwerte der einzelnen subject berechnen
mittelwerte <- data.frame(VP = DS_Daten_YA_Procedure_DTc_clean_filtered_IQR$Sub.ID, DTc_voc = rowMeans(DS_Daten_YA_Procedure_DTc_clean_filtered_IQR[, -1], na.rm = TRUE))
#"Sub" vor der Zahl in der Spalte "VP" entfernen
mittelwerte$VP <- gsub("Sub", "", mittelwerte$VP)
#DTc Mittelwerte in all_means_DS_vocal anhand der VP eintragen
all_means_DS_vocal <- merge(all_means_DS_vocal, mittelwerte, by = "VP", all.x = TRUE)



###DTi
#einlesen
DS_Daten_DTi <- read.csv("reaction_times_DS_vocal_dti.csv")
#Older adults rausfiltern
DS_Daten_YA_DTi <- DS_Daten_DTi[as.numeric(gsub("Sub", "", DS_Daten_DTi$Sub_ID)) < 61, ]
#Practice trials und instruction triggers rausfiltern 
DS_Daten_YA_Procedure_DTi <- DS_Daten_YA_DTi[, -c(2:21, 38:39, 55:56)]
#Subjects ohne Daten rausfiltern
DS_Daten_YA_Procedure_DTi_clean <- subset(DS_Daten_YA_Procedure_DTi, !apply(DS_Daten_YA_Procedure_DTi[, -1], 1, function(row) all(is.na(row))))
#Ein neues DataFrame erstellen
DS_Daten_YA_Procedure_DTi_clean_filtered <- DS_Daten_YA_Procedure_DTi_clean
#Spalten als numerisch definieren
DS_Daten_YA_Procedure_DTi_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_DTi_clean_filtered[, -1], as.numeric)
#Alle Werte unter 100 und über 4000 mit NA ersetzen
DS_Daten_YA_Procedure_DTi_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_DTi_clean_filtered[, -1], function(x) ifelse(x < 100 | x > 4000, NA, x))

##Interquartilsmethode
#Neues Dataframe für IQR Methode anlegen
DS_Daten_YA_Procedure_DTi_clean_filtered_IQR <- DS_Daten_YA_Procedure_DTi_clean_filtered
#Funktion zur Anwendung der Interquartilsmethode auf einen Vektor und Ersetzung der Ausreißer durch NA
apply_IQR <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[(x < lower_bound | x > upper_bound)] <- NA
  return(x)
}
#Anwendung der Funktion auf alle Spalten außer der ersten
DS_Daten_YA_Procedure_DTi_clean_filtered_IQR[, -1] <- lapply(DS_Daten_YA_Procedure_DTi_clean_filtered_IQR[, -1], apply_IQR)

##Mittelwerte berechnen und in all_means_DS_vocal eintragen anhand der VP
#Mittelwerte der einzelnen subject berechnen
mittelwerte <- data.frame(VP = DS_Daten_YA_Procedure_DTi_clean_filtered_IQR$Sub_ID, DTi_voc = rowMeans(DS_Daten_YA_Procedure_DTi_clean_filtered_IQR[, -1], na.rm = TRUE))
#"Sub" vor der Zahl in der Spalte "VP" entfernen
mittelwerte$VP <- gsub("Sub", "", mittelwerte$VP)
#DTi Mittelwerte in all_means_DS_vocal anhand der VP eintragen
all_means_DS_vocal <- merge(all_means_DS_vocal, mittelwerte, by = "VP", all.x = TRUE)



###Single Task

###STc
#einlesen
DS_Daten_STc <- read.csv("reaction_times_DS_vocal_stc.csv")
#Older adults rausfiltern
DS_Daten_YA_STc <- DS_Daten_STc[as.numeric(gsub("Sub", "", DS_Daten_STc$Sub_ID)) < 61, ]
#Practice trials und instruction triggers rausfiltern 
DS_Daten_YA_Procedure_STc <- DS_Daten_YA_STc[, -c(2:12, 29:38, 55)]
#Subjects ohne Daten rausfiltern
DS_Daten_YA_Procedure_STc_clean <- subset(DS_Daten_YA_Procedure_STc, !apply(DS_Daten_YA_Procedure_STc[, -1], 1, function(row) all(is.na(row))))
#Ein neues DataFrame erstellen
DS_Daten_YA_Procedure_STc_clean_filtered <- DS_Daten_YA_Procedure_STc_clean
#Spalten als numerisch definieren
DS_Daten_YA_Procedure_STc_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_STc_clean_filtered[, -1], as.numeric)
#Alle Werte unter 100 und über 4000 mit NA ersetzen
DS_Daten_YA_Procedure_STc_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_STc_clean_filtered[, -1], function(x) ifelse(x < 100 | x > 4000, NA, x))

##Interquartilsmethode
#Neues Dataframe für IQR Methode anlegen
DS_Daten_YA_Procedure_STc_clean_filtered_IQR <- DS_Daten_YA_Procedure_STc_clean_filtered
#Funktion zur Anwendung der Interquartilsmethode auf einen Vektor und Ersetzung der Ausreißer durch NA
apply_IQR <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[(x < lower_bound | x > upper_bound)] <- NA
  return(x)
}
#Anwendung der Funktion auf alle Spalten außer der ersten
DS_Daten_YA_Procedure_STc_clean_filtered_IQR[, -1] <- lapply(DS_Daten_YA_Procedure_STc_clean_filtered_IQR[, -1], apply_IQR)

##Mittelwerte berechnen und in all_means_DS_vocal eintragen anhand der VP
#Mittelwerte der einzelnen subject berechnen
mittelwerte <- data.frame(VP = DS_Daten_YA_Procedure_STc_clean_filtered_IQR$Sub_ID, STc_voc = rowMeans(DS_Daten_YA_Procedure_STc_clean_filtered_IQR[, -1], na.rm = TRUE))
#"Sub" vor der Zahl in der Spalte "VP" entfernen
mittelwerte$VP <- gsub("Sub", "", mittelwerte$VP)
#DTi Mittelwerte in all_means_DS_vocal anhand der VP eintragen
all_means_DS_vocal <- merge(all_means_DS_vocal, mittelwerte, by = "VP", all.x = TRUE)



###STi
#einlesen
DS_Daten_STi <- read.csv("reaction_times_DS_vocal_sti.csv")
#Older adults rausfiltern
DS_Daten_YA_STi <- DS_Daten_STi[as.numeric(gsub("Sub", "", DS_Daten_STi$Sub_ID)) < 61, ]
#Practice trials und instruction triggers rausfiltern 
DS_Daten_YA_Procedure_STi <- DS_Daten_YA_STi[, -c(2:12, 29:38, 55)]
#Subjects ohne Daten rausfiltern
DS_Daten_YA_Procedure_STi_clean <- subset(DS_Daten_YA_Procedure_STi, !apply(DS_Daten_YA_Procedure_STi[, -1], 1, function(row) all(is.na(row))))
#Ein neues DataFrame erstellen
DS_Daten_YA_Procedure_STi_clean_filtered <- DS_Daten_YA_Procedure_STi_clean
#Spalten als numerisch definieren
DS_Daten_YA_Procedure_STi_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_STi_clean_filtered[, -1], as.numeric)
#Alle Werte unter 100 und über 4000 mit NA ersetzen
DS_Daten_YA_Procedure_STi_clean_filtered[, -1] <- lapply(DS_Daten_YA_Procedure_STi_clean_filtered[, -1], function(x) ifelse(x < 100 | x > 4000, NA, x))

##Interquartilsmethode
#Neues Dataframe für IQR Methode anlegen
DS_Daten_YA_Procedure_STi_clean_filtered_IQR <- DS_Daten_YA_Procedure_STi_clean_filtered
#Funktion zur Anwendung der Interquartilsmethode auf einen Vektor und Ersetzung der Ausreißer durch NA
apply_IQR <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[(x < lower_bound | x > upper_bound)] <- NA
  return(x)
}
#Anwendung der Funktion auf alle Spalten außer der ersten
DS_Daten_YA_Procedure_STi_clean_filtered_IQR[, -1] <- lapply(DS_Daten_YA_Procedure_STi_clean_filtered_IQR[, -1], apply_IQR)

##Mittelwerte berechnen und in all_means_DS_vocal eintragen anhand der VP
#Mittelwerte der einzelnen subject berechnen
mittelwerte <- data.frame(VP = DS_Daten_YA_Procedure_STi_clean_filtered_IQR$Sub_ID, STi_voc = rowMeans(DS_Daten_YA_Procedure_STi_clean_filtered_IQR[, -1], na.rm = TRUE))
#"Sub" vor der Zahl in der Spalte "VP" entfernen
mittelwerte$VP <- gsub("Sub", "", mittelwerte$VP)
#DTi Mittelwerte in all_means_DS_vocal anhand der VP eintragen
all_means_DS_vocal <- merge(all_means_DS_vocal, mittelwerte, by = "VP", all.x = TRUE)




###Task Switching


###TSc
#einlesen
DS_Daten_TSc <- read.csv("reaction_times_DS_vocal_TSc.csv")
#Older adults rausfiltern
DS_Daten_YA_TSc <- DS_Daten_TSc[as.numeric(gsub("Sub", "", DS_Daten_TSc$Sub_ID)) < 61, ]
#Spalten mit procedure switch tasks mit AUDITIVEM trigger auswählen 
library(dplyr)
selected_columns <- c(1, 23, 25, 28, 31, 33, 35, 40, 43, 51, 55, 64, 70, 73, 78, 81, 86, 90)
#ausgewählte Spalten in neues Dataframe einfügen
DS_Daten_YA_TSc_switches <- DS_Daten_YA_TSc[, selected_columns]
#Subjects ohne Daten rausfiltern
DS_Daten_YA_TSc_switches_clean <- subset(DS_Daten_YA_TSc_switches, !apply(DS_Daten_YA_TSc_switches[, -1], 1, function(row) all(is.na(row))))
#Ein neues DataFrame erstellen
DS_Daten_YA_TSc_switches_clean_filtered <- DS_Daten_YA_TSc_switches_clean
#Spalten als numerisch definieren
DS_Daten_YA_TSc_switches_clean_filtered[, -1] <- lapply(DS_Daten_YA_TSc_switches_clean_filtered[, -1], as.numeric)
#Alle Werte unter 100 und über 4000 mit NA ersetzen
DS_Daten_YA_TSc_switches_clean_filtered[, -1] <- lapply(DS_Daten_YA_TSc_switches_clean_filtered[, -1], function(x) ifelse(x < 100 | x > 4000, NA, x))

##Interquartilsmethode
#Neues Dataframe für IQR Methode anlegen
DS_Daten_YA_TSc_switches_clean_filtered_IQR <- DS_Daten_YA_TSc_switches_clean_filtered
#Funktion zur Anwendung der Interquartilsmethode auf einen Vektor und Ersetzung der Ausreißer durch NA
apply_IQR <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[(x < lower_bound | x > upper_bound)] <- NA
  return(x)
}
#Anwendung der Funktion auf alle Spalten außer der ersten
DS_Daten_YA_TSc_switches_clean_filtered_IQR[, -1] <- lapply(DS_Daten_YA_TSc_switches_clean_filtered_IQR[, -1], apply_IQR)

##Mittelwerte berechnen und in all_means_DS_vocal eintragen anhand der VP
#Mittelwerte der einzelnen subject berechnen
mittelwerte <- data.frame(VP = DS_Daten_YA_TSc_switches_clean_filtered_IQR$Sub_ID, TSc_voc = rowMeans(DS_Daten_YA_TSc_switches_clean_filtered_IQR[, -1], na.rm = TRUE))
#"Sub" vor der Zahl in der Spalte "VP" entfernen
mittelwerte$VP <- gsub("Sub", "", mittelwerte$VP)
#DTi Mittelwerte in all_means_DS_vocal anhand der VP eintragen
all_means_DS_vocal <- merge(all_means_DS_vocal, mittelwerte, by = "VP", all.x = TRUE)



###TSi
#einlesen
DS_Daten_TSi <- read.csv("reaction_times_DS_vocal_TSi.csv")
#Older adults rausfiltern
DS_Daten_YA_TSi <- DS_Daten_TSi[as.numeric(gsub("Sub", "", DS_Daten_TSc$Sub_ID)) < 61, ]
#Spalten mit procedure switch tasks mit VISUELLEM trigger auswählen 
library(dplyr)
selected_columns <- c(1, 23, 27, 29, 33, 36, 46, 49, 53, 55, 60, 63, 66, 70, 75, 82, 86, 89)
#ausgewählte Spalten in neues Dataframe einfügen
DS_Daten_YA_TSi_switches <- DS_Daten_YA_TSi[, selected_columns]
#Subjects ohne Daten rausfiltern
DS_Daten_YA_TSi_switches_clean <- subset(DS_Daten_YA_TSi_switches, !apply(DS_Daten_YA_TSi_switches[, -1], 1, function(row) all(is.na(row))))
#Ein neues DataFrame erstellen
DS_Daten_YA_TSi_switches_clean_filtered <- DS_Daten_YA_TSi_switches_clean
#Spalten als numerisch definieren
DS_Daten_YA_TSi_switches_clean_filtered[, -1] <- lapply(DS_Daten_YA_TSi_switches_clean_filtered[, -1], as.numeric)
#Alle Werte unter 100 und über 4000 mit NA ersetzen
DS_Daten_YA_TSi_switches_clean_filtered[, -1] <- lapply(DS_Daten_YA_TSi_switches_clean_filtered[, -1], function(x) ifelse(x < 100 | x > 4000, NA, x))

##Interquartilsmethode
#Neues Dataframe für IQR Methode anlegen
DS_Daten_YA_TSi_switches_clean_filtered_IQR <- DS_Daten_YA_TSi_switches_clean_filtered
#Funktion zur Anwendung der Interquartilsmethode auf einen Vektor und Ersetzung der Ausreißer durch NA
apply_IQR <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[(x < lower_bound | x > upper_bound)] <- NA
  return(x)
}
#Anwendung der Funktion auf alle Spalten außer der ersten
DS_Daten_YA_TSi_switches_clean_filtered_IQR[, -1] <- lapply(DS_Daten_YA_TSi_switches_clean_filtered_IQR[, -1], apply_IQR)

##Mittelwerte berechnen und in all_means_DS_vocal eintragen anhand der VP
#Mittelwerte der einzelnen subject berechnen
mittelwerte <- data.frame(VP = DS_Daten_YA_TSi_switches_clean_filtered_IQR$Sub_ID, TSi_voc = rowMeans(DS_Daten_YA_TSi_switches_clean_filtered_IQR[, -1], na.rm = TRUE))
#"Sub" vor der Zahl in der Spalte "VP" entfernen
mittelwerte$VP <- gsub("Sub", "", mittelwerte$VP)
#DTi Mittelwerte in all_means_DS_vocal anhand der VP eintragen
all_means_DS_vocal <- merge(all_means_DS_vocal, mittelwerte, by = "VP", all.x = TRUE)








#### manual DS Daten ####



###DTc

#leere Liste erstellen
data_list <- list()
#Pfad zum Verzeichnis, in dem die .txt-Dateien liegen
directory_path <- "~/Desktop/Masterarbeit/R/Daten/DS_manual_DTc/"

#Iterieren über jede .txt-Datei im Verzeichnis
for (i in 31:56) {
  #Dateipfad zur aktuellen Datei
  file_path <- paste0(directory_path, "sub", i, ".txt")
  #Einlesen der Datei
  data <- read.table(file_path, header = FALSE, sep = "")
  #Benennen der Spalten von 1 bis zur Anzahl der Spalten
  colnames(data) <- 1:ncol(data)
  #Spalte 7 behalten (RTs)
  data <- data[, 7, drop = FALSE]
  # Spalte in Zeile umwandeln
  data_list[[i - 30]] <- as.vector(t(data))
}
#Dataframe aus allen so erstellten Zeilen
DS_Daten_YA_manual_DTc <- as.data.frame(do.call(rbind, data_list))

#Spaltennamen
colnames(DS_Daten_YA_manual_DTc) <- paste0("RT", 1:ncol(DS_Daten_YA_manual_DTc))
#Anzahl der Zeilen
library(dplyr)
num_rows <- nrow(DS_Daten_YA_manual_DTc)
#Neue Spalte "VP" mit den Werten von 1 bis 26 erstellen
DS_Daten_YA_manual_DTc <- DS_Daten_YA_manual_DTc %>%
  mutate(VP = 31:(31 + num_rows - 1)) %>%
  select(VP, everything())

#Practice trials filtern
DS_Daten_YA_manual_DTc_Procedure <- DS_Daten_YA_manual_DTc
DS_Daten_YA_manual_DTc_Procedure <- DS_Daten_YA_manual_DTc_Procedure[, -c(2:17)]

#Grenzwerte filtern
DS_Daten_YA_manual_DTc_Procedure_filtered <- DS_Daten_YA_manual_DTc_Procedure
DS_Daten_YA_manual_DTc_Procedure_filtered[, -1][DS_Daten_YA_manual_DTc_Procedure_filtered[, -1] < 0.1 | DS_Daten_YA_manual_DTc_Procedure_filtered[, -1] > 4] <- NA

##IQR
#neues Dataframe
DS_Daten_YA_manual_DTc_Procedure_filtered_IQR <- DS_Daten_YA_manual_DTc_Procedure_filtered
#Funktion zur Anwendung der IQR-Methode und Ersetzen der Werte mit "NA"
applyIQR <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- NA
  return(column)
}
#Anwenden der IQR-Methode auf alle Spalten außer der ersten (VP)
DS_Daten_YA_manual_DTc_Procedure_filtered_IQR[, -1] <- lapply(DS_Daten_YA_manual_DTc_Procedure_filtered_IQR[, -1], applyIQR)

##Mittelwerte
mittelwerte <- data.frame(VP = DS_Daten_YA_manual_DTc_Procedure_filtered_IQR$VP, DTc_man = rowMeans(DS_Daten_YA_manual_DTc_Procedure_filtered_IQR[, -1], na.rm = TRUE))
#DTc Mittelwerte in all_means_DS_manual anhand der VP eintragen
all_means_DS_manual <- merge(all_means_DS_manual, mittelwerte, by = "VP", all.x = TRUE)




###DTi

#leere Liste erstellen
data_list <- list()
#Pfad zum Verzeichnis, in dem die .txt-Dateien liegen
directory_path <- "~/Desktop/Masterarbeit/R/Daten/DS_manual_DTi/"

#Iterieren über jede .txt-Datei im Verzeichnis
for (i in 31:56) {
  #Dateipfad zur aktuellen Datei
  file_path <- paste0(directory_path, "sub", i, ".txt")
  #Einlesen der Datei
  data <- read.table(file_path, header = FALSE, sep = "")
  #Benennen der Spalten von 1 bis zur Anzahl der Spalten
  colnames(data) <- 1:ncol(data)
  #Spalte 7 behalten (RTs)
  data <- data[, 7, drop = FALSE]
  # Spalte in Zeile umwandeln
  data_list[[i - 30]] <- as.vector(t(data))
}
#Dataframe aus allen so erstellten Zeilen
DS_Daten_YA_manual_DTi <- as.data.frame(do.call(rbind, data_list))

#Spaltennamen
colnames(DS_Daten_YA_manual_DTi) <- paste0("RT", 1:ncol(DS_Daten_YA_manual_DTi))

#Anzahl der Zeilen
library(dplyr)
num_rows <- nrow(DS_Daten_YA_manual_DTi)
#Neue Spalte "VP" mit den Werten von 1 bis 26 erstellen
DS_Daten_YA_manual_DTi <- DS_Daten_YA_manual_DTi %>%
  mutate(VP = 31:(31 + num_rows - 1)) %>%
  select(VP, everything())

#Practice trials filtern
DS_Daten_YA_manual_DTi_Procedure <- DS_Daten_YA_manual_DTi
DS_Daten_YA_manual_DTi_Procedure <- DS_Daten_YA_manual_DTi_Procedure[, -c(2:17)]

#Grenzwerte filtern
DS_Daten_YA_manual_DTi_Procedure_filtered <- DS_Daten_YA_manual_DTi_Procedure
DS_Daten_YA_manual_DTi_Procedure_filtered[, -1][DS_Daten_YA_manual_DTi_Procedure_filtered[, -1] < 0.1 | DS_Daten_YA_manual_DTi_Procedure_filtered[, -1] > 4] <- NA

##IQR
#neues Dataframe
DS_Daten_YA_manual_DTi_Procedure_filtered_IQR <- DS_Daten_YA_manual_DTi_Procedure_filtered
#Funktion zur Anwendung der IQR-Methode und Ersetzen der Werte mit "NA"
applyIQR <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- NA
  return(column)
}
#Anwenden der IQR-Methode auf alle Spalten außer der ersten (VP)
DS_Daten_YA_manual_DTi_Procedure_filtered_IQR[, -1] <- lapply(DS_Daten_YA_manual_DTi_Procedure_filtered_IQR[, -1], applyIQR)

##Mittelwerte
mittelwerte <- data.frame(VP = DS_Daten_YA_manual_DTi_Procedure_filtered_IQR$VP, DTi_man = rowMeans(DS_Daten_YA_manual_DTi_Procedure_filtered_IQR[, -1], na.rm = TRUE))
#DTc Mittelwerte in all_means_DS_manual anhand der VP eintragen
all_means_DS_manual <- merge(all_means_DS_manual, mittelwerte, by = "VP", all.x = TRUE)





###STc

#leere Liste erstellen
data_list <- list()
#Pfad zum Verzeichnis, in dem die .txt-Dateien liegen
directory_path <- "~/Desktop/Masterarbeit/R/Daten/DS_manual_STc/"

#Iterieren über jede .txt-Datei im Verzeichnis
for (i in 31:56) {
  #Dateipfad zur aktuellen Datei
  file_path <- paste0(directory_path, "sub", i, ".txt")
  #Einlesen der Datei
  data <- read.table(file_path, header = FALSE, sep = "")
  #Benennen der Spalten von 1 bis zur Anzahl der Spalten
  colnames(data) <- 1:ncol(data)
  #Spalte 7 behalten (RTs)
  data <- data[, 7, drop = FALSE]
  # Spalte in Zeile umwandeln
  data_list[[i - 30]] <- as.vector(t(data))
}
#Dataframe aus allen so erstellten Zeilen
DS_Daten_YA_manual_STc <- as.data.frame(do.call(rbind, data_list))

#Spaltennamen
colnames(DS_Daten_YA_manual_STc) <- paste0("RT", 1:ncol(DS_Daten_YA_manual_STc))

#Anzahl der Zeilen
library(dplyr)
num_rows <- nrow(DS_Daten_YA_manual_DTi)
#Neue Spalte "VP" mit den Werten von 1 bis 26 erstellen
DS_Daten_YA_manual_STc <- DS_Daten_YA_manual_STc %>%
  mutate(VP = 31:(31 + num_rows - 1)) %>%
  select(VP, everything())

#Practice trials filtern
DS_Daten_YA_manual_STc_Procedure <- DS_Daten_YA_manual_STc
DS_Daten_YA_manual_STc_Procedure <- DS_Daten_YA_manual_STc_Procedure[, -c(2:9)]

#Grenzwerte filtern
DS_Daten_YA_manual_STc_Procedure_filtered <- DS_Daten_YA_manual_STc_Procedure
DS_Daten_YA_manual_STc_Procedure_filtered[, -1][DS_Daten_YA_manual_STc_Procedure_filtered[, -1] < 0.1 | DS_Daten_YA_manual_STc_Procedure_filtered[, -1] > 4] <- NA

##IQR
#neues Dataframe
DS_Daten_YA_manual_STc_Procedure_filtered_IQR <- DS_Daten_YA_manual_STc_Procedure_filtered
#Funktion zur Anwendung der IQR-Methode und Ersetzen der Werte mit "NA"
applyIQR <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- NA
  return(column)
}
#Anwenden der IQR-Methode auf alle Spalten außer der ersten (VP)
DS_Daten_YA_manual_STc_Procedure_filtered_IQR[, -1] <- lapply(DS_Daten_YA_manual_STc_Procedure_filtered_IQR[, -1], applyIQR)

##Mittelwerte
mittelwerte <- data.frame(VP = DS_Daten_YA_manual_STc_Procedure_filtered_IQR$VP, STc_man = rowMeans(DS_Daten_YA_manual_STc_Procedure_filtered_IQR[, -1], na.rm = TRUE))
#DTc Mittelwerte in all_means_DS_manual anhand der VP eintragen
all_means_DS_manual <- merge(all_means_DS_manual, mittelwerte, by = "VP", all.x = TRUE)




###STi

#leere Liste erstellen
data_list <- list()
#Pfad zum Verzeichnis, in dem die .txt-Dateien liegen
directory_path <- "~/Desktop/Masterarbeit/R/Daten/DS_manual_STi/"

#Iterieren über jede .txt-Datei im Verzeichnis
for (i in 31:56) {
  #Dateipfad zur aktuellen Datei
  file_path <- paste0(directory_path, "sub", i, ".txt")
  #Einlesen der Datei
  data <- read.table(file_path, header = FALSE, sep = "")
  #Benennen der Spalten von 1 bis zur Anzahl der Spalten
  colnames(data) <- 1:ncol(data)
  #Spalte 7 behalten (RTs)
  data <- data[, 7, drop = FALSE]
  # Spalte in Zeile umwandeln
  data_list[[i - 30]] <- as.vector(t(data))
}
#Dataframe aus allen so erstellten Zeilen
DS_Daten_YA_manual_STi <- as.data.frame(do.call(rbind, data_list))

#Spaltennamen
colnames(DS_Daten_YA_manual_STi) <- paste0("RT", 1:ncol(DS_Daten_YA_manual_STi))

#Anzahl der Zeilen
library(dplyr)
num_rows <- nrow(DS_Daten_YA_manual_DTi)
#Neue Spalte "VP" mit den Werten von 1 bis 26 erstellen
DS_Daten_YA_manual_STi <- DS_Daten_YA_manual_STi %>%
  mutate(VP = 31:(31 + num_rows - 1)) %>%
  select(VP, everything())

#Practice trials filtern
DS_Daten_YA_manual_STi_Procedure <- DS_Daten_YA_manual_STi
DS_Daten_YA_manual_STi_Procedure <- DS_Daten_YA_manual_STi_Procedure[, -c(2:9)]

#Grenzwerte filtern
DS_Daten_YA_manual_STi_Procedure_filtered <- DS_Daten_YA_manual_STi_Procedure
DS_Daten_YA_manual_STi_Procedure_filtered[, -1][DS_Daten_YA_manual_STi_Procedure_filtered[, -1] < 0.1 | DS_Daten_YA_manual_STi_Procedure_filtered[, -1] > 4] <- NA

##IQR
#neues Dataframe
DS_Daten_YA_manual_STi_Procedure_filtered_IQR <- DS_Daten_YA_manual_STi_Procedure_filtered
#Funktion zur Anwendung der IQR-Methode und Ersetzen der Werte mit "NA"
applyIQR <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- NA
  return(column)
}
#Anwenden der IQR-Methode auf alle Spalten außer der ersten (VP)
DS_Daten_YA_manual_STi_Procedure_filtered_IQR[, -1] <- lapply(DS_Daten_YA_manual_STi_Procedure_filtered_IQR[, -1], applyIQR)

##Mittelwerte
mittelwerte <- data.frame(VP = DS_Daten_YA_manual_STi_Procedure_filtered_IQR$VP, STi_man = rowMeans(DS_Daten_YA_manual_STi_Procedure_filtered_IQR[, -1], na.rm = TRUE))
#DTc Mittelwerte in all_means_DS_manual anhand der VP eintragen
all_means_DS_manual <- merge(all_means_DS_manual, mittelwerte, by = "VP", all.x = TRUE)





###TSc

#leere Liste erstellen
data_list <- list()
#Pfad zum Verzeichnis, in dem die .txt-Dateien liegen
directory_path <- "~/Desktop/Masterarbeit/R/Daten/DS_manual_TSc/"

#Iterieren über jede .txt-Datei im Verzeichnis
for (i in 31:56) {
  #Dateipfad zur aktuellen Datei
  file_path <- paste0(directory_path, "sub", i, ".txt")
  #Einlesen der Datei
  data <- read.table(file_path, header = FALSE, sep = "")
  #Benennen der Spalten von 1 bis zur Anzahl der Spalten
  colnames(data) <- 1:ncol(data)
  #Spalte 7 behalten (RTs)
  data <- data[, 7, drop = FALSE]
  # Spalte in Zeile umwandeln
  data_list[[i - 30]] <- as.vector(t(data))
}
#Dataframe aus allen so erstellten Zeilen
DS_Daten_YA_manual_TSc <- as.data.frame(do.call(rbind, data_list))

#Spaltennamen
colnames(DS_Daten_YA_manual_TSc) <- paste0("RT", 1:ncol(DS_Daten_YA_manual_TSc))

#Anzahl der Zeilen
library(dplyr)
num_rows <- nrow(DS_Daten_YA_manual_TSc)
#Neue Spalte "VP" mit den Werten von 1 bis 26 erstellen
DS_Daten_YA_manual_TSc <- DS_Daten_YA_manual_TSc %>%
  mutate(VP = 31:(31 + num_rows - 1)) %>%
  select(VP, everything())

#Practice trials filtern
DS_Daten_YA_manual_TSc_Procedure <- DS_Daten_YA_manual_TSc
DS_Daten_YA_manual_TSc_Procedure <- DS_Daten_YA_manual_TSc_Procedure[, -c(2:9)]

#Grenzwerte filtern
DS_Daten_YA_manual_TSc_Procedure_filtered <- DS_Daten_YA_manual_TSc_Procedure
DS_Daten_YA_manual_TSc_Procedure_filtered[, -1][DS_Daten_YA_manual_TSc_Procedure_filtered[, -1] < 0.1 | DS_Daten_YA_manual_TSc_Procedure_filtered[, -1] > 4] <- NA

##IQR
#neues Dataframe
DS_Daten_YA_manual_TSc_Procedure_filtered_IQR <- DS_Daten_YA_manual_TSc_Procedure_filtered
#Funktion zur Anwendung der IQR-Methode und Ersetzen der Werte mit "NA"
applyIQR <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- NA
  return(column)
}
#Anwenden der IQR-Methode auf alle Spalten außer der ersten (VP)
DS_Daten_YA_manual_TSc_Procedure_filtered_IQR[, -1] <- lapply(DS_Daten_YA_manual_TSc_Procedure_filtered_IQR[, -1], applyIQR)

##Mittelwerte
mittelwerte <- data.frame(VP = DS_Daten_YA_manual_TSc_Procedure_filtered_IQR$VP, TSc_man = rowMeans(DS_Daten_YA_manual_TSc_Procedure_filtered_IQR[, -1], na.rm = TRUE))
#DTc Mittelwerte in all_means_DS_manual anhand der VP eintragen
all_means_DS_manual <- merge(all_means_DS_manual, mittelwerte, by = "VP", all.x = TRUE)




###TSi

#leere Liste erstellen
data_list <- list()
#Pfad zum Verzeichnis, in dem die .txt-Dateien liegen
directory_path <- "~/Desktop/Masterarbeit/R/Daten/DS_manual_TSi/"

#Iterieren über jede .txt-Datei im Verzeichnis
for (i in 31:56) {
  #Dateipfad zur aktuellen Datei
  file_path <- paste0(directory_path, "sub", i, ".txt")
  #Einlesen der Datei
  data <- read.table(file_path, header = FALSE, sep = "")
  #Benennen der Spalten von 1 bis zur Anzahl der Spalten
  colnames(data) <- 1:ncol(data)
  #Spalte 7 behalten (RTs)
  data <- data[, 7, drop = FALSE]
  # Spalte in Zeile umwandeln
  data_list[[i - 30]] <- as.vector(t(data))
}
#Dataframe aus allen so erstellten Zeilen
DS_Daten_YA_manual_TSi <- as.data.frame(do.call(rbind, data_list))

#Spaltennamen
colnames(DS_Daten_YA_manual_TSi) <- paste0("RT", 1:ncol(DS_Daten_YA_manual_TSi))

#Anzahl der Zeilen
library(dplyr)
num_rows <- nrow(DS_Daten_YA_manual_TSi)
#Neue Spalte "VP" mit den Werten von 1 bis 26 erstellen
DS_Daten_YA_manual_TSi <- DS_Daten_YA_manual_TSi %>%
  mutate(VP = 31:(31 + num_rows - 1)) %>%
  select(VP, everything())

#Practice trials filtern
DS_Daten_YA_manual_TSi_Procedure <- DS_Daten_YA_manual_TSi
DS_Daten_YA_manual_TSi_Procedure <- DS_Daten_YA_manual_TSi_Procedure[, -c(2:9)]

#Grenzwerte filtern
DS_Daten_YA_manual_TSi_Procedure_filtered <- DS_Daten_YA_manual_TSi_Procedure
DS_Daten_YA_manual_TSi_Procedure_filtered[, -1][DS_Daten_YA_manual_TSi_Procedure_filtered[, -1] < 0.1 | DS_Daten_YA_manual_TSi_Procedure_filtered[, -1] > 4] <- NA

##IQR
#neues Dataframe
DS_Daten_YA_manual_TSi_Procedure_filtered_IQR <- DS_Daten_YA_manual_TSi_Procedure_filtered
#Funktion zur Anwendung der IQR-Methode und Ersetzen der Werte mit "NA"
applyIQR <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  column[column < lower_bound | column > upper_bound] <- NA
  return(column)
}
#Anwenden der IQR-Methode auf alle Spalten außer der ersten (VP)
DS_Daten_YA_manual_TSi_Procedure_filtered_IQR[, -1] <- lapply(DS_Daten_YA_manual_TSi_Procedure_filtered_IQR[, -1], applyIQR)

##Mittelwerte
mittelwerte <- data.frame(VP = DS_Daten_YA_manual_TSi_Procedure_filtered_IQR$VP, TSi_man = rowMeans(DS_Daten_YA_manual_TSi_Procedure_filtered_IQR[, -1], na.rm = TRUE))
#DTc Mittelwerte in all_means_DS_manual anhand der VP eintragen
all_means_DS_manual <- merge(all_means_DS_manual, mittelwerte, by = "VP", all.x = TRUE)





### Mittelwert aus vocal und manual bilden


#Multipliziere alle manual Werte mit 1000 für Angabe in ms
all_means_DS_manual[, -1] <- all_means_DS_manual[, -1] * 1000

#Spalten aus manual und vocal zusammenführen
all_means_DS <- cbind(all_means_DS, all_means_DS_manual[, -1])
all_means_DS <- cbind(all_means_DS, all_means_DS_vocal[, -1])

#Mittelwerte berechnen in neuen Spalten
all_means_DS$DTc_both <- ifelse(is.na(all_means_DS$DTc_man) | is.na(all_means_DS$DTc_voc), NA, 
                                (all_means_DS$DTc_man + all_means_DS$DTc_voc) / 2)
all_means_DS$DTi_both <- ifelse(is.na(all_means_DS$DTi_man) | is.na(all_means_DS$DTi_voc), NA, 
                                (all_means_DS$DTi_man + all_means_DS$DTi_voc) / 2)
all_means_DS$STc_both <- ifelse(is.na(all_means_DS$STc_man) | is.na(all_means_DS$STc_voc), NA, 
                                (all_means_DS$STc_man + all_means_DS$STc_voc) / 2)
all_means_DS$STi_both <- ifelse(is.na(all_means_DS$STi_man) | is.na(all_means_DS$STi_voc), NA, 
                                (all_means_DS$STi_man + all_means_DS$STi_voc) / 2)
all_means_DS$TSc_both <- ifelse(is.na(all_means_DS$TSc_man) | is.na(all_means_DS$TSc_voc), NA, 
                                (all_means_DS$TSc_man + all_means_DS$TSc_voc) / 2)
all_means_DS$TSi_both <- ifelse(is.na(all_means_DS$TSi_man) | is.na(all_means_DS$TSi_voc), NA, 
                                (all_means_DS$TSi_man + all_means_DS$TSi_voc) / 2)

#neues Dataframe
all_means_DS_combined <- all_means_DS

#Spalten 2-13 in all_means_DS löschen
all_means_DS <- all_means_DS[, -c(2:13)]

# "_both" aus den Spaltennamen entfernen
names(all_means_DS)[2:ncol(all_means_DS)] <- gsub("_both", "", names(all_means_DS)[2:ncol(all_means_DS)])


#MCE-Spalten erstellen und an passende Stelle einfügen
all_means_DS <- cbind(all_means_DS[, 1:3], DT_MCE = all_means_DS[, 3] - all_means_DS[, 2], all_means_DS[, 4:ncol(all_means_DS)])
all_means_DS <- cbind(all_means_DS[, 1:6], ST_MCE = all_means_DS[, 6] - all_means_DS[, 5], all_means_DS[, 7:ncol(all_means_DS)])
all_means_DS$TS_MCE <- all_means_DS$TSi - all_means_DS$TSc





####Boxplots für DS erstellen

##Einzelne Aufgabentypen
#Daten in das long Format umwandeln
all_means_DS_long_mappings <- pivot_longer(all_means_DS, 
                                           cols = c("DTc", "DTi", "STc", "STi", "TSc", "TSi"), 
                                           names_to = "Gruppe", 
                                           values_to = "Wert")
#Boxplot mit allen Datenpunkten
ggplot(all_means_DS_long_mappings, aes(x = factor(Gruppe), y = Wert)) +
  geom_boxplot() +
  geom_jitter(data = all_means_DS_long_mappings, aes(x = factor(Gruppe), y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "Aufgabentyp", y = "Reaktionszeit in ms") +
  ggtitle("Mittelwerte der einzelnen VPs (Fahrsimulator)")


##MCE Werte
#Daten in das long Format umwandeln
all_means_DS_long_MCE <- pivot_longer(all_means_DS, 
                                       cols = c("DT_MCE", "ST_MCE", "TS_MCE"), 
                                       names_to = "Gruppe", 
                                       values_to = "Wert")
#Boxplot mit allen Datenpunkten
ggplot(all_means_DS_long_MCE, aes(x = factor(Gruppe), y = Wert)) +
  geom_boxplot() +
  geom_jitter(data = all_means_DS_long_MCE, aes(x = factor(Gruppe), y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "Aufgabentyp", y = "MCE Wert") +
  ggtitle("MCE Werte Fahrsimulator")




###Neues Dataframe mit allen MCE Werten

#DS MCE Werte einfügen
all_means_MCE <- all_means_DS %>%
  select(VP, DT_MCE, ST_MCE, TS_MCE)
#Spalten umbenennen
all_means_MCE <- all_means_MCE %>%
  rename_with(~ paste0(., "_DS"), -VP)

#Lab MCE Werte einfügen
#Spalten aus all_means_lab auswählen
selected_columns_lab <- all_means_lab %>% 
  select(VP, DT_MCE, ST_MCE, TS_MCE)

#Den Spalten "_lab" als Suffix hinzufügen
selected_columns_lab <- selected_columns_lab %>% 
  rename_with(~ paste0(., "_lab"), -VP)

#Mit all_means_MCE verbinden
all_means_MCE <- all_means_MCE %>%
  left_join(selected_columns_lab, by = "VP")

##Dataframe mit allen MCEs bereinigt
all_means_MCE_clean <- na.omit(all_means_MCE)




###Boxplots

##all_means_MCE_clean
#Daten in das long Format umwandeln
all_means_MCE_clean_long <- pivot_longer(all_means_MCE_clean, 
                                      cols = c("DT_MCE_DS", "ST_MCE_DS", "TS_MCE_DS", "DT_MCE_lab", "ST_MCE_lab", "TS_MCE_lab"), 
                                      names_to = "Gruppe", 
                                      values_to = "Wert")
#Boxplot mit allen Datenpunkten
ggplot(all_means_MCE_clean_long, aes(x = factor(Gruppe), y = Wert)) +
  geom_boxplot() +
  geom_jitter(data = all_means_MCE_clean_long, aes(x = factor(Gruppe), y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "Aufgabentyp", y = "MCE Wert") +
  ggtitle("MCE Werte DS & Lab")

##Nach Umgebung
#Dataframe erstellen
all_means_MCE_clean_Umgebung <- all_means_MCE_clean
#Spaltenreihenfolge
column_order <- c("VP", "ST_MCE_lab", "TS_MCE_lab", "DT_MCE_lab", "ST_MCE_DS", "TS_MCE_DS", "DT_MCE_DS")
# Dataframe mit neuer Spaltenreihenfolge erstellen
all_means_MCE_clean_Umgebung <- all_means_MCE_clean_Umgebung %>%
  select(all_of(column_order))
##Boxplots
#Spaltenreihenfolge im Dataframe all_means_MCE_clean_Umgebung
desired_order <- c("VP", "ST_MCE_lab", "TS_MCE_lab", "DT_MCE_lab", "ST_MCE_DS", "TS_MCE_DS", "DT_MCE_DS")
#Daten in das long Format umwandeln
all_means_MCE_clean_long <- pivot_longer(all_means_MCE_clean_Umgebung, 
                                         cols = -VP,  # Behalte die VP-Spalte unverändert
                                         names_to = "Gruppe", 
                                         values_to = "Wert")
#Reihenfolge der Faktorstufen anhand von desired_order festlegen
all_means_MCE_clean_long$Gruppe <- factor(all_means_MCE_clean_long$Gruppe, levels = desired_order)
#Boxplot mit allen Datenpunkten
ggplot(all_means_MCE_clean_long, aes(x = Gruppe, y = Wert, fill = Gruppe)) +
  geom_boxplot() +
  geom_jitter(data = all_means_MCE_clean_long, aes(x = Gruppe, y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("green", "yellow", "orange", "green", "yellow", "orange")) +
  labs(x = "Aufgabentyp / Umgebung", y = "MCE-Wert") +
  ggtitle("MCE-Werte der Versuchspersonen") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("ST_MCE_lab" = "ST Lab", 
                              "TS_MCE_lab" = "TS Lab", 
                              "DT_MCE_lab" = "DT Lab", 
                              "ST_MCE_DS" = "ST DS", 
                              "TS_MCE_DS" = "TS DS", 
                              "DT_MCE_DS" = "DT DS"))

##Nach Aufgabentyp
#Dataframe erstellen
all_means_MCE_clean_Aufgabentyp <- all_means_MCE_clean
#Spaltenreihenfolge
column_order <- c("VP", "ST_MCE_lab", "ST_MCE_DS", "TS_MCE_lab", "TS_MCE_DS", "DT_MCE_lab", "DT_MCE_DS")
# Dataframe mit neuer Spaltenreihenfolge erstellen
all_means_MCE_clean_Aufgabentyp <- all_means_MCE_clean_Aufgabentyp %>%
  select(all_of(column_order))
##Boxplots
#Spaltenreihenfolge im Dataframe all_means_MCE_clean_Umgebung
desired_order <- c("VP", "ST_MCE_lab", "ST_MCE_DS", "TS_MCE_lab", "TS_MCE_DS", "DT_MCE_lab", "DT_MCE_DS")
#Daten in das long Format umwandeln
all_means_MCE_clean_long <- pivot_longer(all_means_MCE_clean_Aufgabentyp, 
                                         cols = -VP,  # Behalte die VP-Spalte unverändert
                                         names_to = "Gruppe", 
                                         values_to = "Wert")
#Reihenfolge der Faktorstufen anhand von desired_order festlegen
all_means_MCE_clean_long$Gruppe <- factor(all_means_MCE_clean_long$Gruppe, levels = desired_order)
#Boxplot mit allen Datenpunkten
ggplot(all_means_MCE_clean_long, aes(x = Gruppe, y = Wert, fill = Gruppe)) +
  geom_boxplot() +
  geom_jitter(data = all_means_MCE_clean_long, aes(x = Gruppe, y = Wert), width = 0.2, alpha = 0.5, color = "red") +
  scale_fill_manual(values = c("green", "green", "yellow", "yellow", "orange", "orange")) +
  labs(x = "Aufgabentyp", y = "MCE Wert") +
  ggtitle("MCE Werte nach Aufgabentyp") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("ST_MCE_lab" = "ST Labor",
                              "ST_MCE_DS" = "ST Fahrsimulator",
                              "TS_MCE_lab" = "TS Labor",
                              "TS_MCE_DS" = "TS Fahrsimulator",
                              "DT_MCE_lab" = "DT Labor",
                              "DT_MCE_DS" = "DT Fahrsimulator"))




#### ANOVA ####


#Neues Dataframe
vp_numbers <- c(40, 43, 45, 46, 47, 48, 55, 56)
ANOVA_MCEs <- data.frame(
  VP = rep(vp_numbers, times = 6),
  MCE = numeric(48),
  Umgebung = rep(c("Fahrsimulator", "Labor"), each = 24),
  Aufgabe = rep(rep(c("Dual-Task", "Single-Task", "Task-Switch"), each = 8), times = 2)
)
#MCE Spalte befüllen
ANOVA_MCEs$MCE <- unlist(all_means_MCE_clean[, 2:7])

#Faktorisierung der Spalte "Umgebung"
ANOVA_MCEs$Umgebung <- factor(ANOVA_MCEs$Umgebung, levels = c("Fahrsimulator", "Labor"))
#Faktorisierung der Spalte "Aufgabe"
ANOVA_MCEs$Aufgabe <- factor(ANOVA_MCEs$Aufgabe, levels = c("Single-Task", "Dual-Task", "Task-Switch"))



###Vorbedingungen für ANOVA prüfen


##1. Abhängigkeit der Messungen: 
#CHECK

##2. intervallskalierte abh. Variable: 
#CHECK

##3. Nominalskalierte Innersubjektfaktoren: 
#CHECK

##4. Normalverteilung abh. Variable innerhalb jeder Faktorenstufe
### -> Shapiro-Wilk Test
#Anzahl Spalten
num_cols <- ncol(all_means_MCE_clean)
#Leeres Ergebnis-Dataframe
shapiro_results <- data.frame(Variable = character(num_cols - 1), p_value = numeric(num_cols - 1))
#Schleife über alle Spalten (außer der ersten Spalte)
for (i in 2:num_cols) {
  column_name <- names(all_means_MCE_clean)[i]
  shapiro_result <- shapiro.test(all_means_MCE_clean[[column_name]])
  shapiro_results[i - 1, "Variable"] <- column_name
  shapiro_results[i - 1, "p_value"] <- shapiro_result$p.value
}
#CHECK

##5. keine Ausreißer in den Gruppen
# SUBJEKTIVER CHECK

##6. Spherizität
### -> Levene Test
#neues Dataframe
levene_data <- data.frame(Gruppe = character(0), MCE_Wert = numeric(0))
#Schleife
for (col_name in colnames(all_means_MCE_clean)[-1]) {
  #Extrahieren Sie die Werte aus der aktuellen Spalte
  values <- all_means_MCE_clean[, col_name]
  
  #Fügen Sie den Spaltennamen als 'Gruppe' und die Werte als 'MCE_Wert' in 'levene_data' ein
  levene_data <- rbind(levene_data, data.frame(Gruppe = col_name, MCE_Wert = values))
}
# Konvertieren Sie die "Gruppe"-Spalte in einen Faktor
levene_data$Gruppe <- as.factor(levene_data$Gruppe)
#car laden
library(car)
#Levene Test
levene_result <- leveneTest(MCE_Wert ~ Gruppe, data = levene_data)







#### ANOVA durchführen ####
#car laden
library(car)
#ANOVA-Modell
ANOVA_Modell <- aov(MCE ~ Umgebung * Aufgabe + Error(VP/(Umgebung*Aufgabe)), data=ANOVA_MCEs)








#### ANOVA mit Kontrollvariablen ####
#Neues Dataframe
ANOVA_MCEs_control <- ANOVA_MCEs
#Spalten für Kontrollvariablen erstellen.
ANOVA_MCEs_control$Alter <- c(27, 25, 26, 22, 24, 21, 26, 21)
ANOVA_MCEs_control$Geschlecht <- c("male", "female", "female", "male", "male", "female", "male", "female")
#Faktorisierung
ANOVA_MCEs_control$Geschlecht <- factor(ANOVA_MCEs_control$Geschlecht, levels = c("male", "female"))
ANOVA_MCEs_control$Umgebung <- factor(ANOVA_MCEs_control$Umgebung, levels = c("Fahrsimulator", "Labor"))
ANOVA_MCEs_control$Aufgabe <- factor(ANOVA_MCEs_control$Aufgabe, levels = c("Single-Task", "Dual-Task", "Task-Switch"))
##ANOVA
ANOVA_Modell_control <- aov(MCE ~ Umgebung * Aufgabe + Alter + Geschlecht + Error(VP/(Umgebung*Aufgabe)), data=ANOVA_MCEs_control)



#### Welch-ANOVA




#### korrigierte Boxplots nur mit verbleibenden VPS ####


#Lab korrigieren
#Die gewünschten VP-Nummern auswählen
selected_VP_numbers <- c(40, 43, 45, 46, 47, 48, 55, 56)
#Ein neues DataFrame erstellen, das nur die ausgewählten Zeilen enthält
all_means_lab_final <- all_means_lab[all_means_lab$VP %in% selected_VP_numbers, ]

#DS korrigieren
all_means_DS_final <- all_means_DS[c(10, 13, 15, 16, 17, 18, 25, 26), ]




###Lab Boxplot
#Daten ins Long-Format umwandeln
all_means_lab_final_long <- pivot_longer(all_means_lab_final, 
                                         cols = c("STc", "STi", "TSc", "TSi", "DTc", "DTi"), 
                                         names_to = "Gruppe", 
                                         values_to = "Wert")

#Reihenfolge der Faktorstufen festlegen
desired_order <- c("STc", "STi", "TSc", "TSi", "DTc", "DTi")
all_means_lab_final_long$Gruppe <- factor(all_means_lab_final_long$Gruppe, levels = desired_order)

#Boxplot mit allen Datenpunkten erstellen
library(ggplot2)

ggplot(all_means_lab_final_long, aes(x = Gruppe, y = Wert, fill = Gruppe)) +
  geom_boxplot() +
  geom_jitter(data = all_means_lab_final_long, aes(x = Gruppe, y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("green", "green", "yellow", "yellow", "orange", "orange")) +
  labs(x = "Aufgabentyp", y = "Reaktionszeit in ms") +
  ggtitle("Labor Reaktionszeiten (Mittelwerte der Versuchspersonen)") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(500, 2200))



###DS Boxplot
#Daten ins Long-Format umwandeln
all_means_DS_final_long <- pivot_longer(all_means_DS_final, 
                                         cols = c("STc", "STi", "TSc", "TSi", "DTc", "DTi"), 
                                         names_to = "Gruppe", 
                                         values_to = "Wert")

#Reihenfolge der Faktorstufen festlegen
desired_order <- c("STc", "STi", "TSc", "TSi", "DTc", "DTi")
all_means_DS_final_long$Gruppe <- factor(all_means_DS_final_long$Gruppe, levels = desired_order)

#Boxplot mit allen Datenpunkten erstellen
library(ggplot2)

ggplot(all_means_DS_final_long, aes(x = Gruppe, y = Wert, fill = Gruppe)) +
  geom_boxplot() +
  geom_jitter(data = all_means_DS_final_long, aes(x = Gruppe, y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("green", "green", "yellow", "yellow", "orange", "orange")) +
  labs(x = "Aufgabentyp", y = "Reaktionszeit in ms") +
  ggtitle("Fahrsimulator Reaktionszeiten (Mittelwerte der Versuchspersonen)") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(500, 2200))



###Beide Plots nebeneinander
#gridExtra laden
library(gridExtra)
#Beide Boxplots separat erstellen
plot_lab <- ggplot(all_means_lab_final_long, aes(x = Gruppe, y = Wert, fill = Gruppe)) +
  geom_boxplot() +
  geom_jitter(data = all_means_lab_final_long, aes(x = Gruppe, y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("green", "green", "yellow", "yellow", "orange", "orange")) +
  labs(x = "Aufgabentyp", y = "Reaktionszeit in ms") +
  ggtitle("Reaktionszeiten Labor") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(500, 2200))

plot_DS <- ggplot(all_means_DS_final_long, aes(x = Gruppe, y = Wert, fill = Gruppe)) +
  geom_boxplot() +
  geom_jitter(data = all_means_DS_final_long, aes(x = Gruppe, y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  scale_fill_manual(values = c("green", "green", "yellow", "yellow", "orange", "orange")) +
  labs(x = "Aufgabentyp", y = "Reaktionszeit in ms") +
  ggtitle("Reaktionszeiten Fahrsimulator") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(500, 2200))
#Kombinieren
grid.arrange(plot_lab, plot_DS, ncol = 2)











#### Alterseffekt ####
#Transponieren Sie das Dataframe, um Zeilen in Spalten umzuwandeln
transposed_df <- t(all_means_MCE_clean)
#Extrahieren Sie die erste Zeile für die Spaltennamen
colnames(transposed_df) <- as.character(transposed_df[1,])
#Entfernen Sie die erste Zeile, da sie jetzt die Spaltennamen ist
transposed_df <- transposed_df[-1,]
#Erstellen Sie ein neues Dataframe
Boxplot_MCE_Werte <- as.data.frame(transposed_df)
#Konvertieren Sie die VP-Spaltennamen in numerische Spalten
Boxplot_MCE_Werte <- Boxplot_MCE_Werte %>% mutate(across(starts_with("VP"), as.numeric))
#Altersvektor erstellen
alter <- c(27, 25, 26, 22, 24, 21, 26, 21)
# Setzen Sie die aktuellen Spaltennamen
current_colnames <- colnames(Boxplot_MCE_Werte)
# Die neuen Spaltennamen mit Alterswerten erstellen
new_colnames <- paste(current_colnames, " (", alter, ")", sep = "")
# Die Spaltennamen in Boxplot_MCE_Werte ändern
colnames(Boxplot_MCE_Werte) <- new_colnames
# Neue Reihenfolge der Spalten festlegen
neue_reihenfolge <- c("56 (21)", "48 (21)", "46 (22)", "47 (24)", "43 (25)", "55 (26)", "45 (26)", "40 (27)")
# Dataframe neu anordnen
Boxplot_MCE_Werte <- Boxplot_MCE_Werte[, neue_reihenfolge]


###BOXPLOT VPs nach Alter
# Boxplots für Boxplot_MCE_Werte erstellen
Boxplot_MCE_Werte_long <- pivot_longer(Boxplot_MCE_Werte, 
                                       cols = everything(), 
                                       names_to = "Spalte", 
                                       values_to = "Wert")

# Die Spalten im Dataframe als Faktoren in der gewünschten Reihenfolge umwandeln
desired_order <- c("56 (21)", "48 (21)", "46 (22)", "47 (24)", "43 (25)", "55 (26)", "45 (26)", "40 (27)")
Boxplot_MCE_Werte_long$Spalte <- factor(Boxplot_MCE_Werte_long$Spalte, levels = desired_order)

# Boxplot erstellen
ggplot(Boxplot_MCE_Werte_long, aes(x = Spalte, y = Wert, fill = Spalte)) +
  geom_boxplot() +
  geom_jitter(data = Boxplot_MCE_Werte_long, aes(x = Spalte, y = Wert), width = 0.2, alpha = 0.5, color = "blue") +
  labs(x = "VP (Alter)", y = "MCE Wert") +
  ggtitle("MCE Werte der VPs") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(-500, 1500))







#### Excel Dateien ####

##Paket installieren
if (!require("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
##Paket laden
library(openxlsx)
#Desktop Path
excel_path <- "~/Desktop/Masterarbeit/R/Excel_Output_R"


#Beispiel missing vocal Data
write.xlsx(DS_Daten_YA_TSc_switches, file = file.path(excel_path, "missing_vocal_data_TSc_DS_1.xlsx"))
write.xlsx(all_means_DS_vocal, file = file.path(excel_path, "missing_vocal_data_TSc_DS_2.xlsx"))
write.xlsx(all_means_DS, file = file.path(excel_path, "missing_vocal_data_TSc_DS_3.xlsx"))
write.xlsx(all_means_MCE, file = file.path(excel_path, "missing_vocal_data_TSc_DS_4.xlsx"))
write.xlsx(all_means_MCE_clean, file = file.path(excel_path, "missing_vocal_data_TSc_DS_5.xlsx"))

#Normalverteilung
write.xlsx(shapiro_results, file = file.path(excel_path, "Normalverteilung_Shapiro-Wilk.xlsx"))

#Varianzhomogenität
write.xlsx(levene_result, file = file.path(excel_path, "Varianzhomogenität_Levene.xlsx"))

#ANCOVA Dataframe
write.xlsx(ANOVA_MCEs_control, file = file.path(excel_path, "ANCOVA_Dataframe.xlsx"))







#### ANONVA Ergebnisse ####
summary(ANOVA_Modell)
summary(ANOVA_Modell_control)

#### ANCOVA in Excel ####
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
library(broom)
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
library(writexl)
# Extrahieren Sie die Ergebnisse und wandeln Sie sie in ein Dataframe um
ANCOVA_Ergebnisse <- tidy(ANOVA_Modell_control)
# Exportieren Sie das Dataframe in eine Excel-Datei
write.xlsx(ANCOVA_Ergebnisse, file = file.path(excel_path, "ANCOVA_Ergebnisse.xlsx"))






#### Fehlende Excel Tabellen & Boxplots erstellen ####

##RTs der 8 VPs in allen 12 Bedingungen
write.xlsx(all_means_DS_final, file = file.path(excel_path, "all_means_DS_final.xlsx"))
write.xlsx(all_means_lab_final, file = file.path(excel_path, "all_means_lab_final.xlsx"))


##RTs der 3 Aufgabentypen
#selbst erstellte Excel Datei einlesen
library(readxl)
dateipfad <- "~/Desktop/Masterarbeit/R/Excel_Output_R/RTs_der_3_Aufgabentypen.xlsx"
RTs_der_3_Aufgabentypen <- read_excel(dateipfad)
#in Excel berechnete MEAN usw. löschen
RTs_der_3_Aufgabentypen <- RTs_der_3_Aufgabentypen[-c(33:36), ]
#erste Spalte löschen
RTs_der_3_Aufgabentypen <- RTs_der_3_Aufgabentypen[, -1]

##Boxplot erstellen
long_df <- tidyr::gather(RTs_der_3_Aufgabentypen, key = "Aufgabentyp", value = "value")
# Definiere die Reihenfolge der Spalten
spalten_reihenfolge <- c("ST", "TS", "DT")
long_df$Aufgabentyp <- factor(long_df$Aufgabentyp, levels = spalten_reihenfolge)
# Definiere die Farben für die Boxplots
boxplot_farben <- c("green", "yellow", "orange")
# Erstelle das Boxplot mit ggplot2
ggplot(long_df, aes(x = Aufgabentyp, y = value, fill = Aufgabentyp)) +
  geom_boxplot() +
  geom_point(aes(color = "Datenpunkte"), position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  scale_fill_manual(values = boxplot_farben) +
  scale_color_manual(values = "blue") +
  labs(title = "Reaktionszeiten während der drei Multitaskingparadigmen", x = "Aufgabentyp", y = "Reaktionszeiten in ms") +
  theme_minimal()


##RTs der 2 Umgebungen
#selbst erstellte Excel Datei einlesen
library(readxl)
dateipfad <- "~/Desktop/Masterarbeit/R/Excel_Output_R/RTs_der_2_Umgebungen.xlsx"
RTs_der_2_Umgebungen <- read_excel(dateipfad)
#in Excel berechnete MEAN usw. löschen
RTs_der_2_Umgebungen <- RTs_der_2_Umgebungen[-c(49:52), ]
#erste Spalte löschen
RTs_der_2_Umgebungen <- RTs_der_2_Umgebungen[, -1]

##Boxplot erstellen
long_df <- tidyr::gather(RTs_der_2_Umgebungen, key = "Umgebung", value = "value")
# Definiere die Reihenfolge der Spalten
spalten_reihenfolge <- c("Labor", "Fahrsimulator")
long_df$Umgebung <- factor(long_df$Umgebung, levels = spalten_reihenfolge)
# Definiere die Farben für die Boxplots
boxplot_farben <- c("blue", "green")
# Erstelle das Boxplot mit ggplot2
ggplot(long_df, aes(x = Umgebung, y = value, fill = Umgebung)) +
  geom_boxplot() +
  geom_point(aes(color = "Datenpunkte"), position = position_jitter(width = 0.2), size = 2, alpha = 0.5) +
  scale_fill_manual(values = boxplot_farben) +
  scale_color_manual(values = "red") +
  labs(title = "Reaktionszeiten in den zwei Umgebungen", x = "Umgebung", y = "Reaktionszeiten in ms") +
  theme_minimal()
