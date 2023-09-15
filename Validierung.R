



########################### HINWEIS ###########################################

## Bevor der Code durchlaufen kann, ...

# 2 getrennte Ordner erstellen (einer mit BBD und einer mit den zu validierenden Daten)
# in den Ordnern muessen die Dateien richtig sortiert werden (Name aufsteigend),
# Sodass die Schleife auf die Dateien an gleicher Stelle in der Ordnerstruktur nacheinander zugreifen kann,


# install.packages("reshape2")
# install.packages("dplyr")


library(dplyr)
library(reshape2)
library(readxl)
library(writexl)
library(ggplot2)


# Pfade der Ordner definieren
csv_path <-
  "C:/Users/"
txt_path <-
  "C:/Users/"

# Speicherpfad als Variable
path_speichern <-
  "C:/Users/"


# Liste aller CSV- und TXT-Dateien in den Ordnern erstellen
csv_files <-
  list.files(csv_path, pattern = "*.csv", full.names = TRUE)
txt_files <-
  list.files(txt_path, pattern = "*.txt", full.names = TRUE)


# Sortieren der Dateien in aufsteigender Reihenfolge nach Namen
csv_files <- sort(csv_files, decreasing = FALSE)
txt_files <- sort(txt_files, decreasing = FALSE)



# Leere Datenrahmen fuer kumulative Ergebnisse erstellen
differenz_mean_df_cumulative <- data.frame()

# Define an empty data frame before the loop
summary_df_cumulative <- data.frame()


# Schleife um den ganzen Code
# Sodass alle Dateien der beiden Ordner entspechend ihrer Sortierung als Paare nacheinander durchlaufen
# Schleife endet, sobald alle Dateien der Ordner einmal dran waren
for (i in 1:length(csv_files)) {
  data_csv <-
    read.table(csv_files[i],
               sep = ",",
               header = TRUE,
               dec = ".")
  data_txt <-
    read.table(txt_files[i],
               sep = ";",
               header = TRUE,
               dec = ",")
  
  
  
  ### Tabellen fuer merge aufbereiten ###
  
  # erste Spalte von data_csv in "date" umbenennen
  colnames(data_csv)[1] <- "date"
  
  
  # Namensfilter fuer data_txt definieren (brauchen nur 2020 und ID)
  filters <-
    c(
      "date_2015",
      "date_2016",
      "date_2017",
      "date_2018",
      "date_2019",
      "date_2021",
      "ID",
      "Input",
      "X",
      "Y",
      "Z",
      "mean_velo_east",
      "var_mean_velo_east",
      "PS_ID",
      "var_mean_velo_vert",
      "mean_velo_vert"
    )
  # Filter anwenden
  df_filtered <- select (data_txt,-starts_with(filters))
  df_filtered
  
  
  # neues Dataframe erstellen (new_df), in das die Daten aus (df_filtered) uebertragen werden
  # Denn das Foramt muss zum mergen mit data_csv einheitlich sein
  # Als spalten soll date und alle IDs angelegt werden (jede ID eigene Spalte)
  # df_filtered befuellt das neue dataframe im gleichen Format wie data_csv
  
  # Spalten zahlen (in R beginnt die Zahlung bei 1)
  # Eine Spalte wird absichtlich nicht mitgezaehlt (date)
  num_rows <- ncol(df_filtered) - 1
  
  # Inhalt der "OBJECTID" aus df_filtered als Vektor abrufen
  object_ids <- df_filtered$OBJECTID
  
  # Neue Spaltennamen fuer new_df anlegen (date und Ids)
  new_df_col_names <- c("date", object_ids)
  
  # wir wollen zuerst nur das Datum aus df_filtered ohne die OBJECTID reinkopieren
  new_df <-
    data.frame(date = colnames(df_filtered)[colnames(df_filtered) != "OBJECTID"])
  
  # Die IDs werden als Spalten automatisch in new_df angelegt
  new_df <-
    data.frame(matrix(nrow = num_rows, ncol = length(new_df_col_names)))
  names(new_df) <- new_df_col_names
  
  # Spalte "date" mit den Daten aus df_filtered befuellen
  new_df$date <-
    colnames(df_filtered)[colnames(df_filtered) != "OBJECTID"]
  
  # Spalte "date" als Datum umformatieren
  new_df$date <- gsub("^date_", "", new_df$date)
  new_df$date <- format(as.Date(new_df$date, "%Y%m%d"), "%Y-%m-%d")
  
  
  # Schleife ueber die Zeilen von df_filtered und Uebertragung in die zugehoerige ID Spalte in new_df
  for (i in 1:nrow(df_filtered)) {
    row_i <- df_filtered[i, 2:ncol(df_filtered)]
    row_i_transposed <- t(row_i)
    new_df[, i + 1] <- row_i_transposed
  }
  
  # new_df ist vollstaendig befuellt mit den Daten aus df_filtered
  # vor dem mergen mit csv_data sollen aber die IDs so benannt werden,
  # dass klar ist, welche vom dataframe Data_val und welche vom BBD stammt
  
  # Umbenennung ab der 2. Spalte (1. Splate ist date) von data.csv
  i <- 2
  while (i <= ncol(data_csv)) {
    if (!startsWith(colnames(data_csv)[i], "Data_val_")) {
      # an die Spalte den Namen "Data_val_" anhaengen
      colnames(data_csv)[i] <-
        paste0("Data_val_", colnames(data_csv)[i])
      # das X aus dem Spaltennamen entfernen
      colnames(data_csv)[i] <- gsub("X", "", colnames(data_csv)[i])
    }
    i <- i + 1
  }
  
  
  # Umbenennung ab der 2. Spalte (1. Splate ist date) von new.df
  prefix <- "BBD_"
  i <- 2
  while (i <= ncol(new_df)) {
    if (!startsWith(colnames(new_df)[i], prefix)) {
      # an die Spalte den Namen " Data_val_" anhaengen
      colnames(new_df)[i] <- paste0(prefix, colnames(new_df)[i])
    }
    i <- i + 1
  }
  
  
  
  # Dataframes new_df und data_csv sind im einheitlichen Format
  # und es ist erkenntlich welche Daten aus welchem Datensatz urspruenglich stammen
  # Deshalb koennen sie ueber die identische Spalte date gemerged werden
  merge <- merge(new_df, data_csv, by = "date")
  
  
  # Data_val und BBD haben in merge immer eine gemeinsame ID
  # pro ID Paar soll die Differenz berechnet werden
  # mit abs() wird diese automatisch als positiver Wert ausgegeben
  # Pro ID wird eine neue Differenz Spalte mit dem Ergebnis angelegt
  # Zur Zuordnung wird automatisch die ID dahinter geschrieben: Differenz_ID
  num_cols <- ncol(merge)
  
  for (i in 2:num_cols) {
    col_name <- colnames(merge)[i]
    if (startsWith(col_name, "Data_val_")) {
      bbd_col <- gsub("Data_val_", "BBD_", col_name)
      diff_col <- gsub("Data_val_", "Differenz_", col_name)
      merge[[diff_col]] <- abs(merge[[bbd_col]] - merge[[col_name]])
    }
  }
  
  
  # Alle Spaltennamen in merge, die mit "Differenz_" beginnen sollen markiert werden
  differenz_spalten <-
    names(merge)[grep("^Differenz_", names(merge))]
  
  # mean fuer jede Spalte berechnen, die mit "Differenz_" beginnt
  differenz_mean <-
    colMeans(merge[, differenz_spalten, drop = FALSE], na.rm = TRUE)
  differenz_mean
  
  
  # Neues DataFrame erstellen
  differenz_mean_df <-
    data.frame(Spalte = sub("^Differenz_", "", names(differenz_mean)),
               Durchschnittswert = differenz_mean)
  
  
  
  # differenz_mean_df zu den kumulativen Ergebnissen hinzufuegen
  differenz_mean_df_cumulative <-
    rbind(differenz_mean_df_cumulative, differenz_mean_df)
  
  
  
  
  # lineares Modell
  # Die identischen IDs als Paare definieren
  
  # Zahlen, die nach "Lot_X_" uebereinstimmen
  Data_val_spalten <- names(merge)[grep("^Data_val_", names(merge))]
  Data_val_zahlen <-
    as.numeric(gsub("^Data_val_", "", Data_val_spalten))
  
  # Zahlen, die nach "BBD_" uebereinstimmen
  BBD_spalten <- names(merge)[grep("^BBD_", names(merge))]
  BBD_zahlen <- as.numeric(gsub("^BBD_", "", BBD_spalten))
  
  # IDs werden gesucht, die in beiden Vektoren uebereinstimmen
  uebereinstimmende_zahlen <-
    intersect(Data_val_zahlen, BBD_zahlen)
  
  
  # Leere Listen erstellen, um Zusammenfassungen zu speichern
  r_squared <- vector("numeric")
  adj_r_squared <- vector("numeric")
  f_value <- vector("numeric")
  p_value <- vector("numeric")
  residual_std_error <- vector("numeric")
  
  # Schleife durch jede Zusammenfassung in der Liste
  for (zahl in uebereinstimmende_zahlen) {
    # Lot_X und BBD Spaltennamen erstellen
    Data_val_spalte <- paste0("Data_val_", zahl)
    bbd_spalte <- paste0("BBD_", zahl)
    
    # Lineares Modell bilden
    lm_model <-
      lm(merge[, bbd_spalte] ~ merge[, Data_val_spalte], data = merge)
    
    # Zusammenfassung der statistischen Metriken des linearen Modells erhalten
    lm_summary <- summary(lm_model)
    
    # R-squared extrahieren
    r_squared <- c(r_squared, lm_summary$r.squared)
    
    # Adjusted R-squared extrahieren
    adj_r_squared <- c(adj_r_squared, lm_summary$adj.r.squared)
    
    # F-Statistik extrahieren
    f_value <- c(f_value, lm_summary$fstatistic[1])
    
    # p-Wert extrahieren
    p_value <- c(p_value, lm_summary$coefficients[2, "Pr(>|t|)"])
    
    # Residual standard error extrahieren
    residual_std_error <- c(residual_std_error, lm_summary$sigma)
  }
  
  # DataFrame aus den extrahierten Metriken erstellen
  summary_df <- data.frame(
    Zahl = uebereinstimmende_zahlen,
    r_squared = r_squared,
    Adj_R_squared = adj_r_squared,
    F_value = f_value,
    P_value = p_value,
    Residual_Std_Error = residual_std_error
  )
  
  
  summary_df_cumulative <- rbind(summary_df_cumulative, summary_df)
  
  
  
  
  # Ausgabe der Namen der aktuellen Dateien, die die Schleife durchlaufen
  cat(paste("CSV-Datei:", csv_files[i], "\n"))
  cat(paste("TXT-Datei:", txt_files[i], "\n"))
}



# differenz Plot erstellen mit allen Daten
differenz_plot_all <-
  ggplot(differenz_mean_df_cumulative, aes(x = Durchschnittswert)) +
  geom_histogram(binwidth = 0.05,
                 fill = "dark grey",
                 color = "black") +
  labs(x = "Differenz (mean)", y = "Count") +
  ggtitle("Gesamte Verteilung der Differenz") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# p_value Plot erstellen mit allen Daten
p_value_plot_all <-
  ggplot(summary_df_cumulative, aes(x = P_value)) +
  geom_histogram(binwidth = 0.05,
                 fill = "dark grey",
                 color = "black") +
  labs(x = "P-Value", y = "Count") +
  ggtitle("Gesamte Verteilung der P-Values") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.05))

# r2 Plot erstellen mit allen Daten
r2_plot_all <- ggplot(summary_df_cumulative, aes(x = r_squared)) +
  geom_histogram(binwidth = 0.05,
                 fill = "dark grey",
                 color = "black") +
  labs(x = "r2-Value", y = "Count") +
  ggtitle("Gesamte Verteilung des multiple r squared") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1))

# Dateinamen fuer die Plots
differenz_all_dateiname <- "differenz_plot_all.png"
p_value_all_dateiname <- "p_value_plot_all.png"
r2_all_dateiname <- "r2_plot_all.png"

# Plots speichern
ggsave(file = paste0(path_speichern, differenz_all_dateiname),
       plot = differenz_plot_all)
ggsave(file = paste0(path_speichern, p_value_all_dateiname),
       plot = p_value_plot_all)
ggsave(file = paste0(path_speichern, r2_all_dateiname),
       plot = r2_plot_all)



# Notiz
# manche IDs waren auch doppelt vergeben (value 1)
