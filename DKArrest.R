
library("dkstat")
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(dkstat)
library(dplyr)

By_liste <- dst_meta(table = "BY2", lang = "da")
By_liste$variables

By_meta_filters <- list(
  KOMK = "*",
  BYST = "*",
  TID = "2023"
)

# Hent data fra POSTNR1-tabellen baseret på filteret
By_data <- dst_get_data(table = "BY2", query = By_meta_filters, lang = "da")


Straf_liste <- dst_meta(table = "STRAF44", lang = "da")
Straf_liste$variables


Straf_meta_filters <- list(
  OMRÅDE = "*",
  OVERTRÆD = c("Voldtægt mv.", "Manddrab", "Voldsforbrydelser i alt"),
  ALDER = "Alder i alt",
  KØN = "I alt",
  Tid= c("2023")
)

Straf_data <- dst_get_data(table = "STRAF44", query = Straf_meta_filters, lang = "da")

######################################################################################
colnames(By_data)[1] <- "OMRÅDE"
colnames(By_data)[3] <- "Befolkning"

# urban pop
urban_pop <- By_data %>%
  group_by(OMRÅDE) %>%
  summarize(
    total_befolkning = sum(value),
    landdistrikt_befolkning = value[BYST == "LAND Landdistrikter"],
    urban_pct = (landdistrikt_befolkning / total_befolkning) * 100
  )

By_data <- By_data[,-2]
By_data$urban_pop <- urban_pop$urban_pct

# samlet df
Straf44df <- pivot_wider(Straf_data, names_from = OVERTRÆD, values_from = value)
Straf44df <- Straf44df[,-c(2:4)]

DKArrest <- merge(Straf44df,By_data, by = "OMRÅDE")
