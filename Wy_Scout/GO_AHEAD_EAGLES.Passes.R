
######################
######STRUKTUR########
######################
#indlæs nødvendige pakker
#Opret forbindelse til MongoDB
  # 1) opstil en connection til databasen
#Hent Alle Kampe <> Hollandske liga
  # 1) Hent & filtrer rå kampdata fra "Matches" samlingen 
  # 2) Hent ydereligere data fra "games" samlingen baseret på ID (udtræk de unikke kampe for holdet)
  # 
#########################

library(mongolite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(corrplot)
library(plotly)
library(factoextra)


# Opret forbindelse til MongoDB
conm <- mongo(
  url = "mongodb://localhost",
  db = "fodbold",
  collection = "matches"
)

cong <- mongo(
  url = "mongodb://localhost",
  db = "fodbold",
  collection = "games",
)

# Hent kampe fra matches-samlingen
all_matches <- conm$find(query = "{}", 
                         fields = "{}")

# Filtrer til GoAheadEeagles-kampe
GAEmatches <- all_matches[grepl("Go Ahead Eagles", all_matches$label), ]

# Opdel Resultater i kolonner
GAEmatches$result   <- sub(".*?, ", "", GAEmatches$label)      # Resultatet (1-1, 3-3 osv.)
GAEmatches$home     <- sub(" -.*", "", GAEmatches$label)         # Hjemmeholdet (før " -")
GAEmatches$away     <- sub(",.*", "", sub(".*- ", "", GAEmatches$label))  # Udeholdet (efter " - " og før komma)
GAEmatches$homegoal <- as.numeric(sub("-.*", "", GAEmatches$result)) # Mål for hjemmeholdet
GAEmatches$awaygoal <- as.numeric(sub(".*-", "", GAEmatches$result)) # Mål for udeholdet

# Fjern den gamle label-kolonne
GAEmatches$label <- NULL

# Opret result_type med logik for hjemme- og udehold
GAEmatches$result_type <- ifelse(
  GAEmatches$homegoal == GAEmatches$awaygoal, "Total_draw", 
  ifelse(
    GAEmatches$home == "Ajax" & GAEmatches$homegoal > GAEmatches$awaygoal, "Total_Win",
    ifelse(GAEmatches$away == "Ajax" & GAEmatches$awaygoal > GAEmatches$homegoal, "Total_Win", "Total_loss")
  )
)

match_ids  <- GAEmatches[ , '_id']
selected_match_ids <- match_ids[1:49]
query <- jsonlite::toJSON(list(`_id` = list(`$in` = selected_match_ids)), auto_unbox = TRUE)
result <- cong$find(query=query, fields = '{}')

# Hent events
event_list <- result$events
event_df <- bind_rows(event_list)
flattened_events <- fromJSON(toJSON(event_df), flatten = TRUE)

# Filtrer kun events
GAE_Events <- flattened_events %>% filter(team.name == "Go Ahead Eagles")

# Pass-types dataframe
GAEEventsPasses=GAE_Events %>% filter(type.primary=="pass")

# Relevant passes
Relevant_passes <- GAEEventsPasses %>%
  select(1:30, -c(3:9)) %>%
  mutate(pass_category = sapply(type.secondary, `[`, 1))

# Optælling af forskellige afleveringstyper
Passcount=as.data.frame(table(Relevant_passes$pass_category)) 
filtered_pass_types <- Passcount %>% filter(Freq>410)

nyvektor=unique(as.character(filtered_pass_types$Var1))

# Antal af hver pasningstype per kamp
pass_type_stats <- Relevant_passes %>% 
  group_by(matchId, pass_category) %>% 
  select(matchId, pass_category) %>% 
  filter(pass_category %in% nyvektor) %>% 
  summarise(pass_count = n(), .groups = "drop") %>% 
  ungroup()

# Beregn statistik for hver kamp
match_stats <- Relevant_passes %>% 
  group_by(matchId) %>% 
  mutate(
    avg_pass_length = mean(pass.length),
    pass_length_sd = sd(pass.length),
    total_passes = n(),
    accuracy_ratio = round(sum(pass.accurate) / total_passes, 3)  # Rettet beregning
  ) %>% 
  select(accuracy_ratio, total_passes, matchId, avg_pass_length, pass_length_sd) %>% 
  unique() %>% 
  ungroup()

# Konverter statistik til long-format
match_stats_long <- match_stats %>% 
  pivot_longer(c(total_passes, avg_pass_length, pass_length_sd, accuracy_ratio), names_to = "stat_category", values_to = "stat_value")

# Kombinér data
combined_stats = bind_rows(pass_type_stats, match_stats_long)

wide_stats <- combined_stats %>%
  left_join(match_stats, by = "matchId")

wide_stats <- wide_stats[, -c(4,5)]

wide_stats$pass_category <- as.factor(wide_stats$pass_category)

numeric_cols <- wide_stats[, c("pass_count", "accuracy_ratio", "total_passes", "avg_pass_length", "pass_length_sd")]
scaled_numeric <- scale(numeric_cols)

# Kombinér de scalerede numeriske kolonner med de oprindelige ikke-numeriske kolonner
wide_stats_s <- data.frame(
  matchId = wide_stats$matchId,
  pass_category = wide_stats$pass_category,
  scaled_numeric
)

wide_stats_s <- wide_stats_s[, -c(1:2)]
wide_stats_s[is.na(wide_stats_s)] <- 0

kmeans_model <- kmeans(wide_stats_s, nstart = 10, centers = 3)
fviz_cluster(kmeans_model, data = wide_stats_s)

elbow_data <- data.frame(k = 1:20, twss = 0)

for (i in 1:20) {
  temp_model <- kmeans(wide_stats_s, centers = i, nstart = 10)
  elbow_data[i, 'twss'] <- temp_model$tot.withinss
}

plot(elbow_data, type = "b", main = "Elbow Plot for K-means", 
     xlab = "Antal Klynger", ylab = "Total Within Sum of Squares (TWSS)")

kmeans_model <- kmeans(wide_stats_s, nstart = 10, centers = 3)
fviz_cluster(kmeans_model, data = wide_stats_s)

pca_model <- princomp(wide_stats_s)
summary(pca_model)
pca_model$loadings[, 1:2]
fviz_pca_var(pca_model, col.var = "black")

distance_matrix <- dist(wide_stats_s)
hcl_model <- hclust(distance_matrix, method = "complete")
plot(hcl_model)
hcl_clusters <- cutree(hcl_model, k = 3)
rect.hclust(hcl_model, k = 3, border = 2:6)

###############################################################################

# 1 Tilføj clusters-variablen til datasættet
wide_stats$cluster <- kmeans_model$cluster


# 2. Tilføj matchinfo variabler -----------------------------------------------
# Forbered matchinfo data fra Ajaxmatches
match_info <- GAEmatches %>%
  select(`_id`, home, away, result, homegoal, awaygoal) %>% 
  rename("matchId" = `_id`)

# Kombiner med cluster data
combined_data <- wide_stats %>%
  left_join(match_info, by = "matchId") %>%
  mutate(
    match_label = paste0(home, "-", away, " ", result),
    match_short = paste0(home, "-", away)
  )

# 1. Udvælg og skaler de numeriske variable
numeric_data <- combined_data %>%
  select(matchId, total_passes, avg_pass_length, accuracy_ratio) %>%
  mutate(across(c(total_passes, avg_pass_length, accuracy_ratio), scale))

# 2. K-means clustering (sæt seed for reproducerbarhed)
set.seed(123)
kmeans_model <- kmeans(numeric_data %>% select(-matchId), centers = 3, nstart = 10)

# 3. Tilføj cluster-resultatet til datasættet
combined_data <- combined_data %>% mutate(cluster = kmeans_model$cluster)

# First, create a shorter hover text
combined_data <- combined_data %>%
  mutate(hover_text = paste(
    "ID:", matchId,
    "\nLength:", round(avg_pass_length, 1),
    "\nPasses:", total_passes,
    "\nAcc:", round(accuracy_ratio, 2),
    "\nRes:", match_label
  ))

# Then create the plot with the simplified hover text
plot_ly(
  data = combined_data,
  x = ~total_passes,
  y = ~avg_pass_length,
  z = ~accuracy_ratio,
  type = 'scatter3d',
  mode = 'markers',
  color = ~factor(cluster),
  colors = c("#440154FF", "#21908CFF", "#FDE725FF"),
  marker = list(size = 5),
  text = ~hover_text,
  hoverinfo = "text"
) %>%
  layout(
    title = "3D Interactive Cluster Plot for Ajax Pass Statistics",
    scene = list(
      xaxis = list(title = "Total Passes"),
      yaxis = list(title = "Pass Length"),
      zaxis = list(title = "Accuracy Ratio")
    )
  )


###############################
#### PASNINGSHØJDE ANALYSE ####
###############################

# Tæl forekomster af hver pasningshøjde
height_passes_count = table(GAEEventsPasses$pass.height)


# Beregn procentvis fordeling
height_percentage = round(height_passes_count / sum(height_passes_count) * 100, 2)

print(height_percentage)

###############################
#### PASNINGSPRÆCISION ANALYSE ####
###############################

# Antal afleveringer i dataset
total_passes = nrow(GAEEventsPasses)

# Antal succesfulde afleveringer
successful_passes = sum(GAEEventsPasses$pass.accurate, na.rm = TRUE)

# Beregn pasningspræcision i procent
pass_accuracy = round((successful_passes / total_passes) * 100, 2)

print(pass_accuracy)




# PBDA - Passes & duels
#Passes dividideret med defensive actions. 
# PBDA - 60 på den ene halvdel og 40% på den anden. 
# Defensive duels
# Interceptions
# Fouls
# Sliding tackles
# Der skal også tages højde for det andet hold (Ajax)

