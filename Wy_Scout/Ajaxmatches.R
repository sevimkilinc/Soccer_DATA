library(mongolite)
library(stringr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(plotly)
library(factoextra)
library(ggplot2)

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

#hent alle kampe
all_matches <- conm$find(query = "{}", 
         fields = "{}")

ajax_matches <- all_matches %>% 
  filter(str_detect(tolower(label), fixed("ajax", ignore_case = TRUE))) 

#Vi udtrækker data til at lave kolonnenavne 
ajax_matches <- ajax_matches %>%
  mutate(
    resultat = str_extract(label, "\\d+-\\d+"),
    home = str_extract(label, "^[^,]+") %>% str_extract("^[^-]+") %>% str_trim(),
    away = str_extract(label, "^[^,]+") %>% str_extract("(?<=- ).*") %>% str_trim(),
    homegoal = as.integer(str_extract(resultat, "^\\d+")),
    awaygoal = as.integer(str_extract(resultat, "(?<=-)\\d+"))
  )

# Beregn antal kampe med forskellige resultater for Ajax
home_wins <- sum(ajax_matches$home == "Ajax" & ajax_matches$homegoal > ajax_matches$awaygoal, na.rm = TRUE)
away_wins <- sum(ajax_matches$away == "Ajax" & ajax_matches$awaygoal > ajax_matches$homegoal, na.rm = TRUE)

home_draws <- sum(ajax_matches$home == "Ajax" & ajax_matches$homegoal == ajax_matches$awaygoal, na.rm = TRUE)
away_draws <- sum(ajax_matches$away == "Ajax" & ajax_matches$awaygoal == ajax_matches$homegoal, na.rm = TRUE)

home_losses <- sum(ajax_matches$home == "Ajax" & ajax_matches$homegoal < ajax_matches$awaygoal, na.rm = TRUE)
away_losses <- sum(ajax_matches$away == "Ajax" & ajax_matches$awaygoal < ajax_matches$homegoal, na.rm = TRUE)

# Opret dataframe til ggplot2
Kamp_resultat<- data.frame(
  Resultat = rep(c("Vundet", "Uafgjort", "Tabt"), each = 2),
  Type = rep(c("Hjemme", "Ude"), 3),
  Antal = c(home_wins, away_wins, home_draws, away_draws, home_losses, away_losses)
)

ggplot(Kamp_resultat, aes(x = Resultat, y = Antal, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  labs(title = "Ajax's kampresultater på hjemme- og udebane",
       x = "Kampresultat",
       y = "Antal kampe") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  scale_fill_manual(values = c("dodgerblue", "firebrick")) + # Hjemme (blå) & Ude (rød)
  geom_text(aes(label = Antal), vjust = -0.5, size = 6, fontface = "bold", position = position_dodge(0.9))




#########################################
#### ---- Forespørgsler MongoDB ---- ####
#########################################
#########################################
#############    DEL 2    ############### 
#########################################
###############################
#### HENT AJAX-KAMPE OG EVENTS ####
###############################


# Vælg de første 49 Ajax-kampe ud fra deres _id
match_ids  <- ajax_matches[ , '_id']
selected_match_ids <- match_ids[1:49]

query <- jsonlite::toJSON(list(`_id` = list(`$in` = selected_match_ids)), auto_unbox = TRUE)
result <- cong$find(query=query, fields = '{}')

# Hent events
event_list <- result$events
event_df <- bind_rows(event_list)
flattened_events <- fromJSON(toJSON(event_df), flatten = TRUE)

# Filtrer kun Ajax-events
ajax_events <- flattened_events %>% filter(team.name == "Ajax")

# Filtrer kun afleveringer
ajax_passes <- ajax_events %>% filter(type.primary == "pass")

# Fjern unødvendige kolonner
column_names <- colnames(ajax_passes)
selected_columns <- column_names[1:30]
filtered_columns <- selected_columns[-c(3:9)]
ajax_passes_filtered <- ajax_passes[, filtered_columns]

# Opret kategorier for pasninger
ajax_passes_filtered$pass_category <- unlist(lapply(ajax_passes_filtered$type.secondary, function(x) x[1]))

# Find relevante pasningstyper
pass_type_count <- as.data.frame(table(ajax_passes_filtered$pass_category))
filtered_pass_types <- pass_type_count %>% filter(Freq > 410)

# Antal af hver pasningstype per kamp
pass_type_stats <- ajax_passes_filtered %>% 
  group_by(matchId, pass_category) %>% 
  select(matchId, pass_category) %>% 
  filter(pass_category %in% filtered_pass_types$Var1) %>% 
  summarise(pass_count = n(), .groups = "drop") %>% 
  ungroup()

# Indsaml statistik for hver kamp
match_stats <- ajax_passes_filtered %>% 
  group_by(matchId) %>% 
  mutate(
    avg_pass_length = mean(pass.length),
    pass_length_sd = sd(pass.length),
    total_passes = n(),
    accuracy_ratio = round((total_passes - sum(pass.accurate)) / total_passes, 3)
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

kmeans_model <- kmeans(wide_stats_s, nstart = 10, centers = 4)
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


# # ## # # # # # # # # # # # # #
### SKAL FIKSES

# 2. Tilføj matchinfo variabler -----------------------------------------------
# Forbered matchinfo data fra Ajaxmatches
match_info <- ajax_matches %>%
  select(`_id`, home, away, resultat, homegoal, awaygoal) %>% 
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
    "\nLen:", round(avg_pass_length, 1),
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
height_passes_count = table(ajaxEventsPasses$pass.height)

# Beregn procentvis fordeling
height_percentage = round(height_passes_count / sum(height_passes_count) * 100, 2)

print(height_percentage)

###############################
#### PASNINGSPRÆCISION ANALYSE ####
###############################

# Antal afleveringer i dataset
total_passes = nrow(ajaxEventsPasses)

# Antal succesfulde afleveringer
successful_passes = sum(ajaxEventsPasses$pass.accurate, na.rm = TRUE)

# Beregn pasningspræcision i procent
pass_accuracy = round((successful_passes / total_passes) * 100, 2)

print(pass_accuracy)
