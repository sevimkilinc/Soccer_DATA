# Load required libraries
library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(ggplot2)
library(factoextra)

#### Set the team you want to analyze ####
team_name <- "Go Ahead Eagles"  # Change this to any team, e.g., "Go Ahead Eagles"

#### Data Retrieval ####
# Connect to MongoDB
cong <- mongo(collection = "games", db = "soccer", url = "mongodb://localhost")
conm <- mongo(collection = "matches", db = "soccer", url = "mongodb://localhost")
conp <- mongo(collection = "players", db = "wyscout", url = "mongodb://localhost")

# Retrieve all matches
allmatches <- conm$find(query = '{}', fields = '{}')

# Filter matches for the selected team
allDutch <- allmatches %>% filter(competitionId == 635)
selectedTeamMatches <- allDutch %>% filter(str_detect(label, team_name))

# Extract match IDs
idv <- selectedTeamMatches[,'_id']

# Query MongoDB for game events
query <- jsonlite::toJSON(list(`_id` = list(`$in` = idv)), auto_unbox = TRUE)
result <- cong$find(query = query, fields = '{}')
testl <- result$events
testdf <- bind_rows(testl)

# Convert to dataframe and filter for the selected team
resdf <- fromJSON(toJSON(testdf), flatten = TRUE)
teamEvents <- resdf %>% filter(team.name == team_name)

# Filter only passes
teamPasses <- teamEvents %>% filter(type.primary == "pass")

# Select relevant columns
avsub2 <- c("matchId", "pass.length", "pass.accurate", "type.secondary")
teamPassesSub <- teamPasses[, avsub2]

# Add a new "pass category" variable
teamPassesSub$cat <- unlist(lapply(teamPassesSub$type.secondary, function(x) x[1]))

# Find relevant pass types
ddf <- as.data.frame(table(teamPassesSub$cat)) %>% filter(Freq > 410)
nv <- unique(as.character(ddf$Var1))

# Compute pass statistics per match
pstat <- teamPassesSub %>% 
  group_by(matchId, cat) %>% 
  filter(cat %in% nv) %>% 
  summarise(value = n(), .groups = "drop") %>% 
  ungroup()

# Compute overall match statistics
teamStats <- teamPassesSub %>% 
  group_by(matchId) %>% 
  mutate(
    passl = mean(pass.length),
    passvar = sd(pass.length),
    totpasses = n(),
    accratio = round((totpasses - sum(pass.accurate)) / totpasses, 3)
  ) %>% 
  select(accratio, totpasses, matchId, passl, passvar) %>% 
  unique() %>% 
  ungroup()

# Pivot statistics to wide format
teamStatsLong <- teamStats %>% pivot_longer(c(totpasses, passl, passvar, accratio), names_to = "cat", values_to = "value")
totstat <- bind_rows(pstat, teamStatsLong)
totstat_wide <- totstat %>% pivot_wider(names_from = "cat", values_from = "value")

# Scale data
totstat_wide_scaled <- as.data.frame(scale(totstat_wide))
totstat_wide_scaled <- totstat_wide_scaled[, -1]

#### Do the elbow test ####
##### Wulf's elbow method #####
dftwss=data.frame(k=1:20,twss=0)
for (i in (1:20)) {
  tmod=kmeans(totstat_wide_scaled,centers = i,nstart = 10)
  dftwss[i,'twss']=tmod$tot.withinss
}
plot(dftwss)


# Compute WCSS (total within-cluster sum of squares) for K = 1 to 20
dftwss <- data.frame(k = 1:20, twss = 0)

for (i in 1:20) {
  tmod <- kmeans(totstat_wide_scaled, centers = i, nstart = 10)
  dftwss[i, 'twss'] <- tmod$tot.withinss
}

# Plot the Elbow Method graph
ggplot(dftwss, aes(x = k, y = twss)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

#### kmeans cluster ####
kmod <- kmeans(totstat_wide_scaled, nstart = 10, centers = 3)
fviz_cluster(kmod, data = totstat_wide_scaled)
##### Lav også for 4 cluster, ift. elbow method #####

#### pca ####
data.pca <- princomp(totstat_wide_scaled)
summary(data.pca)
data.pca$loadings[, 1:3]
fviz_pca_var(data.pca, col.var = "black")

colnames(allmatches)[colnames(allmatches) == "_id"] <- "matchId"
totstat_wide_scaled$matchId <- totstat_wide$matchId
totstat_wide_scaled_3d <- merge(allmatches, totstat_wide_scaled, "matchId")
totstat_wide_scaled_3d$cluster <- as.factor(kmod$cluster)
totstat_wide_scaled <- totstat_wide_scaled[,-1]

##### Noget galt med away team, skal fixes #####

# Plotly 3D Cluster Plot
library(plotly)
##### Skal de-scales i plottet #####
totstat_wide_scaled_3d$totpasses_unscaled <- totstat_wide$totpasses
totstat_wide_scaled_3d$back_pass_unscaled <- totstat_wide$back_pass
totstat_wide_scaled_3d$lateral_pass_unscaled <- totstat_wide$lateral_pass

plot_ly(totstat_wide_scaled_3d, 
        x = ~totpasses, 
        y = ~back_pass, 
        z = ~lateral_pass, 
        color = ~cluster,
        colors = c("#ed6a5a", "#f4f1bb", "#9bc1bc"), 
        type = "scatter3d", 
        mode = "markers",
        ##### Få lavet X,Y,Z dyanmisk, så den automatisk tager top 3 #####
        text = ~paste("Match ID:", matchId, "<br>",
                      "Kampen Endte:", label, "<br>",
                      "Total Passes:", totpasses_unscaled, "<br>",
                      "Back Passes:", back_pass_unscaled, "<br>",
                      "Lateral Passes:", lateral_pass_unscaled),
        hoverinfo = "text") %>%
  layout(title = "3D Interactive Cluster Plot for Ajax Pass Statistics",
         scene = list(
           xaxis = list(title = "Pass Length"),
           yaxis = list(title = "Total Passes"),
           zaxis = list(title = "Accuracy Ratio")
         ))


#### hcl (Hierarchical Clustering) ####
totstat_wide_scaled <- totstat_wide_scaled[,-10]
distm <- dist(totstat_wide_scaled)
hi <- hclust(distm, method = "complete")
plot(hi)
cut_avg <- cutree(hi, k = 3)
rect.hclust(hi, k = 3, border = 2:6)