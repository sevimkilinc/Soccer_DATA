library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)

data_US <- USArrests

US_data_scaled <- as.data.frame(scale(data_US))

US_data_scaled_long <- US_data_scaled %>% 
  pivot_longer(cols = everything(),  
               names_to = "Arrest_categories", 
               values_to = "Arrest_values")

# Beregn optimal antal klynger vha. Elbow-metoden
total_within_ss <- data.frame(
  k = 1:10, 
  tot_withinss = numeric(10)  
)

for (i in 1:10) {
  tmpkres <- kmeans(US_data_scaled, centers = i, nstart = 10)
  total_within_ss$tot_withinss[i] <- tmpkres$tot.withinss  
}

# Elbow-metoden
ggplot(total_within_ss, aes(x = k, y = tot_withinss)) +
  geom_line(color = "blue") +
  geom_point(size = 3) +
  labs(title = "Elbow Plot",
       x = "Antal Klynger (k)",
       y = "Total Within Sum of Squares") +
  theme_minimal()

set.seed(123)
US_cluster <- kmeans(US_data_scaled, centers = 3, nstart = 10)

fviz_cluster(US_cluster, data = US_data_scaled)

# ##############################################
# Lav analyse på de tre klynger
# ##############################################

data_US$cluster <- as.factor(US_cluster$cluster)
data_US$state <- rownames(data_US)

# Gruppér efter cluster og beregn gns. af hver variabel
usArrAnalyse <- data_US %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) 

# Afrund decimaler
usArrAnalyse <- usArrAnalyse %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 1)))

# ##############################################
# PCA-Analyse
# ##############################################

pca.res <- princomp(US_data_scaled)  
pca.res$loadings
fviz_pca_var(pca.res, col.var = "black")

# ##############################################
# Barplot: Gennemsnit pr. Cluster for Arrest-kategorier
# ##############################################

# Omdan `usArrAnalyse` til long format til wulfs plot
usArrAnalyse_long <- pivot_longer(
  usArrAnalyse, 
  cols = -cluster, 
  names_to = "Variable", 
  values_to = "Mean"
)

# Visualiser gennemsnit pr. cluster
ggplot(usArrAnalyse_long, aes(x = Variable, y = Mean, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gennemsnit pr. Cluster for Arrest-kategorier",
       x = "Arrest kategori",
       y = "Gennemsnit",
       fill = "Cluster") +
  theme_minimal()


# ##############################################
# 3D-plot af PCA med hovering-info
# ##############################################

#Opret PCA-scores som en data frame
pca_scores <- as.data.frame(pca.res$x)
pca_scores$State <- rownames(data_US)
pca_scores$cluster <- factor(US_cluster$cluster)

# Opret 3D-plot med plotly
plot_ly(pca_scores, x = ~PC1, y = ~PC2, z = ~PC3,
        type = "scatter3d", mode = "markers",
        color = ~cluster, colors = c("red", "green", "blue"),
        text = ~paste("State:", State, "<br>PC1:", round(PC1, 2), 
                      "<br>PC2:", round(PC2, 2), "<br>PC3:", round(PC3, 2))) %>%
  layout(title = "3D PCA Cluster Plot",
         scene = list(
           xaxis = list(title = "PC1"),
           yaxis = list(title = "PC2"),
           zaxis = list(title = "PC3")
         ))

