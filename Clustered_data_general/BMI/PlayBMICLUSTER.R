#y = height
#x = age
#z = weight

require(plotly)
require(dplyr)
require(ggplot2)
require(caret)
require(ggVennDiagram)

BMI_cluster <- read.csv("Clustered_Data_3d.csv")
BMI_data <- read.csv("bmi.csv")

BMI_cluster <- BMI_data[, -c(4,5)]
unique(BMI_data$BmiClass)
colnames(BMI_data)
head(BMI_cluster)

# Omnavngiv kolonnerne til X, Y, Z
colnames(BMI_cluster) <- c("X", "Y", "Z")

# Antal klynger
k <- 6

# Tildel tilfældige klynger fra 1 til k
BMI_cluster$cluster <- sample(1:k, nrow(BMI_cluster), replace = TRUE)

# Initialiser parametre for iterationsprocessen
isChanged <- TRUE
roundcounter <- 0
doPlot <- FALSE

# K-means clustering
kmod <- kmeans(BMI_cluster[, c("X", "Y", "Z")], centers = 6)

# Vis det opdaterede datasæt
print(BMI_cluster)

require(plotly)
require(dplyr)
require(ggplot2)
require(caret)
require(ggVennDiagram)

# Indlæs datasæt
BMI_cluster <- read.csv("Clustered_Data_3d.csv")
BMI_data <- read.csv("bmi.csv")

# Fjern kolonner 4 og 5 fra BMI_data
BMI_cluster <- BMI_data[, -c(4,5)]

# Vis unikke værdier i BmiClass
unique(BMI_data$BmiClass)

# Vis kolonnenavne
colnames(BMI_data)

# Se de første rækker af det rensede datasæt
head(BMI_cluster)

# Omnavngiv kolonnerne til X, Y, Z
colnames(BMI_cluster) <- c("X", "Y", "Z")

# Antal klynger
k <- 6 

# Tildel tilfældige klynger fra 1 til k
BMI_cluster$cluster <- sample(1:k, nrow(BMI_cluster), replace = TRUE)

# Initialiser parametre for iterationsprocessen
isChanged <- TRUE
roundcounter <- 0
doPlot <- TRUE  # <- ÆNDR TIL TRUE SÅ PLOTTET VISES

# K-means clustering
kmod <- kmeans(BMI_cluster[, c("X", "Y", "Z")], centers = 6)

# Iterativ proces til opdatering af klynger
while (isChanged) {
  Sys.sleep(5.30)
  cat("Iteration:", roundcounter + 1, "\n")
  
  changeCounter <- 0
  roundcounter <- roundcounter + 1
  
  # 2. Beregn nye centroider
  centroids <- BMI_cluster %>%
    group_by(cluster) %>%
    summarise(X = mean(X), Y = mean(Y), Z = mean(Z)) %>%
    ungroup()
  
  # 3. Tildel hver observation til den nærmeste centroid
  changecounter <- 0
  
  for (i in 1:nrow(BMI_cluster)) {
    distances <- sapply(1:k, function(j) { 
      sqrt((centroids$X[j] - BMI_cluster$X[i])^2 + 
             (centroids$Y[j] - BMI_cluster$Y[i])^2 + 
             (centroids$Z[j] - BMI_cluster$Z[i])^2)
    })
    new_cluster <- which.min(distances)
    
    if (BMI_cluster[i, 'cluster'] != new_cluster) {
      BMI_cluster[i, 'cluster'] <- new_cluster
      changecounter <- changecounter + 1
    }
  }
  
  # 4. Plot det opdaterede cluster med Plotly
  p <- plot_ly() %>%
    add_trace(data = BMI_cluster, x = ~X, y = ~Y, z = ~Z,
              type = "scatter3d", mode = "markers",
              color = ~as.factor(cluster),
              marker = list(size = 5),
              text = ~paste("Age:", X, 
                            "Height:", Y, 
                            "Weight", Z, 
                            "Cluster:", cluster),  # Hovertekst
              hoverinfo = "text") %>%  # <- Korrekt placeret
    add_trace(data = centroids, x = ~X, y = ~Y, z = ~Z,
              type = "scatter3d", mode = "markers",
              marker = list(size = 10, symbol = "diamond", color = "black"),
              name = "Centroids") %>%
    layout(title = paste("Iteration:", roundcounter),
           scene = list(xaxis = list(title = "X"),
                        yaxis = list(title = "Y"),
                        zaxis = list(title = "Z")))
  
  print(p)
  

  
  isChanged <- ifelse(changecounter > 0, 1, 0)
  print(paste("SLEEP", isChanged))
}
