library(mongolite)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(plotly)

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

all_matches <- conm$find(query = "{}", 
                         fields = "{}")

Go_ahead_eagles_matches <- all_matches %>% 
  filter(str_detect(tolower(label), fixed("Go Ahead Eagles", ignore_case = TRUE))) 

match_ids  <- Go_ahead_eagles_matches[ , '_id']
selected_match_ids <- match_ids[1:49]
query <- jsonlite::toJSON(list(`_id` = list(`$in` = selected_match_ids)), auto_unbox = TRUE)
result <- cong$find(query=query, fields = '{}')

# Hent events
event_list <- result$events
event_df <- bind_rows(event_list)
flattened_events <- fromJSON(toJSON(event_df), flatten = TRUE)

# Filtrer kun Ajax-events
GAE_Events <- flattened_events %>% filter(team.name == "Go Ahead Eagles")

GAE_passes <- GAE_Events %>% filter(type.primary == "pass")







################## Filtrer kun TYPE.primary & TYPE.secondary --> siger noget om passss og duels##############
GAE_PD <- GAE_Events %>%
  group_by(matchId) %>%
  filter(type.primary %in% c("pass", "duel", "interception") | 
           type.secondary %in% c("foul", "sliding_tacles")) %>%
  ungroup()

PPDA <- GAE_PD %>%
  # Antag at y-koordinaten går fra 0-100, så vi filtrerer for nederste 60%
  filter(location.x <= 60) %>%
  group_by(matchId) %>%
  summarise(
    total_passes = sum(type.primary == "pass"),
    
    defensive_actions = sum(
      type.primary == "duel" |
      type.primary == "interception" |
      type.secondary == "foul" |
      type.secondary == "sliding_tacles"
    ),
    
    PPDA = total_passes / defensive_actions
  ) %>%
  ungroup()









