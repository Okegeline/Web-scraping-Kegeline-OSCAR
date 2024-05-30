# Mon premier travail sur web scraping
library(rvest)
library(dplyr)
library(ggplot2)

# Scrap la page web
url <- "https://www.skysports.com/world-cup-table"

# Lire le contenu de la page web
web_content <- read_html(url)

# Extraire tous les tableaux de la page web
table_elements <- web_content %>% html_nodes("table")

# Compter le nombre de tableaux
number_tables <- length(table_elements)
print(paste("Nombre de tableaux retrouvés dans la page web:", number_tables))

# Fonction pour lire la page web et compiler les tableaux en un seul data frame
read_and_combine_tables <- function(url) {
  web_content <- read_html(url)
  table_elements <- web_content %>% html_nodes("table")

  table_list <- lapply(table_elements, function(table) {
    table_df <- table %>% html_table(fill = TRUE)
    return(table_df)
  })

  combined_data <- bind_rows(table_list)
  return(combined_data)
}

# Utiliser la fonction pour compiler les tableaux
world_cup_combined_data <- read_and_combine_tables(url)

# Convertir la colonne des points en numérique
world_cup_combined_data$Pts <- as.numeric(world_cup_combined_data$Pts)

# Filtrer les 10 équipes ayant le plus de points
top_10_teams <- world_cup_combined_data %>%
  arrange(desc(Pts)) %>%
  head(10)

# Ajouter une colonne de rang pour les équipes
top_10_teams <- top_10_teams %>%
  mutate(Rank = row_number())

# Créer un graphique en bâton avec les 10 équipes ayant le nombre de points le plus élevé
ggplot(top_10_teams, aes(x = factor(Rank), y = Pts)) +
  geom_bar(stat = "identity", fill = "blue") +
  scale_x_discrete(labels = top_10_teams$Team) +
  labs(title = "Top 10 équipes ayant le nombre de points le plus élevé",
       x = "Team",
       y = "Points") +
  theme_minimal()
