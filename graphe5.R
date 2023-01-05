# Diagramme 5 : Un graphique en aires montre l'évolution dans le temps de la part de chaque catégorie du volume total de vues

library(tidyverse)

dataframe1 = FR_youtube_trending_data %>%
  group_by(video_id) %>%
  arrange(-view_count)

# Elimination des doublons
dataframe2 = subset(dataframe1,!duplicated(dataframe1$video_id)) %>%
  select(view_count, publishedAt, video_id, title) %>%
  mutate(publishedAt = as.Date(publishedAt)) # Mise en forme de publishedAt en Date

dataframe2[which(dataframe2$publishedAt<"2023-01-01"),] %>%
  mutate(month = as.Date(cut(publishedAt, "month"))) %>% # On calcule les valeurs sur trois mois au lieu de jour par jour pour plus de lisiblité
  group_by(month) %>%
  summarise(mean_title_length = mean(nchar(title))) %>%
  ggplot(aes(x = month, y = mean_title_length)) +
    geom_line(size = 1.5) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y")