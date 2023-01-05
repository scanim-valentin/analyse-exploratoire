# Diagramme 5 : Boîte à moustaches montrant la longueur moyenne en nombre de caractères des titres des vidéos en tendance

library(tidyverse)

dataframe1 = FR_youtube_trending_data %>%
  group_by(video_id) %>%
  arrange(-view_count)

# Elimination des doublons
dataframe2 = subset(dataframe1,!duplicated(dataframe1$video_id)) %>%
  select(view_count, publishedAt, video_id, title) %>%
  mutate(publishedAt = as.Date(publishedAt)) # Mise en forme de publishedAt en Date

dataframe2[which(dataframe2$publishedAt<"2023-01-01" & dataframe2$publishedAt>"2020-08-05"),] %>%
  group_by(publishedAt) %>%
  summarise(mean_title_length = mean(nchar(title))) %>%
  ggplot(aes(x = publishedAt, y = mean_title_length)) + # Calcul de la longueur moyenne des titres
    # geom_line() + # Décommenter pour avoir le graphique en ligne
    geom_boxplot() + # Commenter pour avoir le graphique en ligne
    # geom_smooth(method = "auto", se = TRUE, size = 2) + # Décommenter pour avoir le graphique en ligne
    scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_blank(), # element_text(angle = 40, vjust = 1, hjust=1, size=12, face="bold"), # Décommenter pour avoir le graphique en ligne
      axis.title.x = element_blank(), # element_text(size = 18, face="bold"), # Décommenter pour avoir le graphique en ligne
      axis.text.y = element_text(size=12, face="bold"),
      axis.title.y = element_blank(), # element_text(size = 18, face="bold"), # Décommenter pour avoir le graphique en ligne
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      title = element_text(size = 16, hjust = 0.5, face="bold")) +
  # Ajoût du nom des axes et des titres de la légende
  labs(title = str_wrap("Longueur moyenne des titres (en nombre de caractères)", width = 30))
  # Décommenter ci-dessous et commenter ci-dessus pour avoir le graphique en ligne
  # labs(x = "Date de publication", y= "Longueur moyenne des titres", title = "Longueur moyenne des titres (en nombre de caractères)")
