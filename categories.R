# Diagramme 2 : Un diagramme à barres avec des points qui montre la relation entre:
# - le nombre de vues (hauteur) 
# - le nombre de likes  (taille des points)
# - le nombre de commentaire (couleur des points, +bleu=+de commentaires)

library(tidyverse)

# Elimination des vidéos sans dislikes grâce au ">0" permet d'avoir les valeurs avant la suppression de l'affichage des dislikes (remplacer par "==0" pour avoir les valeurs suivantes)
# Elimination des vidéos sans commentaire
# Elimination des vidéos sans ratings
dataframe = FR_youtube_trending_data[which(FR_youtube_trending_data$dislikes<0 & FR_youtube_trending_data$ratings_disabled == "False" & FR_youtube_trending_data$comments_disabled == "False"),] %>% # [FR_youtube_trending_data$dislikes > 0,]
  group_by(video_id) %>%
  arrange(-view_count) 

# Elimination des doublons (doublons = vidéos apparues plusieurs fois dans les tendances)
subset(dataframe,!duplicated(dataframe$video_id)) %>%
  group_by(categoryId) %>%
  # Ajout des valeurs voulues 
  summarise(mean_views = mean(view_count),
            mean_likes = mean(likes),
            mean_comments = mean(comment_count)) %>%
  # Mise en place des correspondances entre categoryId et le nom des catégories
  mutate(categoryId = as.factor(categoryId)) %>%
  mutate(categoryId = fct_recode(categoryId,
                                 "Film & Animation" = "1",
                                 "Autos & Vehicles" = "2",
                                 "Music" = "10",
                                 "Pets & Animals" = "15",
                                 "Sports" = "17",
                                 "Travel & Events" = "19",
                                 "Gaming" = "20",
                                 "People & Blogs" = "22",
                                 "Comedy" = "23",
                                 "Entertainment" = "24",
                                 "News & Politics" = "25",
                                 "Howto & Style" = "26",
                                 "Education" = "27",
                                 "Science & Technology" = "28",
                                 "Nonprofits & Activism" = "29",
  )) %>%
  ggplot(aes(x = categoryId, y = mean_views)) +
  geom_bar(aes(fill = categoryId), stat = "identity") + # Diagramme à barres
  geom_point(aes(size = mean_likes, color = mean_comments)) + # Nuage de points
  scale_y_continuous() +
  scale_size(range = c(2,10)) + # Augmentation de la taille des points
  scale_color_continuous(trans = "log10") + # Pour plus de lisibilité, les couleurs des points sont calculés logarithmiquement
  guides(fill="none") +
  # Mise en place d'un thème permettant au texte d'être suffisament lisible
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1, size=12, face="bold"),
        axis.title.x = element_text(size = 18, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 16, hjust = 0.5, face="bold")) +
  # Ajoût du nom des axes et des titres de la légende
  labs(size = str_wrap("Nombre moyen de likes", width = 15), color = str_wrap("Nombre moyen de commentaires", width = 15), x = "Catégories", y = "Vues Moyennes", title = "Popularité moyenne de chaque catégories avant la fin des dislikes")