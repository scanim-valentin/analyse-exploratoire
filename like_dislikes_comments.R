# Diagramme 1 : Un diagramme de dispersion qui montre la relation entre:
# - le nombre de vues (taille) 
# - le nombre de likes  (en x) / dislikes (en y) 
# - le nombre de commentaire (couleur, +vert=+de commentaires)
# Constat : 
# Les vidéos les plus populaires des tendances YouTube où les commentaires sont activés
# (sur la période avant la disparission des dislikes en novembre 2021) se démarquent de l'écrasante majorité 
# par un nombre de commentaires bien supérieur (plusieurs ordre de grandeurs)
# En moyenne, l'ordre de grande du nombre de likes semble toujours proportionnel à l'ordre de grande du nombre de dislikes
# On peut donc en déduire que la quantité de likes évolue de manière exponentionnelle par rapport à la quantité de dislikes
library(tidyverse)

# Elimination des vidéos sans dislikes : le dataset témoigne de la disparission de cette feature en Novembre 2021
# On a donc une grosse quantité de vidéos sans dislikes et ce n'est pas intéressant
# Elimination des vidéos sans commentaire : pour une raison inconnue, sur certaines vidéos,
# même si les commentaires sont indiqué comme activé dans le dataset, ils sont en réalité désactivés.
# Dans tous les cas, une vidéos où les commentaires sont activés présente dans les tendances aura naturellement plusieurs commentaires 
dataframe = FR_youtube_trending_data[(FR_youtube_trending_data$dislikes > 0) & (FR_youtube_trending_data$comment_count > 0),] %>% group_by(video_id) %>% arrange(-view_count)
dataframe2 = subset(dataframe,!duplicated(dataframe$video_id))

theme_set(theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1, size=12, face="bold"),
                axis.title.x = element_text(size = 18, face="bold"),
                axis.text.y = element_text(size=12, face="bold"),
                axis.title.y = element_text(size = 18, face="bold"),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14)))

p <- ggplot(dataframe2, aes(dislikes, likes, size = view_count))

# La forme creuse pour voir l'amat au centre
#p <- p + geom_point(aes(color=log(comment_count+1)),shape=5)
p <- p + geom_point(aes(color=log(comment_count+1)))

# L'échelle logarithmique permet de mettre en évidence l'étalement en terme d'ordre de grandeur
# En remttant l'échelle normale on constate cependant que les quelques vidéo les + regardés
# Ont bien
p <- p + scale_x_continuous(trans='log10') 
p <- p + scale_y_continuous(trans='log10') 

# Pas rouge / bleu pour ne pas confondre avec dislike / like
p <- p + scale_color_gradient(low = "black", high="lightgreen") 

p <- p + labs(size = str_wrap("Nombre de vues", width = 15), color = str_wrap("Nombre de commentaires (Log)", width = 15), x = "Likes", y = "Dislikes")

p


  


