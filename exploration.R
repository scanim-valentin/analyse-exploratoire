# Comment évaluer la qualité des vidéos en se basant sur les commentaires et les likes/dislikes ?
library(tidyverse)
# && FR_youtube_trending_data$dislikes > 0 && FR_youtube_trending_data$likes > 0
# Diagramme 1 : Un diagramme de dispersion qui montre la relation entre le nombre de vues et le nombre de likes/dislikes pour chaque vidéo.
dataframe = FR_youtube_trending_data[FR_youtube_trending_data$dislikes > 0 ,] %>% group_by(video_id) %>% arrange(-view_count)
dataframe2 = subset(dataframe,!duplicated(dataframe$video_id))
p <- ggplot(dataframe2, aes(likes, dislikes, size = exp(view_count))) 
p + geom_point() + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10')

