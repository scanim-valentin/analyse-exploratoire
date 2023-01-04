# Comment évaluer la qualité des vidéos en se basant sur les commentaires et les likes/dislikes ?
library(tidyverse)

# Diagramme 1 : Un diagramme de dispersion qui montre la relation entre le nombre de vues et le nombre de likes/dislikes pour chaque vidéo.
dataframe = FR_youtube_trending_data[FR_youtube_trending_data$views>0,] %>% group_by(video_id) %>% arrange(-view_count)
subset(dataframe,!duplicated(dataframe$video_id)) %>% ggplot(aes(x=likes,y=view_count)) + geom_point(shape=4)

# Diagramme 2 : Un diagramme de dispersion qui montre la relation entre le nombre de vues et le nombre de commentaires pour chaque vidéo.
dataframe = FR_youtube_trending_data[FR_youtube_trending_data$comments_disabled=="False" && FR_youtube_trending_data$views > 0,] %>% group_by(video_id) %>% arrange(-view_count)
subset(dataframe,!duplicated(dataframe$video_id)) %>% ggplot(aes(x=comment_count,y=view_count)) + geom_point(shape=4)

# Diagramme 3 : 
dataframe = FR_youtube_trending_data[FR_youtube_trending_data$ratings_disabled=="False",] %>% group_by(video_id) %>% arrange(-view_count)
subset(dataframe,!duplicated(dataframe$video_id)) %>% ggplot(aes(x=likes,y=view_count)) + geom_point(shape=4)

# Diagramme 4 : 

# Diagramme 5 : 