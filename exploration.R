# Comment évaluer la qualité des vidéos en se basant sur les commentaires et les likes/dislikes ?
library(tidyverse)
# Diagramme 1 : Un diagramme de dispersion qui montre la relation entre le nombre de vues et le nombre de likes/dislikes pour chaque vidéo.
FR_youtube_trending_data %>% arrange(view_count) %>% ggplot(aes(x=likes,y=view_count)) + geom_histogram(stat="identity")
# Diagramme 2 : 

# Diagramme 3 : 

# Diagramme 4 : 

# Diagramme 5 : 