library(tidyverse)

dataframe = FR_youtube_trending_data[which(FR_youtube_trending_data$dislikes>0 & FR_youtube_trending_data$ratings_disabled == "False" & FR_youtube_trending_data$comments_disabled == "False"),] %>% # [FR_youtube_trending_data$dislikes > 0,]
  group_by(video_id) %>%
  arrange(-view_count) 

subset(dataframe,!duplicated(dataframe$video_id)) %>%
  group_by(categoryId) %>%
  summarise(mean_views = mean(view_count),
            mean_likes = mean(likes),
            mean_comments = mean(comment_count)) %>%
  mutate(categoryId = as.factor(categoryId)) %>%
  mutate(categoryId = fct_recode(categoryId,
                                 "Film & Animation" = "1",
                                 "Autos & Vehicles" = "2",
                                 "Music" = "10",
                                 "Pets & Animals" = "15",
                                 "Sports" = "17",
                                 # "Short Movies" = "18",
                                 "Travel & Events" = "19",
                                 "Gaming" = "20",
                                 #"Videoblogging" = "21",
                                 "People & Blogs" = "22",
                                 "Comedy" = "23",
                                 "Entertainment" = "24",
                                 "News & Politics" = "25",
                                 "Howto & Style" = "26",
                                 "Education" = "27",
                                 "Science & Technology" = "28",
                                 "Nonprofits & Activism" = "29",
                                 #"Movies" = "30",
                                 #"Anime/Animation" = "31",
                                 #"Action/Adventure" = "32",
                                 #"Classics" = "33",
                                 #"Comedy" = "34",
                                 #"Documentary" = "35",
                                 #"Drama" = "36",
                                 #"Family" = "37",
                                 #"Foreign" = "38",
                                 #"Horror" = "39",
                                 #"Sci-Fi/Fantasy" = "40",
                                 #"Thriller" = "41",
                                 #"Shorts" = "42",
                                 #"Shows" = "43",
                                 #"Trailers" = "44"
  )) %>%
  ggplot(aes(x = categoryId, y = mean_views)) +
  geom_bar(aes(fill = categoryId), stat = "identity") +
  geom_point(aes(size = mean_likes, color = mean_comments)) +
  scale_y_continuous() +
  scale_size(range = c(2,10)) +
  scale_color_continuous(trans = "log10") +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1, size=12, face="bold"),
        axis.title.x = element_text(size = 18, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(size = 18, face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  labs(size = str_wrap("Nombre moyen de likes", width = 15), color = str_wrap("Nombre moyen de commentaires", width = 15), x = "Catégories", y = "Vues Moyennes")