library(tidyverse)

dataframe1 = FR_youtube_trending_data %>%
  group_by(video_id) %>%
  arrange(-view_count)

dataframe2 = subset(dataframe1,!duplicated(dataframe1$video_id)) %>%
  select(categoryId, view_count, publishedAt, video_id) %>%
  mutate(publishedAt = as.Date(publishedAt))

dataframe2[which(dataframe2$publishedAt<"2023-01-01"),] %>%
  mutate(quarter = as.Date(cut(publishedAt, "quarter"))) %>%
  group_by(quarter, categoryId) %>%
  summarise(sum_view_count = sum(view_count)) %>%
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
                                 "Nonprofits & Activism" = "29"
  )) %>%
  ggplot(aes(x = quarter, y = sum_view_count, fill = categoryId)) +
    geom_area(position = "fill", color = "black") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b-%Y") +
    theme(axis.text.x = element_text(angle = 40, vjust = 1, hjust=1, size=12, face="bold"),
          axis.title.x = element_text(size = 18, face="bold"),
          axis.text.y = element_text(size=12, face="bold"),
          axis.title.y = element_text(size = 18, face="bold"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    labs(x = "Date de publication", y = "Part du volume de vues total")