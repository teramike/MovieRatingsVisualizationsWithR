library(ggplot2)
library(ggthemes)
library(data.table)
library(dplyr)
library(corrplot)

# Read movie data
movies <- fread('https://raw.githubusercontent.com/idc9/stor390/master/data/movies.csv')
head(movies)
colnames(movies)

unique(movies$genre)

# Plotting graphs

# Default graph
ggplot(data = movies) + 
  geom_point(mapping = aes(x=imdb_num_votes, y=imdb_rating, shape=genre, color=best_dir_win))+
  scale_x_continuous()+
  scale_y_continuous()+
  theme_light() +
  theme(legend.position = "right", legend.key = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12)) +
  labs(x = "Label x-axis", y = "Label y-axis", title="Title of your visualization", color="Label colors", shape = "Label shapes")
ggsave("assignment2-ggplot-movies.png", width = 8, height = 8)


# Plot 1

moviesQuantitative <- movies %>% mutate(any_actor_win = ifelse(best_actor_win=="no",0,1))%>% select(thtr_rel_year,thtr_rel_month,imdb_rating,critics_score)
M <- cor(moviesQuantitative)
corrplot(cor(moviesQuantitative),method="circle",order="AOE")

