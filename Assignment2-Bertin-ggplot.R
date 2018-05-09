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

# moviesQuantitative <- movies %>% mutate(best_actor_win = ifelse((best_actor_win=="no"),0,1),best_actress_win = ifelse((best_actress_win=="no"),0,1))%>% select(imdb_rating,critics_score,best_actress_win,best_actor_win)
# M <- cor(moviesQuantitative)
# # corrplot.mixed(cor(moviesQuantitative),method="circle",order="AOE")
# corrplot.mixed(cor(moviesQuantitative),order="AOE")

movies %>% count(genre) %>% arrange(desc(n)) %>% head(n=5)

# Most popular genres
movies3MainGenres <- movies %>% filter(genre=="Drama" | genre=="Comedy" | genre=="Action & Adventure")

movies3MainGenres$critics_rating<- gsub(movies3MainGenres$critics_rating,pattern = "Certified Fresh",replacement="Very Good")
movies3MainGenres$critics_rating <- gsub(movies3MainGenres$critics_rating,pattern = "Fresh",replacement="Good")
movies3MainGenres$critics_rating <- gsub(movies3MainGenres$critics_rating,pattern = "Rotten",replacement="Poor")
movies3MainGenres$critics_rating <- factor(movies3MainGenres$critics_rating,levels=c("Very Good","Good","Poor"),ordered=T)


# Plot Comparing scores of two sites, Genres, Number of Imdv votes and Review Quality.
ggplot(data = movies3MainGenres) + 
  geom_jitter(mapping = aes(x=audience_score, y=imdb_rating, color = genre,  size = imdb_num_votes, shape = critics_rating),alpha = 0.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  theme_light() +
  theme(legend.position = "right", legend.key = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12)) +
  labs(x = "Rotten tomatoes - Score", y = "IMDB-Score", title="IMDB vs Rotten Tomatoes Rating", color="Genres", shape = "Review Quality (Rotten Tomatoes)", size  = "Number of imdb votes")


# Plot Comparing time vs avg_score, Genres, Number of Imdv votes and Review Quality.
movies3MainGenres2 <- 
  
ggplot(data = movies3MainGenres) + 
  geom_jitter(mapping = aes(x=critics_score, y=imdb_rating, color = genre,  size = imdb_num_votes, shape = critics_rating),alpha = 0.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  theme_light() +
  theme(legend.position = "right", legend.key = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12)) +
  labs(x = "Critics score, Rotten tomatoes", y = "Imdb-score", title="Imdb vs Rotten tomatoes", color="Genres", shape = "Review Quality (Rotten Tomatoes)", size  = "Number of imdb votes")


# Dates in X axis, Avg Audience & Critic score & IMDB (Normalize) in Y axis, 4 Main Genres hue, 


