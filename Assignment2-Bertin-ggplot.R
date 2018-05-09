library(ggplot2)
library(ggthemes)
library(data.table)
library(dplyr)

#########################
# Read movie data
#########################
movies <- fread('https://raw.githubusercontent.com/idc9/stor390/master/data/movies.csv')
head(movies)
colnames(movies)

#########################
# Plotting graphs
#########################

# Most popular genres
movies %>% count(genre) %>% arrange(desc(n)) %>% head(n=5)

# Keep main genres only
movies3MainGenres <- movies %>% filter(genre=="Drama" | genre=="Comedy" | genre=="Action & Adventure")

# Tidy up data
movies3MainGenres$critics_rating<- gsub(movies3MainGenres$critics_rating,pattern = "Certified Fresh",replacement="Very Good")
movies3MainGenres$critics_rating <- gsub(movies3MainGenres$critics_rating,pattern = "Fresh",replacement="Good")
movies3MainGenres$critics_rating <- gsub(movies3MainGenres$critics_rating,pattern = "Rotten",replacement="Poor")
movies3MainGenres$critics_rating <- factor(movies3MainGenres$critics_rating,levels=c("Very Good","Good","Poor"),ordered=T)


# Plot Comparing scores of two sites, Genres, Number of Imdv votes and Review Quality.
ggplot(data = movies3MainGenres) + 
  geom_jitter(mapping = aes(x=audience_score, y=imdb_rating, color = genre,  size = imdb_num_votes, shape = critics_rating),alpha = 0.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  scale_size(range = c(2, 7))+
  theme_light() +
  theme(legend.position = "right", legend.key = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12)) +
  labs(x = "Rotten Tomatoes - Score", y = "IMDB-Score", title="IMDB vs Rotten Tomatoes Rating", color="Genres", shape = "Quality of Reviews (Rotten Tomatoes)", size  = "Number of IMDB votes")

ggsave("plot1.png", width = 12, height = 8)


# Dates in X axis, Avg Audience & Critic score & IMDB (Normalize) in Y axis, 4 Main Genres hue, 
movies3MainGenres$critics_score <- movies3MainGenres$critics_score/10
movies3MainGenres$audience_score <- movies3MainGenres$audience_score/10
movies3MainGenres$AVG <- (movies3MainGenres$audience_score + movies3MainGenres$critics_score + movies3MainGenres$imdb_rating)/3
movies3MainGenres$AVG <- as.numeric(movies3MainGenres$AVG) 

ggplot(data = movies3MainGenres) + 
  geom_jitter(mapping = aes(x=thtr_rel_year, y=AVG, color = genre,  size = imdb_num_votes, shape = critics_rating),alpha = 0.7)+
  scale_x_continuous()+
  scale_y_continuous()+
  theme_light() +
  theme(legend.position = "right", legend.key = element_blank(),
        axis.text = element_text(size=10),
        axis.title = element_text(size=12)) +
  labs(x = "Rotten tomatoes - Score", y = "IMDB-Score", title="Avg Score Vs Time", color="Genres", shape = "Review Quality (Rotten Tomatoes)", size  = "Number of imdb votes")


