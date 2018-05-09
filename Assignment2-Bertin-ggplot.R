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

movies3MainGenres$genre <- factor(movies3MainGenres$genre,levels = c("Drama","Comedy","Action & Adventure"),ordered = T)

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
  labs(x = "Rotten Tomatoes - Rating", y = "IMDB-Rating", title="IMDB vs Rotten Tomatoes Rating", color="Genres", shape = "Quality of Reviews (Rotten Tomatoes)", size  = "Number of IMDB votes")

ggsave("plot1.png", width = 12, height = 8)


# New Plot using plotly's library

# New cols needed for plotting.
movies3MainGenres <- movies3MainGenres %>% mutate(num_genre = as.numeric(genre),num_quality=as.numeric(critics_rating))

parallelPlot <- movies3MainGenres %>%
  plot_ly(type = 'parcoords',
          line = list(color = ~num_genre,
                      colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
          dimensions = list(
            list(range = c(0,10),
                 constraintrange = c(9,10),
                 label = 'IMDB rating', values = ~imdb_rating),
            list(range = c(0,100),
                 label = 'Rotten Tomatoes rating', values = ~audience_score),
            list(label='IMDB Num Votes',range=c(min(movies3MainGenres$imdb_num_votes),max(movies3MainGenres$imdb_num_votes)),
                 values= ~imdb_num_votes),
            list(label='Review Quality',tickvals = c(1,2,3),ticktext = c("Poor", "Average", "Very Good"),
                               values= ~num_quality)
     )
  ) 

parallelPlot



