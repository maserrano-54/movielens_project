rm(list=ls())
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(gridExtra)
library(lubridate)
library(ggrepel)
library(scales)
library(broom)
library(caret)


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

summary(edx)
nrow(edx)
ncol(edx)
dim(edx)
sum(edx$rating[edx$rating==3])
edx %>% filter(rating == 3) %>% tally()
n_distinct(edx$movieId)
n_distinct(edx$userId)



#find NAs
sum(is.na(edx))
sum(is.na(validation))

#add genres variables
edx <- edx %>% mutate(g_adventure = ifelse(str_detect(genres, "Adventure")==TRUE,1,0))
edx <- edx %>% mutate(g_comedy = ifelse(str_detect(genres, "Comedy")==TRUE,1,0))
edx <- edx %>% mutate(g_action = ifelse(str_detect(genres, "Action")==TRUE,1,0))
edx <- edx %>% mutate(g_animation = ifelse(str_detect(genres, "Animation")==TRUE,1,0))
edx <- edx %>% mutate(g_children = ifelse(str_detect(genres, "Children")==TRUE,1,0))
edx <- edx %>% mutate(g_fantasy = ifelse(str_detect(genres, "Fantasy")==TRUE,1,0))
edx <- edx %>% mutate(g_scifi = ifelse(str_detect(genres, "Sci-Fi")==TRUE,1,0))
edx <- edx %>% mutate(g_drama = ifelse(str_detect(genres, "Drama")==TRUE,1,0))
edx <- edx %>% mutate(g_romance = ifelse(str_detect(genres, "Romance")==TRUE,1,0))
edx <- edx %>% mutate(g_thriller = ifelse(str_detect(genres, "Thriller")==TRUE,1,0))
edx <- edx %>% mutate(g_crime = ifelse(str_detect(genres, "Crime")==TRUE,1,0))
edx <- edx %>% mutate(g_horror = ifelse(str_detect(genres, "Horror")==TRUE,1,0))
edx <- edx %>% mutate(g_war = ifelse(str_detect(genres, "War")==TRUE,1,0))
edx <- edx %>% mutate(g_mystery = ifelse(str_detect(genres, "Mystery")==TRUE,1,0))
edx <- edx %>% mutate(g_musical = ifelse(str_detect(genres, "Musical")==TRUE,1,0))
edx <- edx %>% mutate(g_documentary = ifelse(str_detect(genres, "Documentary")==TRUE,1,0))
edx <- edx %>% mutate(g_western = ifelse(str_detect(genres, "Western")==TRUE, 1,0))
edx <- edx %>% mutate(g_filmnoir = ifelse(str_detect(genres, "Film-Noir")==TRUE, 1,0))
edx <- edx %>% mutate(g_imax = ifelse(str_detect(genres, "IMAX")==TRUE, 1,0))

#do the same for validation set
validation <- validation %>% mutate(g_adventure = ifelse(str_detect(genres, "Adventure")==TRUE,1,0))
validation <- validation %>% mutate(g_comedy = ifelse(str_detect(genres, "Comedy")==TRUE,1,0))
validation <- validation %>% mutate(g_action = ifelse(str_detect(genres, "Action")==TRUE,1,0))
validation <- validation %>% mutate(g_animation = ifelse(str_detect(genres, "Animation")==TRUE,1,0))
validation <- validation %>% mutate(g_children = ifelse(str_detect(genres, "Children")==TRUE,1,0))
validation <- validation %>% mutate(g_fantasy = ifelse(str_detect(genres, "Fantasy")==TRUE,1,0))
validation <- validation %>% mutate(g_scifi = ifelse(str_detect(genres, "Sci-Fi")==TRUE,1,0))
validation <- validation %>% mutate(g_drama = ifelse(str_detect(genres, "Drama")==TRUE,1,0))
validation <- validation %>% mutate(g_romance = ifelse(str_detect(genres, "Romance")==TRUE,1,0))
validation <- validation %>% mutate(g_thriller = ifelse(str_detect(genres, "Thriller")==TRUE,1,0))
validation <- validation %>% mutate(g_crime = ifelse(str_detect(genres, "Crime")==TRUE,1,0))
validation <- validation %>% mutate(g_horror = ifelse(str_detect(genres, "Horror")==TRUE,1,0))
validation <- validation %>% mutate(g_war = ifelse(str_detect(genres, "War")==TRUE,1,0))
validation <- validation %>% mutate(g_mystery = ifelse(str_detect(genres, "Mystery")==TRUE,1,0))
validation <- validation %>% mutate(g_musical = ifelse(str_detect(genres, "Musical")==TRUE,1,0))
validation <- validation %>% mutate(g_documentary = ifelse(str_detect(genres, "Documentary")==TRUE,1,0))
validation <- validation %>% mutate(g_western = ifelse(str_detect(genres, "Western")==TRUE, 1,0))
validation <- validation %>% mutate(g_filmnoir = ifelse(str_detect(genres, "Film-Noir")==TRUE, 1,0))
validation <- validation %>% mutate(g_imax = ifelse(str_detect(genres, "IMAX")==TRUE, 1,0))



#extract years, add the age of movies when rated and add the number of genres per movie
edx <- edx %>% mutate(g_count = str_count(genres, "\\|"))
edx <- edx %>% mutate(g_count = g_count + 1)
edx <- edx %>% mutate(title = str_replace(title,"^(.+)\\s\\((\\d{4})\\)$","\\1_\\2" )) %>% 
  separate(title,c("title","release_year"),"_")
edx$release_year <- as.numeric(edx$release_year)
edx$timestamp <- as_datetime(edx$timestamp)
edx$timestamp <- year(edx$timestamp)
edx <- edx %>% mutate(years_time = timestamp - release_year)
#filter so that there are not movies that were rated before they were released 
edx <- edx %>% filter(years_time>=0)
edx <- edx[complete.cases(edx), ]

#do the same for validation set
validation <- validation %>% mutate(g_count = str_count(genres, "\\|"))
validation <- validation %>% mutate(g_count = g_count + 1)
validation <- validation %>% mutate(title = str_replace(title,"^(.+)\\s\\((\\d{4})\\)$","\\1_\\2" )) %>% 
  separate(title,c("title","release_year"),"_")
validation$release_year <- as.numeric(validation$release_year)
validation$timestamp <- as_datetime(validation$timestamp)
validation$timestamp <- year(validation$timestamp)
validation <- validation %>% mutate(years_time = timestamp - release_year)

validation <- validation %>% filter(years_time>=0)
validation <- validation[complete.cases(validation), ]

adventure <- edx %>% filter(g_adventure==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
comedy <- edx %>% filter(g_comedy==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
action <- edx %>% filter(g_action==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
animation <- edx %>% filter(g_animation==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
children <- edx %>% filter(g_children==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
fantasy <- edx %>% filter(g_fantasy==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
scifi <- edx %>% filter(g_scifi==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
drama <- edx %>% filter(g_drama==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
romance <- edx %>% filter(g_romance==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
thriller <- edx %>% filter(g_thriller==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
crime <- edx %>% filter(g_crime==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
horror <- edx %>% filter(g_horror==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
war <- edx %>% filter(g_war==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
mystery <- edx %>% filter(g_mystery==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
musical <- edx %>% filter(g_musical==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
documentary <- edx %>% filter(g_documentary==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
western <- edx %>% filter(g_western==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
filmnoir <- edx %>% filter(g_filmnoir==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))
imax <- edx %>% filter(g_imax==1) %>% select(rating) %>% summarize(count=n(), avgrating = mean(rating))

table_g <- data.frame(genre= c("adventure", "comedy", "action", "animation", "children", "fantasy", "scifi", "drama", "romance", "thriller","crime", "horror", "war", "mystery", "musical", "documentary", "western", "filmnoir", "imax"), count=c(adventure$count, comedy$count, action$count, animation$count, children$count, fantasy$count, scifi$count, drama$count, romance$count, thriller$count,crime$count, horror$count, war$count, mystery$count, musical$count, documentary$count, western$count, filmnoir$count, imax$count), avg_rating = c(adventure$avgrating, comedy$avgrating, action$avgrating, animation$avgrating, children$avgrating, fantasy$avgrating, scifi$avgrating, drama$avgrating, romance$avgrating, thriller$avgrating,crime$avgrating, horror$avgrating, war$avgrating, mystery$avgrating, musical$avgrating, documentary$avgrating, western$avgrating, filmnoir$avgrating, imax$avgrating))
table_g %>% knitr::kable()
table_g %>% ggplot(aes(x = genre, y = count)) + geom_bar(stat="identity", fill="steelblue") + theme_minimal() + coord_flip() + labs(x="Numer of ratings", y="Genres") + ggtitle("Numer of ratings per genre") + theme(plot.title = element_text(hjust=0.5,face="bold")) 
table_g %>% ggplot(aes(x = genre, y = avg_rating)) + geom_bar(stat="identity", fill="indianred2") + theme_minimal() + coord_flip() + labs(x="Average rating", y="Genres") + ggtitle("Average rating per genre") + theme(plot.title = element_text(hjust=0.5,face="bold")) 
#plot genres
table_g %>% ggplot(aes(avg_rating, count)) + geom_point() + scale_y_continuous(labels=comma) + geom_label_repel(aes(label=genre), box.padding   = 0.35,  point.padding = 0.5, segment.color = 'grey50') + theme(panel.background  = element_rect(fill = "lightskyblue2"), panel.grid.minor = element_line(color="grey"), panel.grid.major = element_line(color="grey")) +labs(x="Average Rating", y="Count")

#create boolean variable to represent whether the genre is a liked genre or not
edx <- edx %>% mutate(liked_genre = ifelse(g_filmnoir==1| g_documentary==1| g_war==1| g_imax==1 ,1,0))

edx %>% ggplot(aes(rating)) + geom_histogram(color="gray43") + facet_wrap(~liked_genre) + labs(title="Distribution of rating depending on whether the movie \n has a liked genre or not" ,x="Rating",  subtitle="1 means the movie has a liked genre, 0 means it doesn't")  + theme_classic() + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill="lightcyan2")) 

#do the same for validation
validation <- validation %>% mutate(liked_genre = ifelse(g_filmnoir==1| g_documentary==1| g_war==1| g_imax==1 ,1,0))

#exploratory data analysis: histograms
gcount_h <- edx %>% select(g_count) %>% ggplot(aes(g_count)) + geom_bar(col = "grey", fill="darkgreen") + scale_y_continuous() + ggtitle("Distribution of the number of genres") + labs(x="Number of ratings") + theme_classic()  + theme(plot.title= element_text(hjust=0.5, face="bold", color="black")) 
gcount_m_hist <- edx %>% select(g_count, rating) %>% group_by(g_count) %>% summarise(m = mean(rating), s=sd(rating)) %>% ggplot(aes(m)) + geom_histogram(col="black", bins=5) + labs(x="mean rating") + ggtitle("Distribution of mean ratings per number of genres") + theme_classic() + theme(plot.title = element_text(hjust=0.5,face = "bold")) 
rating_hist <- edx %>% ggplot(aes(rating)) + geom_bar(col="grey", fill="lightblue") + scale_y_continuous() + theme_classic() + ggtitle("Distribution of rating") + theme(plot.title = element_text(hjust=0.5,face = "bold")) 
rating_hist
user_h <- edx %>% select(userId) %>% group_by(userId) %>% summarize(n=n()) %>% ggplot(aes(n)) + geom_histogram(bins=20, col="grey", fill ="darkolivegreen3") + ggtitle("Distribution of number of ratings per user") + labs(x="Ratings per user") + scale_x_log10()+ theme_classic() + theme(plot.title = element_text(hjust=0.5,face = "bold")) 
user_h
movie_h <- edx %>% select(movieId) %>% group_by(movieId) %>% summarize(n=n()) %>% ggplot(aes(n)) + geom_histogram(bins=20, col="grey", fill="pink2") + ggtitle("Distribution of number of ratings per movie") + labs(x="Ratings per movie") + scale_x_log10() + theme_classic() + theme(plot.title = element_text(hjust=0.5,face = "bold")) 
movie_h
release_h <- edx %>% select(release_year) %>% ggplot(aes(release_year)) + geom_histogram(col="grey", fill="royalblue1") + ggtitle("Distribution of \n the release \n years") + labs(x="Release year") + theme_classic() + theme(plot.title = element_text(hjust=0.5,face = "bold",size = 10),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8)) 
release_h
timestamp_h <- edx %>% select(timestamp) %>% ggplot(aes(timestamp)) + geom_histogram(col="grey", fill="indianred4", bins=15) + scale_y_continuous() + ggtitle("Distribution of \n the rating years") + labs(x="Rating year") + theme_classic() + theme(plot.title = element_text(hjust=0.5,face = "bold", size=10),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8)) 
timestamp_h
yearstime_h <- edx %>% select(years_time) %>% ggplot(aes(years_time)) + geom_histogram(col="grey", fill="tan3") + ggtitle("Distribution of the years \n between release \n and rating year") + labs(x="Years between release \n and rating year") + theme_classic() + theme(plot.title = element_text(hjust=0.5,face = "bold", size=10),axis.title.x = element_text(size=8), axis.title.y = element_text(size=8)) 
yearstime_h



#exploratory data analysis: bivariate analysis
gcount_m <- edx %>% group_by(g_count) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(g_count,m)) + geom_point() + geom_smooth(color="darkgreen",size=1, fill="lightgreen") + labs(x="Number of genres of the film", y="Avg rating") + ggtitle("Avg rating per number of genres") +theme_classic() + theme(plot.title = element_text(hjust=0.5)) 
grid.arrange(gcount_h, gcount_m, nrow=1)

user_m_n <- edx %>% group_by(userId) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(n,m)) + geom_point(color="gray43") + geom_smooth(color="darkgreen", fill="darkolivegreen4") + ggtitle("Avg rating per user's activity") + labs(x="Number of ratings per user", y="Avg rating") + theme_classic() + theme(plot.title = element_text(hjust=0.5)) 
user_m <- edx %>% group_by(userId) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(m)) + geom_histogram(col="grey", fill="darkolivegreen3") + ggtitle("Distribution of avg rating per user") + labs(x="Avg rating per user") + theme(plot.title = element_text(hjust = 0.5)) + theme_classic() +theme_classic() + theme(plot.title = element_text(hjust=0.5)) 
grid.arrange(user_h, user_m_n,user_m, nrow=2)

#we saw some effect of user activity (how many ratings the user has given). We add the variable
edx_user_act <- edx %>% group_by(userId) %>% summarize(user_activity = n())
edx <- left_join(edx, edx_user_act, by="userId")

#do the same for validation
validation_user_act <- validation %>% group_by(userId) %>% summarize(user_activity = n())
validation <- left_join(validation, validation_user_act, by="userId")
#add user effect
avg_user <- edx %>% group_by(userId) %>% summarize(avg_user = mean(rating))
edx <- edx %>% left_join(avg_user, by="userId") 
avg_user <- validation %>% group_by(userId) %>% summarize(avg_user = mean(rating))
validation <- validation %>% left_join(avg_user, by="userId") 


movie_m_n <- edx %>% group_by(movieId) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(n,m)) + geom_point(color="gray43") + geom_smooth(color="deeppink4", fill="palevioletred3") + ggtitle("Avg rating per movie's popularity") + labs(x="Number of ratings per movie (popularity)", y="Avg rating")+theme_classic() + theme(plot.title = element_text(hjust=0.5)) 
movie_m <- edx %>% group_by(movieId) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(m)) + geom_histogram(col="grey", fill="pink2") + ggtitle("Distribution of avg rating per movie") + labs(x="Avg rating per movie")+theme_classic() + theme(plot.title = element_text(hjust=0.5)) 
grid.arrange(movie_h, movie_m_n, movie_m, nrow=2)

#since it looks like the movie's popularity affects rating, we add the popularity variable
edx_movie_pop <- edx %>% group_by(movieId) %>% summarize(popularity = n())
edx <- left_join(edx, edx_movie_pop, by="movieId")
#do the same for validation
validation_movie_pop <- validation %>% group_by(movieId) %>% summarize(popularity = n())
validation <- left_join(validation, validation_movie_pop, by="movieId")
#add movie effect
avg_movie <- edx %>% group_by(movieId) %>% summarize(avg_movie = mean(rating))
edx <- edx %>% left_join(avg_movie, by="movieId")
avg_movie <- validation %>% group_by(movieId) %>% summarize(avg_movie = mean(rating))
validation <- validation %>% left_join(avg_movie, by="movieId")


releaseyear_m <- edx %>% group_by(release_year) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(release_year, m)) + geom_point(color="gray43") + geom_smooth(color="blue4", fill="aquamarine4") + ggtitle("Avg rating \n per relase year") + labs(x="Release year", y="Avg rating") + theme_classic() + theme(plot.title = element_text(hjust=0.5,size=10,face="bold"),axis.title.x = element_text(size=8),axis.title.y= element_text(size=8)) 
yearstime_m <- edx %>% group_by(years_time) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(years_time, m)) + geom_point(col="gray43") + geom_smooth(color="red4", fill="coral1") + ggtitle("Avg rating per \n movie's age when rated") + labs(x="Movie's age when rated", y="Avg rating") + theme_classic() + theme(plot.title = element_text(hjust=0.5,size=10,face="bold"),axis.title.x = element_text(size=8),axis.title.y= element_text(size=8)) 
yearrated_m <- edx %>% group_by(timestamp) %>% summarize(n=n(), m=mean(rating)) %>% ggplot(aes(timestamp, m)) + geom_point() + geom_smooth(color="darkorange4", fill="bisque3") + ggtitle("Avg rating \n per rating year") + labs(x="Rating year", y="Avg rating") + theme_classic() + theme(plot.title = element_text(hjust=0.5,size=10,face="bold"), axis.title.x = element_text(size=8),axis.title.y =element_text(size=8)) 
grid.arrange(release_h, timestamp_h, yearstime_h, releaseyear_m,  yearrated_m, yearstime_m, nrow=2)



#correlations
cor_gcount <- cor(edx$rating, edx$g_count)
cor_useract <- cor(edx$rating, edx$user_activity)
cor_popularity <- cor(edx$rating, edx$popularity)
cor_release <- cor(edx$rating, edx$release_year)
cor_timestamp <- cor(edx$rating, edx$timestamp)
cor_yearstime <- cor(edx$rating, edx$years_time)
means_likedgenre <- tapply(edx$rating, edx$liked_genre, mean)


likedgenre_table <- data.frame("Genre not liked" =c(means_likedgenre[1]), "Liked genre"=c(means_likedgenre[2]))

ptest_gcount <- cor.test(edx$rating, edx$g_count, method="pearson")
gcount_hyp <- ifelse(ptest_gcount$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")
ptest_useract <- cor.test(edx$rating, edx$user_activity, method="pearson")
useract_hyp <- ifelse(ptest_useract$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")
ptest_popularity <- cor.test(edx$rating, edx$popularity, method="pearson")
popularity_hyp <- ifelse(ptest_popularity$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")
ptest_release <- cor.test(edx$rating, edx$release_year, method="pearson")
release_hyp <- ifelse(ptest_release$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")
ptest_timestamp <- cor.test(edx$rating, edx$timestamp, method="pearson")
timestamp_hyp <- ifelse(ptest_timestamp$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")
ptest_yearstime <- cor.test(edx$rating, edx$years_time, method="pearson")
yearstime_hyp <- ifelse(ptest_yearstime$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")
ptest_likedgenre <- t.test(rating~liked_genre, data= edx, paired=F)
likedgenre_hyp <- ifelse(ptest_likedgenre$p.value<0.05, "Reject Null Hypotesis","Not Reject Null Hypothesis")

cor_table <- data.frame("Variables" = c("Number of genres", "User Activity", "Movie's Popularity", "Release Year", "Rating Year", "Movie's Age When Rated"),
                        "Corrlation with Rating"=c(cor_gcount, cor_useract, cor_popularity, cor_release, cor_timestamp, cor_yearstime),
                        "Statistic" = c(ptest_gcount$statistic, ptest_useract$statistic,  ptest_popularity$statistic, ptest_release$statistic, ptest_timestamp$statistic, ptest_yearstime$statistic),
                        "Results" = c(gcount_hyp, useract_hyp, popularity_hyp, release_hyp, timestamp_hyp, yearstime_hyp))


t_table <- data.frame("Variables" = c("Liked Genre"),
                      "Statistic" = c(ptest_likedgenre$statistic),
                      "Results"=c(likedgenre_hyp))

cor_table %>% knitr:: kable()
likedgenre_table %>% knitr::kable()
t_table %>% knitr::kable()

#we create second data partition for model developing
set.seed(1, sample.kind="Rounding")
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_edx <- edx[-test_index2,]
temp_edx <- edx[test_index2,]

test_edx <- temp_edx %>% 
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")

removed2 <- anti_join(temp_edx, test_edx)
train_edx <- rbind(train_edx, removed2)

rm(test_index2, temp_edx,removed2)

sum(is.na(train_edx))
sum(is.na(test_edx))

#Model development
fit_glm1 <- glm(rating~g_count, data=train_edx)
pred_glm1 <- predict(fit_glm1, test_edx)
rmse_glm1 <-sqrt(mean((pred_glm1-test_edx$rating)^2))
pred_train_glm1 <- predict(fit_glm1, train_edx)
rmse_train_glm1 <-sqrt(mean((pred_train_glm1-train_edx$rating)^2))
rmse_glm1

fit_glm2 <- glm(rating~g_count + user_activity, data=train_edx)
pred_glm2 <- predict(fit_glm2, test_edx)
rmse_glm2 <-sqrt(mean((pred_glm2-test_edx$rating)^2))
pred_train_glm2 <- predict(fit_glm2, train_edx)
rmse_train_glm2 <-sqrt(mean((pred_train_glm2-train_edx$rating)^2))

fit_glm3 <- glm(rating~g_count + user_activity + popularity, data=train_edx)
pred_glm3 <- predict(fit_glm3, test_edx)
rmse_glm3 <-sqrt(mean((pred_glm3-test_edx$rating)^2))
pred_train_glm3 <- predict(fit_glm3, train_edx)
rmse_train_glm3 <-sqrt(mean((pred_train_glm3-train_edx$rating)^2))

fit_glm4 <- glm(rating~g_count + user_activity + popularity + release_year, data=train_edx)
pred_glm4 <- predict(fit_glm4, test_edx)
rmse_glm4 <-sqrt(mean((pred_glm4-test_edx$rating)^2))
pred_train_glm4 <- predict(fit_glm4, train_edx)
rmse_train_glm4 <-sqrt(mean((pred_train_glm4-train_edx$rating)^2))


fit_glm5 <- glm(rating~g_count + user_activity + popularity + release_year + years_time, data=train_edx)
pred_glm5 <- predict(fit_glm5, test_edx)
rmse_glm5 <-sqrt(mean((pred_glm5-test_edx$rating)^2))
pred_train_glm5 <- predict(fit_glm5, train_edx)
rmse_train_glm5 <-sqrt(mean((pred_train_glm5-train_edx$rating)^2))


fit_glm6 <- glm(rating~g_count + user_activity + popularity + release_year + years_time + liked_genre, data=train_edx)
pred_glm6 <- predict(fit_glm6, test_edx)
rmse_glm6 <-sqrt(mean((pred_glm6-test_edx$rating)^2))
pred_train_glm6 <- predict(fit_glm6, train_edx)
rmse_train_glm6 <-sqrt(mean((pred_train_glm6-train_edx$rating)^2))


#user effect
fit_glm7 <- glm(rating~g_count + avg_user + user_activity + popularity + release_year + years_time +liked_genre, data=train_edx)
pred_glm7 <- predict(fit_glm7, test_edx)
rmse_glm7 <-sqrt(mean((pred_glm7-test_edx$rating)^2))
pred_train_glm7 <- predict(fit_glm7, train_edx)
rmse_train_glm7 <-sqrt(mean((pred_train_glm7-train_edx$rating)^2))

#movie effect
fit_glm8 <- glm(rating~g_count + avg_user + avg_movie + user_activity + popularity + release_year + years_time + liked_genre, data=train_edx)
pred_glm8 <- predict(fit_glm8, test_edx)
rmse_glm8 <-sqrt(mean((pred_glm8-test_edx$rating)^2))
pred_train_glm8 <- predict(fit_glm8, train_edx)
rmse_train_glm8 <-sqrt(mean((pred_train_glm8-train_edx$rating)^2))

#results
glm_results <- data.frame("Model" =c("GLM 1","GLM 2","GLM 3","GLM 4","GLM 5","GLM 6","GLM 7","GLM 8"),
                          "RMSE validation" =c(rmse_glm1,rmse_glm2,rmse_glm3,rmse_glm4,rmse_glm5,rmse_glm6,rmse_glm7,rmse_glm8),
                          "RMSE edx"=c(rmse_train_glm1,rmse_train_glm2,rmse_train_glm3,rmse_train_glm4,rmse_train_glm5,rmse_train_glm6,rmse_train_glm7,rmse_train_glm8))

glm_results %>% knitr::kable()

#glm model selected: cross-validation algorithm
control <- trainControl(method="cv", number=5, p =0.9)
train_glm <- train(rating~g_count + avg_user + avg_movie + user_activity + popularity + release_year + years_time + liked_genre, method="glm", data=edx, trControl=control)

#test on validation set (and edx set)
pred_validation <- predict(fit_glm8, validation)
rmse_validation <-sqrt(mean((pred_validation-validation$rating)^2))

pred_edx <- predict(fit_glm8, edx)
rmse_edx <- sqrt(mean((pred_edx-edx$rating)^2))

validation_rmse <- data.frame("Dataset" = c("Validation"),
                              "RMSE" = c(rmse_validation))

validation_rmse %>% knitr::kable()

#residuals and summary
dens_fit <- data.frame("dens" = resid(fit_glm8))
dens_fit %>% ggplot(aes(dens)) + geom_density(fill="lightcoral",alpha=0.6, bw=0.8) + theme_classic() + labs(x="Residuals")

summary(fit_glm8)

