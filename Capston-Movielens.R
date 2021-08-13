if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)
library(knitr)
library(dplyr)
library(purrr)
library(gridExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

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
kable(head(movielens),"pandoc",caption = "Table-1- Movielens datasets")
kable(names(movielens),"pandoc", caption = "Table 2- The list of variable in movielens dataset")

######## Movielens add year-rated as variable
movielens <- movielens %>% 
  mutate(year_rated = year(as_datetime(timestamp))) %>% select(-timestamp)
kable(head(movielens),"pandoc",caption ="3- Movielens dataset with replacing year_rated to timestamp")

####### Movilens seperate year-released and title
movielens <- movielens %>% 
  mutate(year_released = as.numeric(str_sub(title,-5,-2))) 
kable(head(movielens),"pandoc",caption = "Table-4- Movielens dataset with Separated year-released from title of monvie")

# slice of movielens dataset to edx and validation datasets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

# Dimention of edx and validation datasets
dim_edx <- edx %>% summarise(No_data_edx = nrow(edx),
                             No_variable_edx = ncol(edx))
dim_validation <- validation %>% summarise(No_data_validation =  nrow(validation),No_variable_validation = ncol(validation))  
kable(t(cbind2(dim_edx,dim_validation)),"pandoc",caption = "Table-5- Dimention of edx and validation datasets")

# the first six rows of edx dataset
kable(head(edx),"pandoc",caption = "Table-6- The edx dataset")

#################Analysis ######################

# the number of unique userID , MovieID , genres in datasets
edx_unique_info <- edx %>% 
  summarise(n_user_edx = n_distinct(userId),
            n_Movie_edx = n_distinct(movieId),
            n_genres_edx = n_distinct(genres))
kable(edx_unique_info,"pandoc",caption = "Table-7- Number of unique usersID,movieID, genres of edx dataset")

# making the distibution movie rating
edx %>% ggplot(aes(rating)) + 
  geom_histogram(binwidth = 0.5 , fill = "green", color = "yellow", lwd = 1)+
  xlab("Rating") + ylab("Count")+ 
  ggtitle("Figure 1-Distibution of Movie rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Effect of MovieId as title on rating 
edx %>% group_by(title) %>% 
  summarise(number_of_movie_rating  = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(number_of_movie_rating,avg_rating)) +
  geom_point(alpha= 0.2, color = "green", lwd = 1) + 
  ggtitle("Figure 2-Average of rating versus number of movie") +
  geom_smooth(method = "loess", color = "yellow") + 
  xlab ("Number of MovieID") +
  ylab("Average of rating") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Highest rate movie 
Highest_rate_movies <- edx %>% group_by(title) %>% summarise(Avg_rating = mean(rating)) %>% arrange(desc(Avg_rating)) 
kable(head(Highest_rate_movies), "pandoc", caption = "8-The highest rating movie", align = "c")

# lowest rate movie 
lowest_rate_movies <- edx %>% group_by(title) %>% summarise(Avg_rating = mean(rating)) %>% arrange(Avg_rating)
kable(head(lowest_rate_movies), "pandoc", caption = "9-The lowest rating movie", align = "c")


## Effect of Number of UserId on movie rating
user_avg <- edx %>% group_by(userId) %>% 
  summarise(number_of_user_rate = n(), avg_user_rating = mean(rating)) %>% 
  filter(number_of_user_rate > 100)

# minimum and maximum rate based on userID 
min_user_rated <- user_avg %>% 
  summarise(Min_rate = min(avg_user_rating),
            userID_min = userId[which.min(avg_user_rating)])
max_user_rated <- user_avg %>% 
  summarise(Max_rate = max(avg_user_rating),
            userId_max = userId[which.max(avg_user_rating)])            
kable(t(cbind(min_user_rated,max_user_rated)),"pandoc", caption = "10-Maximum and Minimum rating and UserID")


edx %>% group_by(userId) %>% 
  mutate(rating = mean(rating)) %>% count(userId,rating) %>% 
  ggplot(aes(n,rating)) + 
  geom_point(color = "green", alpha = 0.1) +
  ggtitle("Figure 3- Effect of Number UserId on rating") +
  geom_smooth(method = "loess", color = "yellow",se = FALSE)+
  xlab("Number of unique userId") +
  ylab ("Rating")+
  theme(plot.title = element_text(hjust = 0.5))


# Relation of type of genres on average of rating 
single_genres <- edx %>% separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>% 
  summarise(number_of_genres = n(), avg_rating = mean(rating))  

kable(single_genres,"pandoc",
      caption = "11- Number of each genres based on Number of genres")

single_genres %>% 
  arrange(desc(number_of_genres)) %>% 
  ggplot(aes(genres,avg_rating)) +
  geom_col(color = "yellow", fill = "green" , size = 1) + 
  ggtitle("Figure 4-Average of rating versus type of genres") +
  xlab("Genres") + 
  ylab("Average of rating") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90 , hjust = 0.5))

# *************************************************************#
#Effect of released year of movieId on rating
seperate_year_released <- edx %>% group_by(year_released) %>% 
  summarise(year_released_rating = mean(rating)) %>% arrange(desc(year_released_rating))

seperate_year_released %>% 
  ggplot(aes(year_released,year_released_rating)) + 
  geom_point(color = "green", alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Figure 5-Effect of released year on Rating") +
  xlab("Released year") +
  ylab("Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#***********************************************************
# split the edx dataset to train and test sets
y <- edx$rating
index <- createDataPartition(y, times = 1 , p = 0.2 , list = FALSE)
train <- edx[-index,]
test <- edx[index,]
test <- test %>% 
  semi_join(train , by = "movieId") %>%
  semi_join(train , by = "userId")

# ******************Naive_model********************
mu <- mean(train$rating)
rmse_naive <- RMSE(train$rating , mu)
rmse_naive
RMSE_Result <- tibble(method = "Naive method" , 
                      RMSE_on_Train_set = rmse_naive,
                      RMSE_on_Validation_set = NA)
kable((RMSE_Result[1:1,]), "pandoc", caption = "9-RMSE Results", align = "c")

# Effect of MovieID on RMSE
movie_avg <- train %>% group_by(movieId) %>%  
  summarise(b_i = mean(rating - mu))

movie_avg %>% ggplot(aes(b_i)) + 
  geom_histogram(color = "white" , fill = "green", binwidth = 0.5) +
  ggtitle("Figure 6 - Ditribution of Movie Bias") + 
  xlab("Movie Bias - b_i") +
  ylab("Number of Movie") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_rating <- mu + test %>% 
  left_join(movie_avg , by = "movieId") %>%
  .$b_i

Effect_of_Movie <- RMSE(test$rating,predicted_rating)

RMSE_Result <- add_row(RMSE_Result,method = "Movie Effect", 
                        RMSE_on_Train_set = Effect_of_Movie,
                        RMSE_on_Validation_set = NA)
kable((RMSE_Result[1:2,]), "pandoc",caption = 'RMSE Result', align = "c")

# Effect of UserId on RMSE
userid_avg <- train %>% 
  left_join(movie_avg , by = "movieId")  %>%
  group_by(userId) %>%  
  summarise(b_u = mean(rating - mu - b_i))

userid_avg %>% ggplot(aes(b_u)) + 
  geom_histogram(color = "white" , fill = "green", binwidth = 0.2) +
  ggtitle("Figure 7 - Ditribution of userId Bias") + 
  xlab("UserId Bias - b_u") +
  ylab("Number of UserId") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_rating <- test %>% 
  left_join(movie_avg , by = "movieId") %>%
  left_join(userid_avg , by = "userId")  %>%
  mutate(pred = mu + b_i + b_u) %>% pull(pred)

Effect_of_Movie_User <- RMSE(test$rating,predicted_rating)

RMSE_Result <- add_row(RMSE_Result,method = "Movie_User Effect", 
                       RMSE_on_Train_set = Effect_of_Movie_User,
                       RMSE_on_Validation_set = NA)
kable((RMSE_Result[1:3,]), "pandoc",caption = 'RMSE Result', align = "c")

#  Effect of genres on RMSE
genres_avg <- train %>% 
  left_join(movie_avg , by = "movieId")  %>%
  left_join(userid_avg , by = "userId")  %>%
  group_by(genres) %>%  
  summarise(b_g = mean(rating - mu - b_i - b_u))

genres_avg %>% ggplot(aes(b_g)) + 
  geom_histogram(color = "white" , fill = "green", binwidth = 0.05) +
  ggtitle("Figure 7 - Ditribution of Genres Bias") + 
  xlab("Genres Bias - b_y") +
  ylab("Genres") +
  theme(plot.title = element_text(hjust = 0.5))

predicted_rating <- test %>% 
  left_join(movie_avg , by = "movieId") %>%
  left_join(userid_avg , by = "userId")  %>%
  left_join(genres_avg, by = "genres")  %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% pull(pred)

Effect_of_Movie_User_genres <- RMSE(test$rating,predicted_rating)

RMSE_Result <- add_row(RMSE_Result,method = "Movie_User_genres Effect", 
                       RMSE_on_Train_set = Effect_of_Movie_User_genres,
                       RMSE_on_Validation_set = NA)
kable((RMSE_Result[1:4,]), "pandoc",caption = 'RMSE Result', align = "c")

#  Effect of released year on RMSE
released_year_avg <- train %>% 
  left_join(movie_avg , by = "movieId")  %>%
  left_join(userid_avg , by = "userId")  %>%
  left_join(genres_avg, by = "genres")  %>%
  group_by(year_released) %>%  
  summarise(b_y = mean(rating - mu - b_i - b_u - b_g))

released_year_avg %>% ggplot(aes(b_y)) + 
  geom_histogram(color = "white" , fill = "green", binwidth = 0.05) +
  ggtitle("Figure 7 - Ditribution of released year Bias") + 
  xlab("released year Bias - b_y") +
  ylab("Released Year") +
  theme(plot.title = element_text(hjust = 0.5))


predicted_rating <- test %>%  
  left_join(movie_avg , by = "movieId") %>%
  left_join(userid_avg , by = "userId")  %>%
  left_join(genres_avg, by = "genres")  %>%
  left_join(released_year_avg, by = "year_released")  %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>% pull(pred)

Effect_of_Movie_User_genres_releasedYear <- RMSE(test$rating,predicted_rating)

RMSE_Result <- add_row(RMSE_Result,method = "Movie_User_genres_releasedYear Effect", 
                       RMSE_on_Train_set = Effect_of_Movie_User_genres_releasedYear,
                       RMSE_on_Validation_set = NA)
kable((RMSE_Result[1:5,]), "pandoc",caption = 'RMSE Result', align = "c")


#  Regularized method
 lambdas <- seq(1,10,0.5)
 rmses <- sapply(lambdas, function(l){
   mu<- mean(train$rating)
   
   b_i <- train %>% group_by(movieId) %>%
     summarise(b_i = sum(rating-mu)/(n() +l))
   
   b_u <- train %>% left_join(b_i , by = "movieId") %>%
     group_by(userId) %>%
     summarise(b_u = sum(rating - mu - b_i)/(n()+l))

   b_g <- train %>% 
     left_join(movie_avg , by = "movieId")  %>%
     left_join(userid_avg , by = "userId")  %>%
     group_by(genres) %>%  
     summarise(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  
   b_y <- train %>% 
     left_join(movie_avg , by = "movieId")  %>%
     left_join(userid_avg , by = "userId")  %>%
     left_join(genres_avg, by = "genres")  %>%
     group_by(year_released) %>%  
     summarise(b_y = sum(rating - mu - b_i - b_u - b_g)/(n()+l))
      
   predicted_ratings <- test %>% 
     left_join(b_i , by = "movieId") %>% 
     left_join(b_u , by = "userId") %>%
     left_join(b_g , by = "genres") %>%
     left_join(b_y , by = "year_released") %>%
     mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
     pull(pred)
   
   return(RMSE(predicted_ratings, test$rating))
  
 })
plot(lambdas, rmses, main = "Plot of RMSE versus lambda")
optimum_lambdas <- lambdas[which.min(rmses)]

Regularized_Movie_User_genres_releasedYear = min(rmses)


# check the validation set
  lambda = lambdas[which.min(rmses)]
  mu<- mean(validation$rating)
  
  b_i <- validation %>% group_by(movieId) %>%
    summarise(b_i = sum(rating-mu)/(n() + lambda))
  
  b_u <- validation %>% left_join(b_i , by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i)/(n()+lambda))
  
  b_g <- validation %>% 
    left_join(b_i , by = "movieId")  %>%
    left_join(b_u , by = "userId")  %>%
    group_by(genres) %>%  
    summarise(b_g = sum(rating - mu - b_i - b_u)/(n()+lambda))
  
  b_y <- validation %>% 
   left_join(b_i , by = "movieId")  %>%
    left_join(b_u , by = "userId")  %>%
    left_join(b_g, by = "genres")  %>%
    group_by(year_released) %>%  
    summarise(b_y = sum(rating - mu - b_i - b_u - b_g)/(n()+lambda))
  
  predicted_ratings <- validation %>% 
    left_join(b_i , by = "movieId") %>% 
    left_join(b_u , by = "userId") %>%
    left_join(b_g , by = "genres") %>%
    left_join(b_y , by = "year_released") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  Regularized_Movie_User_genres_releasedYear_validate <- RMSE(predicted_ratings, validation$rating)
  Regularized_Movie_User_genres_releasedYear_validate
  
  RMSE_Result <- add_row(RMSE_Result,method = "Regularized_Movie_User_genres_releasedYear Effect", 
                         RMSE_on_Train_set = Regularized_Movie_User_genres_releasedYear,
                         RMSE_on_Validation_set = Regularized_Movie_User_genres_releasedYear_validate)
  kable((RMSE_Result[1:6,]), "pandoc",caption = 'RMSE Result', align = "c")
  