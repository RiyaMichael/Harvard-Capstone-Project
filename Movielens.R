##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(dplyr)
library(recosystem)
library(recommenderlab)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
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

######

#Generating the basic metrics to familiarize ourselves witht the data set
dim(edx)
head(edx)
tail(edx)
summary(edx)

#Check for any missing values
anyNA(edx)

#Check the total number of movies and the number of movie ratings
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

#Plotting a graph depicting the number of ratings received by a particular movie
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, binwidth=0.2, color="black", show.legend = FALSE, aes(fill = cut(n, 100))) + 
  scale_x_log10() + 
  ggtitle("Movies Rated")

#Plotting a graph depicting the number of ratings given by a particular user
edx %>%
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", show.legend = FALSE, aes(fill = cut(n, 100))) + 
  scale_x_log10() +
  ggtitle("Users")


#Plotting a graph depicting the number of ratings depending upon the genre of the movie
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% ggplot(aes(genres,count)) + 
  geom_bar(aes(fill =genres),stat = "identity")+ 
  labs(title = " Number of Rating for Each Genre")+
  theme(axis.text.x  = element_text(angle= 90, vjust = 50 ))+
  theme_light()

##Model Building

#Splitting the data into train and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train <- edx[-test_index,]
test <- edx[test_index,]

#To ensure the same users and movies are in the train and test sets, we remove these entries using the semi_join function:
test<- test %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

#Forming the function for RMSE calculation
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#SIMPLE AVERAGE MODEL
#Calculating the basic average of ratings
mu <- mean(train$rating)
mu

#predicting the ratings for the test set using the average and thereby getting the RMSE
avg_RMSE <- RMSE(test$rating,mu)
avg_RMSE

#Creating a table for the RMSE result to store all the result from each method to compare
rmse_results <- data_frame(method = "Just the average", RMSE = avg_RMSE)

#MOVIE EFFECT MODEL
#Now if we consider movie bias effects
mu <- mean(train$rating)
movie_avg <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
bi <- train %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - mu))

#Predicting ratings with mu and bi
predicted_ratings <- mu + test %>% 
  left_join(movie_avg, by = 'movieId') %>% 
  pull(bi)

#Now checking whether the RMSE has improved after accounting for movie bias
bi_RMSE <- RMSE(test$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method = "Movie Effect",
                                     RMSE = bi_RMSE))
rmse_results

#MOVIE + USER EFFECT MODEL
#Taking user effect into account too
train %>% 
  group_by(userId) %>%
  summarise(bu = mean(rating)) %>% 
  filter(n()>= 100) %>%
  ggplot(aes(bu)) + 
  geom_histogram(bins = 30, color = "black")

#Including user bias into the algorithm
user_avg <- train %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mu - bi))

#Checking whether there is any improvement in the RMSE
predicted_ratings <- test %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + bi + bu) %>%
  .$pred

bu_rmse <- RMSE(test$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = bu_rmse))
rmse_results

#REGULARISATION OF MOVIE EFFECTS ONLY

#Start by splitting the data by cross - validation
#Use 10-fold cross validation to pick a lambda for movie effects regularization
#Split the data into 10 parts
set.seed(2019, sample.kind = "Rounding")
cv_splits <- createFolds(edx$rating, k=10, returnTrain =TRUE)

#Define a matrix to store the results of cross validation
rmses <- matrix(nrow=10, ncol=51)
lambdas <- seq(0, 5, 0.1)

#Perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
train <- edx[cv_splits[[k]],]
 test <- edx[-cv_splits[[k]],]
  
#Make sure userId and movieId in test set are also in the train set
test_final <- test %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")
  
removed <- anti_join(test, test_final)
train_final <- rbind(train, removed)
  
mu <- mean(train_final$rating)
just_the_sum <- train_final %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), ni = n())
  
rmses[k,] <- sapply(lambdas, function(l){
  predicted_ratings <- test_final %>% 
  left_join(just_the_sum, by='movieId') %>% 
  mutate(b_i = s/(ni+l)) %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
  return(RMSE(predicted_ratings, test_final$rating))})
 }

rmses_cv <- colMeans(rmses)
qplot(lambdas,rmses_cv)
lambda <- lambdas[which.min(rmses_cv)] 

#2.2 happens to be the optimal value for lamda where the RMSE is the lowest.
#Hence, tuning the train set with a lamda of 2.2 for movie effect and check the RMSE generated.

#Model generation and prediction (on the test set)
mu <- mean(train_final$rating)
movie_reg_avgs <- train_final %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
predicted_ratings <- test_final %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

bi_reg_rmse <- RMSE(predicted_ratings, test_final$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularization using only movie",  
                                     RMSE = bi_reg_rmse))
rmse_results

#We can see a negligible improvement after performing regularization. 
#Applying regularization with movie and user effects to check whether that improves the RMSE.


#REGULARISATION OF MOVIE + USER EFFECTS

#Start by setting a value for lamda for movie and user effects each
lambda_i <- 2.2
lambda_u <- seq(0, 8, 0.1)
rmses_2 <- matrix(nrow=10,ncol=length(lambda_u))

#Perform 10-fold cross validation to determine the optimal lambda
for(k in 1:10) {
  train <- edx[cv_splits[[k]],]
  test <- edx[-cv_splits[[k]],]
  
#Make sure userId and movieId in test set are also in the train set
  test_final <- test %>% 
    semi_join(train, by = "movieId") %>%
    semi_join(train, by = "userId")
  
  removed <- anti_join(test, test)
  train_final <- rbind(train, removed)
  
  mu <- mean(train_final$rating)
  
  rmses_2[k,] <- sapply(lambda_u, function(l){
    b_i <- train_final %>% 
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu)/(n()+lambda_i))
    b_u <- train_final %>% 
      left_join(b_i, by="movieId") %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- 
      test_final %>% 
      left_join(b_i, by = "movieId") %>%
      left_join(b_u, by = "userId") %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
    return(RMSE(predicted_ratings, test_final$rating))
  })
}
rmses_2
rmses_2_cv <- colMeans(rmses_2)
rmses_2_cv
qplot(lambda_u,rmses_2_cv)
lambda_u <-lambda_u[which.min(rmses_2_cv)]   

#4.6 happens to be the optimal lambda value for user bias. 
#Hence, we can generate our final model based on regularization of movie and users with their respective optimla values for lamda.

#Model generation and prediction
lambda_i <- 2.2
lambda_u <- 4.6
rmses_3 <- matrix(nrow=10,ncol=length(lambda_u))
mu <- mean(train_final$rating)
b_i_reg <- train_final %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_i))
b_u_reg <- train_final %>% 
  left_join(b_i_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_u))
predicted_ratings <- test_final %>% 
  left_join(b_i_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

biu_reg_rmse <- RMSE(predicted_ratings, test_final$rating)   
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie & User Effects",  
                                     RMSE = biu_reg_rmse))
rmse_results 


#MATRIX FACTORIZATION
set.seed(123, sample.kind = "Rounding") # This is a randomized algorithm

#Convert the train, test and validation sets into recosystem input format
train_data <-  with(train, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating = rating))
test_data  <-  with(test, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating = rating))
validation_data  <-  with(validation, data_memory(user_index = userId, 
                                       item_index = movieId, 
                                       rating = rating))
#Create the model object
r <-  recosystem::Reco()

#Select the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

#Train the algorithm  
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

#Calculate the predicted values  
predicted_ratings_mf <-  r$predict(test_data, out_memory())
head(predicted_ratings_mf, 10)

rmse_mf <- RMSE(test$rating,predicted_ratings_mf) 
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Matrix Factorization",  
                                     RMSE = rmse_mf))
rmse_results 

#Predicting the ratings using the validation set (final hold - out set)
#Calculate the predicted values for the validation set (final hold - out set)
predicted_ratings_mf <-  r$predict(validation_data, out_memory())
head(predicted_ratings_mf, 10)

rmse_mf <- RMSE(validation$rating,predicted_ratings_mf) 
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Matrix Factorization (Validation Set)",  
                                     RMSE = rmse_mf))
rmse_results     