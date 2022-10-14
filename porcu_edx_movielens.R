# HarvardX - Movielens project
# PORCU Jordan


    # Data set creation (given by edx)

# Loading the packages we will need during the project
library(tidyverse)
library(caret)
library(data.table)

# Creating a temporary file that will be the downloaded folder
dl <- tempfile()

# Assigning the URL outcome to "dl"
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Creating a dataframe from "dl" using columns such as
# "userId","movieId","rating" and "timestamp" in the "ratings.dat" file
# Warning : this might take several minutes to achieve
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Creating a table from "dl" with datas that are located in the "movies.dat" file
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

# Assigning names to "movies" columns
colnames(movies) <- c("movieId", "title", "genres")

# Converting "movies" in a dataframe with right columns names and types
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# Joining "movies" and "ratings" with "movieId" as the common colmun
movielens <- left_join(ratings, movies, by = "movieId")

# Setting the seed to 1 for randomness reproductibility
set.seed(1)

# Creating a list of indexes to split the "movielens" data set in train and test parts
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

# "edx" will be our training set (90% of the original data set)
edx <- movielens[-test_index,]

# "temp" be our test set (10% of the original data set)
temp <- movielens[test_index,]

# "validation" is our new test set, where all the datas are also in "edx"
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding to "edx" what was removed from the "temp" set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Removing variables we wont use anymore
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# RMSE function creation
rmse <- function(prediction,test) {sqrt(mean((test-prediction)^2))}


    # Data analysis

# Print of number of rows and columns of the dataset. 
# First lines of the data set
cat("Number of rows : " , dim(edx)[1])
cat("Number of columns :" , dim(edx)[2])
head(edx)%>%knitr::kable()


# Checks the number of NaNs values in the dataset
sum(is.na(edx))


# Displays the distributions of rating values
mu = mean(edx$rating) # rating mean through all the dataframe
edx %>% 
  ggplot(aes(x=rating)) + 
  geom_histogram(bins=10,color="white",fill="green") +
  geom_vline(xintercept = mu,linetype = "longdash") +
  xlab("Rating") +
  ylab("Number of ratings") +
  ggtitle("Distribution of ratings")


# Displays the number of movies associated to numbers of ratings
edx %>% 
  count(movieId) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins=50,color="white",fill="blue") +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of movies per the number of ratings")


# Essential statistics summary for movieId and its count of values
edx %>% 
  count(movieId) %>% 
  summary()


# Displays the number of movies associated to log10(numbers of ratings)
edx %>% 
  count(movieId) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins=50,color="white",fill="blue") + 
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings (log10 scaled) per the number of movies")


    # Model building

# Creates the rmse of the naive model
rmse_naive <- rmse(mu,validation$rating)


# Creates a list of rating biases grouped by movieId
b_movie <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_movie = mean(rating - mu))


# Displays the distribution of b_movie
b_movie %>% ggplot(aes(b_movie)) +
  geom_histogram(bins=50,color="white",fill="cyan") + 
  geom_vline(xintercept = 0) + 
  ggtitle("Distribution of b_movie")


# Creates a prediction of ratings with movie effect
# Creates the rmse of the movie effect model
predict_movie <- mu + validation %>% 
  left_join(b_movie, by='movieId') %>% 
  pull(b_movie)
rmse_movie <- rmse(predict_movie,validation$rating)


# Creates a list of rating biases grouped by movieId then by userId
b_user <- edx %>% 
  left_join(b_movie, by='movieId') %>% 
  group_by(userId) %>% 
  summarize(b_user = mean(rating - mu - b_movie))



# Displays the distribution of b_user
b_user %>% ggplot(aes(b_user)) + 
  geom_histogram(bins=50,color="white",fill="blue") + 
  geom_vline(xintercept = 0) + 
  ggtitle("Distribution of b_user")


# Creates a prediction of ratings with movie+user effect
# Creates the rmse of the movie+user effect model
pred_user <- validation %>% 
  left_join(b_movie,by="movieId") %>% 
  left_join(b_user,by="userId") %>% 
  summarize(pred = mu+b_user+b_movie) %>% 
  pull(pred)
rmse_user <- rmse(pred_user,validation$rating)


# Creates a list of rating biases grouped by movieId then by userId then again by genres
b_genres <- edx %>% 
  left_join(b_movie, by='movieId') %>% 
  group_by(userId) %>% 
  left_join(b_user,by="userId") %>% 
  group_by(genres) %>% 
  summarize(b_genres = mean(rating - mu - b_movie - b_user))


# Displays the distribution of b_genres
b_genres %>% 
  ggplot(aes(b_genres)) + 
  geom_histogram(bins=50,color="white",fill="purple") + 
  geom_vline(xintercept = 0) +
  ggtitle("Distribution of b_genres")


# Create a dataframe with standard deviation of every effect models
biases <- c("Movie effect","Movie + user effect", "Movie + user + genres effect")
sd <- c(sd(b_movie$b_movie),sd(b_user$b_user),sd(b_genres$b_genres))
sd_df <- tibble(Bias = biases, SD = sd) %>% 
  knitr::kable()


# Creates a prediction of ratings with movie+user+genres effect
# Creates the rmse of the movie+user+genres effect model
pred_genres <- validation %>% 
  left_join(b_movie,by="movieId") %>%
  left_join(b_user,by="userId") %>% 
  left_join(b_genres, by="genres") %>%
  summarize(pred=mu+b_movie+b_user+b_genres) %>% 
  pull(pred)
rmse_genres <- rmse(pred_genres,validation$rating)


# Applies a function that create a movie+user+genres effect model and regularizes it with
# a list of lambdas
list_of_lambdas <- seq(0,15,0.5)
rmses_lambdas <- sapply(list_of_lambdas,function(lambda){
  b_movie <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_movie = sum(rating - mu)/(lambda+n()))
  b_user <- edx %>% 
    left_join(b_movie, by='movieId') %>% 
    group_by(userId) %>% 
    summarize(b_user = sum(rating - mu - b_movie)/(lambda+n()))
  b_genres <- edx %>% 
    left_join(b_movie, by='movieId') %>% 
    group_by(userId) %>% 
    left_join(b_user,by="userId") %>% 
    group_by(genres) %>% 
    summarize(b_genres = sum(rating - mu - b_movie - b_user)/(lambda+n()))
  prediction <- validation %>% 
    left_join(b_movie,by="movieId") %>% 
    left_join(b_user,by="userId") %>% 
    left_join(b_genres, by="genres") %>%
    summarize(pred=mu+b_movie+b_user+b_genres) %>% 
    pull(pred)
  return(rmse(prediction,validation$rating))
})


# Displays a plot rmse = fct(lambda)
rmse_lambda_df <- tibble("lambda" = list_of_lambdas, "rmse" = rmses_lambdas)
rmse_lambda_df %>% 
  ggplot(aes(lambda,rmse)) + 
  geom_point(color="darkblue") +
  geom_line(color="blue") + 
  geom_vline(xintercept = list_of_lambdas[which.min(rmses_lambdas)],linetype="dashed") +   geom_hline(yintercept = min(rmses_lambdas),linetype="dashed") + 
  ggtitle("rmse per lambdas")


# Creates a dataframe with the best lambda selected, and the RMSE associated
best_lambda <- list_of_lambdas[which.min(rmses_lambdas)]
best_rmse <- rmse_lambda_df %>% 
  filter(lambda==best_lambda) %>% 
  pull(rmse)
final_model_result <- tibble("Best lambda"=best_lambda, "Corresponding rmse"=best_rmse) %>% 
  knitr::kable()


    # Results

# Creates a dataframe with RMSE for each models we created
names = c("Naive","Movie only","Movie and user","Movie, user and genres","Optimized final model")
rmses_final = c(rmse_naive,rmse_movie,rmse_user,rmse_genres,best_rmse)
results_df <- tibble("Model" = names, "RMSE" = rmses_final) %>% knitr::kable()
results_df

