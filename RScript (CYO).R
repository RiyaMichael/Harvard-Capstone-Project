#Load the packages required 
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(caret)) install.packages("caret")
if(!require(tidyr)) install.packages("tidyr")
if(!require(randomForest)) install.packages("randomForest")

#Download data from Kaggle.com
#url <- "https://www.kaggle.com/mylesoneill/world-university-rankings/download"

#Aim
#To create a prediction model for the universities of the world, that will predict the score of a given university after taking into consideration various parameters/ variables.

#Now we load the data i.e. The University Rankings data set which has been downloaded from Kaggle.
data <- read.csv("UniData.csv")
view(data)

#We can extract the general information about the loaded data as demonstrated below:
dim(data)
head(data, n=5)
tail(data,n=5)
summary(data)

#This shows us the universities that come under the top 5 in the world in addition to the ones that fall at the bottom.

#Now we can filter the data to generate a graph showing the top 10 universities of the world for the most recent year i.e. 2015.

data2015 <- data %>% filter(world_rank<= 10 & year == 2015)

data2015 %>% select(world_rank, institution, year) %>% ggplot(aes(x=institution, y=world_rank)) + geom_bar(stat="identity", fill = "blue") + geom_label(aes(label = institution), size =2)


#Now we can plot a graph showing us the number of Universities per Country
data %>% filter(year == 2015) %>% group_by(country)  %>% summarise(stat_by_country = n_distinct(institution)) %>% arrange(desc(stat_by_country))  %>% ggplot(aes(x = reorder(country,-stat_by_country), y= stat_by_country)) + geom_bar(stat = "identity", fill = "yellow")  + theme(axis.text.x = element_text(angle=90)) + xlab("Country") + ylab("Number of Universities")

#We can observe that the data for some universities changes every year i.e. the number of universities whose data ha been compiled as been varying over the years (for certain countries).
hist(data$score)

#We can ascertain the same by running the following code:

changes <- data %>% filter(year %in% c(2012, 2013, 2014, 2015)) %>% group_by(country,year) %>% summarise(stat_by_country = n_distinct(institution)) %>% spread(year,stat_by_country)

colnames(changes) <- c("Country","Year2012","Year2013","Year2014","Year2015")

changes <- as.data.frame(changes)

changes %>% ggplot() + geom_line(aes(x = Country, y = Year2013, group = 1, color = "yellow"), size = 1) + geom_line(aes(x = Country, y = Year2014, group = 1, color = "orange"), size = 1) + geom_line(aes(x = Country, y = Year2015, group = 1, color = "red"), size = 1) + theme(axis.text.x = element_text(angle = 90)) + xlab("Country") + ylab("Changes")


#Now we can check which specific countries have altered the number of universities (either increased or decreased)

changes$variance <- changes$Year2015 - changes$Year2014

changes %>% select(Country, variance)

changes %>% select(Country, variance) %>% filter(variance>0)

changes %>% select(Country, variance) %>% filter(variance<0)


#Note: Since, the data for the variable "Broad Impact" is not available for all the years, we will be omitting it from our data set.
#Hence, we will use all other parameters, namely, national rank, quality of education, alumni employment, quality of faculty, publications, influence, citations and patents along with score as our main dependent variable.

#For this project, we will use RMSE as our success indicator.

#The two machine learning models used to predict the results are GLM and Random Forests Algorithms.

#For the purpose of cross checking whether our algorithm has been developed successfully, we will divide our data into train and test sets, where the train set will consist of 70% of the data and the test set will consist of the remaining 30%.
#We will fit our algorithm with the help of the train data and then apply it to the test data to generate predicted scores.
#We will then verify the accuracy of our algorithm by comparing the predicted scores and actual scores of the test set.

#1. GLM Model
train_index <- createDataPartition(data$score, times = 1, p = 0.7, list = FALSE)
train <- data[train_index,]
test <- data[-train_index,]

#Fitting the train set using GLM 
fit <- glm(score ~ national_rank + quality_of_education + alumni_employment + quality_of_faculty  + publications +influence + citations + patents, data  = train)

#Fitting the algorithm with the test set and comparing the results
test <- test %>% mutate(pred_score = predict.glm(fit,newdata = test))

#Generating the RMSE to track the error
RMSE(test$score,test$pred_score)

#After removing publications as one of the variables
fit <- glm(score ~ national_rank + quality_of_education + alumni_employment + quality_of_faculty  + influence + citations + patents, data  = train)

test <- test %>% mutate(pred_score = predict.glm(fit,newdata = test))

RMSE(test$score,test$pred_score)

#Since the RMSE reduced after removing publications as one variable, we can observe a more accurate algorithm 

#setting seed
set.seed(1, sample.kind = "Rounding")
#renaming the sets
train_index <- createDataPartition(data$score, times = 1, p = 0.7, list = FALSE)
train <- data[train_index,]
test <- data[-train_index,]

#fitting the glm model after removing publications

fit <- glm(score ~ national_rank + quality_of_education + alumni_employment + quality_of_faculty  + influence + citations + patents, data  = train)

#adding predicted scores to a data frame named 'results'
results <- test %>% mutate(pred_score = predict.glm(fit,newdata = test))

#plotting predicted scores vs actual scores
ggplot(data = results, aes(score, pred_score)) + geom_point(color = 'black') + geom_smooth(method = "lm", se = TRUE) +geom_abline(color = 'yellow')
      
#generating the coefficients 
c <- coefficients(fit)
c[] <- lapply(c, round, 3)
fit$coefficients


#2. Random Forest Method
#Data partitioning
train_index <- createDataPartition(data$score, times = 1, p = 0.5, list = FALSE)
train <- data[train_index,]
test <- data[-train_index,]

#fitting the random forest model to the train set
fit <- randomForest(score ~ national_rank + quality_of_education + alumni_employment + quality_of_faculty  +influence + citations + patents, data = train)
plot(fit)


#Since the error reduces and stabilizes at around 300 trees, setting ntrees = 300
train_rf <- randomForest(score ~ national_rank + quality_of_education + alumni_employment + quality_of_faculty +influence + citations + patents, data = train, ntree=300)
print(train_rf)

#predicting the scores for the test set
pred_score <- predict(train_rf,test)
result <- data.frame(test$score, pred_score)
plot(result)

results <- test %>% mutate(result)
view(results)
ggplot(data = result, aes(test$score, pred_score)) + geom_point(color = 'black') + geom_smooth(method = "lm", se = TRUE) +geom_abline(color = 'yellow')

#Tracking the accuracy
RMSE(test$score,pred_score)


