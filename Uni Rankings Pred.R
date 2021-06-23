---
title: "University_Ranking_Prediction_(CYO)"
author: "Riya Michael"
date: "22/06/2021"
output:
  pdf_document: default
  html_document: default
---
#The data can be downloaded from the following URL:
#https://www.kaggle.com/mylesoneill/world-university-rankings/download
    
    
#Aim
#To create a prediction model for the universities of the world, that will predict the world rank of a given university after taking into consideration various parameters/ variables.

#For this purpose, let us consider the university score and national rank as our major parameters.

#First, we need to load the packages required to help in building the algorithm 
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(caret)) install.packages("caret")
if(!require(tidyr)) install.packages("tidyr")

#Now we load the data i.e. The University Rankings data set which has been downloaded from Kaggle.
data <- read.csv("UniData.csv")
view(data)
  
#We can extract the general information about the loaded data as demonstrated below:
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


#Now we will implement machine learning techniques to construct our prediction model by considering score, national rank and world rank as our variables with the help of linear regression.
#For this purpose, we will first create train and test data sets.
#I have considered the data from 2012 to 2014 as my train set and 2015 as my test set to evaluate the accuracy of my generated alogrithm.


train <- data %>% filter(year < 2015)
view(train)
test <- data %>% filter(year > 2014)
view(test)

rdata <- sapply(train,is.numeric)

rdata <- data[,rdata]

rdata <- na.omit(rdata)

fall <- c("score", "national_rank")

rdata <- rdata[,!(names(rdata) %in% fall)]

model <- lm(log(world_rank) ~., rdata)

summary(model)

#Now we can test the validity of the model on the test dataset
rdata <- sapply(test,is.numeric)

rdata <- data[,rdata]

rdata <- na.omit(rdata)

fall <- c("score", "national_rank")

rdata <- rdata[,!(names(rdata) %in% fall)]

model <- lm(log(world_rank) ~., rdata)

summary(model)






