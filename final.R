# Final Project: Metrics Tool

# Pre-flight

# Load libraries 
library(tidyverse)
library(dabestr)
library(ggpubr)
library(quanteda)
library(quanteda.textstats)
library(schrute) # this library is used to gain access to the relevant dataset which will be used for the analyses

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- schrute::theoffice # saves dataset from the schrute library


# Part 1: Is there a meaningful mean difference in speaking complexity between Oscar and Kevin in The Office?

# Analyze speaking complexity
data <- data %>%
  bind_cols(textstat_readability(.$text,measure = "ARI")) %>% # bind ARI score to each row
  na.omit(data) # omit rows that have "na" values for ARI score

# Aggregate and arrange data
data %>% 
  select(character,ARI) %>% # select columns of interest
  group_by(character) %>% # group by character
  summarise(ave_complexity = mean(ARI)) %>% # get average speaking complexity  
  arrange(desc(ave_complexity)) %>% # arrange descending by ave_complexity
  filter(character == "Oscar" | character == "Kevin") # filters to include only Oscar and Kevin, the two characters the analyses will be focusing on   

# Calculate mean difference using dabest, please note that this process takes up to 30 seconds to run
Kevin_Oscar_mean_diff <- data %>%
  dabest(character, ARI, 
         idx = c("Kevin", "Oscar"), 
         paired = FALSE) %>% mean_diff()

# Display mean difference and the 95% confidence interval for the comparison
Kevin_Oscar_mean_diff

# Visualize data using a plot
Kevin_Oscar_mean_diff %>% plot()


# Part 2: Is there a correlation of statistical significance between IMDB rating and mean speaking complexity per episode?

# Create a new data frame containing each episode and its IMDB rating
data2 <- data %>%
  select(season, episode, imdb_rating) %>%
  unique

# Get the mean speaking complexity for each episode using a loop

# Count variables which will be used in the loop
season = 1 # season number
episode = 1 # episode number (will go back to 1 at the end of beginning of each new season)
rownum = 1 # current row number (there are 54756 total rows in data)
count = 0 # counts the number of lines in an episode (will go back to 0 at the beginning of a new episode)
sum = 0 # sum of the speaking complexity scores of each line in an episode (will go back to 0 at the beginning of a new episode)
avg = 0 # average speaking complexity of an episode, which is calculated by dividing sum by count (will go back to 0 at the beginning of a new episode)

# New data frame called list which will contain the mean speaking complexity of each episode
list = data.frame("AVG" = c()) # list starts empty but each iteration of the loop adds one episode's mean speaking complexity to list in order, starting with season 1 episode 1

# Each iteration of this loop adds an episode's mean speaking complexity to "list", please note that this process takes up to one minute to run
while(TRUE) # continuous loop until a break
{
    if(data[rownum, "season"] == 9 & data[rownum, "episode"] == 24) # if the loop reaches the last episode of the series
    {
      avg = sum / count # get episode mean speaking complexity
      list <- rbind(list, c(avg)) # append episode mean speaking complexity to list
      sum = 0 # set sum back to 0 for next iteration
      count = 0 # set count back to 0 for next iteration
      while(rownum < 54757) # while rownum is less than or equal to the total number of rows in data (54756 rows)
      {
        sum = sum + data[rownum, "ARI"] # add speaking complexity of each line to get sum
        rownum = rownum + 1 # increase rownum by 1 for each iteration
        count = count + 1 # increase count by 1 for each iteration
      }
      avg = sum / count # get episode mean speaking complexity
      list <- rbind(list, c(avg)) # append episode mean speaking complexity to list
      sum = 0 # set sum back to 0 for next iteration
      count = 0 # set count back to 0 for next iteration
      break # this breaks out of the loop, meaning the list is complete
    }
    if(data[rownum, "episode"] == episode) # if in the next line of the next row, the current episode has not changed
    {
      sum = sum + data[rownum, "ARI"] # add speaking complexity of each line to get sum
      rownum = rownum + 1 # increase rownum by 1 for each iteration
      count = count + 1 # increase count by 1 for each iteration
    }
    else if(data[rownum, "episode"] == episode + 1) # if in the next line of the next row, the current episode has changed and is now one higher
    {
      avg = sum / count # get episode mean speaking complexity
      list <- rbind(list, c(avg)) # append episode mean speaking complexity to list
      sum = 0 # set sum back to 0 for next iteration
      count = 0 # set count back to 0 for next iteration
      episode = episode + 1 # increase episode by 1 for next iteration
    }
    else if(data[rownum, "episode"] == episode + 2) # if in the next line of the next row, the current episode has changed and is now two higher
    {
      avg = sum / count # get episode mean speaking complexity
      list <- rbind(list, c(avg)) # append episode mean speaking complexity to list
      sum = 0 # set sum back to 0 for next iteration
      count = 0 # set count back to 0 for next iteration
      episode = episode + 2 # increase episode by 2 for next iteration
    }
    else # if in the next line of the next row, the current season has changed and it is now one higher
    {
      avg = sum / count # get episode mean speaking complexity
      list <- rbind(list, c(avg)) # append episode mean speaking complexity to list
      sum = 0 # set sum back to 0 for next iteration
      count = 0 # set count back to 0 for next iteration
      episode = 1 # set episode back to 1 for next iteration
      season = season + 1 # increase season by 1 for next iteration
    }
}

# Combine the data frames to get a data frame containing each episode with both their IMDB rating and mean speaking complexity
data3 <- cbind(data2, list)

# Conduct correlation test to determine the correlation between IMDB rating and average speaking complexity
correlation <- cor.test(data3$imdb_rating,data3$ARI,method=c("pearson"))

# Display correlation results
correlation

# Visualize correlation test results using a plot
ggscatter(data3, x = "imdb_rating", y = "ARI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IMDB Rating", ylab = "Speaking Complexity")
