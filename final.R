# Final Project: Metrics Tool

# Load libraries (these libraries were used in class assignments and will be used primarily for the speaking complexity analyses)
library(tidyverse)
library(dabestr)
library(quanteda.textstats)

# Load ggpubr library, which is used for the ggscatter plot to display the correlation results
library(ggpubr)

# Load Schrute library, which is used to gain access to the dataset used for the analyses
# The Schrute library contains data from the TV show The Office, including the entire transcript of the series as well as the writer(s), director, episode name, air date, IMDB score, and number of IMDB reviews of each episode
library(schrute)

# Load and save all data from the schrute library
# Note that the relevant Schrute data includes each line of dialogue spoken by every character, as well as IMDB scores for each episode
data <- schrute::theoffice


# Part 1: Is there a meaningful mean difference in speaking complexity between Kevin and Oscar? What about between Jim and Dwight?
# Part 1 uses the following R skills: readability, mean difference estimation, dabest

# Analyze speaking complexity
data <- data %>%
  bind_cols(textstat_readability(.$text,measure = "ARI")) %>% # bind ARI score to each row
  na.omit(data) # omit rows that have "na" values for ARI score

# First, is there a meaningful mean difference in speaking complexity between Kevin and Oscar?
# My prediction is yes, Oscar's speaking complexity is significantly higher than Kevin's speaking complexity

# Aggregate and arrange data
data %>% 
  select(character,ARI) %>% # select columns of interest
  group_by(character) %>% # group by character
  summarise(ave_complexity = mean(ARI)) %>% # get average speaking complexity  
  arrange(desc(ave_complexity)) %>% # arrange descending by ave_complexity
  filter(character == "Oscar" | character == "Kevin") # filters to include only Oscar and Kevin 

# Calculate mean difference using dabest, please note that this process takes up to one minute to run
Kevin_Oscar_mean_diff <- data %>%
  dabest(character, ARI, 
         idx = c("Kevin", "Oscar"), 
         paired = FALSE) %>% mean_diff()

# Display mean difference and the 95% confidence interval for the comparison
Kevin_Oscar_mean_diff

# Visualize data using a plot
Kevin_Oscar_mean_diff %>% plot()

# Next, is there a meaningful mean difference in speaking complexity between Jim and Dwight?
# My prediction is no, Jim and Dwight have no meaningful difference in speaking complexity

# Aggregate and arrange data
data %>% 
  select(character,ARI) %>% # select columns of interest
  group_by(character) %>% # group by character
  summarise(ave_complexity = mean(ARI)) %>% # get average speaking complexity  
  arrange(desc(ave_complexity)) %>% # arrange descending by ave_complexity
  filter(character == "Dwight" | character == "Jim") # filters to include only Dwight and Jim 

# Due to a very large number of lines of dialogue for both Jim and Dwight, the dabest calculations took an extremely long time to run (sometimes crashing R), so it was shortened to include only the first, middle, and last season
newdata <- data %>%
  filter(season == "1" | season == "5" | season == "9")
  
# Calculate mean difference using dabest, please note that this process takes up to two minutes to run
Jim_Dwight_mean_diff <- newdata %>%
  dabest(character, ARI, 
         idx = c("Jim", "Dwight"), 
         paired = FALSE) %>% mean_diff()

# Display mean difference and the 95% confidence interval for the comparison
Jim_Dwight_mean_diff

# Visualize data using a plot
Jim_Dwight_mean_diff %>% plot()


# Part 2: Is there a correlation of statistical significance between IMDB rating and mean speaking complexity per episode?
# Part 2 uses the following R skills: correlation, ggplot2 (ggpubr)
# My prediction is no, there is not a correlation of statistical significance between speaking complexity and IMDB rating

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
      while(rownum <= 54756) # while rownum is less than or equal to the total number of rows in data (54756 rows)
      {
        sum = sum + data[rownum, "ARI"] # add speaking complexity of each line to get sum
        rownum = rownum + 1 # increase rownum by 1 for each iteration
        count = count + 1 # increase count by 1 for each iteration
      }
      avg = sum / count # get episode mean speaking complexity
      list <- rbind(list, c(avg)) # append episode mean speaking complexity to list
      sum = 0 # set sum back to 0 for next iteration
      count = 0 # set count back to 0 for next iteration
      episode = 24 # change episode variable to 24 for clarity, note that this does not effect the code in any way
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
