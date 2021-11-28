# Final -------------------------------------------------------------

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(dabestr)
library(ggpubr)
library(quanteda)
library(quanteda.textstats)
library(schrute)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- schrute::theoffice

# Analyze Speaking Complexity ---------------------------------------------
data <- data %>%
  bind_cols(textstat_readability(.$text,measure = "ARI")) %>% # bind ARI score to each row
  na.omit(data) # omit rows that have "na" values for ARI score

# Aggregate & Arrange Data ------------------------------------------------
data %>% 
  select(character,ARI) %>% # select columns of interest
  group_by(character) %>% # group by character
  summarise(ave_complexity = mean(ARI)) %>% # get average speaking complexity  
  arrange(desc(ave_complexity)) %>% # arrange descending by ave_complexity
  filter(character == "Oscar" | character == "Kevin") # filters to include only Kevin and Oscar   

# Calculate mean difference
Kevin_Oscar_mean_diff <- data %>%
  dabest(character, ARI, 
         idx = c("Kevin", "Oscar"), 
         paired = FALSE) %>% mean_diff()

# Display mean difference and the 95% confidence interval for the comparison
Kevin_Oscar_mean_diff

# Visualize data
Kevin_Oscar_mean_diff %>% plot()


av2 <- data %>%
  filter(season == 9 & episode == 24) %>%
  summarise(ave_complexity = mean(ARI))

data2 <- data %>%
  select(season, episode, imdb_rating)

data2 <- unique(data2)


season = 1
episode = 1
sum = 0
avg = 0
list = data.frame("AVG" = c())
j = 1
k = 0

while(TRUE)
{
    if(data[j, "season"] == 9 & data[j, "episode"] == 24)
    {
      avg = sum / k
      list <- rbind(list, c(avg))
      sum = 0
      k = 0
      while(j < 54757)
      {
        sum = sum + data[j, "ARI"]
        j = j + 1
        k = k + 1
      }
      avg = sum / k
      list <- rbind(list, c(avg))
      sum = 0
      k = 0
      break
    }
    if(data[j, "episode"] == episode)
    {
      sum = sum + data[j, "ARI"]
      j = j + 1
      k = k + 1
    }
    else if(data[j, "episode"] == episode + 1)
    {
      avg = sum / k
      list <- rbind(list, c(avg))
      sum = 0
      k = 0
      episode = episode + 1
    }
    else if(data[j, "episode"] == episode + 2)
    {
      avg = sum / k
      list <- rbind(list, c(avg))
      sum = 0
      k = 0
      episode = episode + 2
    }
    else
    {
      avg = sum / k
      list <- rbind(list, c(avg))
      sum = 0
      k = 0
      episode = 1
      season = season + 1
    }
}

data3 <- cbind(data2, list)

correlation <- cor.test(data3$imdb_rating,data3$ARI,method=c("pearson"))

correlation

ggscatter(data3, x = "imdb_rating", y = "ARI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "IMDB Rating", ylab = "Speaking Complexity")


