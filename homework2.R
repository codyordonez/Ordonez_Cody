# Homework #2 -------------------------------------------------------------

library(tidyverse) #gives access to tidyverse packages which are needed
setwd("/home/cao2246/R/Ordonez_Cody_2/datasets.zip/gop_frags/") #I had to change the above line of code to avoid the following error: #"Error in setwd("datasets/gop_frags/") : cannot change working directory"
files <- list.files() #initializes "files" as a list of all 11 files in gop_frags
data <- map(files,function(x) read_csv(x)) #initializes "data" to contain the text of each of the 11 files in gop_frags
gop_data <- map2(files,data, function(x,y) cbind(x,y)) #initializes "gop_data" to contain the text of each of the 11 files in gop_frags and combines by columns
gop_df <- do.call(rbind,gop_data) #initializes "gop_df" to call "gop_data" and combines by rows
names(gop_df)[1] <- "date" #adds the date for each file
df1 <- gop_df %>% #runs through each file to find the number of words spoken by each politician
  separate(date,"date",sep = "\\.") %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  mutate(text_length = nchar(text))
df2 <- df1 %>% #gives a clean layout to show the number of turns speaking, total number of words, and average number of words per turn of each politican
  group_by(speaker) %>% 
  summarise(talking_turns = n(), 
            total_length = sum(text_length),
            ave_length = mean(text_length)) %>% 
  pivot_longer(-speaker,names_to = "variable", values_to = "value")

