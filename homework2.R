# Homework #2 -------------------------------------------------------------

library(tidyverse) #gives access to tidyverse packages which are needed
setwd("/home/cao2246/R/Ordonez_Cody_2/datasets.zip/gop_frags/")
#I had to change the above line of code to avoid the following error:
#"Error in setwd("datasets/gop_frags/") : cannot change working directory"
files <- list.files() #initializes "files" as a list of all files in gop_frags
data <- map(files,function(x) read_csv(x))
gop_data <- map2(files,data, function(x,y) cbind(x,y))
gop_df <- do.call(rbind,gop_data)
names(gop_df)[1] <- "date"
df1 <- gop_df %>% 
  separate(date,"date",sep = "\\.") %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  mutate(text_length = nchar(text))
df2 <- df1 %>% 
  group_by(speaker) %>% 
  summarise(talking_turns = n(), 
            total_length = sum(text_length),
            ave_length = mean(text_length)) %>% 
  pivot_longer(-speaker,names_to = "variable", values_to = "value")

