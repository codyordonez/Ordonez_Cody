# Homework #3 -------------------------------------------------------------

#Load Libraries 
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

#Set gop_frags as the working directory to gain easy access to the relevant files
setwd("/home/cao2246/R/Ordonez_Cody_2/datasets.zip/gop_frags/")

#Initializes "files" as a list of all 11 files in gop_frags
files <- list.files()

#Load data (which reads and maps files)
data <- map(files,function(x) read_csv(x))

#Looks at data and aggregates in each file, combines by column, still keeps the 11 files separate
gop_data <- map2(files,data, function(x,y) cbind(x,y))

#Looks at gop_data and aggregates all files into one, combines by row
gop_df <- do.call(rbind,gop_data)

#Finds a measure of the complexity of speech by using the "FLesch" readability metric
gop_df <- gop_df %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))

#Aggregates the data and separates by speaker, giving a cleaner, more sorted layout
df1 <- gop_df %>% 
  separate(text, "speaker", sep = ":", remove = FALSE)

#Output the speaker and ave_complexity (using the "Flesch" readability metric) in two columns
df2 <- df1 %>% 
  group_by(speaker) %>%
  summarise(ave_complexity = mean(Flesch))

#Order the speakers from least easily readable (most complex) to most easily readable (least complex)
#Note that in this case, lower values indicate higher average complexity and vice versa
arrange(df2, ave_complexity)

