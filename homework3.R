# Homework #3 -------------------------------------------------------------

#Load Libraries 
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

#Set gop_frags as the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2/datasets.zip/gop_frags/")


#Initializes "files" as a list of all 11 files in gop_frags
files <- list.files()

#Load Data
data <- map(files,function(x) read_csv(x))
gop_data <- map2(files,data, function(x,y) cbind(x,y)) 
gop_df <- do.call(rbind,gop_data)
gop_df <- gop_df %>% 
  bind_cols(textstat_readability(.$text,measure = c("Flesch")))
names(gop_df)[1] <- "date"

df1 <- gop_df %>% 
  separate(text, "speaker", sep = ":", remove = FALSE) %>% 
  mutate(text_length = nchar(text))

df1 <- df1 %>% corpus(docid_field = "status_id", unique_docnames = FALSE)

df2 <- df1 %>% 
  group_by(speaker) %>%
  summarise(ave_complexity = mean(Flesch))

arrange(df2, desc(ave_complexity))


