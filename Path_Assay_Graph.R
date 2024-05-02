library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(dplyr)
set.seed(123)
setwd("/Users/Kitchen/Library/CloudStorage/OneDrive-UniversityofKent/2nd Rotation/Lab work/Plant work/Path Test/Scoring")
Path_Test_Scoring <-read.csv("Path Test Scoring.csv")
View(Path_Test_Scoring)    
Path_Test_Scoring %>% 
  sample_n_by(Plant_Type, Infection, Solution, size = 1)
Path_Test_Scoring_Clean <- Path_Test_Scoring[ -c(1,3:5) ]
Long_data <- pivot_longer(Path_Test_Scoring_Clean, 
                          cols = -Treatment_No, 
                          names_to = "Week", 
                          values_to = "Disease_score")
Long_data[,1]= lapply(Long_data[,1], as.factor)
Long_data <- Long_data %>%
  group_by(Treatment_No, Week) %>%
  summarise(Mean_Disease_Score = mean(Disease_score, na.rm = TRUE),
            Standard_Error = sd(Disease_score, na.rm = TRUE) / sqrt(sum(!is.na(Disease_score))))
ggplot(Long_data, aes(x = Week, y = Mean_Disease_Score, group = Treatment_No, color = Treatment_No)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = Mean_Disease_Score - Standard_Error, ymax = Mean_Disease_Score + Standard_Error), width=0.2, colour="black", alpha=0.5, linewidth=0.5)

