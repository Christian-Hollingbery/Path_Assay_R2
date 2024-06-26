library(ggpubr)
library(dplyr)
set.seed(123)
setwd("/Users/Kitchen/Library/CloudStorage/OneDrive-UniversityofKent/2nd Rotation/Lab work/Plant work/Path Test/Scoring")
Path_Test_Scoring <-read.csv("Path Test Scoring.csv")
View(Path_Test_Scoring)    
Path_Test_Scoring %>% 
  sample_n_by(Plant_Type, Infection, Solution, size = 1)
Four_Weeks <- Path_Test_Scoring[ -c(1,3:5,10:12) ]
Four_Weeks_Long <- pivot_longer(Four_Weeks, 
                                cols = -Treatment_No, 
                                names_to = "Week", 
                                values_to = "Disease_score")
Four_Weeks_Long[,1]= lapply(Four_Weeks_Long[,1], as.factor)
Four_Weeks_Long <- Four_Weeks_Long %>%
  group_by(Treatment_No, Week) %>%
  summarise(Mean_Disease_Score = mean(Disease_score, na.rm = TRUE),
            Standard_Error = sd(Disease_score, na.rm = TRUE) / sqrt(sum(!is.na(Disease_score))))
ggplot(Four_Weeks_Long, aes(x = Week, y = Mean_Disease_Score, group = Treatment_No, color = Treatment_No)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = Mean_Disease_Score - Standard_Error, ymax = Mean_Disease_Score + Standard_Error), width=0.2, colour="black", alpha=0.5, linewidth=0.5)

library(agricolae) 
setwd("/Users/Kitchen/Library/CloudStorage/OneDrive-UniversityofKent/2nd Rotation/Lab work/Plant work/Path Test/Scoring")
Path_Test_AUDPC <-read.csv("Path Test Scoring.csv")
head(Path_Test_AUDPC)
weeks <-c(1:4) # you could use hours and put more time points
factors_1 <- c(2:5)
Path_Test_AUDPC[,2:5]= lapply(Path_Test_AUDPC[,factors_1], as.factor)
Path_Test_AUDPC <-  Path_Test_AUDPC[-c(10:12)]
# you need to make all extra label/ treatment/rep/block columns in your table factors
Path_Test_AUDPC[,10]=audpc(Path_Test_AUDPC[,c(6:9)],weeks) 
colnames(Path_Test_AUDPC)[10]=c('audpc')
write.csv(Path_Test_AUDPC, file = "AUDPC.csv", row.names=FALSE)
view(Path_Test_AUDPC)
set.seed(123)
library(datarium)
Path_Test_AUDPC_Clean <- Path_Test_AUDPC[ -c(6:9) ]
#Check Random sample
Path_Test_AUDPC_Clean %>%
  sample_n_by(Plant_Type, Infection, Solution, size = 1)
#Look at summary including mean and sd
Path_Test_AUDPC %>%
  group_by(Treatment_No, Plant_Type, Infection, Solution) %>%
  get_summary_stats(audpc, type = "mean_sd")
#Look at Box plot to visulaise
bxp_4 <- ggboxplot(
  Path_Test_AUDPC_Clean, x = "Infection", y = "audpc", 
  color = "Solution", palette = "jco", facet.by = "Plant_Type")
bxp_4
#Look for outliers
Path_Test_AUDPC_Clean %>%
  group_by(Treatment_No, Plant_Type, Infection, Solution) %>%
  identify_outliers(audpc)
#Create regression model 
Normality  <- lm(audpc ~ Infection*Plant_Type*Solution, data = Path_Test_AUDPC_Clean)
# Create a QQ plot of residuals
ggqqplot(residuals(Normality))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(Normality))
#Code below does not work because Treatment 3 only has scores of 1 and you need to have different scores to do this test.
Path_Test_AUDPC_Clean %>%
  group_by(Infection, Plant_Type, Solution, Treatment_No) %>%
  shapiro_test(audpc)
ggqqplot(Path_Test_AUDPC_Clean, "audpc", ggtheme = theme_bw()) +
  facet_grid(Infection + Plant_Type ~ Solution, labeller = "label_both")
Path_Test_AUDPC_Clean %>%
  levene_test(audpc ~ Infection*Plant_Type*Solution) #This tests the homogeneity of varience. IOW, is the varience between the samples equal. Must be above 0.05 p value to show this. 
model <-  anova_test(audpc ~ Infection*Plant_Type*Solution, data = Path_Test_AUDPC_Clean) #This is one way of doing an anova test, which then enables you to remove interaction terms if needed
model
#remove interaction term on quasi model. You need to do this if the three way interaction is not significant.
model_simple <- update(model,~.- Infection:Plant_Type:Solution)
anova(model, model_simple, test = "F") #removal of interaction term does not sig. affect model if greater than 0.05 p value.
summary(model_simple)