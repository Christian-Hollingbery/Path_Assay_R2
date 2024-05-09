library(agricolae) 
setwd("/Users/Kitchen/Library/CloudStorage/OneDrive-UniversityofKent/2nd Rotation/Lab work/Plant work/Path Test/Scoring")
Path_Test_AUDPC <-read.csv("Path Test Scoring.csv")
head(Path_Test_AUDPC)
weeks <-c(1:7) # you could use hours and put more time points
factors_1 <- c(2:5)
Path_Test_AUDPC[,2:5]= lapply(Path_Test_AUDPC[,factors_1], as.factor) # you need to make all extra label/ treatment/rep/block columns in your table factors
Path_Test_AUDPC[,13]=audpc(Path_Test_AUDPC[,c(6:12)],weeks) 
colnames(Path_Test_AUDPC)[13]=c('audpc')
write.csv(Path_Test_AUDPC, file = "AUDPC.csv", row.names=FALSE)
view(Path_Test_AUDPC)
set.seed(123)
library(datarium)
Path_Test_AUDPC_Clean <- Path_Test_AUDPC[ -c(6:12) ]
#Check Random sample
Path_Test_AUDPC_Clean %>%
  sample_n_by(Plant_Type, Infection, Solution, size = 1)
#Look at summary including mean and sd
Path_Test_AUDPC %>%
  group_by(Treatment_No, Plant_Type, Infection, Solution) %>%
  get_summary_stats(audpc, type = "mean_sd")
#Look at Box plot to visulaise
bxp <- ggboxplot(
  Path_Test_AUDPC_Clean, x = "Infection", y = "audpc", 
  color = "Solution", palette = "jco", facet.by = "Plant_Type")
bxp
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
Path_Test_AUDPC %>%
 levene_test(audpc ~ Infection*Plant_Type*Solution) #This tests the homogeneity of varience. IOW, is the varience between the samples equal. Must be above 0.05 p value to show this. 
model <-  anova_test(audpc ~ Infection*Plant_Type*Solution, data = Path_Test_AUDPC) #This is one way of doing an anova test, which then enables you to remove interaction terms if needed
model
#remove interaction term on quasi model. You could do this if the three way interaction is not significant (can only be done with lm rather than anova_test function)
#model_simple <- update(model,~.- Infection:Plant_Type:Solution)
#anova(model, model_simple, test = "F") #removal of interaction term does not sig. affect model if greater than 0.05 p value.
#summary(model_simple)
#continue with model without interaction term

#carry out simple - simple main effects (this is not needed if you do not have a significant 3 way interaction)
#Simple_Main_Effects <- Path_Test_AUDPC %>%
 # group_by(Infection, Solution) %>%
  #anova_test(audpc ~ Plant_Type, error = model_simple) 

#Simple_Main_Effects <- as.data.frame(Simple_Main_Effects)
#Simple_Main_Effects %>% filter(Solution == "E") 
#The code above can be used to just show the rows with Estradiol treatment, but its not neccessary to do this here.

# Do pairwise comparisons
library(emmeans)
pwc <- Path_Test_AUDPC %>%
  group_by(Plant_Type, Infection, Solution) %>%
  emmeans_test(audpc ~ Solution, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic, -p) # Remove details
pwc #This shows a sig difference between E and D when applied to M plants which are infected!
get_emmeans(pwc)
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Treatment_No")
bxp +
  stat_pvalue_manual(
    pwc, color = "Plant_Type", linetype = "Infection", hide.ns = TRUE,
    tip.length = 0, step.increase = 0.1, step.group.by = "Plant_Type"
  ) +
  labs(
    subtitle = get_test_label(model, detailed = TRUE),
    caption = get_pwc_label(pwc)

    mean_audpc <- Path_Test_AUDPC_Clean %>%
      group_by(Treatment_No, Solution) %>%
      summarise(Mean_audpc = mean(audpc, na.rm = TRUE),
                Standard_Error = sd(audpc, na.rm = TRUE) / sqrt(sum(!is.na(audpc))))
    
    bar <-  ggbarplot(
      mean_audpc, x = "Treatment_No", y = "Mean_audpc",
      fill = "Solution", palette = "jco") +
      geom_errorbar(aes(ymin = Mean_audpc - Standard_Error, ymax = Mean_audpc + Standard_Error), width=0.2, colour="black", alpha=0.5, linewidth=0.5)
    bar
    bar +
      stat_pvalue_manual(
        pwc,hide.ns = TRUE,
        tip.length = 0, step.increase = 0.1, step.group.by = "Infection"
      ) 


  

