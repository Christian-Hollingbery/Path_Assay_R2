library(agricolae) 
setwd("/Users/Kitchen/Library/CloudStorage/OneDrive-UniversityofKent/2nd Rotation/Lab work/Plant work/Path Test/Scoring")
Path_Test_AUDPC <-read.csv("Path Test Scoring.csv")
head(Path_Test_AUDPC)
weeks <-c(1:6) # you could use hours and put more time points
factors_1 <- c(2:5)
Path_Test_AUDPC[,2:5]= lapply(Path_Test_AUDPC[,factors_1], as.factor) # you need to make all extra label/ treatment/rep/block columns in your table factors
Path_Test_AUDPC[,12]=audpc(Path_Test_AUDPC[,c(6:11)],weeks) 
colnames(Path_Test_AUDPC)[12]=c('audpc')
write.csv(Path_Test_AUDPC, file = "AUDPC.csv", row.names=FALSE)
view(Path_Test_AUDPC)
set.seed(123)
library(datarium)
Path_Test_AUDPC_Clean <- Path_Test_AUDPC[ -c(6:11) ]
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
  color = "Solution", palette = "jco", facet.by = "Plant_Type"
)
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
Path_Test_AUDPC_Clean %>%
 levene_test(audpc ~ Infection*Plant_Type*Solution)
model <-  lm(audpc ~ Infection*Plant_Type*Solution, data = Path_Test_AUDPC_Clean) #This is one way of doing an anova test, which then enables you to remove interation terms if needed
model
#remove interaction term on quasi model. You need to do this if the three way interaction is not significant.
model_simple <- update(model,~.- Infection:Plant_Type:Solution)
anova(model, model_simple, test = "F") #removal of interaction term does not sig. affect model
#continue with model without interaction term


# Group the data by gender and 
# fit simple two-way interaction 
Path_Test_AUDPC_Clean %>%
  group_by(Plant_Type) %>%
  anova_test(audpc ~ Solution*Infection, error = model_simple)

