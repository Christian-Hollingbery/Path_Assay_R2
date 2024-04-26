set.seed(123)
library(datarium)
Path_Test_AUDPC[ -c(6:11) ] %>%
sample_n_by(Plant_Type, Infection, Solution, size = 1)
Path_Test_AUDPC %>%
  group_by(Plant_Type, Infection, Solution) %>%
  get_summary_stats(audpc, type = "mean_sd")
bxp <- ggboxplot(
  Path_Test_AUDPC, x = "Infection", y = "audpc", 
  color = "Plant_Type", palette = "jco", facet.by = "Solution"
)
bxp
