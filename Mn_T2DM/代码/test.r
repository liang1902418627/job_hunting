rm(list = ls())
library(dplyr)
load("数据/imputed_df_v6.RData", verbose = TRUE)
load("数据/genus_data.RData", verbose = TRUE)
# 检查数据
names(genus_data)
names(imputed_df_v6)
# 合并数据
alpha <- genus_data$alpha_diversity
merge_dt <- merge(imputed_df_v6, alpha, by.x = "SampleID", by.y = "SampleID")
nrow(merge_dt)

# 保存数据
write.csv(merge_dt, file = "数据/Mn_T2DM_alpha.csv", row.names = FALSE)
