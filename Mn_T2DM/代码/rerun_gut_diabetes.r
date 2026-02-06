# -------------------------------------------------
# 题目：具体菌与IFG_or_T2D的关联分析
# 菌的相对丰度
# --------------------------------------------------
rm(list = ls())

# 加载自编函数及常用r包
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData") # metadata
load("数据\\genus_data.RData") # genus

# 合并数据
genus_data$transformed$SampleID <- row.names(genus_data$transformed)
imputed_df_v4 <- merge(imputed_df_v6, genus_data$transformed, by = "SampleID")

# 查看列名
colnames(imputed_df_v4)
# View(imputed_df_v4)

data <- imputed_df_v4 %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2"))

# 设置暴露向量、结局向量、协变量向量
exposure_vars <- c(
  "g_Bradyrhizobium",
  "g_Ralstonia",
  "g_Acidaminococcus",
  "g_Sphingomonas",
  "g_Sediminibacterium",
  "g_Lachnospira"
)

outcome_vars <- c(
    "diabetes",
    "IFG_or_diabetes"
)

covariate_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

tables <- reg_log_lin(exposure_vars, outcome_vars, covariate_vars, data)
names(tables)
result_df <- tables[[3]]
View(result_df)
write.csv(result_df, "主要结果\\表格\\连续血清锰_genus_outcome.csv", row.names = FALSE)