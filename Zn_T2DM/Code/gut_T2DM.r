# -------------------------------------
# 差异特征菌（属水平）与2型糖尿病的关联分析
# 自变量：差异特征菌（属水平，相对丰度log10转换后）
# 因变量：2型糖尿病相关表型
# 协变量：模型3 中的协变量
# 方法：逻辑回归、线性回归
# g_Ralstonia
# g_Sediminibacterium
# g_Bradyrhizobium
# g_Adlercreutzia
# g_Pseudidiomarina
# g_Prauseria
# g_SMB53
# g_Halomonas
# g_Phascolarctobacterium
# g_Bilophila

# -------------------------------------
rm(list = ls())

# 加载自编函数及常用r包
source("Code/myfunctions.r")
paks <- c("dplyr", "tidyr")

# 加载包
lapply(paks, library, character.only = TRUE)

# 读取数据
md <- readRDS("Data\\md_final_v2.rds")
gut <- readRDS("Data\\gut_data.rds")

# 数据检查
names(md)
names(gut)
gut_scale <- gut$transformed %>%
    filter(row.names(.) %in% md$SampleID)

# 合并数据
gut_scale$SampleID <- row.names(gut_scale) # 添加样本ID列以便合并
final_df <- merge(md, gut_scale, by = "SampleID") # 合并数据框
str(final_df)
# 查看列名
colnames(final_df)

# 设置暴露向量、结局向量、协变量向量
exposure_vars <- c(
    "g_Ralstonia",
    "g_Sediminibacterium",
    "g_Bradyrhizobium",
    "g_Adlercreutzia",
    "g_Pseudidiomarina",
    "g_Prauseria",
    "g_SMB53",
    "g_Halomonas",
    "g_Phascolarctobacterium",
    "g_Bilophila"
)

outcome_vars <- c(
    "T2DM", "FPG", "IFG_or_T2DM"
)

covariate_vars <- c(
    "Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history"
)

# 进行回归
# 检查变量是否存在于 数据框 中
check_var_exist(exposure_vars, final_df)
check_var_exist(outcome_vars, final_df)
check_var_exist(covariate_vars, final_df)

# 回归分析
result_df <- reg_log_lin(
    exposure_vars = exposure_vars,
    outcome_vars = outcome_vars,
    covariate_vars = covariate_vars,
    data = final_df
)
# 合并数据框

names(result_df)
result_df <- result_df$result_df
out_dir <- "Results\\gut_T2DM"
if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
}
write.csv(result_df, "Results\\gut_T2DM\\genus_T2DM.csv", row.names = FALSE)
# 对IFG单独做逻辑回归----------------------------
final_df_v2 <- final_df %>%
    filter(IFG %in% c("Yes", "No"))

table(final_df_v2$IFG)

result_df_ifg <- reg_log_lin(
    exposure_vars = exposure_vars,
    outcome_vars = c("IFG"),
    covariate_vars = covariate_vars,
    data = final_df_v2
)
names(result_df_ifg)
result_df_ifg <- result_df_ifg$result_df
write.csv(result_df_ifg, "Results\\gut_T2DM\\genus_IFG.csv", row.names = FALSE)
