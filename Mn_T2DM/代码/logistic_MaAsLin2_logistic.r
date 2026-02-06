rm(list = ls())

# 加载回归分析及数据处理需要的包
packages <- c("dplyr")
lapply(packages, require, character.only = TRUE)
source("代码/myfunctions.r") # 自定义函数

# 加载数据
load("数据/imputed_df_v6.RData") # 元数据
load("数据/genus_data.RData") # 肠道菌数据

# 查看变量名
names(imputed_df_v6)
names(genus_data)
names(genus_data$transformed)

g_trans <- genus_data$transformed
g_trans$SampleID <- row.names(g_trans)

# 合并数据
dt_analysis <- merge(imputed_df_v6, g_trans, by = "SampleID")
names(dt_analysis)

# 确定自变量与因变量、协变量向量
x_var <- "Mn_quartiles"
y_vars <- c(
    "g_Acidaminococcus", "g_Acinetobacter",
    "g_Actinomyces", "g_Akkermansia",
    "g_Alistipes", "g_Anaerostipes",
    "g_Atopobium", "g_Bacteroides",
    "g_Bifidobacterium", "g_Bilophila",
    "g_Blautia", "g_Bradyrhizobium",
    "g_Brevibacterium", "g_Bulleidia",
    "g_Butyricimonas", "g_Catenibacterium",
    "g_Citrobacter", "g_Clostridium",
    "g_Collinsella", "g_Coprobacillus",
    "g_Coprococcus", "g_Desulfovibrio",
    "g_Devosia", "g_Dialister",
    "g_Dorea", "g_Eggerthella",
    "g_Enhydrobacter", "g_Enterobacter",
    "g_Faecalibacterium", "g_Fusobacterium",
    "g_Haemophilus", "g_Halomonas",
    "g_Holdemania", "g_Klebsiella",
    "g_Lachnobacterium", "g_Lachnospira",
    "g_Lacticigenium", "g_Lactobacillus",
    "g_Leptothrix", "g_Megamonas",
    "g_Megasphaera", "g_Methanobrevibacter",
    "g_Methylobacterium", "g_Ochrobactrum",
    "g_Odoribacter", "g_Oscillospira",
    "g_Parabacteroides", "g_Paraprevotella",
    "g_Phascolarctobacterium", "g_Prauseria",
    "g_Pseudidiomarina", "g_Pseudomonas",
    "g_Ralstonia", "g_Roseburia",
    "g_SMB53", "g_Sediminibacterium",
    "g_Serratia", "g_Sphingomonas",
    "g_Staphylococcus", "g_Streptococcus",
    "g_Sutterella", "g_Synergistes",
    "g_Turicibacter", "g_Veillonella",
    "g_Eubacterium", "g_Prevotella",
    "g_Ruminococcus"
)
c_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)
levels(dt_analysis$Mn_quartiles) # 查看自变量水平
# 重新设置参考水平为Q2
dt_analysis$Mn_quartiles <- relevel(dt_analysis$Mn_quartiles, ref = "Q2")
# 进行回归分析（无协变量)
results_no_covar <- reg_log_lin(
    exposure_vars = x_var,
    outcome_vars = y_vars,
    data = dt_analysis,
    covariate_vars = NULL
)
names(results_no_covar)
write.csv(results_no_covar$result_df, "主要结果/表格/linear_MaAsLin2_logistic/linear_Mn_genus_noncovars.csv", row.names = FALSE)
# 进行回归分析（有协变量)
results_covar <- reg_log_lin(
    exposure_vars = x_var,
    outcome_vars = y_vars,
    data = dt_analysis,
    covariate_vars = c_vars
)
write.csv(results_covar$result_df, "主要结果/表格/linear_MaAsLin2_logistic/linear_Mn_genus_covars.csv", row.names = FALSE)

