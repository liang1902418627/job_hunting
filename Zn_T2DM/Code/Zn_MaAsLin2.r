# -----------------------------------------
# 锌与肠道菌（属水平）的关联
# 协变量：模型3中的协变量
# 自变量：Zn
# 因变量：肠道菌（属水平）
# 方法：MaAsLin2
# -----------------------------------------
rm(list = ls())

paks <- c("dplyr", "Maaslin2", "tidyr")
lapply(paks, library, character.only = TRUE)
# install.packages("BiocManager")
# BiocManager::install("Maaslin2")
# 读取数据
md <- readRDS(
    "Data/md_final_v2.rds"
)

gut <- readRDS("Data/gut_data.rds")

# 数据检查
names(md)
names(gut)

# 选择数据
gut_rel <- gut$relative %>%
    filter(row.names(.) %in% md$SampleID)

row.names(gut_rel)[1:5]
row.names(md) <- md$SampleID
row.names(md)[1:5]
# 固定效应
fix_ef <- c(
    "Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history", "Serum_Zn"
)

# 创建保存路径--------------------------------------------------------
out_dir <- "Results/Zn_MaAsLin2"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 主分析
# 运行 Maaslin2
Maaslin2(
    input_data = gut_rel, # 菌群相对丰度数据
    input_metadata = md, # 协变量数据
    min_prevalence = 0, # 最小流行率
    normalization = "CLR", # 标准化方法
    output = out_dir, # 输出目录
    fixed_effects = fix_ef, # 固定效应
    transform = "NONE", # 转换方法
    standardize = TRUE, # 标准化
    plot_heatmap = TRUE
)
