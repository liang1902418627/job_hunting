# ---------------------------------------------
# 题目：血清锰与具体菌的关联，四分位数
# 以具体菌（属）相对丰度，不转换，跑MaAsLin2
# --------------------------------------------------
rm(list = ls()) # 清空环境变量

paks <- c("tidyverse", "Maaslin2")
lapply(paks, require, character.only = TRUE)

# 读取数据----------------------------------
load("数据\\imputed_df_v6.RData", verbose = TRUE) # 读取数据
load("数据\\genus_data.RData", verbose = TRUE) # 读取数据

df_input_data <- genus_data$relative
df_input_metadata <- imputed_df_v6

# 检查数据
colnames(df_input_data)
colnames(df_input_metadata)
row.names(df_input_metadata) <- imputed_df_v6$SampleID
df_input_metadata <- df_input_metadata[df_input_metadata$SampleID %in% row.names(df_input_data), ]
df_input_metadata <- df_input_metadata %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2")) # 设置自变量参照

## Maaslin分析差异菌群
# 固定效应
fixed_effects <- c(
    "Mn_quartiles",
    "gender",
    "age",
    "BMI",
    "Marital_status",
    "education",
    "family_income_cat",
    "family_diabetes_history",
    "smoker",
    "Alcohol_consumption",
    "act_sport",
    "energy_kcal",
    "diet_Mn_mg"
)

# 检查固定效应变量是否存在于 df_input_metadata 中
print(fixed_effects[which(!fixed_effects %in% colnames(df_input_metadata))]) # 打印不存在的固定效应变量

# 1. 看数据矩阵里有没有 NA
any(is.na(df_input_data)) # TRUE → 有缺失
sum(is.na(df_input_data)) # 具体多少缺失值
# 2. 看 metadata 里有没有 NA（尤其是 fixed_effects 列）
df_input_metadata[fixed_effects] %>%
    summarise_all(~ sum(is.na(.))) # 每列 NA 计数
for (i in fixed_effects) {
    if (is.factor(df_input_metadata[[i]])) {
        a <- table(df_input_metadata[[i]]) # 打印每个固定效应变量的频数表
        cat("Variable:", i)
        print(a)
    }
}
str(df_input_data) # 查看数据结构
str(df_input_metadata[fixed_effects])

# 运行 MaAsLin2 分析------------------------------
fit_data2 <- Maaslin2(
    input_data = df_input_data, # 菌群相对丰度数据
    input_metadata = df_input_metadata, # 协变量数据
    min_prevalence = 0, # 最小流行率
    normalization = "NONE", # 标准化方法
    output = "主要结果/四分位数_maaslin2_output_rel", # 输出目录
    fixed_effects = fixed_effects, # 固定效应
    # random_effects = random_effects, # 随机效应
    transform = "NONE", # 转换方法
    standardize = TRUE, # 标准化
    reference = c(
        "Mn_quartiles,Q2",
        "gender,Female",
        "education,Junior_high_school_or_below",
        "family_income_cat,<3000",
        "family_diabetes_history,No",
        "smoker,Nonsmokers",
        "Alcohol_consumption,Neverdrinking",
        "act_sport,No"
    ), # 设置参照组
    plot_heatmap = FALSE
)
