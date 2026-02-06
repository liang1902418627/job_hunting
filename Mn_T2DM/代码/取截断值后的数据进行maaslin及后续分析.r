# =======识别与血清锰相关的差异特征菌===============
# 清除环境
rm(list = ls())

# 加载包
library(tidyverse)
library(Maaslin2)

# 读取数据
load("数据\\imputed_df_v4.RData")
# 菌群数据
df_input_data <- read.csv("数据\\gut.micro_cleaned.csv", row.names = 1)
# metadata
df_input_metadata <- imputed_df_v4[, c(1:25)]
colnames(df_input_metadata)
# 取截断值后的数据
imputed_df_v4 <- imputed_df_v4[imputed_df_v4$ln_Serum_Mn >= 2.64, ]

# 设置行名
rownames(df_input_data) <- df_input_data$SampleID
rownames(df_input_metadata) <- df_input_metadata$SampleID

# 检查数据
colnames(df_input_data)
colnames(df_input_metadata)

## Maaslin分析差异菌群
# 检查固定效应变量是否存在于 df_input_metadata 中
fixed_effects <- c("scale_ln_Serum_Mn")
random_effects <- c(
    "gender",
    "age",
    "BMI",
    "Marital_status",
    "education",
    "occupation",
    "family_income_cat",
    "family_diabetes_history",
    "smoker",
    "Alcohol_consumption",
    "act_sport",
    "energy_kcal",
    "diet_Mn_mg"
) # 随机效应变量

for (var in fixed_effects) {
    if (!var %in% colnames(df_input_metadata)) {
        # 如果变量不存在于 df_input_metadata 中，从 df_input_data 中添加
        df_input_metadata[[var]] <- df_input_data[[var]]
    }
}

# 运行 Maaslin2
fit_data2 <- Maaslin2(
    input_data = df_input_data,
    input_metadata = df_input_metadata,
    min_prevalence = 0,
    normalization = "NONE",
    output = "主要结果/con_maaslin2_output_v2",
    analysis_method = "LM",
    fixed_effects = fixed_effects, # 固定效应
    random_effects = random_effects, # 随机效应
    transform = "NONE"
)

# 提示音
library(beepr)
beep(0)

# 以锰的四分位数识别差异菌
# 固定效应
fixed_effects <- c("Mn_quartiles")
# 随机效应
random_effects <- c(
    "gender",
    "age",
    "BMI",
    "Marital_status",
    "education",
    "occupation",
    "family_income_cat",
    "family_diabetes_history",
    "smoker",
    "Alcohol_consumption",
    "act_sport",
    "energy_kcal",
    "diet_Mn_mg"
) # 随机效应变量

# 运行 Maaslin2
fit_data2 <- Maaslin2(
    input_data = df_input_data,
    input_metadata = df_input_metadata,
    min_prevalence = 0,
    normalization = "NONE",
    output = "主要结果/Q_maaslin2_output",
    analysis_method = "LM",
    fixed_effects = fixed_effects, # 固定效应
    random_effects = random_effects, # 随机效应
    transform = "NONE"
)

# 提示音
library(beepr)
beep(8)
# ============差异特征菌与糖尿病及相关指标的关联（根据四分位数maaslin结果）=============================
rm(list = ls())

# 加载自编函数及常用r包
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v4.RData")

# 查看列名
colnames(imputed_df_v4)
View(imputed_df_v4)

# 将列名后缀为“.y”的列名的后缀去掉
colnames(imputed_df_v4) <- gsub("\\.y$", "", colnames(imputed_df_v4))
# 取截断值后的数据
imputed_df_v4 <- imputed_df_v4[imputed_df_v4$ln_Serum_Mn >= 2.64, ]
# 查看列名
colnames(imputed_df_v4)

data <- imputed_df_v4

# 设置暴露向量、结局向量、协变量向量
exposure_vars <- c(
    "g_Ralstonia",
    "g_Sediminibacterium",
    "g_Turicibacter",
    "g_Lacticigenium",
    "g_Bradyrhizobium",
    "g_Dorea",
    "g_Bifidobacterium",
    "g_Collinsella",
    "g_Ochrobactrum",
    "g_Streptococcus",
    "g_Acinetobacter",
    "g_Lachnospira",
    "g_Acidaminococcus",
    "g_Clostridium",
    "g_Prevotella",
    "g_Anaerostipes",
    "g_Blautia",
    "g_Megamonas",
    "g_Akkermansia",
    "g_Klebsiella"
)

outcome_vars <- c(
    "diabetes",
    "ln_FPG",
    "IFG_or_diabetes"
)

covariate_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "occupation", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 回归
tables <- reg_log_lin(exposure_vars, outcome_vars, covariate_vars, data)
names(tables)
result_df <- tables[[3]]
View(result_df)
write.csv(result_df, "主要结果\\表格\\Q血清锰_genus_outcome.csv", row.names = FALSE)

p <- ggplot(result_df, aes(x = bacterium, y = phenotype, fill = beta)) +
    geom_tile(width = 0.9, height = 0.9) + # 调整方块大小
    geom_text(aes(label = sig_FDR_p), color = "black", size = 4, vjust = 1.5) + # 显著性标注
    scale_fill_gradient2(
        low = "#0509f4", # 蓝色
        mid = "#FFFFFF", # 白色
        high = "#f80808", # 红色
        midpoint = 0, # 中间值
        limits = c(-0.20, 0.20), # 设置颜色范围
        name = "Estimate" # 图例标题
    ) +
    theme_minimal() + # 使用简洁主题
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # 标题样式
        axis.title = element_text(size = 14, face = "bold"), # 轴标题样式
        axis.text = element_text(size = 12), # 轴标签样式
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # x轴标签倾斜
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5), # y轴标签样式
        legend.title = element_text(size = 12, face = "bold"), # 图例标题样式
        legend.text = element_text(size = 10), # 图例文字样式
        legend.position = "right", # 图例位置
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2), # 添加网格线
        panel.grid.minor = element_blank(), # 去掉次要网格线
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # 添加边框
        panel.background = element_rect(fill = "white") # 背景颜色
    ) +
    labs(
        title = "", # 图标题
        x = "", # x轴标题
        y = "" # y轴标题
    )

ggsave(p, file = "主要结果\\图片\\Fig 4-a. Q_genus_diabetes_heatmap_v2.pdf", width = 8, height = 4, dpi = 300) # 保存图片
# ============差异特征菌与糖尿病及相关指标的关联（根据连续性锰maaslin结果）=============================
rm(list = ls())

# 加载自编函数及常用r包
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v4.RData")

# 查看列名
colnames(imputed_df_v4)
View(imputed_df_v4)

# 将列名后缀为“.y”的列名的后缀去掉
colnames(imputed_df_v4) <- gsub("\\.y$", "", colnames(imputed_df_v4))
# 取截断值后的数据
imputed_df_v4 <- imputed_df_v4[imputed_df_v4$ln_Serum_Mn >= 2.64, ]
# 查看列名
colnames(imputed_df_v4)

data <- imputed_df_v4

# 设置暴露向量、结局向量、协变量向量
exposure_vars <- c(
    "g_Ralstonia",
    "g_Sediminibacterium",
    "g_Turicibacter",
    "g_Bradyrhizobium",
    "g_Lacticigenium",
    "g_Ochrobactrum",
    "g_Acidaminococcus",
    "g_Bifidobacterium",
    "g_Clostridium",
    "g_Dorea",
    "g_Klebsiella",
    "g_Odoribacter",
    "g_Streptococcus"
)

outcome_vars <- c(
    "diabetes",
    "ln_FPG",
    "IFG_or_diabetes"
)

covariate_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "occupation", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)
# 回归
tables <- reg_log_lin(exposure_vars, outcome_vars, covariate_vars, data)
names(tables)
result_df <- tables[[3]]
View(result_df)
write.csv(result_df, "主要结果\\表格\\连续血清锰_genus_outcome_截", row.names = FALSE)
# 绘图
# 绘制热图并添加显著性标记
p <- ggplot(result_df, aes(x = bacterium, y = phenotype, fill = beta)) +
    geom_tile(width = 0.9, height = 0.9) + # 调整方块大小
    geom_text(aes(label = sig_FDR_p), color = "black", size = 4, vjust = 1.5) + # 显著性标注
    scale_fill_gradient2(
        low = "#0509f4", # 蓝色
        mid = "#FFFFFF", # 白色
        high = "#f80808", # 红色
        midpoint = 0, # 中间值
        limits = c(-0.20, 0.20), # 设置颜色范围
        name = "Estimate" # 图例标题
    ) +
    theme_minimal() + # 使用简洁主题
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # 标题样式
        axis.title = element_text(size = 14, face = "bold"), # 轴标题样式
        axis.text = element_text(size = 12), # 轴标签样式
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # x轴标签倾斜
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5), # y轴标签样式
        legend.title = element_text(size = 12, face = "bold"), # 图例标题样式
        legend.text = element_text(size = 10), # 图例文字样式
        legend.position = "right", # 图例位置
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2), # 添加网格线
        panel.grid.minor = element_blank(), # 去掉次要网格线
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # 添加边框
        panel.background = element_rect(fill = "white") # 背景颜色
    ) +
    labs(
        title = "", # 图标题
        x = "", # x轴标题
        y = "" # y轴标题
    )

ggsave(p, file = "主要结果\\图片\\Fig 4-b. continute_genus_diabetes_heatmap_截.pdf", width = 8, height = 4, dpi = 300) # 保存图片
