# ----------------------------------
# 题目：各因素对菌群结构的影响贡献程度
# -----------------------------------
rm(list = ls()) # 清空环境变量

paks <- c("ggplot2", "dplyr", "vegan", "rstatix", "extrafont", "ggpubr")

for (pkg in paks) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 读取数据：元数据，肠道菌相对丰度数据
md <- readRDS("Data/md_final_v2.rds")
gut <- readRDS("Data/gut_data.rds")

# 检查数据
names(md)
names(gut)
gut_rel <- gut$relative %>%
    filter(row.names(.) %in% md$SampleID)
row.names(md) <- md$SampleID
# 计算距离矩阵，使用 Bray-Curtis 距离
bray_dist <- vegdist(gut_rel, method = "bray")

# Adonis 分析
set.seed(123) # 设置随机种子以确保结果可重复
# 确定分析的变量
# Age、Gender、BMI、Education、Marital_status、family_income_cat、Smoking_status、Dringking_status、Energy_intake、Zn_intake、act_sports_intense、Family_diabetes_history
factors <- c(
    "Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat",
    "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake",
    "act_sports_intense", "Family_diabetes_history", "Serum_Zn"
)

# 构建公式函数
build_formula <- function(factor) {
    formula_str <- paste("bray_dist ~", factor)
    as.formula(formula_str)
}
# 先做单个变量的 Adonis 分析测试
res <- adonis2(build_formula("Age"), data = md, permutations = 999)
(tidy(res))

# 逐个变量进行 Adonis 分析并收集主要结果指标R2和p值
adonis_results <- lapply(factors, function(factor) {
    formula <- build_formula(factor)
    adonis_res <- adonis2(formula, data = md, permutations = 999)
    tidy_res <- broom::tidy(adonis_res)
    res_for <- data.frame(
        Factor = factor,
        R2 = tidy_res[1, "R2"],
        p_value = tidy_res[1, "p.value"]
    )
    return(res_for)
})

# 合并结果为数据框
adonis_summary <- do.call(rbind, adonis_results)
# 保存到csv文件
out_dir <- "Results\\Factors_adonis"
write.csv(adonis_summary, file.path(out_dir, "adonis_summary.csv"), row.names = FALSE)
