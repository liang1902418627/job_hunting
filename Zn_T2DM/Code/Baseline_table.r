,# -----------------------------------------------------------------------------------------------------------
# 绘制基线表，并保存为word文档
# -----------------------------------------------------------------------------------------------------------

rm(list = ls())

# 加载r包
packages <- c("dplyr", "gtsummary", "summarytools", "flextable", "officer", "cards","cardx") # 需要加载的R包
# 使用自定义库路径（避免系统库被占用导致安装/加载失败）
.libPaths("C:/R/library")
# 设计函数，如果不存在上述r包，则指出哪个r包不存在，并安装后加载；如果存在，则直接加载
load_packages <- function(packages) {
  # 找出未安装的包
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  # 加载并返回加载结果（隐式返回）
  loaded <- vapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Package not available after installation: ", pkg)
      return(FALSE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
    TRUE
  }, logical(1))
  loaded
}
library(cards)
library(cardx)
load_packages(packages)
# 读取数据
MetaData <- readRDS("Data/md_final_v2.rds")
names(MetaData) # 查看变量名
str(MetaData) # 查看数据结构
sapply(MetaData, class) # 查看变量类型
# 选取变量
vars <- c(
  "Age", "Gender", "Education", "Marital_status", "family_income_cat", 
  "BMI", "Smoking_status", "Dringking_status", "act_sports_intense", 
  "Energy_intake", "Zn_intake", 
  "FPG", "Family_diabetes_history", 
  "Serum_Zn_Q4", "Serum_Zn", 
  "T2DM", "IFG", "IFG_or_T2DM"
)

# 检查变量是否存在于数据框中
missing_vars <- setdiff(vars, names(MetaData)) # 差集：存在于vars但不存在于数据框列名中的变量
cat("Missing variables in the dataset: ", paste(missing_vars, collapse = ", "), "\n")

# 创建基线表
table1 <- MetaData %>%
  select(all_of(vars)) %>%
  tbl_summary(
    by = "Serum_Zn_Q4", # 按Zn四分位数分组
    type = list(all_dichotomous() ~ "categorical"),
    statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
    missing = "no", # 不显示缺失值
    percent = "column", # 按列显示百分比
    digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2) # 设置小数位数
  ) %>%
  add_overall() %>%
  add_p(
    test = list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"), # 设置统计检验方法
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()
print(table1)

# 保存基线表为Word文档
table1_flex <- as_flex_table(table1)
save_as_docx(table1_flex, path = "Results\\baseline_table\\baseline_table.docx")

# 针对IFG结局，另外做一个基线表，再合并到第一个基线表中---------------------
# 数据子集

MetaData_IFG <- MetaData %>%
  filter(IFG %in% c("Yes", "No")) %>% # 仅保留IFG为Yes或No的行
  droplevels()
# 选取变量
vars <- c(
  "Age", "Gender", "Education", "Marital_status", "family_income_cat", 
  "BMI", "Smoking_status", "Dringking_status", "act_sports_intense", 
  "Energy_intake", "Zn_intake", 
  "FPG", "Family_diabetes_history", 
  "Serum_Zn_Q4", "Serum_Zn", 
  "IFG", "IFG_or_T2DM"
)
# 创建IFG基线表
table2 <- MetaData_IFG %>%
  select(all_of(vars)) %>%
  tbl_summary(
    by = "Serum_Zn_Q4", # 按Zn四分位数分组
    type = list(all_dichotomous() ~ "categorical"),
    statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
    missing = "no", # 不显示缺失值
    percent = "column", # 按列显示百分比
    digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2) # 设置小数位数
  ) %>%
  add_overall() %>%
  add_p(
    test = list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"), # 设置统计检验方法
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label = "**Variable**") %>%
  bold_labels()
print(table2)
# 保存基线表为Word文档
table1_flex2 <- as_flex_table(table2)
save_as_docx(table1_flex2, path = "Results\\baseline_table\\baseline_table_辅助.docx")
