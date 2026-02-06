# --------------------------------------------------------
# 题目：绘制基线表格
# 分层变量：Eating_time_window_cat、Eating_frequency_cat
# 其它变量：Age、Gender、Racial、Education、Marriage、Economic.status、
# BMI、Smoking、Drinking、HBP、DM、CVD、DEI、mortstat、CVDmortality、CancerMortality
# --------------------------------------------------------
rm(list = ls())
setwd("D:/lianghao/孙莹数据分析20251215/")

# 加载r包
packages <- c("dplyr", "gtsummary", "summarytools", "flextable", "officer", "cards","cardx") # 需要加载的R包

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

load_packages(packages)

# 读取数据
dt <- readRDS("workspace/projects/data/final_analysis_data.rds")
names(dt)
# 选取变量
vars <- c(
    "Eating_time_window_cat", "Eating_frequency_cat",
    "Age", "Gender", "Racial", "Education", "Marriage", "Economic.status", "BMXBMI",
    "BMI", "Smoking", "Drinking", "sleep_time","DEI", "CARB_average", "FAT_average", "PROT_average", "PA_total", 
    "mortstat", "HBP", "DM", "CVDmortality", "CancerMortality"
)
# 检查变量是否存在于数据框中
missing_vars <- setdiff(vars, names(dt))
if (length(missing_vars) > 0) {
    stop(sprintf("数据缺少字段：%s", paste(missing_vars, collapse = ", ")))
}
# 缺失值检查--------------------------
sapply(dt[vars], function(x) sum(is.na(x)))
summary(dt$Eating_freq)

table(dt$Eating_freq)
# 定义用于汇总的变量（不包括分组变量）
summary_vars <- c(
    "Age", "Gender", "Racial", "Education", "Marriage", "Economic.status", "BMXBMI",
    "BMI", "Smoking", "Drinking",  "sleep_time",  "PA_total","CARB_average", "FAT_average", "PROT_average", 
    "DEI", "HBP", "DM","mortstat", "CVDmortality", "CancerMortality"
)

# 表1：按 Eating_time_window_cat 分组
table_time <- dt %>%
  dplyr::select(all_of(summary_vars), Eating_time_window_cat) %>%
  gtsummary::tbl_summary(
    by = Eating_time_window_cat,
    type = list(all_dichotomous() ~ "categorical"),
    statistic = list(
        all_continuous() ~ "{mean}±{sd}",
        all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 2))
  ) %>%
  gtsummary::add_p(
    test = list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"),
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  gtsummary::add_overall()

# 表2：按 Eating_frequency_cat 分组
table_freq <- dt %>%
  dplyr::select(all_of(summary_vars), Eating_frequency_cat) %>%
  gtsummary::tbl_summary(
    by = Eating_frequency_cat,
    type = list(all_dichotomous() ~ "categorical"),
    statistic = list(
        all_continuous() ~ "{mean}±{sd}",
        all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 2))
  ) %>%
  gtsummary::add_p(
    test = list(all_continuous() ~ "oneway.test", all_categorical() ~ "chisq.test"),
    pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 3)
  ) %>%
  gtsummary::add_overall()

# 将两个表格左右合并
table_merged <- gtsummary::tbl_merge(
  tbls = list(table_time, table_freq),
  tab_spanner = c("**Eating Window**", "**Eating Frequency**")
) %>%
  gtsummary::modify_header(label = "**Variable**") %>%
  gtsummary::bold_labels()

print(table_merged)

# 保存基线表为Word文档
path <- "workspace/projects/results/tables/baseline_table/"
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}
gtsummary::as_flex_table(table_merged) %>%
  flextable::save_as_docx(path = file.path(path, "baseline_table_merged.docx"))
