# ------------------------------------
# 题目：亚组分析 (基于 jstable 文档重构)
# 新逻辑：循环处理每个分层变量
# ------------------------------------

# 1. 环境准备
# ------------------------------------
# 清理环境
rm(list = ls())

# 加载必要的包
paks <- c("dplyr", "readr", "jstable", "purrr")
# 检查并加载 'jstable' 包
if (!require("jstable")) {
  install.packages("jstable")
  library(jstable)
}
invisible(lapply(paks, require, character.only = TRUE))

# 加载自定义函数 (如果存在)
if (file.exists("Code/myfunctions.r")) {
  source("Code/myfunctions.r")
}

# 2. 数据加载与预处理
# ------------------------------------
# 检查并读取数据
if (!file.exists("Data/md_final_v2.rds")) {
    stop("错误：数据文件 'Data/md_final_v2.rds' 未找到。")
}
dt <- readRDS("Data/md_final_v2.rds")
names(dt)
# 创建一个用于操作的副本
dt2 <- dt

# --- 数据清洗和转换 ---
# 年龄分组
dt2 <- dt2 %>%
    mutate(
        Age_group = factor(ifelse(Age < 65, "<65", "≥65"), levels = c("<65", "≥65"))
    )

# 稳健地将二分类结局变量转换为 0/1 整数
# 这是确保 'binomial' family 正常工作的关键
to_binary <- function(var) {
  as.integer(var %in% c(1, "1", TRUE, "TRUE", "Yes", "YES"))
}
dt2 <- dt2 %>%
    mutate(
        T2DM = to_binary(T2DM),
        IFG_or_T2DM = to_binary(IFG_or_T2DM)
    )

# 确保暴露变量是具有正确参考水平的因子
dt2 <- dt2 %>%
  mutate(
    Serum_Zn_Q4 = factor(Serum_Zn_Q4, levels = c("Q1", "Q2", "Q3", "Q4")),
    # 使用 relevel 明确设置 Q1 为参考组
    Serum_Zn_Q4 = relevel(Serum_Zn_Q4, ref = "Q1")
  )

# 3. 变量定义
# ------------------------------------
# 暴露变量
exposure_var <- "Serum_Zn_Q4"
# 分层变量池
var_subgroup <- c("Age_group", "Gender", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "act_sports_intense", "Family_diabetes_history")
# 协变量池
var_cov <- c("Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history")

# 检查所有变量是否存在于数据中，防止因变量名错误导致失败
all_vars_needed <- unique(c("T2DM", "IFG_or_T2DM", "IFG", "FPG", exposure_var, var_subgroup, var_cov))
missing_vars <- setdiff(all_vars_needed, names(dt2))
if (length(missing_vars) > 0) {
  warning(paste("警告：以下变量在数据中缺失：", paste(missing_vars, collapse = ", ")))
}


# 4. 亚组分析新逻辑：循环 + 合并
# ------------------------------------
# 定义一个统一的函数来执行此逻辑，以提高代码复用性

run_subgroup_analysis <- function(outcome_var, outcome_family) {
  
  # 创建一个列表来存储每个分层变量的分析结果
  results_list <- list()
  
  # 使用 `lapply` 或 `for` 循环遍历每个分层变量
  # `lapply` 返回一个列表，更简洁
  results_list <- lapply(var_subgroup, function(subgroup_var) {
    
    # 确定当前的协变量集合
    # 1. 协变量不能是当前的分层变量自身
    current_covs <- setdiff(var_cov, subgroup_var)
    
    # 2. 特殊规则：如果按年龄组分层，则协变量中不应再包含连续的年龄变量
    if (subgroup_var == "Age_group") {
      current_covs <- setdiff(current_covs, "Age")
    }
    
    # 构建公式
    formula_str <- paste(outcome_var, "~", exposure_var)
    
    # 使用 TableSubgroupGLM 函数进行亚组分析
    # 这个函数每次只处理一个分层变量，正好符合我们的循环逻辑
    sub_result <- TableSubgroupGLM(
      formula = as.formula(formula_str),
      var_subgroup = subgroup_var,
      var_cov = current_covs,
      data = dt2,   # 或 dt3
      family = outcome_family
    )

    # 关键：把 list 提取为 data.frame
    if (is.list(sub_result) && "Table" %in% names(sub_result)) {
      sub_result <- sub_result$Table
    }
    sub_result <- as.data.frame(sub_result)
    
    return(sub_result)
  })
  
  # 使用 `bind_rows` 将列表中的所有数据框（结果表格）合并成一个大表格
  final_table <- bind_rows(results_list)
  
  return(final_table)
}

# 结果写出前：将 list 列安全转换为可写出的字符列
flatten_list_columns <- function(df) {
  list_cols <- vapply(df, is.list, logical(1))
  if (any(list_cols)) {
    df[list_cols] <- lapply(df[list_cols], function(x) {
      vapply(x, function(v) {
        if (is.null(v)) {
          NA_character_
        } else if (length(v) == 0) {
          NA_character_
        } else if (length(v) == 1) {
          as.character(v)
        } else {
          paste(v, collapse = "; ")
        }
      }, character(1))
    })
  }
  df
}

# 5. 运行所有结局变量的分析
# ------------------------------------
# 1. 结局：T2DM (二分类)
res_t2dm_final <- run_subgroup_analysis("T2DM", "binomial")
# View(res_t2dm_final)
# 2. 结局：IFG_or_T2DM (二分类)
res_ifg_or_t2dm_final <- run_subgroup_analysis("IFG_or_T2DM", "binomial")
# View(res_ifg_or_t2dm_final)

# 3. 结局：FPG (连续型)
res_fpg_final <- run_subgroup_analysis("FPG", "gaussian")
# View(res_fpg_final)
# 对于IFG--------------
# 选择子集
table(dt2$IFG)
dt3 <- dt2 %>% filter(IFG %in% c("Yes", "No"))
table(dt3$IFG)
run_subgroup_analysis <- function(outcome_var, outcome_family) {
  
  # 创建一个列表来存储每个分层变量的分析结果
  results_list <- list()
  
  # 使用 `lapply` 或 `for` 循环遍历每个分层变量
  # `lapply` 返回一个列表，更简洁
  results_list <- lapply(var_subgroup, function(subgroup_var) {
    
    # 确定当前的协变量集合
    # 1. 协变量不能是当前的分层变量自身
    current_covs <- setdiff(var_cov, subgroup_var)
    
    # 2. 特殊规则：如果按年龄组分层，则协变量中不应再包含连续的年龄变量
    if (subgroup_var == "Age_group") {
      current_covs <- setdiff(current_covs, "Age")
    }
    
    # 构建公式
    formula_str <- paste(outcome_var, "~", exposure_var)
    
    # 使用 TableSubgroupGLM 函数进行亚组分析
    # 这个函数每次只处理一个分层变量，正好符合我们的循环逻辑
    sub_result <- TableSubgroupGLM(
      formula = as.formula(formula_str),
      var_subgroup = subgroup_var,
      var_cov = current_covs,
      data = dt3,
      family = outcome_family
    )

    # 关键：把 list 提取为 data.frame
    if (is.list(sub_result) && "Table" %in% names(sub_result)) {
      sub_result <- sub_result$Table
    }
    sub_result <- as.data.frame(sub_result)
    
    return(sub_result)
  })
  
  # 使用 `bind_rows` 将列表中的所有数据框（结果表格）合并成一个大表格
  final_table <- bind_rows(results_list)
  
  return(final_table)
}

res_ifg_final <- run_subgroup_analysis("IFG", "binomial")
View(res_ifg_final)
# 保存结果---------------------------
out_dir <- "Results\\subgroup\\Zn_Q4"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
write.csv(res_t2dm_final, file = file.path(out_dir, "subgroup_T2DM_Zn_Q4.csv"), row.names = FALSE)
write.csv(res_ifg_or_t2dm_final, file = file.path(out_dir, "subgroup_IFG_or_T2DM_Zn_Q4.csv"), row.names = FALSE)
write.csv(res_fpg_final, file = file.path(out_dir, "subgroup_FPG_Zn_Q4.csv"), row.names = FALSE)
write.csv(res_ifg_final, file = file.path(out_dir, "subgroup_IFG_Zn_Q4.csv"), row.names = FALSE)
# 对于暴露psd_Serum_Zn--------------------------------
# 定义一个统一的函数来执行此逻辑，以提高代码复用性
# 暴露变量
exposure_var <- "psd_Serum_Zn"
run_subgroup_analysis <- function(outcome_var, outcome_family) {
  
  # 创建一个列表来存储每个分层变量的分析结果
  results_list <- list()
  
  # 使用 `lapply` 或 `for` 循环遍历每个分层变量
  # `lapply` 返回一个列表，更简洁
  results_list <- lapply(var_subgroup, function(subgroup_var) {
    
    # 确定当前的协变量集合
    # 1. 协变量不能是当前的分层变量自身
    current_covs <- setdiff(var_cov, subgroup_var)
    
    # 2. 特殊规则：如果按年龄组分层，则协变量中不应再包含连续的年龄变量
    if (subgroup_var == "Age_group") {
      current_covs <- setdiff(current_covs, "Age")
    }
    
    # 构建公式
    formula_str <- paste(outcome_var, "~", exposure_var)
    
    # 使用 TableSubgroupGLM 函数进行亚组分析
    # 这个函数每次只处理一个分层变量，正好符合我们的循环逻辑
    sub_result <- TableSubgroupGLM(
      formula = as.formula(formula_str),
      var_subgroup = subgroup_var,
      var_cov = current_covs,
      data = dt2,
      family = outcome_family
    )

    # 关键：把 list 提取为 data.frame
    if (is.list(sub_result) && "Table" %in% names(sub_result)) {
      sub_result <- sub_result$Table
    }
    sub_result <- as.data.frame(sub_result)
    
    return(sub_result)
  })
  
  # 使用 `bind_rows` 将列表中的所有数据框（结果表格）合并成一个大表格
  final_table <- bind_rows(results_list)
  
  return(final_table)
}

# 5. 运行所有结局变量的分析
# ------------------------------------
# 1. 结局：T2DM (二分类)
res_t2dm_final <- run_subgroup_analysis("T2DM", "binomial") %>%
  as.data.frame()
res_t2dm_final <- flatten_list_columns(res_t2dm_final)
View(res_t2dm_final)
write.csv(res_t2dm_final, file = "Results\\subgroup\\psd_Zn\\subgroup_T2DM_psd_Zn.csv", row.names = FALSE)
# 2. 结局：IFG_or_T2DM (二分类)
res_ifg_or_t2dm_final <- run_subgroup_analysis("IFG_or_T2DM", "binomial")
res_ifg_or_t2dm_final <- flatten_list_columns(res_ifg_or_t2dm_final)
# View(res_ifg_or_t2dm_final)

# 3. 结局：FPG (连续型)
res_fpg_final <- run_subgroup_analysis("FPG", "gaussian")
res_fpg_final <- flatten_list_columns(res_fpg_final)
# View(res_fpg_final)

# 4. 运行IFG的分析--------------
# 选择子集
table(dt2$IFG)
dt3 <- dt2 %>% filter(IFG %in% c("Yes", "No"))
table(dt3$IFG)

run_subgroup_analysis <- function(outcome_var, outcome_family) {
  
  # 创建一个列表来存储每个分层变量的分析结果
  results_list <- list()
  
  # 使用 `lapply` 或 `for` 循环遍历每个分层变量
  # `lapply` 返回一个列表，更简洁
  results_list <- lapply(var_subgroup, function(subgroup_var) {
    
    # 确定当前的协变量集合
    # 1. 协变量不能是当前的分层变量自身
    current_covs <- setdiff(var_cov, subgroup_var)
    
    # 2. 特殊规则：如果按年龄组分层，则协变量中不应再包含连续的年龄变量
    if (subgroup_var == "Age_group") {
      current_covs <- setdiff(current_covs, "Age")
    }
    
    # 构建公式
    formula_str <- paste(outcome_var, "~", exposure_var)
    
    # 使用 TableSubgroupGLM 函数进行亚组分析
    # 这个函数每次只处理一个分层变量，正好符合我们的循环逻辑
    sub_result <- TableSubgroupGLM(
      formula = as.formula(formula_str),
      var_subgroup = subgroup_var,
      var_cov = current_covs,
      data = dt3,
      family = outcome_family
    )

    # 关键：把 list 提取为 data.frame
    if (is.list(sub_result) && "Table" %in% names(sub_result)) {
      sub_result <- sub_result$Table
    }
    sub_result <- as.data.frame(sub_result)
    
    return(sub_result)
  })
  
  # 使用 `bind_rows` 将列表中的所有数据框（结果表格）合并成一个大表格
  final_table <- bind_rows(results_list)
  
  return(final_table)
}

res_ifg_final <- run_subgroup_analysis("IFG", "binomial")
res_ifg_final <- flatten_list_columns(res_ifg_final)
View(res_ifg_final)
# 保存结果---------------------------
out_dir <- "Results\\subgroup\\psd_Zn"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}
# 把NA值写成空字符串，方便查看
res_t2dm_final[is.na(res_t2dm_final)] <- ""
res_ifg_or_t2dm_final[is.na(res_ifg_or_t2dm_final)] <- ""
res_fpg_final[is.na(res_fpg_final)] <- ""
res_ifg_final[is.na(res_ifg_final)] <- ""

write.csv(res_t2dm_final, file = file.path(out_dir, "subgroup_T2DM_psd_Zn.csv"), row.names = FALSE)
write.csv(res_ifg_or_t2dm_final, file = file.path(out_dir, "subgroup_IFG_or_T2DM_psd_Zn.csv"), row.names = FALSE)
write.csv(res_fpg_final, file = file.path(out_dir, "subgroup_FPG_psd_Zn.csv"), row.names = FALSE)
write.csv(res_ifg_final, file = file.path(out_dir, "subgroup_IFG_psd_Zn.csv"), row.names = FALSE)
