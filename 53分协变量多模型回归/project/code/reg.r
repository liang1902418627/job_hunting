# ---------------------------------------------------------
# 题目：回归分析（变量重命名 + 虚拟数据生成）

# 暴露变量：
#   screen_time_group       （分类：低/中/高）
#   daily_screen_hours      （连续：小时数）
#   screen_time_zscore      （连续：标准化后的屏幕时间）
#   screen_time_quartile    （有序：1~4，用于趋势检验）
# 结局变量：
#   phq9_score              （抑郁评分，0-27，连续）

# 协变量模型：
#   model1: NULL
#   model2: age, sex, smoking_status
#   model3: age, sex, race, smoking_status, diabetes, cardiovascular_disease
# ---------------------------------------------------------
rm(list = ls()) # 清空环境变量
# 包载入
paks <- c("dplyr")
lapply(X = paks, FUN = library, character.only = TRUE)

# 数据载入
meta_data <- readRDS(file = "project/data/meta_data_clean.rds")
names(meta_data)
str(meta_data)
# 回归配置
base_info <- list(
  exposure_vars = c("screen_time_group", "daily_screen_hours", "screen_time_zscore", "screen_time_quartile"),
  outcome_var = "phq9_score",
  covariates = list(
    model1 = c(""),
    model2 = c("age", "sex", "smoking_status"),
    model3 = c("age", "sex", "race", "smoking_status", "diabetes", "cardiovascular_disease")
  ),
  data = meta_data
)

# ========工具函数==========================================

# 1. 分类暴露变量的回归函数
run_categorical_regression <- function(data, outcome_var, exposure_cat, exposure_trend, covariates, model_name) {
    is_logistic <- is.factor(data[[outcome_var]]) # 判断是否为分类结局变量
    family <- if (is_logistic) "binomial" else "gaussian" # 选择回归族

    cov_str <- if (length(covariates) == 0) "" else paste(covariates, collapse = " + ")
    formula_cat_str <- if (cov_str == "") {
        paste(outcome_var, "~", exposure_cat)
    } else {
        paste(outcome_var, "~", exposure_cat, "+", cov_str)
    }
    model_fit <- glm(as.formula(formula_cat_str), data = data, family = family) # 拟合模型
    summary_fit <- summary(model_fit)$coefficients # 提取系数摘要
    
    num_levels <- nlevels(data[[exposure_cat]]) # 分类变量水平数
    estimates <- summary_fit[2:num_levels, "Estimate"] # 提取估计值
    std_errors <- summary_fit[2:num_levels, "Std. Error"] # 提取标准误

    format_result <- function(est, se) {
        if (is_logistic) {
            sprintf("%.2f (%.2f, %.2f)", exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se))
        } else {
            sprintf("%.2f (%.2f, %.2f)", est, est - 1.96 * se, est + 1.96 * se)
        }
    } # 格式化结果
    q_results <- sapply(seq_along(estimates), function(i) format_result(estimates[i], std_errors[i]))

    p_trend_val <- NA
    if (!is.na(exposure_trend)) {
        formula_trend_str <- if (cov_str == "") {
            paste(outcome_var, "~", exposure_trend)
        } else {
            paste(outcome_var, "~", exposure_trend, "+", cov_str)
        }
        trend_fit <- glm(as.formula(formula_trend_str), data = data, family = family)
        p_trend_val <- summary(trend_fit)$coefficients[exposure_trend, 4]
    }

    # --- 构建结果 ---
    result_df <- data.frame(Exposure = exposure_cat, Model = model_name, Q1 = "(reference)")
    for(i in 1:(num_levels - 1)) {
        result_df[[paste0("Q", i + 1)]] <- q_results[i]
    }
    result_df$`p for trend` <- if(!is.na(p_trend_val)) sprintf("%.3f", p_trend_val) else NA
    
    return(result_df)
}

# 2. 连续暴露变量的回归函数
run_continuous_regression <- function(data, outcome_var, exposure_cont, covariates, model_name) {
    is_logistic <- is.factor(data[[outcome_var]])
    family <- if (is_logistic) "binomial" else "gaussian"

    cov_str <- if (length(covariates) == 0) "" else paste(covariates, collapse = " + ")
    formula_str <- if (cov_str == "") {
        paste(outcome_var, "~", exposure_cont)
    } else {
        paste(outcome_var, "~", exposure_cont, "+", cov_str)
    }
    model_fit <- glm(as.formula(formula_str), data = data, family = family)
    summary_fit <- summary(model_fit)$coefficients

    # 检查暴露变量是否存在于模型系数中
    if (!exposure_cont %in% rownames(summary_fit)) {
        warning(paste("在模型", model_name, "中, 变量 '", exposure_cont, "' 可能因共线性被移除。"))
        return(data.frame(
            Exposure = exposure_cont,
            Model = model_name,
            `Effect (95% CI)` = "Error: Variable removed",
            `P value` = NA,
            check.names = FALSE
        ))
    }

    # 自适应选择 p 值列名（glm 通常为 Pr(>|z|)，lm 为 Pr(>|t|)）
    p_col <- if ("Pr(>|z|)" %in% colnames(summary_fit)) {
        "Pr(>|z|)"
    } else if ("Pr(>|t|)" %in% colnames(summary_fit)) {
        "Pr(>|t|)"
    } else {
        # 回退：选择最后一列作为 p 值（极端情况下）
        tail(colnames(summary_fit), 1)
    }

    est <- summary_fit[exposure_cont, "Estimate"]
    se <- summary_fit[exposure_cont, "Std. Error"]
    p_val <- summary_fit[exposure_cont, p_col]

    effect_label <- if (is_logistic) "OR (95% CI)" else "Beta (95% CI)"
    effect_val <- if (is_logistic) {
        sprintf("%.2f (%.2f, %.2f)", exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se))
    } else {
        sprintf("%.2f (%.2f, %.2f)", est, est - 1.96 * se, est + 1.96 * se)
    }

    result_df <- data.frame(
        Exposure = exposure_cont,
        Model = model_name,
        `Effect (95% CI)` = effect_val,
        `P value` = sprintf("%.3f", p_val),
        check.names = FALSE
    )
    return(result_df)
}


# ========主分析==========================================

exposure_list <- list(
  list(type = "categorical", cat = "screen_time_group", trend = "screen_time_quartile"),
  list(type = "continuous", cont = "daily_screen_hours"),
  list(type = "continuous", cont = "screen_time_zscore")
)

all_results_list <- lapply(exposure_list, function(exp_item) {
  results_per_exposure <- lapply(names(base_info$covariates), function(model_name) {
    if (exp_item$type == "categorical") {
      run_categorical_regression(
        data = base_info$data,
        outcome_var = base_info$outcome_var,
        exposure_cat = exp_item$cat,
        exposure_trend = exp_item$trend,
        covariates = base_info$covariates[[model_name]],
        model_name = model_name
      )
    } else if (exp_item$type == "continuous") {
      run_continuous_regression(
        data = base_info$data,
        outcome_var = base_info$outcome_var,
        exposure_cont = exp_item$cont,
        covariates = base_info$covariates[[model_name]],
        model_name = model_name
      )
    }
  })
  dplyr::bind_rows(results_per_exposure)
})

final_result_df <- dplyr::bind_rows(all_results_list)

# 查看结果
print(final_result_df)

# 保存结果（可选）
write.csv(final_result_df, "project/results/tables/regression_output_all_exposures.csv", row.names = FALSE, na = "")