# ----------------------------------------------------------------
# 血清锌与2型糖尿病关联分析
# 自变量：Serum_Zn_Q4_num、Serum_Zn_Q4、psd_Serum_Zn
# 因变量：T2DM、FPG、IFG、IFG_or_T2DM
# 协变量：
# 模型1: 
# 模型2：Age、Gender
# 模型3：Age、Gender、BMI、Education、Marital_status、family_income_cat、Smoking_status、Dringking_status、Energy_intake、Zn_intake、act_sports_intense、Family_diabetes_history
# 结果验证：风暴统计平台
# ------------------------------------------------------------------
rm(list = ls()) # 清空环境变量
setwd("C:/lianghao/lianghao/Zn_Gut_T2DM/")
# 加载需要的r包
paks <- c("dplyr") # 需要加载的R包
lapply(paks, function(x) {
    if (!require(x, character.only = T)) {
        install.packages(x, dependencies = T)
        library(x, character.only = T)
    }
})

# 读取处理好的数据
meta_data <- readRDS("Data/md_final_v2.rds")
names(meta_data)
# 数据检查
# 缺失值检查
missing_values <- sapply(meta_data, function(x) sum(is.na(x)))
print(missing_values)

# 变量类型检查
variable_types <- sapply(meta_data, class)
print(variable_types)

# 回归配置
base_info <- list(
  exposure_vars = c("Serum_Zn_Q4_num", "Serum_Zn_Q4", "psd_Serum_Zn"),
  outcome_var = c("T2DM", "FPG", "IFG_or_T2DM"),
  covariates = list(
    model1 = c(),
    model2 = c("Age", "Gender"),
    model3 = c("Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history")
  ),
  data = meta_data
)

# 结果验证
fit <- glm(T2DM ~ Serum_Zn_Q4 + Age + Gender, data = meta_data, family = "binomial")
summary(fit)
exp(coef(fit)); exp(confint(fit))

# ========工具函数==========================================

# 1. 分类暴露变量的回归函数
run_categorical_regression <- function(data, outcome_var, exposure_cat, exposure_trend, covariates, model_name) {
    is_logistic <- is.factor(data[[outcome_var]])
    family <- if (is_logistic) "binomial" else "gaussian"

    # --- 分类模型 ---
    cov_str <- if (length(covariates) > 0) paste("+", paste(covariates, collapse = " + ")) else ""
    formula_cat_str <- paste(outcome_var, "~", exposure_cat, cov_str)
    model_fit <- glm(as.formula(formula_cat_str), data = data, family = family)
    summary_fit <- summary(model_fit)$coefficients
    
    num_levels <- nlevels(data[[exposure_cat]])
    estimates <- summary_fit[2:num_levels, "Estimate"]
    std_errors <- summary_fit[2:num_levels, "Std. Error"]

    format_result <- function(est, se) {
        if (is_logistic) {
            sprintf("%.2f (%.2f, %.2f)", exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se))
        } else {
            sprintf("%.2f (%.2f, %.2f)", est, est - 1.96 * se, est + 1.96 * se)
        }
    }
    q_results <- sapply(seq_along(estimates), function(i) format_result(estimates[i], std_errors[i]))

    # --- 趋势检验模型 ---
    p_trend_val <- NA
    if (!is.na(exposure_trend)) {
        formula_trend_str <- paste(outcome_var, "~", exposure_trend, cov_str)
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

    cov_str <- if (length(covariates) > 0) paste("+", paste(covariates, collapse = " + ")) else ""
    formula_str <- paste(outcome_var, "~", exposure_cont, cov_str)
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

# 1. 定义要分析的暴露变量列表
#    type: 'categorical' 或 'continuous'
#    cat: 分类变量名, trend: 趋势检验变量名
#    cont: 连续变量名
exposure_list <- list(
    list(type = "categorical", cat = "Serum_Zn_Q4", trend = "Serum_Zn_Q4_num"),
    list(type = "continuous", cont = "psd_Serum_Zn")
)

# 2. 对每个暴露变量、每个结果变量和每个模型进行分析
all_results_list <- lapply(base_info$outcome_var, function(current_outcome) {
    
    results_per_outcome <- lapply(exposure_list, function(exp_item) {
        
        results_per_exposure <- lapply(names(base_info$covariates), function(model_name) {
            
            if (exp_item$type == "categorical") {
                run_categorical_regression(
                    data = base_info$data,
                    outcome_var = current_outcome, # 使用单个结果变量
                    exposure_cat = exp_item$cat,
                    exposure_trend = exp_item$trend,
                    covariates = base_info$covariates[[model_name]],
                    model_name = model_name
                )
            } else if (exp_item$type == "continuous") {
                run_continuous_regression(
                    data = base_info$data,
                    outcome_var = current_outcome, # 使用单个结果变量
                    exposure_cont = exp_item$cont,
                    covariates = base_info$covariates[[model_name]],
                    model_name = model_name
                )
            }
        })
        
        # 合并单个暴露变量的所有模型结果
        results_df <- dplyr::bind_rows(results_per_exposure)
        # 在结果中加入当前分析的结果变量名
        results_df <- dplyr::mutate(results_df, Outcome = current_outcome, .before = 1)
        return(results_df)
    })
    
    # 使用 dplyr::bind_rows 合并单个暴露变量的所有模型结果
    dplyr::bind_rows(results_per_outcome)
})

# 3. 将所有暴露变量的分析结果合并成一个最终的数据框
final_result_df <- dplyr::bind_rows(all_results_list)

# 4. 查看并保存结果
View(final_result_df)
write.csv(final_result_df, "Results\\Zn_T2DM_Association\\Zn_T2DM_Association_Results.csv", row.names = FALSE)
# 对于IFG结局单独做---------------------------------------------------------
# 取子集
table(meta_data$IFG, useNA = "ifany")
meta_data_IFG <- meta_data %>%
    filter(IFG %in% c("No", "Yes"))
table(meta_data_IFG$IFG, useNA = "ifany")

# 回归配置
base_info <- list(
  exposure_vars = c("Serum_Zn_Q4_num", "Serum_Zn_Q4", "psd_Serum_Zn"),
  outcome_var = c("IFG"),
  covariates = list(
    model1 = c(),
    model2 = c("Age", "Gender"),
    model3 = c("Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history")
  ),
  data = meta_data_IFG
)
# ========工具函数==========================================

# 1. 分类暴露变量的回归函数
run_categorical_regression <- function(data, outcome_var, exposure_cat, exposure_trend, covariates, model_name) {
    is_logistic <- is.factor(data[[outcome_var]])
    family <- if (is_logistic) "binomial" else "gaussian"

    # --- 分类模型 ---
    cov_str <- if (length(covariates) > 0) paste("+", paste(covariates, collapse = " + ")) else ""
    formula_cat_str <- paste(outcome_var, "~", exposure_cat, cov_str)
    model_fit <- glm(as.formula(formula_cat_str), data = data, family = family)
    summary_fit <- summary(model_fit)$coefficients
    
    num_levels <- nlevels(data[[exposure_cat]])
    estimates <- summary_fit[2:num_levels, "Estimate"]
    std_errors <- summary_fit[2:num_levels, "Std. Error"]

    format_result <- function(est, se) {
        if (is_logistic) {
            sprintf("%.2f (%.2f, %.2f)", exp(est), exp(est - 1.96 * se), exp(est + 1.96 * se))
        } else {
            sprintf("%.2f (%.2f, %.2f)", est, est - 1.96 * se, est + 1.96 * se)
        }
    }
    q_results <- sapply(seq_along(estimates), function(i) format_result(estimates[i], std_errors[i]))

    # --- 趋势检验模型 ---
    p_trend_val <- NA
    if (!is.na(exposure_trend)) {
        formula_trend_str <- paste(outcome_var, "~", exposure_trend, cov_str)
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

    cov_str <- if (length(covariates) > 0) paste("+", paste(covariates, collapse = " + ")) else ""
    formula_str <- paste(outcome_var, "~", exposure_cont, cov_str)
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

# 1. 定义要分析的暴露变量列表
#    type: 'categorical' 或 'continuous'
#    cat: 分类变量名, trend: 趋势检验变量名
#    cont: 连续变量名
exposure_list <- list(
    list(type = "categorical", cat = "Serum_Zn_Q4", trend = "Serum_Zn_Q4_num"),
    list(type = "continuous", cont = "Serum_Zn"),
    list(type = "continuous", cont = "psd_Serum_Zn")
)

# 2. 对每个暴露变量、每个结果变量和每个模型进行分析
all_results_list <- lapply(base_info$outcome_var, function(current_outcome) {
    
    results_per_outcome <- lapply(exposure_list, function(exp_item) {
        
        results_per_exposure <- lapply(names(base_info$covariates), function(model_name) {
            
            if (exp_item$type == "categorical") {
                run_categorical_regression(
                    data = base_info$data,
                    outcome_var = current_outcome, # 使用单个结果变量
                    exposure_cat = exp_item$cat,
                    exposure_trend = exp_item$trend,
                    covariates = base_info$covariates[[model_name]],
                    model_name = model_name
                )
            } else if (exp_item$type == "continuous") {
                run_continuous_regression(
                    data = base_info$data,
                    outcome_var = current_outcome, # 使用单个结果变量
                    exposure_cont = exp_item$cont,
                    covariates = base_info$covariates[[model_name]],
                    model_name = model_name
                )
            }
        })
        
        # 合并单个暴露变量的所有模型结果
        results_df <- dplyr::bind_rows(results_per_exposure)
        # 在结果中加入当前分析的结果变量名
        results_df <- dplyr::mutate(results_df, Outcome = current_outcome, .before = 1)
        return(results_df)
    })
    
    # 使用 dplyr::bind_rows 合并单个暴露变量的所有模型结果
    dplyr::bind_rows(results_per_outcome)
})

# 3. 将所有暴露变量的分析结果合并成一个最终的数据框
final_result_df <- dplyr::bind_rows(all_results_list)

# 4. 查看并保存结果
View(final_result_df)
write.csv(final_result_df, "Results\\Zn_T2DM_Association\\Zn_T2DM_Association_Results_IFG.csv", row.names = FALSE)
