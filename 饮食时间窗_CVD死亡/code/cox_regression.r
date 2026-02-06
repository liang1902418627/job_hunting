# -------------------------------------------------
# 题目：暴露因素与结局的关系分析——Cox回归模型 (不加权版本)
# 暴露变量：Eating_frequency_cat、Eating_freq、Eating_window、
# Eating_time_window_cat
# 结局变量：mortstat、CVDmortality、CancerMortality
# 生存时间：time
# 协变量：
# 模型1：不校正协变量
# 模型2：校正Age，Gender，Racial
# 模型3：Age，Gender，Racial，Education，Marriage，Economic.status，BMI，Smoking，Drinking，
# HBP，DM，DEI,CARB_average、FAT_average、PROT_average、PA_total、sleep_time
# 方法：标准Cox比例风险回归模型
# ----------------------------------------------------
rm(list = ls())
setwd("D:/lianghao/孙莹数据分析20251215/")
# 移除 survey 包, 因为不再需要加权分析
paks <- c("survival", "survminer", "dplyr", "readr", "tidyverse")
# install.packages(setdiff(paks, rownames(installed.packages())))
lapply(paks, require, character.only = TRUE)

# 读取数据
dt <- readRDS("workspace/projects/data/final_analysis_data.rds")
table(dt$Eating_frequency_cat)
# 把结局变量转为数值型
dt2 <- dt %>%
    mutate(across(c(mortstat, CVDmortality, CancerMortality), ~ as.numeric(as.character(.))))
table(dt2$mortstat)
table(dt2$CVDmortality)
table(dt2$CancerMortality)
# 重新设置参考水平为T2
dt2$Eating_frequency_cat <- relevel(dt2$Eating_frequency_cat, ref = "T2")
dt2$Eating_time_window_cat <- relevel(dt2$Eating_time_window_cat, ref = "T2")
levels(dt2$Eating_frequency_cat)
levels(dt2$Eating_time_window_cat)
# 配置基本信息-------------------------------------
base_info <- list(
    time_var = "time",
    status_vars = c("mortstat", "CVDmortality", "CancerMortality"),
    exposure_vars = c(
        "Eating_frequency_cat",
        "Eating_freq", "Eating_window", "Eating_time_window_cat"
    ),
    covariates_model1 = NULL,
    covariates_model2 = c("Age", "Gender", "Racial"),
    covariates_model3 = c(
        "Age", "Gender", "Racial", "Education", "Marriage", "Economic.status",
        "BMI", "Smoking", "Drinking", "HBP", "DM", "DEI", "CARB_average", "FAT_average", "PROT_average", "PA_total", "sleep_time"
    ),
    data = dt2
)
# 检查结局的缺失情况
sapply(dt2[base_info$status_vars], function(x) sum(is.na(x)))
# 检查协变量的缺失情况
sapply(dt2[base_info$covariates_model3], function(x) sum(is.na(x)))

# 拟合标准Cox比例风险模型（增加稳健性，处理完全分离）-----------------------------
fit_cox_robustly <- function(data, time_var, status_var, exposure_var, covariates) {
    current_covariates <- covariates
    removed_covariates <- c()

    while (TRUE) {
        # 构建公式
        covariate_str <- if (length(current_covariates) == 0) {
            ""
        } else {
            paste("+", paste(current_covariates, collapse = "+"))
        }
        formula_str <- paste0("Surv(", time_var, ", ", status_var, ") ~ ", exposure_var, " ", covariate_str)
        formula <- as.formula(formula_str)

        # 使用 tryCatch 捕获警告和错误
        fit_result <- tryCatch(
            {
                # 拟合标准Cox模型
                model <- coxph(formula, data = data, iter.max = 100)

                # 检查是否有系数为NA（这是分离的另一个迹象）
                if (any(is.na(coef(model)))) {
                    # 找到NA系数对应的变量
                    na_coef_name <- names(coef(model)[is.na(coef(model))])
                    # 移除变量名中的因子水平部分
                    problem_var <- gsub("([A-Za-z0-9_.]+).*", "\\1", na_coef_name[1])

                    # 抛出一个自定义的警告，以便被捕获
                    warning(paste("beta may be infinite for variable", problem_var))
                }

                list(model = model, warning = NULL)
            },
            warning = function(w) {
                list(model = NULL, warning = w)
            }
        )

        # 检查是否捕获到关于分离的警告
        if (!is.null(fit_result$warning) && grepl("beta may be infinite", fit_result$warning$message)) {
            # 从警告信息中提取有问题的变量名
            # 警告格式通常是: Log-likelihood converged before variable X; beta may be infinite
            problem_var <- sub(".*variable (\\S+);.*", "\\1", fit_result$warning$message)
            # 另一种可能的警告格式
            if (!problem_var %in% current_covariates) {
                problem_var <- sub(".*variable (\\S+).*", "\\1", fit_result$warning$message)
            }

            if (problem_var %in% current_covariates) {
                cat(sprintf("  [INFO] 完全分离警告: 正在移除协变量 '%s' 并重试...\n", problem_var))
                current_covariates <- setdiff(current_covariates, problem_var)
                removed_covariates <- c(removed_covariates, problem_var)
            } else if (grepl(exposure_var, problem_var)) {
                # 如果问题出在暴露变量本身，则无法继续，返回NULL
                cat(sprintf("  [ERROR] 暴露变量 '%s' 导致完全分离，无法拟合模型。\n", exposure_var))
                return(list(model = NULL, removed_covariates = problem_var))
            } else {
                # 如果无法识别变量，则停止循环以避免死循环
                cat(sprintf("  [ERROR] 无法从警告中识别要移除的变量: %s\n", fit_result$warning$message))
                return(list(model = NULL, removed_covariates = removed_covariates))
            }
        } else {
            # 如果没有警告或警告与分离无关，则返回成功拟合的模型
            return(list(model = fit_result$model, removed_covariates = removed_covariates))
        }
    }
}


# 循环拟合所有模型-----------------------------------
results <- list()

for (status_var in base_info$status_vars) {
    cat(paste0("\n--- 开始处理结局变量: ", status_var, " ---\n"))

    current_data <- base_info$data %>%
        mutate(event_status = as.numeric(!!sym(status_var) == 1))

    for (exposure_var in base_info$exposure_vars) {
        cat(paste0("  - 暴露变量: ", exposure_var, "\n"))

        for (model_num in 1:3) {
            covariates <- switch(model_num,
                base_info$covariates_model1,
                base_info$covariates_model2,
                base_info$covariates_model3
            )

            # 使用稳健的Cox模型拟合函数
            robust_fit <- fit_cox_robustly(
                current_data,
                base_info$time_var,
                "event_status",
                exposure_var,
                covariates
            )

            cox_model <- robust_fit$model
            removed_vars <- robust_fit$removed_covariates

            # 如果模型拟合失败，则跳过
            if (is.null(cox_model)) {
                cat(sprintf("    Model %d: 拟合失败，跳过。\n", model_num))
                next
            }

            model_summary <- summary(cox_model)

            # 智能提取暴露变量的结果
            exposure_indices <- grep(paste0("^", exposure_var), rownames(model_summary$coefficients))

            if (length(exposure_indices) == 0) {
                warning(paste("No coefficients found for exposure", exposure_var, "in model with outcome", status_var))
                next
            }

            hr <- model_summary$conf.int[exposure_indices, "exp(coef)"]
            ci_lower <- model_summary$conf.int[exposure_indices, "lower .95"]
            ci_upper <- model_summary$conf.int[exposure_indices, "upper .95"]
            p_values <- model_summary$coefficients[exposure_indices, "Pr(>|z|)"]

            exposure_term_names <- rownames(model_summary$coefficients)[exposure_indices]

            result_df <- data.frame(
                Model = paste0("Model_", model_num),
                Exposure = exposure_var,
                Term = exposure_term_names,
                Outcome = status_var,
                HR = hr,
                CI_Lower = ci_lower,
                CI_Upper = ci_upper,
                P_Value = p_values,
                Removed_Covariates = paste(removed_vars, collapse = ", "),
                row.names = NULL
            )
            results[[length(results) + 1]] <- result_df
        }
    }
}

final_results <- do.call(rbind, results)

# 定义输出文件路径
output_file_path <- "workspace/projects/results/tables/multicox/cox_regression_results_unweighted_complete_separate_process.csv"
output_dir <- dirname(output_file_path)
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
}
write.csv(final_results, output_file_path, row.names = FALSE)

# 整理格式-----------------------------------------
df_formatted <- final_results %>%
    distinct() %>%
    mutate(
        model = gsub("Model_(\\d+)", "\\1", Model),
        HR = round(HR, 3),
        `95%CI` = paste0(round(CI_Lower, 3), "-", round(CI_Upper, 3)),
        P = if_else(P_Value < 0.001, "<0.001", as.character(round(P_Value, 3)))
    ) %>%
    select(Exposure, Term, Outcome, model, HR, `95%CI`, P, Removed_Covariates) %>%
    pivot_wider(
        names_from = model,
        values_from = c(HR, `95%CI`, P, Removed_Covariates),
        names_glue = "Model {model} { .value }"
    ) %>%
    select(
        Exposure, Term, Outcome,
        starts_with("Model 1"),
        starts_with("Model 2"),
        starts_with("Model 3")
    )

# View(df_formatted)
# 将整理好的数据框保存到新的 CSV 文件
write.csv(df_formatted, "workspace/projects/results/tables/multicox/cox_regression_results_formatted_unweighted_complete_separate_process.csv", row.names = FALSE)


# 核验结果------------------------------------
# 第一个暴露变量Time_of_first_meal与CancerMortality的模型3 (不加权)
class(dt$CancerMortality)
table(dt$CancerMortality)
test_model <- coxph(
    Surv(time, CancerMortality) ~ Eating_time_window_cat + Age + Gender + Racial + Education + Marriage + Economic.status + BMI + Smoking + Drinking + HBP + DM + CVD + DEI,
    data = dt2
)

summary(test_model)
