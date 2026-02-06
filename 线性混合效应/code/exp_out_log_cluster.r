#-----------------------------------------------------------
# 软饮与体格发育相关性分析 (Fixest Version - Fixed)
# 结局指标：Obesity_status, bmi_zscore, WHtR, central_obesity, OWOB
# 方法：fixest (feols/feglm) + Cluster Robust SE (vcov = ~Sampling_site)
#-----------------------------------------------------------
rm(list = ls()) # 清空环境变量
setwd("D:\\梁豪文件\\谢佳欣20260202中小学\\")
# 加载需要的r包
required_pkgs <- c(
    "dplyr", "fixest", "broom", "purrr", "knitr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
    install_cmd <- paste0("install.packages(c(", paste0("'", missing_pkgs, "'", collapse = ", "), "), repos = 'https://cran.r-project.org', dependencies = TRUE)")
    if (interactive()) {
        message("检测到缺失的 R 包： ", paste(missing_pkgs, collapse = ", "), "\n尝试自动安装...")
        try(install.packages(missing_pkgs, repos = "https://cran.r-project.org", dependencies = TRUE), silent = TRUE)
    } else {
        stop("缺少 R 包： ", paste(missing_pkgs, collapse = ", "), "\n请运行：\n", install_cmd)
    }
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

# 读取元数据--------------------------------------------------
if(file.exists("data/analysis_data_v1.rds")) {
    meta_data <- readRDS("data/analysis_data_v1.rds")
} else {
    warning("数据文件 data/analysis_data_v1.rds 不存在。")
    # meta_data <- data.frame(...) 
}

base_info <- list(
    outcome_vars = c("Obesity_status", "bmi_zscore", "WHtR", "central_obesity", "OWOB"), 
    exposure_var = "soft_drink_category", 
    covars0 = NULL, 
    covars1 = c("Age", "Gender"), 
    covars2 = c(
        "Age", "Gender", "Racial", "Household_income", "Father_Edu", "Mother_Edu", 
        "Softdrink_freq", "Fruit_amount", "Veg_amount", "Meat_amount", 
        "Exercise_time", "Temp_mean", "Humidity_mean", "PM2.5_mean"
    ), 
    cluster_var = "Sampling_site", # 聚类变量
    data = meta_data,
    alpha = 0.05,
    reference_level = "No", 
    output_path = "results/beverage_obesity_fixest"
)

# 核心分析函数 (基于 fixest)
run_fixest_models <- function(
    base_info,
    outcome_cont = NULL,
    outcome_bin = NULL,
    filename = NULL
) {
    
    # 确定模型类型
    if (!is.null(outcome_cont)) {
        outcome <- outcome_cont
        is_binary <- FALSE
    } else if (!is.null(outcome_bin)) {
        outcome <- outcome_bin
        is_binary <- TRUE
    } else {
        stop("必须指定 outcome_cont 或 outcome_bin")
    }

    # 数据预处理
    df <- base_info$data
    exposure <- base_info$exposure_var
    cluster_formula <- as.formula(paste0("~", base_info$cluster_var)) 
    
    # 1. 处理暴露变量 (Factor relevel)
    if (!is.factor(df[[exposure]])) df[[exposure]] <- factor(df[[exposure]])
    if (base_info$reference_level %in% levels(df[[exposure]])) {
        df[[exposure]] <- stats::relevel(df[[exposure]], ref = base_info$reference_level)
    }

    # 2. [关键修复] 处理二分类结局变量：feglm 必须接受数值型 0/1
    if (is_binary) {
        y_col <- df[[outcome]]
        # 检查是否需要转换
        if (is.factor(y_col) || is.character(y_col)) {
            ref <- base_info$reference_level
            # 如果 reference_level ("No") 存在于数据中，则将其设为 0，其他设为 1
            if (ref %in% unique(y_col)) {
                df[[outcome]] <- as.numeric(y_col != ref)
            } else {
                # 如果找不到 reference_level，尝试转为因子后转数值-1 (适用于 0/1 因子或 Level1/Level2)
                # 警告：这取决于因子顺序，请确保参考组是第一个 Level
                df[[outcome]] <- as.numeric(as.factor(y_col)) - 1
                warning(paste("结局", outcome, "未找到参考水平", ref, "，已按因子顺序转换为 0/1。请检查因子 Level 顺序。"))
            }
        }
    }

    # 定义公式构建函数
    make_formula <- function(outcome, exposure, covars) {
        rhs <- exposure
        if (!is.null(covars) && length(covars) > 0) rhs <- paste(c(exposure, covars), collapse = " + ")
        as.formula(paste0(outcome, " ~ ", rhs))
    }

    formulas <- list(
        Model1 = make_formula(outcome, exposure, base_info$covars0),
        Model2 = make_formula(outcome, exposure, base_info$covars1),
        Model3 = make_formula(outcome, exposure, base_info$covars2)
    )

    # 运行模型
    results_list <- purrr::imap(formulas, function(formula, model_name) {
        
        # 拟合模型
        if (is_binary) {
            # feglm 此时接收到的 LHS 已经是 numeric 0/1
            fit <- fixest::feglm(formula, data = df, family = binomial("logit"), vcov = cluster_formula)
        } else {
            fit <- fixest::feols(formula, data = df, vcov = cluster_formula)
        }

        # 提取结果
        tid <- broom::tidy(fit, conf.int = TRUE, conf.level = 1 - base_info$alpha)
        
        # 筛选暴露变量项
        target_rows <- tid %>% 
            filter(term == "(Intercept)" | grepl(base_info$exposure_var, term))
        
        # 格式化输出
        processed <- target_rows %>% mutate(
            model = model_name,
            pval = p.value
        )
        
        if (is_binary) {
            # Logistic: OR (95% CI)
            processed <- processed %>% mutate(
                OR = exp(estimate),
                OR_low = exp(conf.low),
                OR_high = exp(conf.high),
                val_print = sprintf("%.2f (%.2f, %.2f)", OR, OR_low, OR_high),
                pval_print = sprintf("%.3f", pval)
            )
        } else {
            # Linear: Beta (95% CI)
            processed <- processed %>% mutate(
                val_print = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high),
                pval_print = sprintf("%.3f", pval)
            )
        }
        
        return(processed %>% select(term, val_print, pval_print))
    })

    # 合并结果表格
    final_table <- results_list$Model1 %>% 
        rename(Model1_Val = val_print, Model1_P = pval_print) %>%
        left_join(results_list$Model2 %>% rename(Model2_Val = val_print, Model2_P = pval_print), by = "term") %>%
        left_join(results_list$Model3 %>% rename(Model3_Val = val_print, Model3_P = pval_print), by = "term")
    
    # 保存结果
    dir.create(base_info$output_path, recursive = TRUE, showWarnings = FALSE)
    if (!is.null(filename)) {
        write.csv(final_table, file = file.path(base_info$output_path, filename), row.names = FALSE)
    }
    
    return(final_table)
}

# 批量运行----------------------------------------------------

# 1. 连续变量 (feols)
cont_outcomes <- c("bmi_zscore", "WHtR")
purrr::walk(cont_outcomes, ~ {
    cat("Processing Continuous (feols):", .x, "\n")
    run_fixest_models(
        base_info,
        outcome_cont = .x,
        outcome_bin = NULL,
        filename = paste0("table_feols_", .x, ".csv")
    )
})

# 2. 二分类变量 (feglm)
bin_outcomes <- c("Obesity_status", "central_obesity", "OWOB")
purrr::walk(bin_outcomes, ~ {
    cat("Processing Binary (feglm):", .x, "\n")
    run_fixest_models(
        base_info,
        outcome_cont = NULL,
        outcome_bin = .x,
        filename = paste0("table_feglm_", .x, ".csv")
    )
})

message("fixest 分析完成。结果保存在：", base_info$output_path)
