#-----------------------------------------------------------
# 软饮与体格发育相关性分析
# 结局指标：Obesity_status（二分类因子）、bmi_zscore（连续数值）、WHtR（连续数值）、central_obesity（二分类因子）、OWOB（二分类因子）
# 自变量：软饮类别soft_drink_category（五分类因子）
# 协变量1：年龄、性别、省份
# 协变量2：年龄、性别、省份、种族、家庭年收入、父亲教育程度、母亲教育程度、软饮摄入频率、每日水果摄入量、每日蔬菜摄入量、每日肉摄入量、户外活动、温度、湿度、PM2.5
# 方法：线性混合效应模型
#-----------------------------------------------------------
rm(list = ls()) # 清空环境变量
setwd("D:\\梁豪文件\\谢佳欣20260202中小学\\")
# 加载需要的r包
required_pkgs <- c(
    "dplyr", "lme4", "lmerTest", "sjPlot", "ggplot2",
    "emmeans", "broom.mixed", "tidyr", "purrr", "knitr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
    install_cmd <- paste0("install.packages(c(", paste0("'", missing_pkgs, "'", collapse = ", "), "), repos = 'https://cran.r-project.org', dependencies = TRUE)")
    if (interactive()) {
        message("检测到缺失的 R 包： ", paste(missing_pkgs, collapse = ", "), "\n尝试在交互式会话中自动安装。若失败，请手动运行以下命令：\n", install_cmd)
        try(install.packages(missing_pkgs, repos = "https://cran.r-project.org", dependencies = TRUE), silent = TRUE)
        # 重新检测
        missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
        if (length(missing_pkgs) > 0) stop("仍缺少以下包，请在 R 控制台手动安装： ", paste(missing_pkgs, collapse = ", "))
    } else {
        stop(
            "缺少 R 包： ", paste(missing_pkgs, collapse = ", "),
            "\n请在 R 控制台手动运行：\n", install_cmd,
            "\n（或在无法联网环境下采用离线安装）"
        )
    }
}

invisible(lapply(required_pkgs, library, character.only = TRUE))
# 读取元数据--------------------------------------------------
meta_data <- readRDS("data/analysis_data_v1.rds")
names(meta_data)
sapply(meta_data, class)
str(meta_data)
base_info <- list(
    outcome_vars = c("Obesity_status", "bmi_zscore", "WHtR", "central_obesity", "OWOB"), # 结局变量
    exposure_var = "soft_drink_category", # 自变量
    covars0 = NULL, # 无协变量
    covars1 = c("Age", "Gender"), # 协变量1
    covars2 = c(
        c(
            "Age", # 年龄
            "Gender", # 性别
            "Racial", # 种族
            "Household_income", # 家庭年收入
            "Father_Edu", # 父亲教育程度
            "Mother_Edu", # 母亲教育程度
            "Softdrink_freq", # 软饮摄入频率
            "Fruit_amount", # 每日水果摄入量
            "Veg_amount", # 每日蔬菜摄入量
            "Meat_amount", # 每日肉摄入量
            "Exercise_time", # 户外活动（注：原数据中为“Exercise_time”，可能指锻炼/户外活动时间）
            "Temp_mean", # 温度
            "Humidity_mean", # 湿度
            "PM2.5_mean" # PM2.5
        )
    ), # 协变量2
    random_effect = "Sampling_site", # 随机效应
    data = meta_data,
    alpha = 0.05,
    reference_level = "No", # 参考组别
    output_path = "results\\beverage_obesity"
)


# 多模型分析与结果合并输出（LMM + GLMM）
run_multi_models <- function(
    base_info,
    outcome_cont = "bmi_zscore",
    outcome_bin = "Obesity_status",
    lmm_filename = "table_LMM_beverage_obesity_bmi_zscore_models1-3.csv",
    glmm_filename = "table_GLMM_logit_beverage_obesity_Obesity_status_models1-3.csv") {
    lmm_models <- NULL
    glmm_models <- NULL

    # ---------- LMM（连续结局） ----------
    if (!is.null(outcome_cont)) {
        make_lmm_formula <- function(outcome, exposure, covars, random) {
            rhs <- exposure
            if (!is.null(covars) && length(covars) > 0) rhs <- paste(c(exposure, covars), collapse = " + ")
            as.formula(paste0(outcome, " ~ ", rhs, " + (1 | ", random, ")"))
        }

        lmm_models <- list(
            Model1 = lmer(make_lmm_formula(outcome_cont, base_info$exposure_var, base_info$covars0, base_info$random_effect),
                data = base_info$data, REML = FALSE
            ),
            Model2 = lmer(make_lmm_formula(outcome_cont, base_info$exposure_var, base_info$covars1, base_info$random_effect),
                data = base_info$data, REML = FALSE
            ),
            Model3 = lmer(make_lmm_formula(outcome_cont, base_info$exposure_var, base_info$covars2, base_info$random_effect),
                data = base_info$data, REML = FALSE
            )
        )

        fixed_lmm <- purrr::imap(lmm_models, ~ {
            tid <- broom.mixed::tidy(.x, effects = "fixed", conf.int = TRUE, conf.level = 1 - base_info$alpha)
            tid <- tid %>% dplyr::filter(term == "(Intercept)" | grepl(base_info$exposure_var, term))
            tid %>% dplyr::mutate(
                model = .y,
                estimate_round = round(estimate, 3),
                ci = paste0("(", round(conf.low, 3), ", ", round(conf.high, 3), ")"),
                est_ci = paste0(estimate_round, " ", ci),
                pval = signif(p.value, 3)
            )
        }) %>% dplyr::bind_rows()

        model_info_lmm <- purrr::map_dfr(names(lmm_models), function(model_name) {
            .x <- lmm_models[[model_name]]
            vc <- as.data.frame(VarCorr(.x))
            var_site <- vc$vcov[vc$grp == base_info$random_effect & vc$var1 == "(Intercept)"]
            var_res <- vc$vcov[vc$grp == "Residual"]
            icc <- var_site / (var_site + var_res)
            tibble::tibble(
                model = model_name,
                var_site = var_site,
                var_res = var_res,
                ICC = round(icc, 3),
                N = nobs(.x),
                clusters = length(unique(base_info$data[[base_info$random_effect]])),
                AIC = AIC(.x),
                BIC = BIC(.x)
            )
        })

        fixed_wide_lmm <- fixed_lmm %>%
            dplyr::select(model, term, est_ci, pval) %>%
            tidyr::pivot_wider(names_from = model, values_from = c(est_ci, pval), names_sep = "_")

        final_lmm <- fixed_wide_lmm %>%
            dplyr::cross_join(model_info_lmm %>% dplyr::filter(model == "Model1") %>% dplyr::select(-model) %>% dplyr::rename_with(~ paste0(.x, "_Model1"))) %>%
            dplyr::cross_join(model_info_lmm %>% dplyr::filter(model == "Model2") %>% dplyr::select(-model) %>% dplyr::rename_with(~ paste0(.x, "_Model2"))) %>%
            dplyr::cross_join(model_info_lmm %>% dplyr::filter(model == "Model3") %>% dplyr::select(-model) %>% dplyr::rename_with(~ paste0(.x, "_Model3")))

        dir.create(base_info$output_path, recursive = TRUE, showWarnings = FALSE)
        if (!is.null(lmm_filename)) {
            write.csv(final_lmm, file = file.path(base_info$output_path, lmm_filename), row.names = FALSE)
        }
    }

    # ---------- GLMM（二分类结局） ----------
    if (!is.null(outcome_bin)) {
        exposure <- base_info$exposure_var
        if (!is.factor(base_info$data[[exposure]])) base_info$data[[exposure]] <- factor(base_info$data[[exposure]])
        if (base_info$reference_level %in% levels(base_info$data[[exposure]])) {
            base_info$data[[exposure]] <- stats::relevel(base_info$data[[exposure]], ref = base_info$reference_level)
        }

        make_glmm_formula <- function(outcome, exposure, covars, random) {
            rhs <- exposure
            if (!is.null(covars) && length(covars) > 0) rhs <- paste(c(exposure, covars), collapse = " + ")
            as.formula(paste0(outcome, " ~ ", rhs, " + (1 | ", random, ")"))
        }

        glmm_models <- list(
            Model1 = glmer(make_glmm_formula(outcome_bin, base_info$exposure_var, base_info$covars0, base_info$random_effect),
                data = base_info$data, family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
            ),
            Model2 = glmer(make_glmm_formula(outcome_bin, base_info$exposure_var, base_info$covars1, base_info$random_effect),
                data = base_info$data, family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
            ),
            Model3 = glmer(make_glmm_formula(outcome_bin, base_info$exposure_var, base_info$covars2, base_info$random_effect),
                data = base_info$data, family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
            )
        )

        fixed_glmm <- purrr::imap(glmm_models, ~ {
            tid <- broom.mixed::tidy(.x, effects = "fixed", conf.int = TRUE, conf.level = 1 - base_info$alpha)
            tid <- tid %>% dplyr::filter(term == "(Intercept)" | grepl(base_info$exposure_var, term))
            tid %>% dplyr::mutate(
                OR = exp(estimate),
                OR_low = exp(conf.low),
                OR_high = exp(conf.high),
                OR_round = round(OR, 3),
                ci = paste0("(", round(OR_low, 3), ", ", round(OR_high, 3), ")"),
                OR_CI = paste0(OR_round, " ", ci),
                pval = signif(p.value, 3),
                model = .y
            )
        }) %>% dplyr::bind_rows()

        model_info_glmm <- purrr::map_dfr(names(glmm_models), function(model_name) {
            .x <- glmm_models[[model_name]]
            vc <- as.data.frame(VarCorr(.x))
            var_site <- vc$vcov[vc$grp == base_info$random_effect & vc$var1 == "(Intercept)"]
            var_res_latent <- (pi^2) / 3
            icc <- var_site / (var_site + var_res_latent)
            tibble::tibble(
                model = model_name,
                var_site = var_site,
                var_res_latent = var_res_latent,
                ICC = round(icc, 3),
                N = nobs(.x),
                clusters = length(unique(base_info$data[[base_info$random_effect]])),
                AIC = AIC(.x),
                BIC = BIC(.x)
            )
        })

        fixed_wide_glmm <- fixed_glmm %>%
            dplyr::select(model, term, OR_CI, pval) %>%
            tidyr::pivot_wider(names_from = model, values_from = c(OR_CI, pval), names_sep = "_")

        final_glmm <- fixed_wide_glmm %>%
            dplyr::cross_join(
                model_info_glmm %>%
                    dplyr::rename_with(~ paste0(.x, "_Model1"), -model) %>%
                    dplyr::filter(model == "Model1") %>%
                    dplyr::select(-model)
            ) %>%
            dplyr::cross_join(
                model_info_glmm %>%
                    dplyr::rename_with(~ paste0(.x, "_Model2"), -model) %>%
                    dplyr::filter(model == "Model2") %>%
                    dplyr::select(-model)
            ) %>%
            dplyr::cross_join(
                model_info_glmm %>%
                    dplyr::rename_with(~ paste0(.x, "_Model3"), -model) %>%
                    dplyr::filter(model == "Model3") %>%
                    dplyr::select(-model)
            )

        dir.create(base_info$output_path, recursive = TRUE, showWarnings = FALSE)
        if (!is.null(glmm_filename)) {
            write.csv(final_glmm, file = file.path(base_info$output_path, glmm_filename), row.names = FALSE)
        }
    }

    invisible(list(lmm = lmm_models, glmm = glmm_models))
}

# 运行：base_info 中指定的五个结局
cont_outcomes <- c("bmi_zscore", "WHtR")
bin_outcomes <- c("Obesity_status", "central_obesity", "OWOB")

purrr::walk(cont_outcomes, ~ {
    run_multi_models(
        base_info,
        outcome_cont = .x,
        outcome_bin = NULL,
        lmm_filename = paste0("table_LMM_beverage_obesity_", .x, "_models1-3.csv"),
        glmm_filename = NULL
    )
})

purrr::walk(bin_outcomes, ~ {
    run_multi_models(
        base_info,
        outcome_cont = NULL,
        outcome_bin = .x,
        lmm_filename = NULL,
        glmm_filename = paste0("table_GLMM_logit_beverage_obesity_", .x, "_models1-3.csv")
    )
})
