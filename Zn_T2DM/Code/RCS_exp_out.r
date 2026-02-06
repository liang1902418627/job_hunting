# 锌与2型糖尿病RCS模型----------------------
# 自变量：血清锌
# 因变量：2型糖尿病、空腹血糖受损及糖尿病、空腹血糖、空腹血糖受损
# 1. 模型1-3 非线性RCS回归p值提取
# 2. 绘图：模型3 协变量调整后RCS回归曲线 + 直方图
#  因变量：2型糖尿病、空腹血糖受损及糖尿病、空腹血糖、空腹血糖受损
# 结果验证：风暴统计平台
# ----------------------------------------------------------------
rm(list = ls()) # 清空环境变量
setwd("C:/lianghao/lianghao/Zn_Gut_T2DM/")

# 载入必要的包
paks <- c("dplyr", "rms", "splines", "ggplot2")
lapply(X = paks, FUN = library, character.only = TRUE)

# 读取数据
meta_data <- readRDS(file = "Data/md_final_v2.rds")

# 检查数据
str(meta_data)

# 设置RCS回归的参数, 先不做IFG的，IFG单独跑
exposure_vars <- c("Serum_Zn") # 血清锌, 连续变量
outcome_vars <- c("T2DM", "IFG_or_T2DM", "FPG") # 因变量 ； T2DM：2分类因子变量；FPG：连续变量；IFG_or_T2DM：2分类因子变量
model_covariates <- list(
    model1 = c(),
    model2 = c("Age", "Gender"),
    model3 = c("Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history")
)

# 建立限制性立方样条模型并提取相关参数------------------------------------------------------
# 创建 datadist 对象并指定
dd <- datadist(meta_data)
options(datadist = "dd")

# 建立循环提取结果
results_list <- list()

for (outcome_var in outcome_vars) {
    # 判断结局变量类型：因子或字符型视为分类变量，否则视为连续变量
    is_binary <- is.factor(meta_data[[outcome_var]]) || is.character(meta_data[[outcome_var]])
    fit_func <- if (is_binary) lrm else ols

    for (m_name in names(model_covariates)) {
        covs <- model_covariates[[m_name]]

        # 寻找最佳 AIC 结点数
        ks_to_try <- 3:7
        aic_vals <- sapply(ks_to_try, function(k) {
            formula_str <- if (length(covs) > 0) {
                paste0(outcome_var, " ~ rcs(", exposure_vars, ", ", k, ") + ", paste(covs, collapse = " + "))
            } else {
                paste0(outcome_var, " ~ rcs(", exposure_vars, ", ", k, ")")
            }
            fit <- fit_func(as.formula(formula_str), data = meta_data)
            AIC(fit)
        })

        best_k <- ks_to_try[which.min(aic_vals)]

        # 拟合最终模型
        final_formula <- if (length(covs) > 0) {
            paste0(outcome_var, " ~ rcs(", exposure_vars, ", ", best_k, ") + ", paste(covs, collapse = " + "))
        } else {
            paste0(outcome_var, " ~ rcs(", exposure_vars, ", ", best_k, ")")
        }

        rcs_model <- fit_func(as.formula(final_formula), data = meta_data)
        an <- anova(rcs_model)

        # 提取非线性 P 值 (通常在 anova 表的 'Nonlinear' 行)
        p_nonlinear <- an[" Nonlinear", "P"]
        p_total <- an[exposure_vars, "P"]

        results_list[[length(results_list) + 1]] <- data.frame(
            Outcome = outcome_var,
            Model = m_name,
            Best_K = best_k,
            AIC = min(aic_vals),
            P_Total = p_total,
            P_Nonlinear = p_nonlinear
        )
    }
}

# 汇总并保存结果
final_results <- do.call(rbind, results_list)

# 格式化 P 值为 3 位小数
final_results$P_Total <- round(final_results$P_Total, 3)
final_results$P_Nonlinear <- round(final_results$P_Nonlinear, 3)

# 创建保存路径并保存为 CSV
output_dir <- "Results/RCS/tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
write.csv(final_results, file.path(output_dir, "rcs_results_summary.csv"), row.names = FALSE)
# 针对IFG结局单独跑RCS模型-------------------------------------
# 取子集
table(meta_data$IFG, useNA = "ifany")
meta_data_IFG <- meta_data %>%
    filter(IFG %in% c("No", "Yes")) %>%
    mutate(IFG = droplevels(IFG)) # 新增这行以移除多余的因子水平
table(meta_data_IFG$IFG, useNA = "ifany")

# 设置RCS回归的参数
exposure_var <- "Serum_Zn" # 血清锌, 连续
outcome_var <- "IFG" # 因变量 ； IFG：2分类因子变量
model_cov1 <- c() # 模型1 协变量
model_cov2 <- c("Age", "Gender")
model_cov3 <- c("Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history")

# 建立限制性立方样条模型并提取相关参数------------------------------------------------------
# 创建 datadist 对象并指定
dd <- datadist(meta_data_IFG)
options(datadist = "dd")

# 建立循环提取结果
results_list_IFG <- list()
outcome_var_IFG <- "IFG"

for (m_name in names(model_covariates)) {
    covs <- model_covariates[[m_name]]

    # 寻找最佳 AIC 结点数
    ks_to_try <- 3:7
    aic_vals <- sapply(ks_to_try, function(k) {
        formula_str <- if (length(covs) > 0) {
            paste0(outcome_var_IFG, " ~ rcs(", exposure_var, ", ", k, ") + ", paste(covs, collapse = " + "))
        } else {
            paste0(outcome_var_IFG, " ~ rcs(", exposure_var, ", ", k, ")")
        }
        # 使用子集数据 meta_data_IFG，因为 IFG 是二分类变量，使用 lrm
        fit <- lrm(as.formula(formula_str), data = meta_data_IFG)
        AIC(fit)
    })

    best_k <- ks_to_try[which.min(aic_vals)]

    # 拟合最终模型
    final_formula <- if (length(covs) > 0) {
        paste0(outcome_var_IFG, " ~ rcs(", exposure_var, ", ", best_k, ") + ", paste(covs, collapse = " + "))
    } else {
        paste0(outcome_var_IFG, " ~ rcs(", exposure_var, ", ", best_k, ")")
    }

    rcs_model <- lrm(as.formula(final_formula), data = meta_data_IFG)
    an <- anova(rcs_model)

    # 提取 P 值
    p_nonlinear <- an[" Nonlinear", "P"]
    p_total <- an[exposure_var, "P"]

    results_list_IFG[[length(results_list_IFG) + 1]] <- data.frame(
        Outcome = outcome_var_IFG,
        Model = m_name,
        Best_K = best_k,
        AIC = min(aic_vals),
        P_Total = round(p_total, 3),
        P_Nonlinear = round(p_nonlinear, 3)
    )
}

# 汇总 IFG 结果并保存
final_results_IFG <- do.call(rbind, results_list_IFG)
View(final_results_IFG)
write.csv(final_results_IFG, file.path(output_dir, "rcs_results_IFG_summary.csv"), row.names = FALSE)
# 验证 IFG 模型 3
test_covs <- c(
    "Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat",
    "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake",
    "act_sports_intense", "Family_diabetes_history"
)

# 手动拟合一个 4 结点的模型进行测试
test_formula <- paste0("IFG ~ rcs(Serum_Zn, 4) + ", paste(test_covs, collapse = " + "))
test_model <- lrm(as.formula(test_formula), data = meta_data_IFG)

# 查看 ANOVA 表原始输出
test_an <- anova(test_model)
print(test_an)

# 检查提取过程
p_total_raw <- test_an["Serum_Zn", "P"]
p_nonlinear_raw <- test_an[" Nonlinear", "P"]

cat("\n--- 验证结果 ---\n")
cat("原始总 P 值:", p_total_raw, "\n")
cat("原始非线性 P 值:", p_nonlinear_raw, "\n")
cat("格式化后总 P 值:", round(p_total_raw, 3), "\n")
cat("格式化后非线性 P 值:", round(p_nonlinear_raw, 3), "\n")

# ==========绘图代码（RCS + 直方图叠加 RCS 曲线）：#93C8ED #F7C1C1 #F9C780 #AEDBD2================
# 图1：模型3的协变量，自变量：血清锌，因变量：2型糖尿病；数据集：meta_data；节点数：3；RCS曲线配色：#93C8ED

# =========================
# 图1 绘图：T2DM（模型3）RCS + 直方图叠加
# =========================

# 重要：前面 dd 被 IFG 子集覆盖过，这里需要重置为 meta_data
dd <- datadist(meta_data)
options(datadist = "dd")

# 小工具
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

format_p <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) return("< 0.001")
    sprintf("%.3f", p)
}

# 通用绘图：直方图（右轴）+ RCS 曲线（左轴）
plot_rcs_hist_overlay <- function(
    x, yhat, lower, upper,
    xlab = "Serum Zn", ylab_left = "OR (95% CI)",
    inset_v = NULL,
    col_line = "#93C8ED",
    col_ribbon = adjustcolor("#93C8ED", alpha.f = 0.35),
    right_ylabel = expression(paste(Probability ~ density)),
    p_overall = NA, p_nonlinear = NA,
    left_ylim = NULL, ref_hline = 1,
    pdf_file = NULL, save = FALSE,
    width_cm = 18.69, height_cm = 15.27,
    font_family = "sans",
    breaks = 30,
    hist_x = NULL, 
    limit_hist_to_curve = TRUE,
    hist_fill = NA,
    hist_border = "#000000",
    hist_alpha = 0.45,
    hist_lwd = 0.8,
    knots_k = NA,
    annotate_intersection = TRUE,
    intersection_pick = c("first", "all"),
    intersection_digits = 2,
    intersection_col = "#fd0404",
    intersection_cex = 0.9,
    intersection_pos = 3,
    # ---- 新增：只标注“竖线(inset_v) 与 横线(ref_hline)”的交点 ----
    annotate_inset_ref = FALSE,
    inset_ref_digits = 2,
    inset_ref_col = "#fd0404",
    inset_ref_cex = 0.9,
    inset_ref_pos = 3
) {
    x_range <- range(x, na.rm = TRUE)

    if (is.null(left_ylim) || any(!is.finite(left_ylim))) {
        left_ylim <- range(pretty(c(lower, upper, ref_hline)), na.rm = TRUE)
    }

    if (save && !is.null(pdf_file)) {
        ok <- tryCatch({
            grDevices::cairo_pdf(
                filename = pdf_file,
                width  = width_cm / 2.54,
                height = height_cm / 2.54,
                family = font_family,
                pointsize = 12
            )
            TRUE
        }, error = function(e) FALSE)

        if (!ok) {
            grDevices::pdf(
                file = pdf_file,
                width  = width_cm / 2.54,
                height = height_cm / 2.54,
                family = "sans",
                pointsize = 12
            )
        }
        on.exit(grDevices::dev.off(), add = TRUE)
    }

    # --- 直方图（右轴）---
    if (!is.null(hist_x)) {
        if (isTRUE(limit_hist_to_curve)) {
            hist_x <- hist_x[hist_x >= x_range[1] & hist_x <= x_range[2]]
        }
        hist_data <- hist(hist_x, breaks = breaks, plot = FALSE)
        hist_max <- max(hist_data$density, na.rm = TRUE)
        hist_ylim <- c(0, hist_max * 1.10)

        op <- par(no.readonly = TRUE)
        on.exit(par(op), add = TRUE)

        par(
            ps = 12,
            cex = 1, cex.axis = 1, cex.lab = 1,
            mar = c(5, 4, 4, 10) + 0.1
        )

        plot(NA, NA,
            xlim = x_range, ylim = hist_ylim,
            xlab = xlab, ylab = "",
            axes = FALSE
        )

        fill_col <- if (is.null(hist_fill) || isTRUE(is.na(hist_fill))) {
            NA
        } else {
            adjustcolor(hist_fill, alpha.f = hist_alpha)
        }

        brks <- hist_data$breaks
        dens <- hist_data$density
        if (length(dens) > 0) {
            for (i in seq_along(dens)) {
                if (is.finite(dens[i])) {
                    rect(
                        xleft = brks[i], ybottom = 0,
                        xright = brks[i + 1], ytop = dens[i],
                        col = fill_col,
                        border = hist_border,
                        lwd = hist_lwd
                    )
                }
            }
        }

        axis(4, las = 1)
        mtext(right_ylabel, side = 4, line = 5)
    }

    # --- RCS 曲线（左轴）---
    par(new = TRUE)
    plot(x, yhat,
        type = "l", lty = 1, lwd = 2, col = col_line,
        xlim = x_range, ylim = left_ylim,
        xlab = xlab, ylab = ylab_left, axes = TRUE
    )

    ord <- order(x)
    polygon(
        c(x[ord], rev(x[ord])),
        c(lower[ord], rev(upper[ord])),
        col = col_ribbon, border = col_ribbon
    )

    if (!is.null(ref_hline) && is.finite(ref_hline)) {
        abline(h = ref_hline, col = "#ee1212", lty = 2)
    }

    if (!is.null(inset_v) && is.numeric(inset_v)) {
        inset_v <- inset_v[is.finite(inset_v)]
        if (length(inset_v) > 0) {
            abline(v = inset_v, col = "#f70606", lty = 2)
        }
    }

    # ---- 新增：标注“竖线(inset_v) 与 横线(ref_hline)”交点（只会标注 inset_v 的 x 值）----
    if (isTRUE(annotate_inset_ref) && !is.null(ref_hline) && is.finite(ref_hline) &&
        !is.null(inset_v) && is.numeric(inset_v)) {
        v0 <- inset_v[is.finite(inset_v)][1]
        if (length(v0) == 1 && is.finite(v0)) {
            y_off <- 0.03 * diff(left_ylim)
            lab <- format(round(v0, inset_ref_digits), nsmall = inset_ref_digits)

            points(v0, ref_hline, pch = 16, cex = 0.7, col = inset_ref_col)
            text(
                x = v0,
                y = ref_hline + y_off,
                labels = lab,
                col = inset_ref_col,
                cex = inset_ref_cex,
                pos = inset_ref_pos,
                xpd = NA
            )
        }
    }

    # ---- 原逻辑：曲线与 ref_hline 的交点标注（如不需要就 annotate_intersection=FALSE）----
    if (isTRUE(annotate_intersection) && !is.null(ref_hline) && is.finite(ref_hline)) {
        intersection_pick <- match.arg(intersection_pick)

        xs <- x[ord]
        ys <- yhat[ord]
        ok <- is.finite(xs) & is.finite(ys)
        xs <- xs[ok]; ys <- ys[ok]

        if (length(xs) >= 2) {
            d <- ys - ref_hline

            x0s <- numeric(0)
            for (i in seq_len(length(d) - 1)) {
                d1 <- d[i]; d2 <- d[i + 1]
                if (!is.finite(d1) || !is.finite(d2)) next

                if (d1 == 0) { x0s <- c(x0s, xs[i]); next }
                if (d2 == 0) { x0s <- c(x0s, xs[i + 1]); next }

                if (d1 * d2 < 0) {
                    x0 <- xs[i] + (ref_hline - ys[i]) * (xs[i + 1] - xs[i]) / (ys[i + 1] - ys[i])
                    x0s <- c(x0s, x0)
                }
            }

            if (length(x0s) > 0) {
                if (intersection_pick == "first") x0s <- x0s[1]

                y_off <- 0.03 * diff(left_ylim)
                labs <- format(round(x0s, intersection_digits), nsmall = intersection_digits)

                points(x0s, rep(ref_hline, length(x0s)), pch = 16, cex = 0.7, col = intersection_col)
                text(
                    x = x0s,
                    y = rep(ref_hline + y_off, length(x0s)),
                    labels = labs,
                    col = intersection_col,
                    cex = intersection_cex,
                    pos = intersection_pos,
                    xpd = NA
                )
            }
        }
    }

    legend_text <- paste0(
        if (is.finite(knots_k)) paste0("Knots: ", knots_k, "\n") else "",
        "P-overall ", format_p(p_overall), "\nP-non-linear ", format_p(p_nonlinear)
    )
    legend("topright", legend_text, bty = "n")
}

# ========== 拟合图1所需模型 ==========
# 模型3协变量（与你上面 model_covariates$model3 保持一致）
cov_m3 <- model_covariates$model3

k_fixed <- 3
fml_t2dm_m3 <- as.formula(
    paste0(
        "T2DM ~ rcs(Serum_Zn, ", k_fixed, ") + ",
        paste(cov_m3, collapse = " + ")
    )
)

fit_t2dm_m3 <- lrm(fml_t2dm_m3, data = meta_data, x = TRUE, y = TRUE)

an_t2dm_m3 <- anova(fit_t2dm_m3)
p_overall_t2dm_m3 <- an_t2dm_m3["Serum_Zn", "P"]
p_nonlinear_t2dm_m3 <- an_t2dm_m3[" Nonlinear", "P"]

# Predict：输出 OR（相对参考值 ref.zero=TRUE）
pred_t2dm_m3 <- as.data.frame(
    Predict(fit_t2dm_m3, Serum_Zn, conf.int = 0.95, fun = exp, ref.zero = TRUE)
)

# 竖线：默认用暴露中位数（如需指定阈值可改这里）
inset_v <- median(meta_data$Serum_Zn, na.rm = TRUE)

# 保存路径
fig_dir <- "Results/RCS/figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

pdf_file <- file.path(fig_dir, "Fig1_RCS_SerumZn_T2DM_model3_knots3_overlay.pdf")

# 提取 Predict 输出列（兼容不同列名）
x <- as.numeric(pred_t2dm_m3$Serum_Zn %||% pred_t2dm_m3$x)
yhat <- as.numeric(pred_t2dm_m3$yhat %||% pred_t2dm_m3$y)
lower <- as.numeric(pred_t2dm_m3$lower)
upper <- as.numeric(pred_t2dm_m3$upper)

plot_rcs_hist_overlay(
    x = x, yhat = yhat, lower = lower, upper = upper,
    xlab = "Serum Zn (μg/L)",
    ylab_left = "T2DM OR (95% CI)",
    inset_v = inset_v,
    col_line = "#93C8ED",
    col_ribbon = adjustcolor("#93C8ED", alpha.f = 0.35),
    hist_x = meta_data$Serum_Zn,
    breaks = 25,
    p_overall = p_overall_t2dm_m3,
    p_nonlinear = p_nonlinear_t2dm_m3,
    ref_hline = 1,
    knots_k = k_fixed,
    pdf_file = pdf_file,
    save = TRUE
)

cat("Fig1 saved to: ", pdf_file, "\n")
# 图2----------------------------------------
# 模型3 的协变量；自变量：血清锌；因变量：空腹血糖受损或糖尿病；数据集：meta_data；节点数：5；RCS曲线配色：#F7C1C1

# 重要：确保 dd 指向 meta_data（若前面被其他子集覆盖）
dd <- datadist(meta_data)
options(datadist = "dd")

# 确保 IFG_or_T2DM 为二分类因子（lrm 需要）
if (!is.factor(meta_data$IFG_or_T2DM)) {
    meta_data$IFG_or_T2DM <- as.factor(meta_data$IFG_or_T2DM)
}

# ========== 拟合图2所需模型（IFG_or_T2DM，模型3，knots=5） ==========
cov_m3 <- model_covariates$model3

k_fixed2 <- 5
fml_ifgor_m3 <- as.formula(
    paste0(
        "IFG_or_T2DM ~ rcs(Serum_Zn, ", k_fixed2, ") + ",
        paste(cov_m3, collapse = " + ")
    )
)

fit_ifgor_m3 <- lrm(fml_ifgor_m3, data = meta_data, x = TRUE, y = TRUE)

an_ifgor_m3 <- anova(fit_ifgor_m3)
p_overall_ifgor_m3 <- an_ifgor_m3["Serum_Zn", "P"]
p_nonlinear_ifgor_m3 <- an_ifgor_m3[" Nonlinear", "P"]

pred_ifgor_m3 <- as.data.frame(
    Predict(fit_ifgor_m3, Serum_Zn, conf.int = 0.95, fun = exp, ref.zero = TRUE)
)

# 竖线：默认用暴露中位数
inset_v2 <- median(meta_data$Serum_Zn, na.rm = TRUE)

# 保存路径（沿用前面 fig_dir）
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

pdf_file2 <- file.path(fig_dir, "Fig2_RCS_SerumZn_IFGorT2DM_model3_knots5_overlay.pdf")

# 提取 Predict 输出列（兼容不同列名）
x2 <- as.numeric(pred_ifgor_m3$Serum_Zn %||% pred_ifgor_m3$x)
yhat2 <- as.numeric(pred_ifgor_m3$yhat %||% pred_ifgor_m3$y)
lower2 <- as.numeric(pred_ifgor_m3$lower)
upper2 <- as.numeric(pred_ifgor_m3$upper)

plot_rcs_hist_overlay(
    x = x2, yhat = yhat2, lower = lower2, upper = upper2,
    xlab = "Serum Zn (μg/L)",
    ylab_left = "IFG or T2DM OR (95% CI)",
    inset_v = inset_v2,
    col_line = "#F7C1C1",
    col_ribbon = adjustcolor("#F7C1C1", alpha.f = 0.35),
    hist_x = meta_data$Serum_Zn,
    breaks = 25,
    p_overall = p_overall_ifgor_m3,
    p_nonlinear = p_nonlinear_ifgor_m3,
    ref_hline = 1,
    knots_k = k_fixed2,
    pdf_file = pdf_file2,
    save = TRUE,
    annotate_intersection = FALSE,   # 关掉“曲线×OR=1”交点标注
    annotate_inset_ref = TRUE,       # 打开“中位数竖线×OR=1”交点标注
    inset_ref_digits = 2
)

cat("Fig2 saved to: ", pdf_file2, "\n")
# 图3----------------------------------------
# 模型3 的协变量；自变量：血清锌(Serum Zn)；因变量：空腹血糖(FPG)；数据集：meta_data；节点数：7；RCS曲线配色：#F9C780

# =========================
# 图3 绘图：FPG（模型3）RCS + 直方图叠加（连续结局：用 ols）
# =========================

# 重要：确保 dd 指向 meta_data（若前面被其他子集覆盖）
dd <- datadist(meta_data)
options(datadist = "dd")

# 确保 FPG 为数值型（ols 需要连续数值）
if (!is.numeric(meta_data$FPG)) {
    meta_data$FPG <- as.numeric(as.character(meta_data$FPG))
}

# ========== 拟合图3所需模型（FPG，模型3，knots=7） ==========
cov_m3 <- model_covariates$model3

k_fixed3 <- 7
fml_fpg_m3 <- as.formula(
    paste0(
        "FPG ~ rcs(Serum_Zn, ", k_fixed3, ") + ",
        paste(cov_m3, collapse = " + ")
    )
)

fit_fpg_m3 <- ols(fml_fpg_m3, data = meta_data, x = TRUE, y = TRUE)

an_fpg_m3 <- anova(fit_fpg_m3)
p_overall_fpg_m3 <- an_fpg_m3["Serum_Zn", "P"]
p_nonlinear_fpg_m3 <- an_fpg_m3[" Nonlinear", "P"]

# Predict：连续结局——输出“相对参考值(默认=datadist的Adjust to，通常为中位数)”的 β 及95%CI
pred_fpg_m3 <- as.data.frame(
    Predict(fit_fpg_m3, Serum_Zn, conf.int = 0.95, ref.zero = TRUE)
)

# 竖线：暴露中位数（与 ref.zero 的参考点保持一致：datadist 默认也通常取中位数）
inset_v3 <- median(meta_data$Serum_Zn, na.rm = TRUE)

# 保存路径（沿用前面 fig_dir）
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

pdf_file3 <- file.path(fig_dir, "Fig3_RCS_SerumZn_FPG_model3_knots7_overlay.pdf")

# 提取 Predict 输出列（兼容不同列名）
x3 <- as.numeric(pred_fpg_m3$Serum_Zn %||% pred_fpg_m3$x)
yhat3 <- as.numeric(pred_fpg_m3$yhat %||% pred_fpg_m3$y)
lower3 <- as.numeric(pred_fpg_m3$lower)
upper3 <- as.numeric(pred_fpg_m3$upper)

plot_rcs_hist_overlay(
    x = x3, yhat = yhat3, lower = lower3, upper = upper3,
    xlab = "Serum Zn (μg/L)",
    ylab_left = expression("FPG" ~ beta ~ " (95% CI)"),
    inset_v = inset_v3,
    col_line = "#F9C780",
    col_ribbon = adjustcolor("#F9C780", alpha.f = 0.35),
    hist_x = meta_data$Serum_Zn,
    breaks = 25,
    p_overall = p_overall_fpg_m3,
    p_nonlinear = p_nonlinear_fpg_m3,
    ref_hline = 0,                 # y轴参考线：β=0
    knots_k = k_fixed3,
    pdf_file = pdf_file3,
    save = TRUE,
    annotate_intersection = FALSE,  # 保持不标注“曲线×参考线”交点（可按需改 TRUE）
    annotate_inset_ref = TRUE,      # 打开“中位数竖线×参考线”交点标注
    inset_ref_digits = 2
)

cat("Fig3 saved to: ", pdf_file3, "\n")
# 图4----------------------------------------
# 模型3的协变量；自变量：血清锌(Serum Zn)；因变量：空腹血糖受损(IFG)；
# 数据集：meta_data_IFG；节点数：5；RCS曲线配色：#AEDBD2

# 重要：确保 dd 指向 meta_data_IFG（IFG 子集）
dd <- datadist(meta_data_IFG)
options(datadist = "dd")

# 确保 IFG 为二分类因子（lrm 需要）
if (!is.factor(meta_data_IFG$IFG)) {
    meta_data_IFG$IFG <- as.factor(meta_data_IFG$IFG)
}
meta_data_IFG$IFG <- droplevels(meta_data_IFG$IFG)

# ========== 拟合图4所需模型（IFG，模型3，knots=5） ==========
cov_m3 <- model_covariates$model3

k_fixed4 <- 5
fml_ifg_m3 <- as.formula(
    paste0(
        "IFG ~ rcs(Serum_Zn, ", k_fixed4, ") + ",
        paste(cov_m3, collapse = " + ")
    )
)

fit_ifg_m3 <- lrm(fml_ifg_m3, data = meta_data_IFG, x = TRUE, y = TRUE)

an_ifg_m3 <- anova(fit_ifg_m3)
p_overall_ifg_m3 <- an_ifg_m3["Serum_Zn", "P"]
p_nonlinear_ifg_m3 <- an_ifg_m3[" Nonlinear", "P"]

pred_ifg_m3 <- as.data.frame(
    Predict(fit_ifg_m3, Serum_Zn, conf.int = 0.95, fun = exp, ref.zero = TRUE)
)

# 竖线：暴露中位数
inset_v4 <- median(meta_data_IFG$Serum_Zn, na.rm = TRUE)

# 保存路径（沿用前面 fig_dir）
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

pdf_file4 <- file.path(fig_dir, "Fig4_RCS_SerumZn_IFG_model3_knots5_overlay.pdf")

# 提取 Predict 输出列（兼容不同列名）
x4 <- as.numeric(pred_ifg_m3$Serum_Zn %||% pred_ifg_m3$x)
yhat4 <- as.numeric(pred_ifg_m3$yhat %||% pred_ifg_m3$y)
lower4 <- as.numeric(pred_ifg_m3$lower)
upper4 <- as.numeric(pred_ifg_m3$upper)

plot_rcs_hist_overlay(
    x = x4, yhat = yhat4, lower = lower4, upper = upper4,
    xlab = "Serum Zn (μg/L)",
    ylab_left = "IFG OR (95% CI)",
    inset_v = inset_v4,
    col_line = "#AEDBD2",
    col_ribbon = adjustcolor("#AEDBD2", alpha.f = 0.35),
    hist_x = meta_data_IFG$Serum_Zn,
    breaks = 25,
    p_overall = p_overall_ifg_m3,
    p_nonlinear = p_nonlinear_ifg_m3,
    ref_hline = 1,
    knots_k = k_fixed4,
    pdf_file = pdf_file4,
    save = TRUE,
    annotate_intersection = FALSE,   # 关掉“曲线×OR=1”的交点标注
    annotate_inset_ref = TRUE,       # 打开“中位数竖线×OR=1”的交点标注
    inset_ref_digits = 2
)

cat("Fig4 saved to: ", pdf_file4, "\n")
