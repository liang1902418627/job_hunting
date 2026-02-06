# -----------------------------------------------
# 题目：FPG与alpha多样性的RCS分析
# 自变量：alpha多样性指数 Richness, Shannon, Pielou, Simpson
# 因变量：FPG
# 校正协变量：模型3 中的协变量
# 配色：#93C8ED #F7C1C1 #F9C780 #AEDBD2
# -----------------------------------------------

rm(list = ls())
# 设置工作目录
setwd("C:\\lianghao\\lianghao\\Zn_Gut_T2DM")

# 加载所需的R包
paks <- c("dplyr", "rms", "splines", "ggplot2")
lapply(paks, require, character.only = TRUE)

# 读取数据
meta_data <- readRDS(file = "Data/md_final_v2.rds")
gut <- readRDS("Data/gut_data.rds")

# 检查数据
names(meta_data)
names(gut)
names(gut$alpha_diversity)

# 合并数据
alpha <- gut$alpha_diversity
md_v2 <- meta_data %>%
    left_join(alpha, by = "SampleID")
nrow(md_v2)

# 缺失值检查
sum(is.na(md_v2$Pielou))

# 设置RCS回归的参数
outcome_vars <- c("FPG") # 空腹血糖, 连续变量
exposure_vars <- c("Richness", "Shannon", "Pielou", "Simpson") # 自变量全是连续变量
model_covariates <- list(
    model1 = c(),
    model2 = c("Age", "Gender"),
    model3 = c("Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history")
)

saveRDS(md_v2, file = "Data/过程数据.rds")
# 建立限制性立方样条模型并提取相关参数------------------------------------------------------
# 创建 datadist 对象并指定
dd <- datadist(md_v2)
options(datadist = "dd")

# 建立循环提取结果
results_list <- list()

# 工具函数：从 rms::anova 输出中稳健提取 P 值
get_rcs_pvals <- function(an, exposure_var) {
    df <- as.data.frame(an)
    df$term <- trimws(rownames(df))

    # 找到 P 列（rms::anova 通常叫 "P"）
    p_col <- if ("P" %in% names(df)) "P" else {
        # 兜底：匹配包含 P 的列名
        cand <- grep("^P$|p", names(df), ignore.case = TRUE, value = TRUE)
        if (length(cand) == 0) stop("在 anova 表中找不到 P 值列。")
        cand[1]
    }

    # Total：通常行名就是 exposure_var
    p_total <- df[df$term == exposure_var, p_col]

    # Nonlinear：通常是 "exposure_var (Nonlinear)" 或类似形式
    p_nonlinear <- df[" Nonlinear", p_col]

    if (length(p_total) == 0) p_total <- NA_real_
    if (length(p_nonlinear) == 0) p_nonlinear <- NA_real_

    list(
        P_Total = as.numeric(p_total[1]),
        P_Nonlinear = as.numeric(p_nonlinear[1])
    )
}

for (outcome_var in outcome_vars) {

    # 更稳健地判断二分类：factor/character 或者数值且唯一值为 0/1
    y <- md_v2[[outcome_var]]
    is_binary <- is.factor(y) || is.character(y) ||
        (is.numeric(y) && all(stats::na.omit(unique(y)) %in% c(0, 1)) && length(stats::na.omit(unique(y))) <= 2)

    fit_func <- if (is_binary) lrm else ols

    for (exposure_var in exposure_vars) {

        for (m_name in names(model_covariates)) {
            covs <- model_covariates[[m_name]]

            # 寻找最佳 AIC 结点数
            ks_to_try <- 3:7
            aic_vals <- sapply(ks_to_try, function(k) {

                rhs <- paste0("rcs(", exposure_var, ", ", k, ")")
                if (length(covs) > 0) rhs <- paste(rhs, paste(covs, collapse = " + "), sep = " + ")

                formula_str <- paste(outcome_var, "~", rhs)
                fit <- fit_func(stats::as.formula(formula_str), data = md_v2)

                AIC(fit)
            })

            best_k <- ks_to_try[which.min(aic_vals)]

            # 拟合最终模型
            rhs_final <- paste0("rcs(", exposure_var, ", ", best_k, ")")
            if (length(covs) > 0) rhs_final <- paste(rhs_final, paste(covs, collapse = " + "), sep = " + ")

            final_formula <- stats::as.formula(paste(outcome_var, "~", rhs_final))
            rcs_model <- fit_func(final_formula, data = md_v2)

            an <- anova(rcs_model)
            pvals <- get_rcs_pvals(an, exposure_var)

            results_list[[length(results_list) + 1]] <- data.frame(
                Outcome = outcome_var,
                Exposure = exposure_var,
                Model = m_name,
                Best_K = best_k,
                AIC = min(aic_vals, na.rm = TRUE),
                P_Total = pvals$P_Total,
                P_Nonlinear = pvals$P_Nonlinear
            )
        }
    }
}

# 汇总并保存结果
final_results <- do.call(rbind, results_list)

# 格式化 P 值为 3 位小数（保留 NA）
final_results$P_Total <- round(final_results$P_Total, 3)
final_results$P_Nonlinear <- round(final_results$P_Nonlinear, 3)
View(final_results)

# ---------------------------
# 单独验证：Richness -> FPG，k=3，协变量=模型3
# ---------------------------
exposure_var <- "Richness"
outcome_var  <- "FPG"
k_fixed <- 3
covs <- model_covariates$model3

# 确保 datadist 已设置（你前面已设置过，这里再兜底一次）
dd <- datadist(md_v2)
options(datadist = "dd")

# 构造公式
rhs <- paste0("rcs(", exposure_var, ", ", k_fixed, ")")
rhs <- paste(rhs, paste(covs, collapse = " + "), sep = " + ")
fml <- stats::as.formula(paste(outcome_var, "~", rhs))
print(fml)

# 拟合（FPG 为连续变量时用 ols）
fit_single <- rms::ols(fml, data = md_v2)

# 查看模型与AIC
print(fit_single)
cat("AIC(single) =", AIC(fit_single), "\n")

# anova 与 P 值提取（上面写的 get_rcs_pvals）
an_single <- anova(fit_single)
print(an_single)

p_single <- get_rcs_pvals(an_single, exposure_var)
print(p_single)

# 汇总成一行，便于和循环结果对照
single_row <- data.frame(
  Outcome = outcome_var,
  Exposure = exposure_var,
  Model = "model3",
  Best_K = k_fixed,
  AIC = AIC(fit_single),
  P_Total = p_single$P_Total,
  P_Nonlinear = p_single$P_Nonlinear
)
print(single_row)

# 对照循环结果中对应的那一行
loop_row <- subset(final_results, Outcome == outcome_var & Exposure == exposure_var & Model == "model3")
print(loop_row)

# 绘图 ----------------------------------------------
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
    xlab = "FPG", ylab_left = "beta (95% CI)",
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
    annotate_inset_ref = FALSE,
    inset_ref_digits = 2,
    inset_ref_col = "#fd0404",
    inset_ref_cex = 0.9,
    inset_ref_pos = 3,
    # ---- 新增：控制左侧 y 轴刻度标签方向 ----
    las_left = 1
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

    # 改为 axes=FALSE，然后手动画左 y 轴并指定 las
    plot(x, yhat,
        type = "l", lty = 1, lwd = 2, col = col_line,
        xlim = x_range, ylim = left_ylim,
        xlab = xlab, ylab = ylab_left,
        axes = FALSE
    )
    axis(1)                 # x 轴
    axis(2, las = las_left) # 左 y 轴：水平标签
    box()

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

# Richness -> FPG-----------------------------------
cov3 <- model_covariates$model3
k_fixed1 <- 3

# 确保 rms 需要的 datadist 已设置
dd <- datadist(md_v2)
options(datadist = "dd")

fml_fpg_m1 <- as.formula(
  paste0(
    "FPG ~ rcs(Richness, ", k_fixed1, ") + ",
    paste(cov3, collapse = " + ")
  )
)

print(fml_fpg_m1)

# 用 rms::ols（不要用 lm），这样才能用 rms::Predict
fit_fpg_m1 <- rms::ols(fml_fpg_m1, data = md_v2, x = TRUE, y = TRUE)

# 用 rms::anova，并复用你上面写的 get_rcs_pvals()
an_fpg_m1 <- anova(fit_fpg_m1)
p_fpg_m1 <- get_rcs_pvals(an_fpg_m1, exposure_var = "Richness")
p_overall_fpg_m1   <- p_fpg_m1$P_Total
p_nonlinear_fpg_m1 <- p_fpg_m1$P_Nonlinear

# Predict：连续结局——输出“相对参考值(默认=datadist的Adjust to，通常为中位数)”的  beta 及95%CI
pred_fpg_m1 <- as.data.frame(
    Predict(fit_fpg_m1, Richness, conf.int = 0.95, ref.zero = TRUE)
)

# 竖线：暴露中位数（与 ref.zero 的参考点保持一致：datadist 默认也通常取中位数）
inset_v1 <- median(md_v2$Richness, na.rm = TRUE)
# 保存路径（沿用前面 fig_dir）
fig_dir <- "Results/RCS_FPG_alpha"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

pdf_file1 <- file.path(fig_dir, "Fig1_RCS_Richness_model3_knots5_overlay.pdf")
# 提取 Predict 输出列（兼容不同列名）
x1 <- as.numeric(pred_fpg_m1$Richness %||% pred_fpg_m1$x)
yhat1 <- as.numeric(pred_fpg_m1$yhat %||% pred_fpg_m1$y)
lower1 <- as.numeric(pred_fpg_m1$lower)
upper1 <- as.numeric(pred_fpg_m1$upper)
# 在调用绘图函数前，设置图形参数
plot_rcs_hist_overlay(
    x = x1, yhat = yhat1, lower = lower1, upper = upper1,
    xlab = "Richness",
    ylab_left = expression("FPG" ~ beta ~ "(95% CI)"),
    inset_v = inset_v1,
    col_line = "#93C8ED",
    col_ribbon = adjustcolor("#93C8ED", alpha.f = 0.35),
    hist_x = md_v2$Richness,
    breaks = 25,
    p_overall = p_overall_fpg_m1,
    p_nonlinear = p_nonlinear_fpg_m1,
    ref_hline = 0,                 # y轴参考线：beta=0
    knots_k = k_fixed1,
    pdf_file = pdf_file1,
    save = TRUE,
    annotate_intersection = FALSE,  # 保持不标注“曲线×参考线”交点（可按需改 TRUE）
    annotate_inset_ref = TRUE,      # 打开“中位数竖线×参考线”交点标注
    inset_ref_digits = 2,
    las_left = 1 # 设置左侧 y 轴刻度标签为水平方向
)
# Shannon -> FPG-----------------------------------
cov3 <- model_covariates$model3
k_fixed1 <- 3

# 确保 rms 需要的 datadist 已设置
dd <- datadist(md_v2)
options(datadist = "dd")

fml_fpg_m2 <- as.formula(
  paste0(
    "FPG ~ rcs(Shannon, ", k_fixed1, ") + ",
    paste(cov3, collapse = " + ")
  )
)

print(fml_fpg_m2)

# 用 rms::ols（不要用 lm），这样才能用 rms::Predict
fit_fpg_m2 <- rms::ols(fml_fpg_m2, data = md_v2, x = TRUE, y = TRUE)

# 用 rms::anova，并复用你上面写的 get_rcs_pvals()
an_fpg_m2 <- anova(fit_fpg_m2)
p_fpg_m2 <- get_rcs_pvals(an_fpg_m2, exposure_var = "Shannon")
p_overall_fpg_m2   <- p_fpg_m2$P_Total
p_nonlinear_fpg_m2 <- p_fpg_m2$P_Nonlinear

# Predict：连续结局——输出“相对参考值(默认=datadist的Adjust to，通常为中位数)”的  beta 及95%CI
pred_fpg_m2 <- as.data.frame(
    Predict(fit_fpg_m2, Shannon, conf.int = 0.95, ref.zero = TRUE)
)

# 竖线：暴露中位数（与 ref.zero 的参考点保持一致：datadist 默认也通常取中位数）
inset_v2 <- median(md_v2$Shannon, na.rm = TRUE)
# 保存路径（沿用前面 fig_dir）
fig_dir <- "Results/RCS_FPG_alpha"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
pdf_file2 <- file.path(fig_dir, "Fig2_RCS_Shannon_model3_knots5_overlay.pdf")
# 提取 Predict 输出列（兼容不同列名）
x2 <- as.numeric(pred_fpg_m2$Shannon %||% pred_fpg_m2$x)
yhat2 <- as.numeric(pred_fpg_m2$yhat %||% pred_fpg_m2$y)
lower2 <- as.numeric(pred_fpg_m2$lower)
upper2 <- as.numeric(pred_fpg_m2$upper)
# 在调用绘图函数前，设置图形参数
plot_rcs_hist_overlay(
    x = x2, yhat = yhat2, lower = lower2, upper = upper2,
    xlab = "Shannon",
    ylab_left = expression("FPG" ~ beta ~ "(95% CI)"),
    inset_v = inset_v2,
    col_line = "#F7C1C1",
    col_ribbon = adjustcolor("#F7C1C1", alpha.f = 0.35),
    hist_x = md_v2$Shannon,
    breaks = 25,
    p_overall = p_overall_fpg_m2,
    p_nonlinear = p_nonlinear_fpg_m2,
    ref_hline = 0,                 # y轴参考线：beta=0
    knots_k = k_fixed1,
    pdf_file = pdf_file2,
    save = TRUE,
    annotate_intersection = FALSE,  # 保持不标注“曲线×参考线”交点（可按需改 TRUE）
    annotate_inset_ref = TRUE,      # 打开“中位数竖线×参考线”交点标注
    inset_ref_digits = 2,
    las_left = 1 # 设置左侧 y 轴刻度标签为水平方向
)

# Pielou -> FPG-----------------------------------
cov3 <- model_covariates$model3
k_fixed1 <- 3
# 确保 rms 需要的 datadist 已设置
dd <- datadist(md_v2)
options(datadist = "dd")
fml_fpg_m3 <- as.formula(
  paste0(
    "FPG ~ rcs(Pielou, ", k_fixed1, ") + ",
    paste(cov3, collapse = " + ")
  )
)
print(fml_fpg_m3)
# 用 rms::ols（不要用 lm），这样才能用 rms::Predict
fit_fpg_m3 <- rms::ols(fml_fpg_m3, data = md_v2, x = TRUE, y = TRUE)
# 用 rms::anova，并复用你上面写的 get_rcs_pvals()
an_fpg_m3 <- anova(fit_fpg_m3)
p_fpg_m3 <- get_rcs_pvals(an_fpg_m3, exposure_var = "Pielou")
p_overall_fpg_m3   <- p_fpg_m3$P_Total
p_nonlinear_fpg_m3 <- p_fpg_m3$P_Nonlinear
# Predict：连续结局——输出“相对参考值(默认=datadist的Adjust to，通常为中位数)”的  beta 及95%CI
pred_fpg_m3 <- as.data.frame(
    Predict(fit_fpg_m3, Pielou, conf.int = 0.95, ref.zero = TRUE)
)
# 竖线：暴露中位数（与 ref.zero 的参考点保持一致：datadist 默认也通常取中位数）
inset_v3 <- median(md_v2$Pielou, na.rm = TRUE)
# 保存路径（沿用前面 fig_dir）
fig_dir <- "Results/RCS_FPG_alpha"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
pdf_file3 <- file.path(fig_dir, "Fig3_RCS_Pielou_model3_knots5_overlay.pdf")
# 提取 Predict 输出列（兼容不同列名）
x3 <- as.numeric(pred_fpg_m3$Pielou %||% pred_fpg_m3$x)
yhat3 <- as.numeric(pred_fpg_m3$yhat %||% pred_fpg_m3$y)
lower3 <- as.numeric(pred_fpg_m3$lower)
upper3 <- as.numeric(pred_fpg_m3$upper)

# 在调用绘图函数前，设置图形参数
plot_rcs_hist_overlay(
    x = x3, yhat = yhat3, lower = lower3, upper = upper3,
    xlab = "Pielou",
    ylab_left = expression("FPG" ~ beta ~ "(95% CI)"),
    inset_v = inset_v3,
    col_line = "#F9C780",
    col_ribbon = adjustcolor("#F9C780", alpha.f = 0.35),
    hist_x = md_v2$Pielou,
    breaks = 25,
    p_overall = p_overall_fpg_m3,
    p_nonlinear = p_nonlinear_fpg_m3,
    ref_hline = 0,                 # y轴参考线：beta=0
    knots_k = k_fixed1,
    pdf_file = pdf_file3,
    save = TRUE,
    annotate_intersection = FALSE,  # 保持不标注“曲线×参考线”交点（可按需改 TRUE）
    annotate_inset_ref = TRUE,      # 打开“中位数竖线×参考线”交点标注
    inset_ref_digits = 2,
    las_left = 1 # 设置左侧 y 轴刻度标签为水平方向
)
# Simpson -> FPG-----------------------------------
cov3 <- model_covariates$model3
k_fixed1 <- 5
# 确保 rms 需要的 datadist 已设置
dd <- datadist(md_v2)
options(datadist = "dd")
fml_fpg_m4 <- as.formula(
  paste0(
    "FPG ~ rcs(Simpson, ", k_fixed1, ") + ",
    paste(cov3, collapse = " + ")
  )
)
print(fml_fpg_m4)
# 用 rms::ols（不要用 lm），这样才能用 rms::Predict
fit_fpg_m4 <- rms::ols(fml_fpg_m4, data = md_v2, x = TRUE, y = TRUE)
# 用 rms::anova，并复用你上面写的 get_rcs_pvals()
an_fpg_m4 <- anova(fit_fpg_m4)
p_fpg_m4 <- get_rcs_pvals(an_fpg_m4, exposure_var = "Simpson")
p_overall_fpg_m4   <- p_fpg_m4$P_Total
p_nonlinear_fpg_m4 <- p_fpg_m4$P_Nonlinear
# Predict：连续结局——输出“相对参考值(默认=datadist的Adjust to，通常为中位数)”的  beta 及95%CI
pred_fpg_m4 <- as.data.frame(
    Predict(fit_fpg_m4, Simpson, conf.int = 0.95, ref.zero = TRUE)
)

# 竖线：暴露中位数（与 ref.zero 的参考点保持一致：datadist 默认也通常取中位数）
inset_v4 <- median(md_v2$Simpson, na.rm = TRUE)
# 保存路径（沿用前面 fig_dir）
fig_dir <- "Results/RCS_FPG_alpha"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
pdf_file4 <- file.path(fig_dir, "Fig4_RCS_Simpson_model3_knots5_overlay.pdf")
# 提取 Predict 输出列（兼容不同列名）
x4 <- as.numeric(pred_fpg_m4$Simpson %||% pred_fpg_m4$x)
yhat4 <- as.numeric(pred_fpg_m4$yhat %||% pred_fpg_m4$y)
lower4 <- as.numeric(pred_fpg_m4$lower)
upper4 <- as.numeric(pred_fpg_m4$upper)
# 在调用绘图函数前，设置图形参数
plot_rcs_hist_overlay(
    x = x4, yhat = yhat4, lower = lower4, upper = upper4,
    xlab = "Simpson",
    ylab_left = expression("FPG" ~ beta ~ "(95% CI)"),
    inset_v = inset_v4,
    col_line = "#AEDBD2",
    col_ribbon = adjustcolor("#AEDBD2", alpha.f = 0.35),
    hist_x = md_v2$Simpson,
    breaks = 25,
    p_overall = p_overall_fpg_m4,
    p_nonlinear = p_nonlinear_fpg_m4,
    ref_hline = 0,                 # y轴参考线：beta=0
    knots_k = k_fixed1,
    pdf_file = pdf_file4,
    save = TRUE,
    annotate_intersection = FALSE,  # 保持不标注“曲线×参考线”交点（可按需改 TRUE）
    annotate_inset_ref = TRUE,      # 打开“中位数竖线×参考线”交点标注
    inset_ref_digits = 2,
    las_left = 1 # 设置左侧 y 轴刻度标签为水平方向
)
