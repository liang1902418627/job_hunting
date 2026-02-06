# -------------------------------------------------
# 题目：暴露与结局的限制性立方样条模型
# 暴露：Eating_freq、Eating_window
# 结局：mortstat、CVDmortality、CancerMortality
# 生存时间：time
# 协变量：
# 模型1：不校正协变量
# 模型2：校正Age，Gender，Racial
# 模型3：Age，Gender，Racial，Education，Marriage，Economic.status，BMI，Smoking，Drinking，
# HBP，DM，DEI, CARB_average、FAT_average、PROT_average、PA_total、sleep_time
# 方法：标准Cox比例风险回归模型，限制性立方样条函数
# ----------------------------------------------------
rm(list = ls())
setwd("D:/lianghao/孙莹数据分析20251215/")
# 加载所需包
paks <- c("survival", "survminer", "dplyr", "readr", "tidyverse", "rms")
lapply(paks, require, character.only = TRUE)

# 读取数据
dt <- readRDS("workspace/projects/data/final_analysis_data.rds")
# 把结局变量转为数值型
dt2 <- dt %>%
    mutate(across(c(mortstat, CVDmortality, CancerMortality), ~ as.numeric(as.character(.))))

table(dt2$mortstat)
table(dt2$CVDmortality)
table(dt2$CancerMortality)

# 配置基本信息-------------------------------------
base_info <- list(
    time_var = "time",
    status_vars = c("mortstat", "CVDmortality", "CancerMortality"),
    exposure_vars = c("Eating_freq", "Eating_window"),
    covariates_model1 = NULL,
    covariates_model2 = c("Age", "Gender", "Racial"),
    covariates_model3 = c(
        "Age", "Gender", "Racial", "Education", "Marriage", "Economic.status",
        "BMI", "Smoking", "Drinking", "HBP", "DM", "DEI", "CARB_average", "FAT_average", "PROT_average", "PA_total", "sleep_time"
    ),
    data = dt2
)
# 构建数据分布对象
dd <- datadist(base_info$data)
options(datadist = "dd")
# Eating_freq vs mortstat-------------------------------------
# 模型1 - 不校正协变量
for (aic in c(3:7)) {
    fit_rcs_eating_freq_mortstat_model3 <- cph(
        as.formula(
            paste0(
                "Surv(", base_info$time_var, ", ", base_info$status_vars[1], ") ~ ",
                "rcs(", base_info$exposure_vars[1], ", ", aic, ")",
                if (is.null(base_info$covariates_model3)) {
                    ""
                } else {
                    paste0(" + ", paste(base_info$covariates_model3, collapse = " + "))
                }
            )
        ),
        data = base_info$data,
        x = TRUE,
        y = TRUE,
        surv = TRUE
    )
    cat("AIC for Eating_freq with", aic, "knots:", AIC(fit_rcs_eating_freq_mortstat_model3), "\n")
}
# AIC for Eating_freq with 3 knots: 6787.678 
# AIC for Eating_freq with 4 knots: 6786.557 
# AIC for Eating_freq with 5 knots: 6785.572 
# AIC for Eating_freq with 6 knots: 6786.903 
# AIC for Eating_freq with 7 knots: 6787.181 
# 选择5个节点
fit_rcs_eating_freq_mortstat_model3 <- cph(
    as.formula(
        paste0(
            "Surv(", base_info$time_var, ", ", base_info$status_vars[1], ") ~ ",
            "rcs(", base_info$exposure_vars[1], ", 5)",
            if (is.null(base_info$covariates_model3)) {
                ""
            } else {
                paste0(" + ", paste(base_info$covariates_model3, collapse = " + "))
            }
        )
    ),
    data = base_info$data,
    x = TRUE,
    y = TRUE,
    surv = TRUE
)
summary(fit_rcs_eating_freq_mortstat_model3)
# 提取p-overall和p-nonlinear
anova_eating_freq_mortstat_model3 <- anova(fit_rcs_eating_freq_mortstat_model3)
p_overall_eating_freq_mortstat_model3 <- anova_eating_freq_mortstat_model3["Eating_freq", "P"]
p_nonlinear_eating_freq_mortstat_model3 <- anova_eating_freq_mortstat_model3[" Nonlinear", "P"]

# 预测值和置信区间
pred_eating_freq_mortstat_model3 <- Predict(
    fit_rcs_eating_freq_mortstat_model3,
    Eating_freq,
    conf.int = 0.95,
    fun = exp,
    ref.zero = TRUE
)
# View(pred_eating_freq_mortstat_model3)

# x轴插线
x_inset <- 8.31

# -------------------------------------------------
# 绘图
# ========= 重构后的绘图代码（RCS + 直方图叠加 OR 曲线）=========

format_p <- function(p) {
    if (is.na(p)) {
        return("NA")
    }
    if (p < 0.001) {
        return("< 0.001")
    }
    sprintf("%.3f", p)
}

p_overall <- p_overall_eating_freq_mortstat_model3
p_nonlinear <- p_nonlinear_eating_freq_mortstat_model3
p <- c(p_overall, p_nonlinear)

# 颜色
col <- c("white", "#ec8989")
ribbon_col <- adjustcolor(col[2], alpha.f = 0.50)

# 设置路径---------
# workspace\projects\results\pictures\RCS\
dir <- "workspace/projects/results/pictures/RCS/"
if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
}

# 小工具：空合并运算（draw_rcs_overlay 中已用到）
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# 通用绘图：在同一页叠加直方图（右轴）与曲线（左轴）
plot_rcs_hist_overlay <- function(
    x, yhat, lower, upper,
    xlab = "Exposure", ylab_left = "OR (95% CI)",
    inset_v = NULL, # 竖线位置(支持向量)
    col_line = "#87CEEB", col_ribbon = adjustcolor("#87CEEB", alpha.f = 0.50),
    right_ylabel = expression(paste(Probability ~ density)),
    p_overall = NA, p_nonlinear = NA,
    left_ylim = NULL, ref_hline = NULL,
    pdf_file = NULL, save = FALSE,
    width_cm = 18.69, height_cm = 15.27,
    font_family = getOption("rcs_plot_family", "Times"),
    breaks = NULL,
    hist_x = NULL,
    limit_hist_to_curve = TRUE,
    cex_axis = 1.2,
    cex_lab = 1.3,
    cex_legend = 1.1,
    cex_mtext = 1.2,
    knots_k = NA,
    annotate_inset = TRUE,
    inset_label_digits = 2,
    # 新增：直方图外观
    hist_fill = NULL,
    hist_border = "#000000",
    hist_alpha = 0.5,
    hist_lwd = 1) {
    x_range <- range(x, na.rm = TRUE)
    if (is.null(left_ylim) || any(!is.finite(left_ylim))) {
        left_ylim <- range(pretty(c(lower, upper, ref_hline)), na.rm = TRUE)
    }

    # 打开设备...
    if (save && !is.null(pdf_file)) {
        pdf(pdf_file, width = width_cm / 2.54, height = height_cm / 2.54, family = font_family)
        on.exit(dev.off(), add = TRUE)
    }

    # 直方图（右轴）——使用矩形柱，而非针状线
    if (!is.null(hist_x)) {
        if (isTRUE(limit_hist_to_curve)) {
            hist_x <- hist_x[hist_x >= x_range[1] & hist_x <= x_range[2]]
        }
        hist_data <- hist(hist_x, breaks = breaks, plot = FALSE)
        hist_max <- max(hist_data$density, na.rm = TRUE)
        hist_ylim <- c(0, hist_max * 1.1)

        op <- par(no.readonly = TRUE)
        on.exit(par(op), add = TRUE)
        par(mar = c(5, 4, 4, 4) + 0.1)

        # 先画空图框（右轴）
        plot(NA, NA,
            xlim = x_range, ylim = hist_ylim,
            xlab = xlab, ylab = "", axes = FALSE
        )

        # 不填充：当 hist_fill 为 NULL 时，仅画边框
        fill_col <- if (is.null(hist_fill)) NA else adjustcolor(hist_fill, alpha.f = hist_alpha)

        brks <- hist_data$breaks
        dens <- hist_data$density
        if (length(dens) > 0) {
            for (i in seq_along(dens)) {
                if (is.finite(dens[i])) {
                    rect(
                        xleft  = brks[i],
                        ybottom = 0,
                        xright = brks[i + 1],
                        ytop   = dens[i],
                        col    = fill_col,          # NA -> 不填充
                        border = hist_border,       # 黑色边框
                        lwd    = hist_lwd
                    )
                }
            }
        }
        axis(4, las = 1, cex.axis = cex_axis)
        mtext(right_ylabel, side = 4, line = 3, cex = cex_mtext)
    }

    # 曲线（左轴）
    par(new = TRUE)
    plot(x, yhat,
        type = "l",
        lty = 1, lwd = 2, col = col_line,
        xlim = x_range, ylim = left_ylim,
        xlab = xlab, ylab = ylab_left, axes = TRUE
    )

    # 置信区间
    ord <- order(x)
    polygon(c(x[ord], rev(x[ord])),
        c(lower[ord], rev(upper[ord])),
        col = col_ribbon, border = col_ribbon
    )

    # 参考线
    if (!is.null(ref_hline) && is.finite(ref_hline)) {
        abline(h = ref_hline, col = "#ee1212", lty = 2)
    }

    # 插值竖线 + 标注
    if (!is.null(inset_v) && is.numeric(inset_v)) {
        inset_v <- inset_v[is.finite(inset_v)]
        if (length(inset_v) > 0) {
            abline(v = inset_v, col = "#f70606", lty = 2)
            rug(inset_v, col = "#f70606", lwd = 2, ticksize = 0.03)

            if (isTRUE(annotate_inset)) {
                usr <- par("usr")
                y_span <- usr[4] - usr[3]
                y_text <- usr[3] + 0.06 * y_span
                for (v in inset_v) {
                    lab <- paste0("x = ", round(v, inset_label_digits))
                    text(x = v, y = y_text, labels = lab, col = "#f70606", cex = 0.9, pos = 3, xpd = NA)
                }
            }
        }
    }

    # P 值与节点数
    legend_text <- paste0(
        if (is.finite(knots_k)) paste0("Knots: ", knots_k, "\n") else "",
        "P-overall ", format_p(p_overall), "\nP-non-linear ", format_p(p_nonlinear)
    )
    legend("topright", legend_text, bty = "n", cex = cex_legend)
}

# ====== 将 Predict 输出整理后调用通用模版绘图 ======
draw_rcs_overlay <- function(
    pred_df,
    hist_x = NULL,
    xlab = "Eating frequency",
    ylab_left = "HR (95% CI)",
    inset_v = NULL,
    ref_hline = 1,
    p_overall = NA,
    p_nonlinear = NA,
    save = TRUE,
    filename = file.path(dir, "Eating_freq_mortstat_model3_RCS_overlay.pdf"),
    col_line = "#87CEEB",
    col_ribbon = adjustcolor("#87CEEB", alpha.f = 0.50),
    right_ylabel = expression(paste(Probability ~ density)),
    left_ylim = NULL,
    breaks = NULL,
    width_cm = 18.69, height_cm = 15.27,
    font_family = getOption("rcs_plot_family", "Times"),
    limit_hist_to_curve = TRUE,
    knots_k = NA # 新增：传入节点数用于标注
    ) {
    # 从 Predict 输出中取向量
    x <- as.numeric(pred_df$Eating_freq %||% pred_df$Eating_window)
    yhat <- as.numeric(pred_df$yhat %||% pred_df$y) # 兼容不同列名
    lower <- as.numeric(pred_df$lower)
    upper <- as.numeric(pred_df$upper)

    # 默认直方图数据：使用暴露原始数据
    if (is.null(hist_x)) {
        hist_x <- if (!all(is.na(pred_df$Eating_freq %||% NA))) {
            base_info$data[[base_info$exposure_vars[1]]]
        } else {
            base_info$data[[base_info$exposure_vars[2]]]
        }
    }

    # 绘制
    plot_rcs_hist_overlay(
        x = x, yhat = yhat, lower = lower, upper = upper,
        xlab = xlab, ylab_left = ylab_left,
        inset_v = inset_v,
        col_line = col_line, col_ribbon = col_ribbon,
        right_ylabel = right_ylabel,
        p_overall = p_overall, p_nonlinear = p_nonlinear,
        left_ylim = left_ylim, ref_hline = ref_hline,
        pdf_file = filename, save = save,
        width_cm = width_cm, height_cm = height_cm,
        font_family = font_family,
        breaks = breaks,
        hist_x = hist_x,
        limit_hist_to_curve = limit_hist_to_curve,
        knots_k = knots_k
    )
}

# ====== 新增：按 AIC 选节点、拟合并返回预测 ======
fit_rcs_model3 <- function(exposure, outcome, knots_range = 3:7) {
    covs <- base_info$covariates_model3
    form_base <- paste0(
        "Surv(", base_info$time_var, ", ", outcome, ") ~ ",
        "rcs(", exposure, ", %K%)",
        if (length(covs)) paste0(" + ", paste(covs, collapse = " + ")) else ""
    )

    aic_vec <- numeric(length(knots_range))
    names(aic_vec) <- knots_range
    for (i in seq_along(knots_range)) {
        k <- knots_range[i]
        fml <- as.formula(gsub("%K%", k, form_base, fixed = TRUE))
        fit_tmp <- cph(fml, data = base_info$data, x = TRUE, y = TRUE, surv = TRUE)
        aic_vec[i] <- AIC(fit_tmp)
    }
    for (i in seq_along(knots_range)) {
        cat("AIC for", exposure, "->", outcome, "with", knots_range[i], "knots:", aic_vec[i], "\n")
    }
    best_k <- knots_range[which.min(aic_vec)]
    cat("Selected", best_k, "knots for", exposure, "->", outcome, "\n")

    fml_best <- as.formula(gsub("%K%", best_k, form_base, fixed = TRUE))
    fit_best <- cph(fml_best, data = base_info$data, x = TRUE, y = TRUE, surv = TRUE)

    an <- anova(fit_best)
    p_overall <- an[exposure, "P"]
    p_nonlinear <- an[" Nonlinear", "P"]

    # Predict 动态传参：针对 exposure 生成默认网格
    pr_args <- list(object = fit_best, conf.int = 0.95, fun = exp, ref.zero = TRUE)
    pr_args[[exposure]] <- NA
    pred <- do.call(Predict, pr_args)

    list(
        fit = fit_best,
        pred = as.data.frame(pred),
        best_k = best_k,
        p_overall = p_overall,
        p_nonlinear = p_nonlinear
    )
}

# ====== 新增：单个组合的绘图封装（不使用循环绘图）======
plot_rcs_for_combo <- function(exposure, outcome, xlab, file_stub,
                               inset_v = NULL, breaks = 10,
                               view_pred = TRUE) { # 新增：自动 View 预测表
    res <- fit_rcs_model3(exposure = exposure, outcome = outcome, knots_range = 3:7)

    # 直接查看预测表（VS Code 支持 View）
    if (isTRUE(view_pred)) {
        View(res$pred, title = paste0("Predict: ", exposure, " -> ", outcome))
        x_col <- res$pred[[exposure %||% base_info$exposure_vars[2]]]
        cat(
            "[", exposure, "->", outcome, "] x grid: min =", min(x_col, na.rm = TRUE),
            ", max =", max(x_col, na.rm = TRUE),
            ", n unique =", length(unique(x_col)), "\n"
        )
    }

    # 若未给定竖线位置，默认用中位数
    if (is.null(inset_v)) {
        inset_v <- median(base_info$data[[exposure]], na.rm = TRUE)
        cat("Vertical reference line (", exposure, ") set to median:", inset_v, "\n")
    }

    filename <- file.path(dir, paste0(file_stub, "_model3_RCS_overlay.pdf"))
    ylab_left <- paste0(outcome, " HR (95% CI)")
    draw_rcs_overlay(
        pred_df = res$pred,
        hist_x = base_info$data[[exposure]],
        xlab = xlab,
        ylab_left = ylab_left,
        inset_v = inset_v,
        ref_hline = 1,
        p_overall = res$p_overall,
        p_nonlinear = res$p_nonlinear,
        save = TRUE,
        filename = filename,
        col_line = col[2],
        col_ribbon = adjustcolor(col[2], alpha.f = 0.35),
        right_ylabel = expression(paste(Probability ~ density)),
        left_ylim = NULL,
        breaks = breaks,
        knots_k = res$best_k
    )
    invisible(res)
}

# ====== 新增：其余暴露-结局组合的单图调用（无循环，便于单独细改）======
# 1) Eating_freq -> mortstat
res_freq_mortstat <- plot_rcs_for_combo(
    exposure = "Eating_freq",
    outcome = "mortstat",
    xlab = "Eating frequency",
    file_stub = "Eating_freq_mortstat",
    inset_v = c(3.89, 7.23), # 如需自定义请改这里
    breaks = 10
)

# 1) Eating_freq -> CVDmortality
res_freq_cvd <- plot_rcs_for_combo(
    exposure = "Eating_freq",
    outcome = "CVDmortality",
    xlab = "Eating frequency",
    file_stub = "Eating_freq_CVDmortality",
    inset_v = c(3.49, 4.01, 5.54, 7.91), # 如需自定义请改这里
    breaks = 10
)

# 2) Eating_freq -> CancerMortality
res_freq_cancer <- plot_rcs_for_combo(
    exposure = "Eating_freq",
    outcome = "CancerMortality",
    xlab = "Eating frequency",
    file_stub = "Eating_freq_CancerMortality",
    inset_v = c(4.01, 7.43), # 如需自定义请改这里
    breaks = 10
)

# 3) Eating_window -> mortstat
res_window_all <- plot_rcs_for_combo(
    exposure = "Eating_window",
    outcome = "mortstat",
    xlab = "Eating window",
    file_stub = "Eating_window_mortstat",
    inset_v = c(10.70, 12.26), # 如需自定义请改这里
    breaks = 15
)

# 4) Eating_window -> CVDmortality
res_window_cvd <- plot_rcs_for_combo(
    exposure = "Eating_window",
    outcome = "CVDmortality",
    xlab = "Eating window",
    file_stub = "Eating_window_CVDmortality",
    inset_v = c(10.89, 12.26), # 如需自定义请改这里
    breaks = 15
)

# 5) Eating_window -> CancerMortality
res_window_cancer <- plot_rcs_for_combo(
    exposure = "Eating_window",
    outcome = "CancerMortality",
    xlab = "Eating window",
    file_stub = "Eating_window_CancerMortality",
    inset_v = c(12.26), # 如需自定义请改这里
    breaks = 15
)
