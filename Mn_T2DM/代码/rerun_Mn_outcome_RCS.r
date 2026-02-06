# -----------------------------------------
# 题目：RCS曲线重新绘制，加柱状图底图
# --------------------------------------------
setwd("D:/lianghao/金属锰—肠道菌—疾病/血清锰-肠道菌-糖尿病相关")
rm(list = ls())

# 加载必要的包
# install.packages("pak")
# pak::pkg_install(c("ggplot2", "rms", "grid"))
library(ggplot2)
library(rms)
library(ggrcs)
library(extrafont)
# 自编函数
source("代码\\myfunctions.r") # 加载自编函数
# 加载字体到 Windows 设备（每次启动 R 后需要运行一次）
loadfonts(device = "win")

# ========== 字体与输出控制（新增） ==========
if (.Platform$OS.type == "windows") {
  windowsFonts(TimesNR = windowsFont("Times New Roman"))
}
options(rcs_plot_family = if (.Platform$OS.type == "windows") "TimesNR" else "Times")
save_plots <- FALSE   # FALSE=仅预览到绘图面板；TRUE=保存 PDF

# 读取数据
load("数据\\imputed_df_v6.RData")
data <- imputed_df_v6
# 查看数据
colnames(data)

# 建立限制性立方样条模型并提取相关参数
# 创建 datadist 对象并指定
dd <- datadist(data)
options(datadist = "dd") # 设置全局选项

# 1. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "ln_FPG")

# 2. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 自变量
exposure <- "rcs(ln_Serum_Mn, 3)"

# ====== 工具函数：提取/格式化 P 值，与通用绘图（直方图 + 曲线） ======
get_p_from_anova <- function(an_tab, row_name, pattern = NULL) {
  rn <- rownames(an_tab)
  idx <- match(row_name, rn)
  if (is.na(idx)) {
    if (is.null(pattern)) pattern <- row_name
    hits <- grep(pattern, rn, ignore.case = TRUE)
    if (length(hits)) idx <- hits[1]
  }
  if (is.na(idx)) return(NA_real_)
  p_raw <- suppressWarnings(as.numeric(an_tab[idx, "P"]))
  if (is.na(p_raw)) {
    chi <- suppressWarnings(as.numeric(an_tab[idx, "Chi-Square"]))
    df  <- suppressWarnings(as.numeric(an_tab[idx, "d.f."]))
    if (!is.na(chi) && !is.na(df)) p_raw <- pchisq(chi, df, lower.tail = FALSE)
  }
  p_raw
}

format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("%.3f", p)
}

# 通用绘图：在同一页叠加直方图（右轴）与曲线（左轴）
plot_rcs_hist_overlay <- function(
  x, yhat, lower, upper,
  xlab = "Exposure", ylab_left = "OR (95% CI)",
  inset_v = NULL, # 竖线位置
  col_line = "#87CEEB", col_ribbon = adjustcolor("#87CEEB", alpha.f = 0.50),
  right_ylabel = expression(paste(Probability~density)),
  p_overall = NA, p_nonlinear = NA,
  left_ylim = NULL, ref_hline = NULL,   # 新增：自定义左轴范围与参考线
  pdf_file = NULL, save = FALSE,        # 新增：预览/保存开关
  width_cm = 18.69, height_cm = 15.27,
  font_family = getOption("rcs_plot_family", "Times"),
  breaks = NULL,                          # 新增：直方图分箱控制
  hist_x = NULL,                 # 新增：直方图用的数据（如 data$ln_Serum_Mn）
  limit_hist_to_curve = TRUE     # 新增：是否将直方图数据限制到曲线的 x 范围
) {
  x_range <- range(x, na.rm = TRUE)
  if (is.null(left_ylim) || any(!is.finite(left_ylim))) {
    left_ylim <- range(pretty(c(lower, upper, ref_hline)), na.rm = TRUE)
  }

  # 输出设备（保存为 PDF 或仅预览）
  if (isTRUE(save) && !is.null(pdf_file)) {
    use_cairo <- requireNamespace("Cairo", quietly = TRUE) && capabilities("cairo")
    w <- width_cm / 2.54; h <- height_cm / 2.54
    if (use_cairo) {
      Cairo::CairoPDF(pdf_file, width = w, height = h, bg = "white", family = "Times New Roman")
    } else {
      pdf(pdf_file, width = w, height = h, paper = "special", bg = "white", useDingbats = FALSE, family = "serif")
    }
    op <- par(no.readonly = TRUE)
    on.exit({ par(op); dev.off() }, add = TRUE)
  } else {
    op <- par(no.readonly = TRUE)
    on.exit({ par(op) }, add = TRUE)
  }

  par(family = font_family)
  par(mar = c(5, 4, 1, 5))

  # 直方图（右轴）使用 hist_x；若未提供则用曲线的 x
  hist_data <- if (is.null(hist_x)) x else hist_x
  # 将直方图数据裁剪到曲线范围，保证两者一致
  if (isTRUE(limit_hist_to_curve)) {
    hist_data <- hist_data[is.finite(hist_data) & hist_data >= x_range[1] & hist_data <= x_range[2]]
  }
  # 分箱：若指定数值 breaks，则在 x_range 内生成等距断点；否则使用 nclass.FD 但仍受限于范围
  if (is.null(breaks)) {
    nb <- max(30, nclass.FD(hist_data))
    hist_breaks <- pretty(x_range, n = nb)
  } else if (length(breaks) == 1L && is.numeric(breaks)) {
    hist_breaks <- pretty(x_range, n = breaks)
  } else {
    # 若用户提供了向量 breaks，使用其与范围的交集
    hist_breaks <- sort(unique(c(x_range, breaks[breaks >= x_range[1] & breaks <= x_range[2]])))
  }

  hist(hist_data,
       axes = FALSE, xlab = "", ylab = "",
       xlim = x_range,
       breaks = hist_breaks,
       col = adjustcolor("#ffffff", alpha.f = 0.3),
       border = "black", main = "", freq = FALSE)
  axis(4)
  mtext(right_ylabel, side = 4, line = 3, outer = FALSE)

  # 曲线（左轴）
  par(new = TRUE)
  plot(x, yhat, type = "l",
       lty = 1, lwd = 2, col = col_line,
       xlim = x_range, ylim = left_ylim,
       xlab = xlab, ylab = ylab_left, axes = TRUE)

  # 置信区间
  ord <- order(x)
  polygon(c(x[ord], rev(x[ord])),
          c(lower[ord], rev(upper[ord])),
          col = col_ribbon, border = col_ribbon)

  # 参考线和竖线
  if (!is.null(ref_hline) && is.finite(ref_hline)) abline(h = ref_hline, col = "#ee1212", lty = 3)
  if (!is.null(inset_v) && is.finite(inset_v)) abline(v = inset_v, col = "#f70606", lty = 3)

  # P 值标注
  legend("topright",
         paste0("P-overall ", format_p(p_overall), "\nP-non-linear ", format_p(p_nonlinear)),
         bty = "n", cex = 0.8)
}

##### diabetes-----------------------------------------------------------------------------------
# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure, covariates = covariate)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, ln_Serum_Mn, fun = exp, ref.zero = TRUE)

# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(2.64)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

# 计算 P 值（overall 与 non-linear）
an_tab <- as.data.frame(an)
p_overall_diab   <- get_p_from_anova(an_tab, "ln_Serum_Mn", pattern = "ln_Serum_Mn")
p_nonlinear_diab <- get_p_from_anova(an_tab, "Nonlinear",   pattern = "Nonlinear")

# 绘图（直方图 + OR 曲线）
plot_rcs_hist_overlay(
  x = OR$ln_Serum_Mn,
  yhat = OR$yhat,
  lower = OR$lower,
  upper = OR$upper,
  xlab = "Log transformed Mn, μg/L",
  ylab_left = "T2DM, Adjusted OR (95% CI)",
  inset_v = x_at_y_1,
  p_overall = p_overall_diab,
  p_nonlinear = p_nonlinear_diab,
  left_ylim = NULL,            # 自动
  ref_hline = 1,               # OR 参考线
  pdf_file = if (save_plots) "主要结果/图片/RCS/Dose-response relationship between serum manganese and diabetes.pdf" else NULL,
  save = save_plots,
  width_cm = 18.69, height_cm = 15.27,
  font_family = getOption("rcs_plot_family"),
  breaks = 15,               # 直方图分箱数
  hist_x = data$ln_Serum_Mn   # 直方图用的数据
)

##### IFG or diabetes--------------------------------------------------------------------
#  自变量
exposure <- "ln_Serum_Mn"
# 构建logistic回归模型公式
covarite_formula <- formula_rcs(outcome[2], exposure, 3, covariates = covariate)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, ln_Serum_Mn, fun = exp, ref.zero = TRUE)

# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(2.64)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

an_tab <- as.data.frame(an)
p_overall_ifg   <- get_p_from_anova(an_tab, "ln_Serum_Mn", pattern = "ln_Serum_Mn")
p_nonlinear_ifg <- get_p_from_anova(an_tab, "Nonlinear",   pattern = "Nonlinear")

plot_rcs_hist_overlay(
  x = OR$ln_Serum_Mn,
  yhat = OR$yhat,
  lower = OR$lower,
  upper = OR$upper,
  xlab = "Log transformed Mn, μg/L",
  ylab_left = "IFG or T2DM, Adjusted OR (95% CI)",
  inset_v = x_at_y_1,
  p_overall = p_overall_ifg,
  p_nonlinear = p_nonlinear_ifg,
  left_ylim = NULL,            # 自动
  ref_hline = 1,
  pdf_file = if (save_plots) "主要结果/图片/RCS/Dose-response relationship between serum manganese and IFG or diabetes.pdf" else NULL,
  save = save_plots,
  width_cm = 18.69, height_cm = 15.27,
  font_family = getOption("rcs_plot_family"),
  breaks = 15,               # 直方图分箱数
  hist_x = data$ln_Serum_Mn   # 直方图用的数据
)

# ==============FPG（线性回归，显示 Beta 与 95%CI）================
library(plotRCS) ## plotRCS包用于绘制限制性立方样条图
# # install.packages("remotes")
# # pak::pkg_install("segmented")
# # library(scitable)
# # library(segmented)

exposure <- "ln_Serum_Mn" ## 自变量
# # 回归模型公式
covarite_formula <- formula_rcs(outcome[3], exposure, 3, covariates = covariate) # # 回归模型公式

# # 使用限制性立方样条模型
fit <- ols(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit))) ## 获取p for non-linear

# # 用 Predict 生成线性预测（不取 exp）
OR_FPG <- Predict(fit, ln_Serum_Mn, ref.zero = TRUE)
# View(OR_FPG)
an_tab <- as.data.frame(an)
p_overall_fpg   <- get_p_from_anova(an_tab, "ln_Serum_Mn", pattern = "ln_Serum_Mn")
p_nonlinear_fpg <- get_p_from_anova(an_tab, "Nonlinear",   pattern = "Nonlinear")

plot_rcs_hist_overlay(
  x = OR_FPG$ln_Serum_Mn,
  yhat = OR_FPG$yhat,
  lower = OR_FPG$lower,
  upper = OR_FPG$upper,
  xlab = "Log transformed Mn, μg/L",
  ylab_left = "Log transformed FPG, mmol/L, β (95%CI)",
  inset_v = 2.64,
  p_overall = p_overall_fpg,
  p_nonlinear = p_nonlinear_fpg,
  left_ylim = c(-0.1, 0.1),    ## 固定 Beta 展示范围
  ref_hline = 0,               ## 线性模型参考线
  pdf_file = if (save_plots) "主要结果/图片/RCS/Dose-response relationship between serum manganese and FPG.pdf" else NULL,
  save = save_plots,
  width_cm = 18.69, height_cm = 15.27,
  font_family = getOption("rcs_plot_family"),
  breaks = 15,               # 直方图分箱数
  hist_x = data$ln_Serum_Mn   # 直方图用的数据
)
