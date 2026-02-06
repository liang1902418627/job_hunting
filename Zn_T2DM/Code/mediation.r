# -------------------------------------------------
# 中介分析
# 暴露：锌（连续），Serum_Zn
# 中介：肠道菌属水平（连续）
# 结局："T2DM", "FPG", "IFG_or_T2DM"
# 协变量：模型3中的协变量
# 方法：mediation中介分析
# c("g_Prauseria", "g_SMB53", "g_Bilophila", "g_Sediminibacterium", "g_Bradyrhizobium", "g_Pseudidiomarina", "g_Ralstonia", "g_Phascolarctobacterium")
# -------------------------------------------------
rm(list = ls())
paks <- c("dplyr", "mediation", "broom", "readr", "purrr", "tibble")
lapply(paks, library, character.only = TRUE)
# 读取数据
md <- readRDS("Data\\md_final_v2.rds")
gut <- readRDS("Data\\gut_data.rds")

# 数据检查
names(md)
names(gut)
gut_scale <- gut$transformed %>%
    filter(row.names(.) %in% md$SampleID)

# 合并数据
gut_scale$SampleID <- row.names(gut_scale) # 添加样本ID列以便合并
final_df <- merge(md, gut_scale, by = "SampleID") # 合并数据框
final_df_for_mediation <- final_df
save(final_df_for_mediation, file = "Data/final_df_for_mediation.RData")
# ===== 新增：用于分析的数据对象（你后面用的是 dt_analysis）=====
dt_analysis <- final_df

# ===== 新增：连续暴露的对比设置（psd: per SD；0->1 表示每+1SD）=====
contrasts <- list(
  per_1SD = list(control = 0, treat = 1)
)

# 设置暴露变量、结局变量、中介变量、协变量
# 中介变量（菌属）
g_vars <- c(
    "g_Prauseria", "g_SMB53", "g_Bilophila", "g_Sediminibacterium", "g_Bradyrhizobium", "g_Pseudidiomarina", "g_Ralstonia", "g_Phascolarctobacterium"
)

# 暴露：Zn(per sd)
x_var <- "psd_Serum_Zn"

# 协变量
c_vars <- c(
  "Age", "Gender", "BMI", "Education", "Marital_status", "family_income_cat", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake", "act_sports_intense", "Family_diabetes_history"
)

# 结局变量
# 两个二分类结局（分别单独分析，先各自去除缺失）, 一个连续结局FPG
y_diabetes <- "T2DM"
y_ifg      <- "IFG_or_T2DM"
y_fpg      <- "FPG"

# 单独对g_Sediminibacterium、psd_Serum_Zn、IFG_or_T2DM分析验证结果
fit.m <- lm(g_Sediminibacterium ~ psd_Serum_Zn + Age + Gender + BMI + Education + Marital_status +
              family_income_cat + Smoking_status + Dringking_status +
              Energy_intake + Zn_intake + act_sports_intense + Family_diabetes_history,
            data = dt_analysis)
fit.y <- glm(IFG_or_T2DM ~ g_Sediminibacterium + psd_Serum_Zn + Age + Gender + BMI + Education + Marital_status +
               family_income_cat + Smoking_status + Dringking_status +
               Energy_intake + Zn_intake + act_sports_intense + Family_diabetes_history,
             data = dt_analysis,
             family = binomial())

med.out <- mediation::mediate(
  model.m = fit.m,
  model.y = fit.y,
  treat = "psd_Serum_Zn",
  mediator = "g_Sediminibacterium",
  control.value = 0,
  treat.value = 1,
  boot = TRUE,
  sims = 1000
)
summary(med.out)

# 4) 通用单次中介分析函数（适配：连续暴露 psd_Serum_Zn）-----------------------
run_one_mediation <- function(data, mediator, y_var, x_var, covars,
                              control_val, treat_val, sims = 1000,
                              y_type = c("auto", "binary", "continuous")) {
  y_type <- match.arg(y_type)

  # 仅保留当次分析需要的变量，并去除其中缺失
  vars_needed <- c("SampleID", mediator, y_var, x_var, covars)
  df <- data %>%
    dplyr::select(dplyr::all_of(vars_needed)) %>%
    stats::na.omit()

  if (nrow(df) < 5) stop(sprintf("可用样本量过少：n=%d（Outcome=%s, Mediator=%s）", nrow(df), y_var, mediator))

  # ---- 暴露（连续）：保持 numeric，control/treat 必须是数值 ----
  x <- df[[x_var]]
  if (!is.numeric(x)) {
    x_num <- suppressWarnings(as.numeric(as.character(x)))
    if (anyNA(x_num)) stop(sprintf("暴露 %s 不是数值型，且无法安全转换为数值型。", x_var))
    df[[x_var]] <- x_num
  }

  if (!is.numeric(control_val) || !is.numeric(treat_val)) {
    stop(sprintf("连续暴露 %s 需要数值型 control_val / treat_val（例如 0 vs 1 表示每+1SD）。", x_var))
  }

  # ---- 中介（连续）：确保 numeric ----
  m <- df[[mediator]]
  if (!is.numeric(m)) {
    m_num <- suppressWarnings(as.numeric(as.character(m)))
    if (anyNA(m_num)) stop(sprintf("中介 %s 不是数值型，且无法安全转换为数值型。", mediator))
    df[[mediator]] <- m_num
  }

  # ---- 结局类型：二分类用logit，连续用lm ----
  y <- df[[y_var]]
  is_binary <- FALSE
  if (y_type == "binary") {
    is_binary <- TRUE
  } else if (y_type == "continuous") {
    is_binary <- FALSE
  } else {
    # auto
    if (is.factor(y) && nlevels(y) == 2) {
      is_binary <- TRUE
    } else if (is.numeric(y)) {
      uy <- sort(unique(y))
      is_binary <- length(uy) == 2 && all(uy %in% c(0, 1))
    } else {
      # 尝试转成两水平因子
      y_fac <- factor(y)
      if (nlevels(y_fac) == 2) {
        is_binary <- TRUE
        df[[y_var]] <- y_fac
      } else {
        stop(sprintf("无法自动识别结局 %s 类型；请设置 y_type='binary' 或 'continuous'。", y_var))
      }
    }
  }

  # 公式（字符串 -> formula，避免 bootstrap 环境问题）
  rhs_m <- paste(c(x_var, covars), collapse = " + ")
  form_m <- stats::as.formula(sprintf("%s ~ %s", mediator, rhs_m))

  rhs_y <- paste(c(mediator, x_var, covars), collapse = " + ")
  form_y <- stats::as.formula(sprintf("%s ~ %s", y_var, rhs_y))

  # 中介模型：连续中介
  mod_m <- stats::lm(formula = form_m, data = df)

  # 结局模型：二分类 / 连续
  mod_y <- if (is_binary) {
    stats::glm(formula = form_y, data = df, family = binomial())
  } else {
    stats::lm(formula = form_y, data = df)
  }

  # 减少环境依赖
  mod_m$call$formula <- stats::formula(mod_m)
  mod_y$call$formula <- stats::formula(mod_y)

  # 中介分析：连续暴露用 control.value / treat.value 作为数值对比
  med <- mediation::mediate(
    model.m       = mod_m,
    model.y       = mod_y,
    treat         = x_var,
    mediator      = mediator,
    control.value = control_val,
    treat.value   = treat_val,
    boot          = TRUE,
    sims          = sims
  )
  s <- summary(med)

  tibble::tibble(
    outcome = y_var,
    outcome_type = ifelse(is_binary, "binary", "continuous"),
    x_var = x_var,
    contrast = paste0(treat_val, " vs ", control_val),
    mediator = mediator,
    n = nrow(df),

    ACME_control = s$d0,
    ACME_treated = s$d1,
    ACME_avg = s$d.avg,
    ACME_avg_CI_low = s$d.avg.ci[1],
    ACME_avg_CI_high = s$d.avg.ci[2],
    ACME_avg_p = s$d.avg.p,

    ADE_control = s$z0,
    ADE_treated = s$z1,
    ADE_avg = s$z.avg,
    ADE_avg_CI_low = s$z.avg.ci[1],
    ADE_avg_CI_high = s$z.avg.ci[2],
    ADE_avg_p = s$z.avg.p,

    Total_Effect = s$tau.coef,
    Total_Effect_CI_low = s$tau.ci[1],
    Total_Effect_CI_high = s$tau.ci[2],
    Total_Effect_p = s$tau.p,

    Prop_Mediated_control = s$n0,
    Prop_Mediated_treated = s$n1,
    Prop_Mediated_avg = s$n.avg,
    Prop_Mediated_avg_CI_low = s$n.avg.ci[1],
    Prop_Mediated_avg_CI_high = s$n.avg.ci[2]
  )
}

# 5) 针对“单个结局”的批量运行封装（各自独立去缺失） -----------------------
run_for_outcome <- function(data, y_var, g_vars, x_var, covars, contrasts,
                            sims = 1000, y_type = c("auto", "binary", "continuous")) {
  y_type <- match.arg(y_type)

  data_y <- data %>% dplyr::filter(!is.na(.data[[y_var]]))

  results <- list()
  for (cn in names(contrasts)) {
    control_val <- contrasts[[cn]]$control
    treat_val   <- contrasts[[cn]]$treat

    for (m in g_vars) {
      cat(sprintf("运行中介：Outcome=%s | %s vs %s | Mediator=%s\n",
                  y_var, treat_val, control_val, m))

      res <- tryCatch(
        run_one_mediation(
          data = data_y, mediator = m, y_var = y_var, x_var = x_var, covars = covars,
          control_val = control_val, treat_val = treat_val,
          sims = sims, y_type = y_type
        ),
        error = function(e) {
          message(sprintf("失败：Outcome=%s | %s vs %s | Mediator=%s | 原因：%s",
                          y_var, treat_val, control_val, m, e$message))
          tibble::tibble(
            outcome = y_var,
            outcome_type = NA_character_,
            x_var = x_var,
            contrast = paste0(treat_val, " vs ", control_val),
            mediator = m,
            n = NA_real_,
            ACME_control = NA_real_, ACME_treated = NA_real_,
            ACME_avg = NA_real_, ACME_avg_CI_low = NA_real_, ACME_avg_CI_high = NA_real_, ACME_avg_p = NA_real_,
            ADE_control = NA_real_, ADE_treated = NA_real_,
            ADE_avg = NA_real_, ADE_avg_CI_low = NA_real_, ADE_avg_CI_high = NA_real_, ADE_avg_p = NA_real_,
            Total_Effect = NA_real_, Total_Effect_CI_low = NA_real_, Total_Effect_CI_high = NA_real_, Total_Effect_p = NA_real_,
            Prop_Mediated_control = NA_real_, Prop_Mediated_treated = NA_real_,
            Prop_Mediated_avg = NA_real_, Prop_Mediated_avg_CI_low = NA_real_, Prop_Mediated_avg_CI_high = NA_real_
          )
        }
      )

      results[[length(results) + 1]] <- res
    }
  }

  dplyr::bind_rows(results) %>%
    dplyr::mutate(
      ACME_sig = dplyr::case_when(!is.na(ACME_avg_p) & ACME_avg_p < 0.05 ~ "*", TRUE ~ ""),
      ADE_sig  = dplyr::case_when(!is.na(ADE_avg_p)  & ADE_avg_p  < 0.05 ~ "*", TRUE ~ ""),
      Prop_Mediated_sig = dplyr::case_when(
        !is.na(Prop_Mediated_avg_CI_low) & !is.na(Prop_Mediated_avg_CI_high) &
          (Prop_Mediated_avg_CI_low > 0 | Prop_Mediated_avg_CI_high < 0) ~ "*",
        TRUE ~ ""
      ),
      Prop_Mediated_formatted = dplyr::case_when(
        !is.na(Prop_Mediated_avg) ~ sprintf(
          "%.2f%% (%.2f%% to %.2f%%)%s",
          Prop_Mediated_avg * 100,
          Prop_Mediated_avg_CI_low * 100,
          Prop_Mediated_avg_CI_high * 100,
          Prop_Mediated_sig
        ),
        TRUE ~ NA_character_
      )
    )
}

# 6) 分别对三个结局运行（各自先去除自身缺失） -----------------------------
set.seed(12345)

# T2DM：二分类
res_diabetes <- run_for_outcome(
  data = dt_analysis, y_var = y_diabetes,
  g_vars = g_vars, x_var = x_var, covars = c_vars,
  contrasts = contrasts, sims = 1000,
  y_type = "binary"
)

# IFG_or_T2DM：二分类
res_ifg <- run_for_outcome(
  data = dt_analysis, y_var = y_ifg,
  g_vars = g_vars, x_var = x_var, covars = c_vars,
  contrasts = contrasts, sims = 1000,
  y_type = "binary"
)

# FPG：连续
res_fpg <- run_for_outcome(
  data = dt_analysis, y_var = y_fpg,
  g_vars = g_vars, x_var = x_var, covars = c_vars,
  contrasts = contrasts, sims = 1000,
  y_type = "continuous"
)

# ===== 可选：合并三类结局结果，便于统一导出/汇总 =====
res_all <- dplyr::bind_rows(res_diabetes, res_ifg, res_fpg)
View(res_all)
# # ===== 可选：导出结果 =====
# dir <- "Results/mediation"
# readr::write_csv(res_diabetes, file.path(dir, "mediation_T2DM.csv"))
# readr::write_csv(res_ifg,      file.path(dir, "mediation_IFG_or_T2DM.csv"))
# readr::write_csv(res_fpg,      file.path(dir, "mediation_FPG.csv"))
# readr::write_csv(res_all, file.path(dir, "mediation_all_outcomes.csv"))
# 针对IFG结局----------------------------------------
# 数据子集
dt_ifg <- dt_analysis %>%
  dplyr::filter(dt_analysis$IFG %in% c("No", "Yes"))
# 结局变量
y_ifg2 <- "IFG"

# IFG：二分类
res_ifg2 <- run_for_outcome(
  data = dt_ifg, y_var = y_ifg2,
  g_vars = g_vars, x_var = x_var, covars = c_vars,
  contrasts = contrasts, sims = 1000,
  y_type = "binary"
)
View(res_ifg2)
# ===== 可选：导出结果 =====
# dir <- "Results/mediation"
readr::write_csv(res_ifg2, file.path(dir, "mediation_IFG.csv"))
