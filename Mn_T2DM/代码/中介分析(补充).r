# 清空环境
rm(list = ls())

# 1) 加载依赖包 -----------------------------------------------------------
packages <- c("dplyr", "mediation", "broom", "readr", "purrr", "tibble")
invisible(lapply(packages, require, character.only = TRUE))

# 如需你自定义函数，可自行启用
if (file.exists("代码/myfunctions.r")) {
  source("代码/myfunctions.r")
}

# 2) 读取数据并合并 -------------------------------------------------------
# imputed_df_v6: 应含 SampleID、暴露、协变量、两个结局
# genus_data$transformed: 行名为 SampleID 的菌属矩阵
load("数据/imputed_df_v6.RData")
load("数据/genus_data.RData")

g_trans <- genus_data$transformed
g_trans$SampleID <- row.names(g_trans)

# 合并
dt_analysis <- merge(imputed_df_v6, g_trans, by = "SampleID")

# 3) 设定变量 -------------------------------------------------------------
# 中介变量（菌属）
g_vars <- c(
  "g_Acidaminococcus", "g_Acinetobacter",
  "g_Actinomyces", "g_Akkermansia",
  "g_Alistipes", "g_Anaerostipes",
  "g_Atopobium", "g_Bacteroides",
  "g_Bifidobacterium", "g_Bilophila",
  "g_Blautia", "g_Bradyrhizobium",
  "g_Brevibacterium", "g_Bulleidia",
  "g_Butyricimonas", "g_Catenibacterium",
  "g_Citrobacter", "g_Clostridium",
  "g_Collinsella", "g_Coprobacillus",
  "g_Coprococcus", "g_Desulfovibrio",
  "g_Devosia", "g_Dialister",
  "g_Dorea", "g_Eggerthella",
  "g_Enhydrobacter", "g_Enterobacter",
  "g_Faecalibacterium", "g_Fusobacterium",
  "g_Haemophilus", "g_Halomonas",
  "g_Holdemania", "g_Klebsiella",
  "g_Lachnobacterium", "g_Lachnospira",
  "g_Lacticigenium", "g_Lactobacillus",
  "g_Leptothrix", "g_Megamonas",
  "g_Megasphaera", "g_Methanobrevibacter",
  "g_Methylobacterium", "g_Ochrobactrum",
  "g_Odoribacter", "g_Oscillospira",
  "g_Parabacteroides", "g_Paraprevotella",
  "g_Phascolarctobacterium", "g_Prauseria",
  "g_Pseudidiomarina", "g_Pseudomonas",
  "g_Ralstonia", "g_Roseburia",
  "g_SMB53", "g_Sediminibacterium",
  "g_Serratia", "g_Sphingomonas",
  "g_Staphylococcus", "g_Streptococcus",
  "g_Sutterella", "g_Synergistes",
  "g_Turicibacter", "g_Veillonella",
  "g_Eubacterium", "g_Prevotella",
  "g_Ruminococcus"
)

# 暴露：Mn 的四分位
x_var <- "Mn_quartiles"

# 协变量
c_vars <- c(
  "gender", "age", "BMI", "Marital_status",
  "education", "family_income_cat",
  "family_diabetes_history", "smoker", "Alcohol_consumption",
  "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 两个二分类结局（分别单独分析，先各自去除缺失）
y_diabetes <- "diabetes"
y_ifg      <- "IFG_or_diabetes"

# 明确对比（不依赖参考水平）
contrasts <- list(
  "Q4_vs_Q2" = list(control = "Q2", treat = "Q4")
  # ,
  # "Q3_vs_Q2" = list(control = "Q2", treat = "Q3")
)

# 4) 通用单次中介分析函数（修复了 f_m/f_y 丢失问题） -----------------------
run_one_mediation <- function(data, mediator, y_var, x_var, covars,
                              control_val, treat_val, sims = 1000) {
  # 仅保留当次分析需要的变量，并去除其中缺失
  vars_needed <- c("SampleID", mediator, y_var, x_var, covars)
  df <- data %>%
    dplyr::select(dplyr::all_of(vars_needed)) %>%
    stats::na.omit()

  # 暴露因子化并检查水平是否包含所需对比
  df[[x_var]] <- factor(df[[x_var]])
  if (!all(c(control_val, treat_val) %in% levels(df[[x_var]]))) {
    stop(sprintf("在 %s 中不存在对比所需水平：%s 或 %s", x_var, control_val, treat_val))
  }

  # 使用“字面量公式字符串”来避免 mediate 在 bootstrap 里找不到局部对象
  rhs_m <- paste(c(x_var, covars), collapse = " + ")
  form_m_str <- sprintf("%s ~ %s", mediator, rhs_m)
  form_m <- stats::as.formula(form_m_str)

  rhs_y <- paste(c(mediator, x_var, covars), collapse = " + ")
  form_y_str <- sprintf("%s ~ %s", y_var, rhs_y)
  form_y <- stats::as.formula(form_y_str)

  # 中介模型（连续型中介，lm）
  mod_m <- stats::lm(formula = form_m, data = df)

  # 结局模型（二分类，glm logit）
  mod_y <- stats::glm(formula = form_y, data = df, family = binomial())

  # 保险起见：将模型调用中的 formula 替换为真实公式对象（进一步避免环境依赖）
  mod_m$call$formula <- stats::formula(mod_m)
  mod_y$call$formula <- stats::formula(mod_y)

  # 中介分析（指定 control.value / treat.value 精确对比）
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

  # 统一提取（d0/d1/d.avg, z0/z1/z.avg, tau, n0/n1/n.avg）
  tibble::tibble(
    outcome = y_var,
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
    Prop_Mediated_avg = s$n.avg
  )
}

# 5) 针对“单个结局”的批量运行封装（各自独立去缺失） -----------------------
run_for_outcome <- function(data, y_var, g_vars, x_var, covars, contrasts, sims = 1000) {
  # 仅对该结局做“自身缺失”的剔除；不会因为另一个结局缺失而剔除
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
          control_val = control_val, treat_val = treat_val, sims = sims
        ),
        error = function(e) {
          message(sprintf("失败：Outcome=%s | %s vs %s | Mediator=%s | 原因：%s",
                          y_var, treat_val, control_val, m, e$message))
          tibble::tibble(
            outcome = y_var,
            contrast = paste0(treat_val, " vs ", control_val),
            mediator = m,
            n = NA_real_,
            ACME_control = NA_real_, ACME_treated = NA_real_,
            ACME_avg = NA_real_, ACME_avg_CI_low = NA_real_, ACME_avg_CI_high = NA_real_, ACME_avg_p = NA_real_,
            ADE_control = NA_real_, ADE_treated = NA_real_,
            ADE_avg = NA_real_, ADE_avg_CI_low = NA_real_, ADE_avg_CI_high = NA_real_, ADE_avg_p = NA_real_,
            Total_Effect = NA_real_, Total_Effect_CI_low = NA_real_, Total_Effect_CI_high = NA_real_, Total_Effect_p = NA_real_,
            Prop_Mediated_control = NA_real_, Prop_Mediated_treated = NA_real_, Prop_Mediated_avg = NA_real_
          )
        }
      )
      results[[length(results) + 1]] <- res
    }
  }

  dplyr::bind_rows(results) %>%
    dplyr::mutate(
      ACME_sig = dplyr::case_when(!is.na(ACME_avg_p) & ACME_avg_p < 0.05 ~ "*", TRUE ~ ""),
      ADE_sig  = dplyr::case_when(!is.na(ADE_avg_p)  & ADE_avg_p  < 0.05 ~ "*", TRUE ~ "")
    )
}

# 6) 分别对两个结局运行（各自先去除自身缺失） -----------------------------
set.seed(12345)

# diabetes：仅去除 diabetes 缺失
res_diabetes <- run_for_outcome(
  data = dt_analysis, y_var = y_diabetes,
  g_vars = g_vars, x_var = x_var, covars = c_vars,
  contrasts = contrasts, sims = 1000
)

# IFG_or_diabetes：仅去除 IFG_or_diabetes 缺失
res_ifg <- run_for_outcome(
  data = dt_analysis, y_var = y_ifg,
  g_vars = g_vars, x_var = x_var, covars = c_vars,
  contrasts = contrasts, sims = 1000
)

# 7) 导出结果 -------------------------------------------------------------
# dir.create("结果/分开结局", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(res_diabetes, file = "主要结果\\表格\\中介分析（补充）\\不筛菌_直接中介分析\\mediation_diabetes_Mn_quartiles.csv")
readr::write_csv(res_ifg,      file = "主要结果\\表格\\中介分析（补充）\\不筛菌_直接中介分析\\mediation_IFG_or_diabetes_Mn_quartiles.csv")

# 简要查看
cat("\n--- diabetes 中介结果（前几行） ---\n")
print(utils::head(res_diabetes, 8))
cat("\n--- IFG_or_diabetes 中介结果（前几行） ---\n")
print(utils::head(res_ifg, 8))
