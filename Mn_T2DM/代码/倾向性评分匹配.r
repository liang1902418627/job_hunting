rm(list = ls()) # 清空工作空间

# 加载所需的R包
library(dplyr)
library(tidyr)
pak::pkg_install(c("MatchIt", "gtsummary", "flextable", "officer"))
library(MatchIt)
library(gtsummary) # gtsummary 包用于生成表格
# 保存表格
library(flextable) # flextable 包用于创建 Word 表格
library(officer) # officer 包用于操作 Word 文档

# 加载数据
load("数据/imputed_df_v6.RData")
colnames(imputed_df_v6)

# 匹配前基线表
vars <- c(
    "gender", "age", "BMI", "Marital_status", "education",
    "family_income_cat", "smoker", "Alcohol_consumption", "act_sport",
    "family_diabetes_history",
    "energy_kcal", "diet_Mn_mg", "diabetes"
)

table1 <- imputed_df_v6 %>%
    select(all_of(vars)) %>%
    tbl_summary(
        by = "diabetes", # 按diabetes分组
        statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
        missing = "no", # 不显示缺失值
        percent = "column", # 计算列百分比
        digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2) # 设置小数位数
    ) %>%
    add_overall() %>% # 添加合计列
    add_p() %>%
    modify_header(label = "**Variable**") %>%
    bold_labels()
print(table1)


# 将表格转换为flextable对象
flextable1 <- as_flex_table(table1) # 将 gtsummary 表格转换为 flextable 对象

# 创建一个Word文档并添加表格
doc <- read_docx() %>% # 创建一个新的 Word 文档
    body_add_flextable(flextable1) # 将 flextable 添加到文档中

# 保存Word文档
print(doc, target = "倾向性评分匹配结果/表格/基线表_均值_匹配前.docx") # 保存文档

##### 倾向性评分匹配
set.seed(123) 
data <- imputed_df_v6[!is.na(imputed_df_v6$diabetes), ] # 只保留diabetes不为NA的行
#PSM
m_out <- matchit(
  data = data,
  formula = diabetes ~ gender + age + BMI + Marital_status + education +
    family_income_cat + smoker + Alcohol_consumption + act_sport +
    family_diabetes_history + energy_kcal + diet_Mn_mg,
  method = "nearest", # nearest: 最近邻匹配
  distance = "logit", # logit: 对数几率
  replace = FALSE,    # replace: 是否放回
  caliper = 0.05      # caliper: 匹配的最大距离
)

#匹配后样本数据
lalonde_matched <- match.data(m_out)

# 匹配后基线表
table2 <- lalonde_matched %>%
    select(all_of(vars)) %>%
    tbl_summary(
        by = "diabetes", # 按diabetes分组
        statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
        missing = "no", # 不显示缺失值
        percent = "column", # 计算列百分比
        digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2) # 设置小数位数
    ) %>%
    add_overall() %>% # 添加合计列
    add_p() %>%
    modify_header(label = "**Variable**") %>%
    bold_labels()
print(table2)


# 将表格转换为flextable对象
flextable2 <- as_flex_table(table2) # 将 gtsummary 表格转换为 flextable 对象

# 创建一个Word文档并添加表格
doc2 <- read_docx() %>% # 创建一个新的 Word 文档
    body_add_flextable(flextable2) # 将 flextable 添加到文档中

# 保存Word文档
print(doc2, target = "倾向性评分匹配结果/表格/基线表_均值_匹配后.docx") # 保存文档
dt_match <- lalonde_matched
save(dt_match, file = "数据/倾向性评分匹配/dt_match.RData")
# =======基线表============================
rm(list = ls()) # 清空工作空间

# 加载所需的R包
packages <- c("dplyr", "tidyr", "gtsummary", "flextable", "officer")
lapply(packages, library, character.only = TRUE)

###### 匹配alpha多样性指数
load("数据/genus_data.RData") # 加载肠道菌群相关数据
load("数据/倾向性评分匹配/dt_match.RData") # 加载倾向性评分匹配结果

dt_match_v2 <- dt_match %>%
    left_join(genus_data[["alpha_diversity"]], by = "SampleID")

# View(dt_match_v2)
dt_match_v2$Mn_quartiles_2 <- cut(dt_match_v2$Serum_Mn, breaks = quantile(dt_match_v2$Serum_Mn, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
dt_match_v2$Mn_quartiles <- factor(dt_match_v2$Mn_quartiles_2, levels = levels(dt_match_v2$Mn_quartiles_2), labels = c("Q1", "Q2", "Q3", "Q4"))
# save(dt_match_v2, file = "数据/倾向性评分匹配/dt_match_v2.RData")

###### 基线表
# 基线表
vars <- c(
    "gender", "age", "BMI", "Marital_status", "education",
    "family_income_cat", "smoker", "Alcohol_consumption", "act_sport",
    "family_diabetes_history",
    "energy_kcal", "diet_Mn_mg", "IFG_or_diabetes", "diabetes",
    "FPG", "hba1c", "Mn_quartiles_2", "Serum_Mn", "Richness", "Shannon", "Simpson", "Pielou"
)


# 创建基线表
table1 <- dt_match_v2 %>%
    select(all_of(vars)) %>%
    tbl_summary(
        by = "Mn_quartiles_2", # 按血清锰四分位分组
        statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
        missing = "no", # 不显示缺失值
        percent = "column", # 计算列百分比
        digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2) # 设置小数位数
    ) %>%
    add_overall() %>% # 添加合计列
    add_p(
        test = list(
            all_categorical() ~ "chisq.test"
            # 连续变量不指定，自动用t检验或wilcox检验
        )
    ) %>%
    modify_header(label = "**Variable**") %>%
    bold_labels()
print(table1)

# 将表格转换为flextable对象
flextable1 <- as_flex_table(table1) # 将 gtsummary 表格转换为 flextable 对象

# 创建一个Word文档并添加表格
doc <- read_docx() %>% # 创建一个新的 Word 文档
    body_add_flextable(flextable1) # 将 flextable 添加到文档中

# 保存Word文档
print(doc, target = "倾向性评分匹配结果/表格/基线表_Mn.docx") # 保存文档

# ======回归分析===========
rm(list = ls())

### 加载r包
packages <- c("dplyr", "tidyr")
lapply(packages, library, character.only = TRUE)
source("代码\\myfunctions.r") # 自编函数

load("数据/倾向性评分匹配/dt_match_v2.RData") # 加载倾向性评分匹配结果
imputed_df_v4 <- dt_match_v2
colnames(imputed_df_v4)
table(imputed_df_v4$Mn_quartiles_2)
imputed_df_v4$Mn_quartiles_2 <- factor(imputed_df_v4$Mn_quartiles_2, levels = c(
   "[2.04,10.3]", "(10.3,13.5]", "(13.5,16.9]", "(16.9,35.7]"
), labels = c("Q1", "Q2", "Q3", "Q4"))
imputed_df_v4 <- imputed_df_v4 %>%
    mutate(Mn_Q_con = as.numeric(Mn_quartiles_2)) %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles_2, ref = "Q2"))

imputed_df_v4$sd_ln_Mn <- imputed_df_v4$ln_Serum_Mn /
    sd(imputed_df_v4$ln_Serum_Mn, na.rm = TRUE) # per_sd
imputed_df_v4$sd_ln_FPG <- imputed_df_v4$ln_FPG /
    sd(imputed_df_v4$ln_FPG, na.rm = TRUE) # per_sd
data <- imputed_df_v4
# ========函数==========================================
# 1. 数据框
df <- function(Model, Q2 = "", Q1 = "", Q3 = "", Q4 = "", `p for trend` = "") {
    data.frame(
        Model = Model,
        Q2 = Q2,
        Q1 = Q1,
        Q3 = Q3,
        Q4 = Q4,
        `p for trend` = `p for trend`
    )
}

# 2. 计算置信区间下限的函数
low_ci_log <- function(x, sd) {
    exp(x - 1.96 * sd)
}

# 3. 计算置信区间上限的函数
up_ci_log <- function(x, sd) {
    exp(x + 1.96 * sd)
}

# 2. 计算置信区间下限的函数
low_ci_linear <- function(x, sd) {
    x - 1.96 * sd
}

# 3. 计算置信区间上限的函数
up_ci_linear <- function(x, sd) {
    x + 1.96 * sd
}

# 4. 格式化为2位小数字符串的函数
format_2 <- function(x) {
    sprintf("%.2f", x)
}

# 5. 提取需要的值的函数
value_for_reg_log <- function(Estimate, sd) {
    low_ci <- low_ci_log(Estimate, sd)
    up_ci <- up_ci_log(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(exp(Estimate))
    Q <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    return(Q)
}

value_for_reg_linear <- function(Estimate, sd) {
    low_ci <- low_ci_linear(Estimate, sd)
    up_ci <- up_ci_linear(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(Estimate)
    Q <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    return(Q)
}

# 7. p for trend
p_for_trend1 <- function(data, formula) {
    # formula: 其中的自变量为四分位因子型变量转换回1,2,3,4数值型

    # 拟合模型
    model <- glm(formula, data = data, family = "binomial")

    # 提取p值
    p_value <- summary(model)$coefficients[2, 4]

    return(p_value)
}

# 7. p for trend
p_for_trend2 <- function(data, formula) {
    # formula: 其中的自变量为四分位因子型变量转换回1,2,3,4数值型

    # 拟合模型
    model <- glm(formula, data = data)

    # 提取p值
    p_value <- summary(model)$coefficients[2, 4]

    return(p_value)
}

# 8. logistic回归
logistic_regression <- function(data, formula1, formula2, model_name) {
    # formula1: 自变量为四分位因子型变量
    # formula2: 自变量为四分位因子型变量转换回1,2,3,4数值型
    # model_name: 为模型命名
    # 拟合模型
    fitted_model <- glm(formula1, data = data, family = "binomial")

    # 提取系数
    result <- as.data.frame(summary(fitted_model)$coefficients)

    # 提取需要的值
    Estimate2 <- result$Estimate[2]
    Estimate3 <- result$Estimate[3]
    Estimate4 <- result$Estimate[4]

    sd2 <- result$`Std. Error`[2]
    sd3 <- result$`Std. Error`[3]
    sd4 <- result$`Std. Error`[4]

    Q2 <- "(reference)"
    Q1 <- value_for_reg_log(Estimate2, sd2)
    Q3 <- value_for_reg_log(Estimate3, sd3)
    Q4 <- value_for_reg_log(Estimate4, sd4)

    # 计算p for trend
    p_for_trend <- p_for_trend1(data, formula = formula2)

    return(df(
        Model = model_name,
        Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4,
        `p for trend` = p_for_trend
    ))
}

# 9. 线性回归
linear_regression <- function(data, formula1, formula2, model_name) {
    # formula1: 自变量为四分位因子型变量
    # formula2: 自变量为四分位因子型变量转换回1,2,3,4数值型
    # model_name: 为模型命名
    # 拟合模型
    fitted_model <- lm(formula1, data = data)

    # 提取系数
    result <- as.data.frame(summary(fitted_model)$coefficients)

    # 提取需要的值
    Estimate2 <- result$Estimate[2]
    Estimate3 <- result$Estimate[3]
    Estimate4 <- result$Estimate[4]

    sd2 <- result$`Std. Error`[2]
    sd3 <- result$`Std. Error`[3]
    sd4 <- result$`Std. Error`[4]

    Q2 <- "(reference)"
    Q1 <- value_for_reg_linear(Estimate2, sd2)
    Q3 <- value_for_reg_linear(Estimate3, sd3)
    Q4 <- value_for_reg_linear(Estimate4, sd4)

    # 计算p for trend
    p_for_trend <- p_for_trend2(data, formula2)

    return(df(
        Model = model_name,
        Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4,
        `p for trend` = p_for_trend
    ))
}

# ================================================
# 准备
# 1. 协变量（性别、年龄）
covariate1 <- c("gender", "age")

# 2. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate2 <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "IFG", "sd_ln_FPG")

# 4. 自变量1(因子型Mn)
exposure1 <- "Mn_quartiles_2"

# 5. 自变量2(数值型Mn四分位)
exposure2 <- "Mn_Q_con"

# 6. 模型名称
model <- c("model1", "model2", "model3")

result_df <- data.frame()
# 回归循环
for (j in outcome) {
    result_df <- rbind(result_df, df(Model = j))
    for (i in model) {
        if (i == "model1") {
            formula1 <- create_formula(j, exposure1)
            formula2 <- create_formula(j, exposure2)
        } else if (i == "model2") {
            formula1 <- create_formula(j, exposure1, covariate1)
            formula2 <- create_formula(j, exposure2, covariate1)
        } else if (i == "model3") {
            formula1 <- create_formula(j, exposure1, covariate2)
            formula2 <- create_formula(j, exposure2, covariate2)
        }

        if (class(data[[j]]) == "factor") {
            result <- logistic_regression(data, formula1, formula2, i)
        } else {
            result <- linear_regression(data, formula1, formula2, i)
        }

        result_df <- rbind(result_df, result)
    }
}

View(result_df)
write.csv(result_df, file = "倾向性评分匹配结果/表格/logistic_reg_Mn_quartiles_diabetes.csv", row.names = FALSE)
# =======RCS限制性立方样条模型==================
rm(list = ls())   # 清空工作空间

# 加载必要的库
packages <- c("dplyr", "tidyr", "rms", "ggplot2", "ggrcs")
lapply(packages, library, character.only = TRUE)
source("代码\\myfunctions.r") # 加载自编函数

# 加载数据
load("数据/倾向性评分匹配/dt_match_v2.RData") # 加载倾向性评分匹配结果

# 准备数据
dt_match_v2 <- dt_match_v2 %>%
    mutate(Mn_quartiles_2 = factor(Mn_quartiles_2, levels = c(
        "[2.04,10.3]", "(10.3,13.5]", "(13.5,16.9]", "(16.9,35.7]"
    ), labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
    mutate(Mn_Q_con = as.numeric(Mn_quartiles_2))
dt <- dt_match_v2
# 建立限制性立方样条模型并提取相关参数
# 创建 datadist 对象并指定
dd <- datadist(dt)
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

##### diabetes
# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure, covariate)
    
# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = dt)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, ln_Serum_Mn, fun = exp, ref.zero = TRUE)
# View(OR)

# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(2.58)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

# 绘图
p_for_diabetes <- ggplot() +
    geom_line(
        data = OR, aes(ln_Serum_Mn, yhat),
        linetype = 1, linewidth = 0.2, alpha = 0.9, colour = "skyblue"
    ) +
    geom_ribbon(
        data = OR, aes(ln_Serum_Mn, ymin = lower, ymax = upper),
        alpha = 0.3, fill = "skyblue"
    ) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") +
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("lnMn = ", round(x_at_y_1, 2)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) +
    theme_bw(base_size = 9) + # 直接使用带边框的主题
    labs(x = "Manganese", y = "Diabetes, Adjusted OR (95% CI)") +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(an$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 2
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(an$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 2
    )


# 保存图片
ggsave("倾向性评分匹配结果/plots/RCS/Dose-response relationship between serum manganese and diabetes.pdf", p_for_diabetes, width = 2.57, height = 3, dpi = 300)

##### IFG or diabetes
#  自变量
exposure <- "ln_Serum_Mn"
# 构建logistic回归模型公式
covarite_formula <- formula_rcs(outcome[2], exposure, 6, covariate)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = dt)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, ln_Serum_Mn, fun = exp, ref.zero = TRUE)
View(OR)
# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(2.38, 2.60)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

# 绘图
p_for_IFG_or_diabetes <- ggplot() +
    geom_line(data = OR, aes(ln_Serum_Mn, yhat), linetype = 1, linewidth = 0.2, alpha = 0.9, colour = "skyblue") +
    geom_ribbon(data = OR, aes(ln_Serum_Mn, ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") + # 添加竖线
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("lnMn = ", round(x_at_y_1, 2)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) + # 添加标注
    theme_bw() +
    theme(
        text = element_text(size = 9)
    ) +
    labs(x = "Manganese", y = "IFG or diabetes, Adjusted OR (95%CI)") +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(an$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 2
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(an$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 2
    )

# 保存图片
ggsave("倾向性评分匹配结果/plots/RCS/Dose-response relationship between serum manganese and IFG or diabetes .pdf", p_for_IFG_or_diabetes, width = 2.57, height = 3, dpi = 300)

##### FPG
# remotes::install_github("kunhuo/plotRCS")
#  自变量
library(plotRCS) # plotRCS包用于绘制限制性立方样条图


exposure <- "ln_Serum_Mn" # 自变量
# 回归模型公式
covarite_formula <- formula_rcs(outcome[3], exposure, 4, covariate) # 回归模型公式

# 使用限制性立方样条模型
fit <- ols(covarite_formula, data = dt)
AIC(fit)
(an <- as.data.frame(anova(fit))) # 获取p for non-linear

p_FPG <- rcsplot(
    data = dt,
    outcome = "ln_FPG",
    exposure = "ln_Serum_Mn",
    covariates = covariate,
    linecolor = "skyblue",
    fillcolor = "skyblue",
    pvalue.position = c(0.3, 0.95),
    knots.line = TRUE,
    ref.value = "k1",
    fontsize = 9,
    explain = TRUE,
    xlab = "Manganese",
    ylab = "FPG, β (95%)",
    alpha = 0.3
)
p_FPG <- p_FPG +
    theme_bw(base_size = 9) +  # base_size 控制全局字体
    theme(
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2)
    )
names(p_FPG)
# Display the plot
p_FPG
ggsave("倾向性评分匹配结果/plots/RCS/Dose-response relationship between serum manganese and FPG.pdf", p_FPG, width = 2.6, height = 3, dpi = 300)
