# ============限制性立方样条模型(绘图)==========================
rm(list = ls())
library(ggrcs)
library(rms) # 需要安装 rms 包
library(ggplot2)
library(tidyverse)
# 自编函数
source("代码\\myfunctions.r") # 加载自编函数

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
exposure <- "rcs(Serum_Mn, 3)"

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure, covariate)
fit <- lrm(covarite_formula, data = data)
p <- ggrcs(
    data = data,
    fit = fit,
    x = "Serum_Mn",
    histbinwidth = 1,
    histcol = "#BAE0CD",
    ribcol = "#AFD3DF"
)
p <- p + 
    geom_vline(aes(xintercept = 13.60), linetype = "dashed", color = "red") +
    geom_hline(aes(yintercept = 1), linetype = "dashed", color = "red") +
    annotate("text", x = 13.60, y = 1, label = "Mn = 13.60", vjust = -1, hjust = 0.5, color = "red", size = 4)
         
print(p)
# 保存图片
# ggsave("其他结果/图片/Fig 2-a. Dose-response relationship between serum manganese and diabetes.pdf", p, width = 5, height = 7, dpi = 300)





















# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, Serum_Mn, fun = exp, ref.zero = TRUE)
View(OR)
# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(13.60)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

# 绘图
p_for_diabetes <- ggplot() +
    geom_line(data = OR, aes(Serum_Mn, yhat), linetype = 1, linewidth = 1, alpha = 0.9, colour = "#0055ff") +
    geom_ribbon(data = OR, aes(Serum_Mn, ymin = lower, ymax = upper), alpha = 0.3, fill = "#0055ff") + # 绘制置信区间
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.5) + # 添加水平线
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.5, colour = "red") + # 添加竖线
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("Mn = ", round(x_at_y_1, 2)),
        hjust = -0.1, vjust = 1.1, size = 4, colour = "red"
    ) + # 添加标注
    theme_classic() +
    theme(
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5) # 添加图表边框
    ) +
    labs(x = "Manganese", y = "Ajusted OR (95%CI)") +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(an$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 5
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(an$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 5
    )

# 保存图片
# ggsave("其他结果/图片/Fig 2-a. Dose-response relationship between serum manganese and diabetes.pdf", p_for_diabetes, width = 5, height = 7, dpi = 300)
