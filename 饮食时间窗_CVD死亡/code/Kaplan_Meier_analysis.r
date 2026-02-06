# ------------------------------------------
# 题目：暴露与三结局的Kaplan-Meier生存分析
# 暴露因素：Eating_time_window_cat、Eating_frequency_cat
# 结局变量：mortstat、CVDmortality、CancerMortality
# 生存时间：time
# 配色："#93C8ED", "#F7C1C1", "#F9C780"
# 字体：Times New Roman
# ---------------------------------------
rm(list = ls()) # 清空环境变量
setwd("D:/lianghao/孙莹数据分析20251215/")

# 加载所需包
paks <- c("survival", "survminer", "dplyr", "readr", "tidyverse", "extrafont", "ggplot2")
lapply(paks, library, character.only = TRUE)
# 字体
loadfonts(device = "win")
# 读取数据
dt <- readRDS("workspace/projects/data/final_analysis_data.rds")
# 结局变量数值化, 目前因子为（0,1），0表示无事件, 1表示有事件
dt2 <- dt %>%
    mutate(across(c(mortstat, CVDmortality, CancerMortality), ~ as.numeric(as.character(.))))
table(dt2$mortstat)
table(dt2$CVDmortality)
table(dt2$CancerMortality)
# 绘制Kaplan-Meier生存曲线, 进行Log-rank检验-----------------------------
# 保存路径：workspace\projects\results\pictures
# ---------------------------------------------------------------------
# mortstat
# 创建生存对象
attach(dt2) # 绑定数据集
surv_obj <- Surv(time, mortstat) # 创建生存对象，将事件时间和删失信息合并在一起的数据结构
fit <- survfit(surv_obj ~ Eating_time_window_cat, data = dt2) # 拟合曲线信息

# 计算log-rank检验的p值并创建标签
p_val <- surv_pvalue(fit, data = dt2)$pval
p_label <- paste("Log-rank p =", format(p_val, digits = 3))

p1 <- ggsurvplot(fit, # 创建的拟合对象
    data = dt2, # 指定变量数据来源
    conf.int = FALSE, # 显示置信区间
    pval = p_label, # 添加自定义格式的P值
    palette = c("#93C8ED", "#F7C1C1", "#F9C780"), # 设置自定义调色板
    surv.median.line = "hv", # 添加中位生存时间线
    risk.table = TRUE, # 添加风险表
    risk.table.fontsize = 5, # 风险表字体大小
    legend = c(0.8, 0.25), # 指定图例位置
    legend.title = "Eating Window", # 设置图例标题
    legend.labs = c("T1", "T2", "T3"), # 指定图例分组标签
    xlab = "Follow up time(m)", # 指定x轴标签
    break.x.by = 25, # 设置x轴刻度间距
    risk.table.title.font = c(10, "plain", "black"), # 风险表标题字体大小
    title = "Overall Survival",
    font.title = 12, # 标题字体大小
    font.x = 12, # X轴标签字体大小
    font.y = 12, # Y轴标签字体大小
    font.tickslab = 12, # 刻度标签字体大小
    font.legend = 12,
    font.family = "Times New Roman"
) # 添加图表标题
print(p1)

# CVDmortality ~ Eating_time_window_cat
surv_obj_cvd <- Surv(time, CVDmortality)
fit_cvd <- survfit(surv_obj_cvd ~ Eating_time_window_cat, data = dt2)

p_val_cvd <- surv_pvalue(fit_cvd, data = dt2)$pval
p_label_cvd <- paste("Log-rank p =", format(p_val_cvd, digits = 3))

p2 <- ggsurvplot(fit_cvd,
    data = dt2,
    conf.int = FALSE,
    pval = p_label_cvd,
    palette = c("#93C8ED", "#F7C1C1", "#F9C780"),
    surv.median.line = "hv",
    risk.table = TRUE,
    risk.table.fontsize = 5,
    legend = c(0.8, 0.25),
    legend.title = "Eating Window",
    legend.labs = c("T1", "T2", "T3"),
    xlab = "Follow up time(m)",
    break.x.by = 25,
    risk.table.title.font = c(10, "plain", "black"),
    title = "Cardiovascular Disease Survival",
    font.title = 12,
    font.x = 12,
    font.y = 12,
    font.tickslab = 12,
    font.legend = 12,
    font.family = "Times New Roman"
)
print(p2)

# CancerMortality ~ Eating_time_window_cat
surv_obj_cancer <- Surv(time, CancerMortality)
fit_cancer <- survfit(surv_obj_cancer ~ Eating_time_window_cat, data = dt2)

p_val_cancer <- surv_pvalue(fit_cancer, data = dt2)$pval
p_label_cancer <- paste("Log-rank p =", format(p_val_cancer, digits = 3))

p3 <- ggsurvplot(fit_cancer,
    data = dt2,
    conf.int = FALSE,
    pval = p_label_cancer,
    palette = c("#93C8ED", "#F7C1C1", "#F9C780"),
    surv.median.line = "hv",
    risk.table = TRUE,
    risk.table.fontsize = 5,
    legend = c(0.8, 0.25),
    legend.title = "Eating Window",
    legend.labs = c("T1", "T2", "T3"),
    xlab = "Follow up time(m)",
    break.x.by = 25,
    risk.table.title.font = c(10, "plain", "black"),
    title = "Cancer Survival",
    font.title = 12,
    font.x = 12,
    font.y = 12,
    font.tickslab = 12,
    font.legend = 12,
    font.family = "Times New Roman"
)
print(p3)

# mortstat ~ Eating_frequency_cat
fit_freq <- survfit(surv_obj ~ Eating_frequency_cat, data = dt2)

p_val_freq <- surv_pvalue(fit_freq, data = dt2)$pval
p_label_freq <- paste("Log-rank p =", format(p_val_freq, digits = 3))

p4 <- ggsurvplot(fit_freq,
    data = dt2,
    conf.int = FALSE,
    pval = p_label_freq,
    palette = c("#93C8ED", "#F7C1C1", "#F9C780"),
    surv.median.line = "hv",
    risk.table = TRUE,
    risk.table.fontsize = 5,
    legend = c(0.8, 0.25),
    legend.title = "Eating Frequency",
    legend.labs = c("T1", "T2", "T3"),
    xlab = "Follow up time(m)",
    break.x.by = 25,
    risk.table.title.font = c(10, "plain", "black"),
    title = "Overall Survival",
    font.title = 12,
    font.x = 12,
    font.y = 12,
    font.tickslab = 12,
    font.legend = 12,
    font.family = "Times New Roman"
)
print(p4)

# CVDmortality ~ Eating_frequency_cat
fit_cvd_freq <- survfit(surv_obj_cvd ~ Eating_frequency_cat, data = dt2)

p_val_cvd_freq <- surv_pvalue(fit_cvd_freq, data = dt2)$pval
p_label_cvd_freq <- paste("Log-rank p =", format(p_val_cvd_freq, digits = 3))

p5 <- ggsurvplot(fit_cvd_freq,
    data = dt2,
    conf.int = FALSE,
    pval = p_label_cvd_freq,
    palette = c("#93C8ED", "#F7C1C1", "#F9C780"),
    surv.median.line = "hv",
    risk.table = TRUE,
    risk.table.fontsize = 5,
    legend = c(0.8, 0.25),
    legend.title = "Eating Frequency",
    legend.labs = c("T1", "T2", "T3"),
    xlab = "Follow up time(m)",
    break.x.by = 25,
    risk.table.title.font = c(10, "plain", "black"),
    title = "Cardiovascular Disease Survival",
    font.title = 12,
    font.x = 12,
    font.y = 12,
    font.tickslab = 12,
    font.legend = 12,
    font.family = "Times New Roman"
)
print(p5)

# CancerMortality ~ Eating_frequency_cat
fit_cancer_freq <- survfit(surv_obj_cancer ~ Eating_frequency_cat, data = dt2)

p_val_cancer_freq <- surv_pvalue(fit_cancer_freq, data = dt2)$pval
p_label_cancer_freq <- paste("Log-rank p =", format(p_val_cancer_freq, digits = 3))

p6 <- ggsurvplot(fit_cancer_freq,
    data = dt2,
    conf.int = FALSE,
    pval = p_label_cancer_freq,
    palette = c("#93C8ED", "#F7C1C1", "#F9C780"),
    surv.median.line = "hv",
    risk.table = TRUE,
    risk.table.fontsize = 5,
    legend = c(0.8, 0.25),
    legend.title = "Eating Frequency",
    legend.labs = c("T1", "T2", "T3"),
    xlab = "Follow up time(m)",
    break.x.by = 25,
    risk.table.title.font = c(10, "plain", "black"),
    title = "Cancer Survival",
    font.title = 12,
    font.x = 12,
    font.y = 12,
    font.tickslab = 12,
    font.legend = 12,
    font.family = "Times New Roman"
)
print(p6)

# ---------------------------------------------------------------------
# 单独保存每个图形
# ---------------------------------------------------------------------
# 确保目录存在
dir.create("workspace/projects/results/pictures/K-M/", showWarnings = FALSE, recursive = TRUE)

# 将图形和文件名存储在列表中
plot_list <- list(p1, p2, p3, p4, p5, p6)
file_names <- c(
    "OS_vs_EatingWindow", "CVD_vs_EatingWindow", "Cancer_vs_EatingWindow",
    "OS_vs_EatingFrequency", "CVD_vs_EatingFrequency", "Cancer_vs_EatingFrequency"
)

# 循环保存每个图形
for (i in 1:length(plot_list)) {
    file_path <- paste0("workspace/projects/results/pictures/K-M/", file_names[i], ".pdf")
    pdf(file_path, width = 5.97, height = 6.35)
    print(plot_list[[i]])
    dev.off()
}

# ---------------------------------------------------------------------
# 组合图形
# ---------------------------------------------------------------------
# 将ggsurvplot对象转换为grob对象列表
plots <- list(p1, p2, p3, p4, p5, p6)
# 组合图形
res <- arrange_ggsurvplots(plots, print = TRUE, ncol = 2, nrow = 3)
print(res)
# 保存组合图形
ggsave("workspace/projects/results/pictures/K-M/combined_survival_plots.pdf", plot = res, width = 16, height = 24, units = "in", dpi = 300)
