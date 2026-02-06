setwd("D:\\桌面\\肠道菌")
library(tidyverse)
library(mediation)
library(ggpubr)
library(broom)
data <- read.csv("./终版/四分位_gm_outcome1.csv",fileEncoding = "GBK")
df <- read.csv("gm_outcome.csv",fileEncoding = "GBK")
data$feature
######四分位
sbp_vars <- data$feature[c(4,7,9,12)]
dbp_vars <- data$feature[c(29)]
colnames(df)
# 步骤3：中介分析（以单个变量示例）
med_model <- mediate(
  model.m = lm(k__Bacteria..p__Bacteroidetes..c__Bacteroidia..o__Bacteroidales..f__Bacteroidaceae.g__Bacteroides 
               ~ quartile + age + gender + anthrop_BMI, data = df), # 中介模型
  model.y = lm(anthrop_SBP ~ k__Bacteria..p__Bacteroidetes..c__Bacteroidia..o__Bacteroidales..f__Bacteroidaceae.g__Bacteroides
               + quartile + age + gender + anthrop_BMI, data = df), # 结局模型
  treat = "quartile",
  mediator = "k__Bacteria..p__Bacteroidetes..c__Bacteroidia..o__Bacteroidales..f__Bacteroidaceae.g__Bacteroides",
  boot = TRUE,
  sims = 1000
)

summary(med_model)
plot(med_model)
######敏感性分析
sens.out <- medsens(med_model, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)
par(mfrow = c(1,1))
plot(sens.out, sens.par = "rho", main = "SBP", ylim = c(-0.2, 0.2))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")

setwd("D:\\桌面\\肠道菌")
library(tidyverse)
library(mediation)
library(ggpubr)
library(broom)

# 读取数据
data <- read.csv("中介分析.csv", fileEncoding = "GBK")
df <- read.csv("gm_outcome.csv")
print(colnames(df))

# 确保 feature 列的内容是正确的
dbp_vars <- data$feature[c(20:22)]
sbp_vars <- trimws(data$feature[c(3,9)]) # 去除前后空格

# 初始化中介分析结果列表
mediation_results <- list()

# 中介分析
for (var in dbp_vars) {
  tryCatch({
    # 构建中介模型的公式
    formula_m <- reformulate(
      termlabels = c("quartile", "age", "gender", "anthrop_BMI"),
      response = var  # 使用当前的 sbp_vars
    )
    
    formula_y <- reformulate(
      termlabels = c(var, "quartile", "age", "gender", "anthrop_BMI"),
      response = "anthrop_DBP"
    )
    
    # 运行中介分析
    med <- mediate(
      model.m = lm(formula_m, data = df),
      model.y = lm(formula_y, data = df),
      treat = "quartile",
      mediator = var,
      boot = TRUE,
      sims = 500
    )
    
    # 保存结果
    mediation_results[[var]] <- summary(med)
  }, error = function(e) {
    message(paste("Error in variable:", var, " - ", e$message))
    NULL
  })
}
# 构建结果表格（关键修改）
results_table <- bind_rows(lapply(names(mediation_results), function(var) {
  med <- mediation_results[[var]]
  s <- summary(med)
  
  # 提取效应值（使用正确的字段名）
  data.frame(
    Variable = var,
    ACME = s$d.avg,
    ACME_CI_Lower = s$d.avg.ci[1],
    ACME_CI_Upper = s$d.avg.ci[2],
    ACME_p = s$d.avg.p,
    ADE = s$z.avg,          # 平均直接效应
    ADE_CI_Lower = s$z.avg.ci[1],
    ADE_CI_Upper = s$z.avg.ci[2],
    ADE_p = s$z.avg.p,
    Total_Effect = s$tau.coef,
    Total_Effect_CI_Lower = s$tau.ci[1],
    Total_Effect_CI_Upper = s$tau.ci[2],
    Total_Effect_p = s$tau.p,
    Proportion_Mediated = s$n.avg,  # 中介效应占比[5](@ref)
    stringsAsFactors = FALSE
  )
}))

# 添加显著性标记
results_table <- results_table %>%
  mutate(
    ACME_Sig = ifelse(ACME_p < 0.05, "*", ""),
    ADE_Sig = ifelse(ADE_p < 0.05, "*", "")
  )

# 保存结果
write_csv(results_table, "./终版/DBP_mediation_results_with_proportion.csv")



# 构建结果表格
results_table <- bind_rows(lapply(names(mediation_results), function(var) {
  result <- mediation_results[[var]]
  data.frame(
    Variable = var,
    ACME = ifelse(!is.null(result$d0), result$d0, NA),
    ACME_CI_Lower = ifelse(!is.null(result$d0.ci), result$d0.ci[1], NA),
    ACME_CI_Upper = ifelse(!is.null(result$d0.ci), result$d0.ci[2], NA),
    ACME_p = ifelse(!is.null(result$d0.p), result$d0.p, NA),  # 注意 p 值字段名
    ADE = ifelse(!is.null(result$z1), result$z1, NA),         # 直接效应为 z1
    ADE_CI_Lower = ifelse(!is.null(result$z1.ci), result$z1.ci[1], NA),
    ADE_CI_Upper = ifelse(!is.null(result$z1.ci), result$z1.ci[2], NA),
    ADE_p = ifelse(!is.null(result$z1.p), result$z1.p, NA),
    Total_Effect = ifelse(!is.null(result$tau.coef), result$tau.coef, NA),
    Total_Effect_CI_Lower = ifelse(!is.null(result$tau.ci), result$tau.ci[1], NA),
    Total_Effect_CI_Upper = ifelse(!is.null(result$tau.ci), result$tau.ci[2], NA),
    Total_Effect_p = ifelse(!is.null(result$tau.p), result$tau.p, NA),
    stringsAsFactors = FALSE
  )
}))

# 构建结果表格
results_table <- bind_rows(lapply(names(mediation_results), function(var) {
  result <- mediation_results[[var]]
  data.frame(
    Variable = var,
    ACME = round(ifelse(!is.null(result$d0), result$d0, NA), 3),
    ACME_CI_Lower = round(ifelse(!is.null(result$d0.ci), result$d0.ci[1], NA), 3),
    ACME_CI_Upper = round(ifelse(!is.null(result$d0.ci), result$d0.ci[2], NA), 3),
    ACME_p = round(ifelse(!is.null(result$d0.p), result$d0.p, NA), 3),  # 注意 p 值字段名
    ADE = round(ifelse(!is.null(result$z1), result$z1, NA), 3),         # 直接效应为 z1
    ADE_CI_Lower = round(ifelse(!is.null(result$z1.ci), result$z1.ci[1], NA), 3),
    ADE_CI_Upper = round(ifelse(!is.null(result$z1.ci), result$z1.ci[2], NA), 3),
    ADE_p = round(ifelse(!is.null(result$z1.p), result$z1.p, NA), 3),
    Total_Effect = round(ifelse(!is.null(result$tau.coef), result$tau.coef, NA), 3),
    Total_Effect_CI_Lower = round(ifelse(!is.null(result$tau.ci), result$tau.ci[1], NA), 3),
    Total_Effect_CI_Upper = round(ifelse(!is.null(result$tau.ci), result$tau.ci[2], NA), 3),
    Total_Effect_p = round(ifelse(!is.null(result$tau.p), result$tau.p, NA), 3),
    stringsAsFactors = FALSE
  )
}))

# 输出结果表
write.csv(results_table, "./终版/中介效应/四分位/sbp_mediation_results1.csv", row.names = FALSE)
write.csv(results_table, "./终版/中介效应/四分位/dbp_mediation_results1.csv", row.names = FALSE)
write.csv(results_table, "./results/中介效应/连续性/sbp_mediation_results.csv", row.names = FALSE)
write.csv(results_table, "./results/中介效应/连续性/dbp_mediation_results.csv", row.names = FALSE)


library(tidyverse)
library(mediation)
library(ggpubr)
library(broom)
library(ggplot2)
library(grid)
library(gridExtra)

# 读取数据
data <- read.csv("中介分析.csv", fileEncoding = "GBK")
df <- read.csv("gm_outcome.csv", fileEncoding = "GBK")
print(colnames(df))

# 确保 feature 列的内容是正确的
dbp_vars <- data$feature[c(20:22)]
sbp_vars <- trimws(data$feature[c(3,9)])

# 初始化结果存储
mediation_results <- list()
mediation_plots <- list()

# 中介分析
for (var in dbp_vars) {
  tryCatch({
    # 构建中介模型的公式
    formula_m <- reformulate(
      termlabels = c("quartile", "age", "gender", "anthrop_BMI"),
      response = var
    )
    
    formula_y <- reformulate(
      termlabels = c(var, "quartile", "age", "gender", "anthrop_BMI"),
      response = "anthrop_DBP"
    )
    
    # 运行中介分析
    med_model <- mediate(
      model.m = lm(formula_m, data = df),
      model.y = lm(formula_y, data = df),
      treat = "quartile",
      mediator = var,
      boot = TRUE,
      sims = 500
    )
    
    # 提取模型系数
    model_m_coef <- tidy(lm(formula_m, data = df))
    model_y_coef <- tidy(lm(formula_y, data = df))
    
    # 提取路径系数
    a_path <- model_m_coef %>% filter(term == "quartile") %>% pull(estimate)
    b_path <- model_y_coef %>% filter(term == var) %>% pull(estimate)
    c_prime_path <- model_y_coef %>% filter(term == "quartile") %>% pull(estimate)
    
    # 计算中介效应占比
    total_effect <- med_model$tau.coef
    prop_mediated <- ifelse(total_effect != 0, med_model$d0 / total_effect, NA)
    
    # 保存完整结果
    mediation_results[[var]] <- list(
      summary = summary(med_model),
      a_path = a_path,
      b_path = b_path,
      c_prime_path = c_prime_path,
      prop_mediated = prop_mediated
    )
    
    # ================== 绘制优化后的三角图 ==================
    # 创建节点数据 - 使用圆形节点
    nodes <- data.frame(
      x = c(0, 1, 0.5),
      y = c(1, 1, 0),
      label = c("Nighttime\nTemperature", "Systolic Blood\nPressure", var)
    )
    
    # 创建边数据
    edges <- data.frame(
      from_x = c(0, 0.5, 0),
      from_y = c(1, 0, 1),
      to_x = c(0.5, 1, 1),
      to_y = c(0, 1, 1),
      # 创建单行标签 (β值和p值在同一行)
      label = c(
        sprintf("a = %.3f (p = %.3f)", 
                a_path, 
                model_m_coef %>% filter(term == "quartile") %>% pull(p.value)),
        sprintf("b = %.3f (p = %.3f)", 
                b_path, 
                model_y_coef %>% filter(term == var) %>% pull(p.value)),
        sprintf("c' = %.3f (p = %.3f)", 
                c_prime_path, 
                model_y_coef %>% filter(term == "quartile") %>% pull(p.value))
      ),
      # 计算标签旋转角度
      angle = c(
        atan2(0 - 1, 0.5 - 0) * 180/pi,  # a路径角度
        atan2(1 - 0, 1 - 0.5) * 180/pi,  # b路径角度
        atan2(1 - 1, 1 - 0) * 180/pi     # c'路径角度 (水平)
      )
    )
    
    # 创建三角图
    p <- ggplot() +
      # 添加圆形节点 - 浅蓝色填充
      geom_point(data = nodes, aes(x = x, y = y), 
                 shape = 21, size = 45, fill = "lightblue", color = "black") +
      # 添加节点标签 - 字体放大
      geom_text(data = nodes, aes(x = x, y = y, label = label), 
                size = 4.5, fontface = "bold") +
      
      # 添加边和箭头 - 减小箭头大小
      geom_segment(data = edges, 
                   aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
                   arrow = arrow(type = "closed", length = unit(0.1, "inches"), 
                                 angle = 20, ends = "last"),
                   linewidth = 1, color = "black") +
      
      # 添加路径标签 - 平行于箭头
      geom_text(data = edges, 
                aes(x = (from_x + to_x)/2, y = (from_y + to_y)/2, 
                    label = label, angle = angle),
                size = 4, vjust = -0.5, hjust = 0.5) +
      
      # 添加中介效应占比
      geom_label(aes(x = 0.5, y = 0.5, 
                     label = sprintf("Proportion Mediated: %.2f%%", 
                                     prop_mediated * 100)),
                 size = 4.5, fill = "white", alpha = 0.8) +
      
      # 设置坐标范围
      coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
      
      # 主题设置 - 完全空白
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            plot.margin = margin(20, 20, 20, 20)) +
      ggtitle(paste("Mediation Analysis:", var))
    
    mediation_plots[[var]] <- p
    
  }, error = function(e) {
    message(paste("Error in variable:", var, " - ", e$message))
    NULL
  })
}
p
# ================== 构建结果表格 ==================
results_table <- bind_rows(lapply(names(mediation_results), function(var) {
  res <- mediation_results[[var]]$summary
  
  data.frame(
    Variable = var,
    ACME = res$d0,
    ACME_CI_Lower = res$d0.ci[1],
    ACME_CI_Upper = res$d0.ci[2],
    ACME_p = res$d0.p,
    ADE = res$z1,
    ADE_CI_Lower = res$z1.ci[1],
    ADE_CI_Upper = res$z1.ci[2],
    ADE_p = res$z1.p,
    Total_Effect = res$tau.coef,
    Total_Effect_CI_Lower = res$tau.ci[1],
    Total_Effect_CI_Upper = res$tau.ci[2],
    Total_Effect_p = res$tau.p,
    Proportion_Mediated = mediation_results[[var]]$prop_mediated,
    stringsAsFactors = FALSE
  )
}))

# 打印结果表格
print(results_table)

# ================== 保存结果 ==================
# 保存表格
write.csv(results_table, "./终版/mediation_results_DBP.csv", row.names = FALSE)

# 保存所有图形
for (var in names(mediation_plots)) {
  ggsave(paste0("mediation_plot_", var, ".png"), 
         mediation_plots[[var]], 
         width = 8, height = 6, dpi = 300)
}

# 在R中查看所有图形
grid.arrange(grobs = mediation_plots, ncol = 2)





#####敏感性分析
# 在现有中介分析循环中增加敏感性分析
mediation_results <- list()
sensitivity_results <- list()

for (var in sbp_vars) {
  tryCatch({
    formula_m <- reformulate(
      termlabels = c("tm_quartile", "age", "gender", "anthrop_BMI"),
      response = var  # 使用当前的 sbp_vars
    )
    
    formula_y <- reformulate(
      termlabels = c(var, "tm_quartile", "age", "gender", "anthrop_BMI"),
      response = "anthrop_SBP"
    )
    
    # 运行中介分析
    med <- mediate(
      model.m = lm(formula_m, data = df),
      model.y = lm(formula_y, data = df),
      treat = "tm_quartile",
      mediator = var,
      boot = TRUE,
      sims = 500
    )
    
    # 保存中介模型
    med_model <- med
    
    # 敏感性分析
    sens.out <- mediation::medsens(
      med_model, 
      rho.by = 0.1, 
      effect.type = "indirect", 
      sims = 100
    )
    
    # 保存结果
    mediation_results[[var]] <- summary(med_model)
    sensitivity_results[[var]] <- list(
      summary = summary(sens.out),
      plots = list()
    )
    
    # 可视化设置
    plot_file <- paste0("./results/sensitivity_plots_", var, ".pdf")
    pdf(plot_file, width=10, height=6)
    
    # 绘制敏感性分析图
    par(mfrow=c(1,2))
    plot(sens.out, 
         sens.par = "rho",
         main = paste("Sensitivity (rho) -", var),
         ylim = c(-0.2, 0.2),
         col = "blue",
         lwd = 2)
    
    plot(sens.out, 
         sens.par = "R2",
         r.type = "total",
         sign.prod = "positive",
         main = paste("R-squared Sensitivity -", var),
         col = "red",
         lwd = 2)
    
    dev.off()
    
    # 保存绘图对象
    sensitivity_results[[var]]$plots <- recordPlot()
    
  }, error = function(e) {
    message(paste("Error in variable:", var, " - ", e$message))
    NULL
  })
}

# 结果汇总输出
output_list <- list(
  mediation_results = mediation_results,
  sensitivity_results = sensitivity_results
)

# 保存R数据文件
saveRDS(output_list, "./results/full_analysis_results.rds")

# 生成可读的CSV报告
sens_report <- do.call(rbind, lapply(names(sensitivity_results), function(var){
  data.frame(
    Variable = var,
    Rho_Threshold = sensitivity_results[[var]]$summary$rho.thresh,
    R2_Threshold = sensitivity_results[[var]]$summary$R2.thresh,
    Robustness = ifelse(sensitivity_results[[var]]$summary$robust, "Yes", "No")
  )
}))

write.csv(sens_report, "./results/sensitivity_summary.csv", row.names = FALSE)



setwd("D:\\桌面\\肠道菌")
library(tidyverse)
library(mediation)
library(ggpubr)
library(broom)

# 读取数据
data <- read.csv("中介分析.csv", fileEncoding = "GBK")
df <- read.csv("gm_outcome.csv")

# 确保 feature 列的内容是正确的
dbp_vars <- data$feature[c(20:22)]
sbp_vars <- trimws(data$feature[c(3,9)])

# 初始化中介分析结果列表
mediation_results <- list()

# 中介分析
for (var in sbp_vars) {
  tryCatch({
    # 构建中介模型的公式
    formula_m <- reformulate(
      termlabels = c("quartile", "age", "gender", "anthrop_BMI"),
      response = var
    )
    
    formula_y <- reformulate(
      termlabels = c(var, "quartile", "age", "gender", "anthrop_BMI"),
      response = "anthrop_SBP"
    )
    
    # 运行中介分析
    med <- mediate(
      model.m = lm(formula_m, data = df),
      model.y = lm(formula_y, data = df),
      treat = "quartile",
      mediator = var,
      boot = TRUE,
      sims = 500
    )
    
    # 保存完整结果对象
    mediation_results[[var]] <- med
    
  }, error = function(e) {
    message(paste("Error in variable:", var, " - ", e$message))
    NULL
  })
}

# 构建结果表格（关键修改）
results_table <- bind_rows(lapply(names(mediation_results), function(var) {
  med <- mediation_results[[var]]
  s <- summary(med)
  
  # 提取效应值（使用正确的字段名）
  data.frame(
    Variable = var,
    ACME = s$d.avg,
    ACME_CI_Lower = s$d.avg.ci[1],
    ACME_CI_Upper = s$d.avg.ci[2],
    ACME_p = s$d.avg.p,
    ADE = s$z.avg,          # 平均直接效应
    ADE_CI_Lower = s$z.avg.ci[1],
    ADE_CI_Upper = s$z.avg.ci[2],
    ADE_p = s$z.avg.p,
    Total_Effect = s$tau.coef,
    Total_Effect_CI_Lower = s$tau.ci[1],
    Total_Effect_CI_Upper = s$tau.ci[2],
    Total_Effect_p = s$tau.p,
    Proportion_Mediated = s$n.avg,  # 中介效应占比[5](@ref)
    stringsAsFactors = FALSE
  )
}))

# 添加显著性标记
results_table <- results_table %>%
  mutate(
    ACME_Sig = ifelse(ACME_p < 0.05, "*", ""),
    ADE_Sig = ifelse(ADE_p < 0.05, "*", "")
  )

# 保存结果
write_csv(results_table, "./终版/SBP_mediation_results_with_proportion.csv")

# 打印简化结果
cat("\n中介分析结果（含效应占比）：\n")
print(results_table %>% select(Variable, ACME, ADE, Total_Effect, Proportion_Mediated, ACME_Sig))