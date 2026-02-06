######Model 3 ggplot######
# 创建列表以存储结果
{p_values <- list()
prediction <- list()
exposure_response_plot<-list()
}
# 循环处理每个变量
for (variable in variables) {
  # 去除仅该变量为NA的行
  data_subset <- data1[!is.na(data1[[variable]]), ]
  # 构建模型公式
  formula <- as.formula(paste(variable, "~ 本期是否干预 +(性别+ 年龄+ bmi + CHOL+TG+ HDL+ LDL+ GLU +(1|IDi))"))
  # 拟合混合线性模型
  model <- lmerTest::lmer(formula, data = data_subset)
  # 提取 p 值
  p_value <- summary(model)$coefficients["本期是否干预", "Pr(>|t|)"]
  
  # 预测并获取置信区间
  predictions <- predict(model, newdata = data_subset, type = "response", allow.new.levels = TRUE)
  # 使用dplyr的mutate函数添加新列  
  data_subset <- data_subset %>%  
    cbind(predictions = predictions) 
  
  data_subset$本期是否干预 <- factor(data_subset$本期是否干预,
                             labels = c("否", "是"),
                             levels = c(0, 1)) # 假设0代表NO，1代表YES
  
  # 创建图形
  exposure_response_plot[[variable]] <- ggplot(data_subset, aes(x = factor(本期是否干预), y = predictions)) +
    geom_violin(aes(fill = factor(本期是否干预)), alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.8) +
    geom_jitter(width = 0.01, aes(color = "black"), alpha = 0.8) +  # 设置散点颜色为黑色，调整散点大小为0.1
    labs(x = "干预", y = paste(variable, "(", unit_labels[variable], ")"), title = "") +
    scale_color_manual(values = "black") +  # 设置散点颜色为黑色
    scale_fill_manual(values = c("#ADD8E6", "#FFFFE0")) +  # 设置填充颜色为浅蓝色和浅黄色
    theme_classic() +
    theme(
      text = element_text(color = "gray30", size = 30),
      axis.line = element_line(size = 0.6, color = "gray30"),
      axis.ticks = element_line(size = 0.6, color = "gray30"),
      axis.ticks.length = unit(1.5, units = "mm"),
      plot.margin = unit(rep(1, 4), units = "lines"),
      legend.position = "none"  # 将图例位置设为顶部
    )+
    annotate("text", x = 2, y = max(data_subset[[variable]]) * 1.05,
             label = paste("p =", formatC(p_value, digits = 3, format = "f")),
             size = 12)
  
  # 显示图形
  print(exposure_response_plot[[variable]])
  
  # 将 p 值、系数、置信区间存储在列表