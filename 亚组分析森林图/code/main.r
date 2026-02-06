rm(list = ls()) # 清空当前工作空间中的所有对象

library(forestploter) # 用于绘制森林图
library(dplyr)        # 用于数据整理
library(grid)         # 用于网格图形布局
library(ggplot2)      # 绘图基础包
library(Cairo)        # 高质量PDF输出
# pak::pkg_install("Cairo")
library(showtext)     # 字体显示支持

font_add("SimHei", "C:/Windows/Fonts/simhei.ttf") # 加载黑体字体
showtext_auto() # 自动启用字体渲染

data <- read.csv("data/subgroup forestplot.csv", fileEncoding = "gbk") # 读取数据文件
# View(data)
forest_plot_data <- data

# 将缺失值、无效字符统一替换为空字符串
forest_plot_data[is.na(forest_plot_data) | 
                   forest_plot_data == "Invalid Number" | 
                   forest_plot_data == "X"] <- ""

# 查看列名
colnames(forest_plot_data)

# 重命名列名以适配 forestploter 要求
forest_plot_data <- rename(
  .data = forest_plot_data,
  '95%CI' = 'X',
  `OR(95%CI)` = `OR..95..CI.`,
  `P for interaction` = `P.for.interaction`,
  `P value` = `P.value`
)

# 查看数据结构
str(forest_plot_data)

# 将 OR、Lower、Upper 转为数值型
forest_plot_data[, which(colnames(forest_plot_data) %in% c("Lower", "Upper", "OR"))] <-
  lapply(
    forest_plot_data[, which(colnames(forest_plot_data) %in% c("Lower", "Upper", "OR"))],
    as.numeric
  )

# 为 95%CI 列预留空白宽度
forest_plot_data$`95%CI` <- "                        "

# 自定义置信区间绘制函数
my_fn_ci <- function(est, lower, upper, sizes, xlim, pch, gp, t_height, nudge_y, ...) {

  # 绘制置信区间线（蓝色）
  line_grob <- grid::segmentsGrob(
    x0 = unit((lower - xlim[1]) / diff(xlim), "npc"),
    x1 = unit((upper - xlim[1]) / diff(xlim), "npc"),
    y0 = unit(0.5 + nudge_y, "npc"),
    y1 = unit(0.5 + nudge_y, "npc"),
    gp = grid::gpar(col = "#005BAC", lwd = 0.5, lty = 1)
  )

  # 绘制效应值点（绿色）
  point_grob <- grid::pointsGrob(
    x = unit((est - xlim[1]) / diff(xlim), "npc"),
    y = unit(0.5 + nudge_y, "npc"),
    pch = 23,
    size = unit(2, "mm"),
    gp = grid::gpar(col = "#35B597", fill = "#35B597")
  )

  grid::grobTree(line_grob, point_grob)
}

# 设置森林图主题样式
my_theme <- forest_theme(
  ci_lty = 1,
  ci_lwd = 0.5,
  ci_pch = 15,
  summary_col = "#35B597",
  summary_fill = "#35B597",
  refline_gp = gpar(col = "red", lwd = 0.3, lty = 2)
)

# 生成森林图对象
p <- forest(
  data = forest_plot_data[, c(1:3, 7:9)],
  lower = forest_plot_data$Lower,
  upper = forest_plot_data$Upper,
  est = forest_plot_data$OR,
  ci_column = 3,
  ref_line = 1,
  xlim = c(0, 10),
  theme = my_theme,
  fn_ci = my_fn_ci,  # 指定自定义置信区间绘制函数
  ticks_at = c(0, 1, 2, 5, 10)
)

# 导出为PDF文件
CairoPDF("output/forest_plot.pdf", width = 8, height = 10, family = "微软雅黑")
print(p)  # 输出森林图
dev.off() # 关闭图形设备

