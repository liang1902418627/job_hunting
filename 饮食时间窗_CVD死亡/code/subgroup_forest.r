rm(list = ls()) # 清空环境变量
setwd("D:\\lianghao\\孙莹数据分析20251215")

library(forestploter) # For creating forest plots
library(dplyr) # For data manipulation
library(grid) # For arranging grid layouts
library(ggplot2)
library(Cairo)
library(tibble)


# final_results_eating_freq.csv------------------------------------------------------------
dt1 <- read.csv("workspace/projects/data/subgroup_analysis/final_results_eating_freq.csv") 

# 示例：将所有列名中的 "." 替换为 "_"
dt1 <- dt1 %>% 
  rename_with(~ gsub("\\.", "_", .x))

names(dt1)
dt1 <- rename(.data = dt1, Subgroup = Variable)
names(dt1)
dt1 <- dt1[, -which(colnames(dt1) %in% c("Count", "Percent", "P_value"))]

# 使用 dplyr 和 tidyr 来处理数据
dt1 <- dt1 %>%
  # 步骤 1: 创建 HR(95%CI) 列，格式化为两位小数
  mutate(`HR(95%CI)` = ifelse(is.na(Point_Estimate), "",
                              paste0(sprintf("%.2f", Point_Estimate), " (",
                                     sprintf("%.2f", Lower), "-",
                                     sprintf("%.2f", Upper), ")")),
         # 将新列移动到第二列位置
         .after = Subgroup)

# 步骤 3: 将数据框中所有"Point.Estimate", "Lower", "Upper"以外的列的 NA 替换为空字符串
dt1 <- dt1 %>%
  mutate(across(-c(Point_Estimate, Lower, Upper), ~ tidyr::replace_na(as.character(.), "")))

# 步骤 4: 在第二和第三列之间插入一个空格列
dt1 <- add_column(dt1, "                       " = "                       ", .after = 2)

# 查看最终结果的列名和前几行
names(dt1)
# View(dt1)
str(dt1)
# 绘制森林图
# 自定义置信区间绘制函数
my_fn_ci <- function(est, lower, upper, sizes, xlim, pch, gp, t_height, nudge_y, 
                     line_col = "#93C8ED", point_col = "#1a5b8a", point_pch = 18, ...) {
  # 画线
  line_grob <- grid::segmentsGrob(
    x0 = unit((lower - xlim[1]) / diff(xlim), "npc"),
    x1 = unit((upper - xlim[1]) / diff(xlim), "npc"),
    y0 = unit(0.5 + nudge_y, "npc"),
    y1 = unit(0.5 + nudge_y, "npc"),
    gp = grid::gpar(col = line_col, lwd = 2, lty = 1)
  )
  # 画点
  point_grob <- grid::pointsGrob(
    x = unit((est - xlim[1]) / diff(xlim), "npc"),
    y = unit(0.5 + nudge_y, "npc"),
    pch = point_pch,
    size = unit(3, "mm"),
    gp = grid::gpar(col = point_col, fill = point_col)
  )
  grid::grobTree(line_grob, point_grob)
}

my_theme <- forest_theme(
  ci_lty = 1,
  ci_lwd = 2,
  ci_pch = 15,
  summary_col = "#1a5b8a",
  summary_fill = "#1a5b8a",
  refline_gp = gpar(col = "red", lwd = 0.3, lty = 2)
)


p <- forest(
    data = dt1[, c(1:3, 7)],
    lower = dt1$Lower,
    upper = dt1$Upper,
    est = dt1$`Point_Estimate`,
    ci_column = 3,
    ref_line = 1,
    xlim = c(-0.5, 2),
    theme = my_theme,
    fn_ci = my_fn_ci,  # 关键：指定自定义置信区间绘制函数
    ticks_at = c(-0.5, 1, 2)
)
# print(p)
# 保存路径
dir <- "workspace/projects/results/pictures/subgroup_forest"
if (!dir.exists(dir)) {
  dir.create(dir, recursive = TRUE)
}
CairoPDF("workspace/projects/results/pictures/subgroup_forest/subgroup_forest_eating_freq.pdf", width = 6, height = 11, family = "Times New Roman")
print(p)
dev.off()
# final_results_eating_frequency_cat.csv------------------------------------------------------------
dt2 <- read.csv("workspace/projects/data/subgroup_analysis/final_results_eating_frequency_cat.csv")
# 示例：将所有列名中的 "." 替换为 "_"
dt2 <- dt2 %>% 
  rename_with(~ gsub("\\.", "_", .x))
names(dt2)
# 去掉“KM”列
dt2 <- dt2[, -which(colnames(dt2) == "KM")]
dt2 <- rename(.data = dt2, Subgroup = Variable)
names(dt2)
dt2 <- dt2[, -which(colnames(dt2) %in% c("Count", "Percent", "P_value"))]
str(dt2)
# 如果Point_Estimate列的值为Reference，则替换为1.00,然后将Point_Estimate列转换为数值型
dt2$Point_Estimate <- ifelse(dt2$Point_Estimate == "Reference", 1.00, dt2$Point_Estimate)
dt2$Point_Estimate <- as.numeric(dt2$Point_Estimate)
# 使用 dplyr 和 tidyr 来处理数据
dt2 <- dt2 %>%
  # 步骤 1: 创建 HR(95%CI) 列，格式化为两位小数
  mutate(`HR(95%CI)` = ifelse(is.na(Point_Estimate), "",
                              paste0(sprintf("%.2f", Point_Estimate), " (",
                                     sprintf("%.2f", Lower), "-",
                                     sprintf("%.2f", Upper), ")")),
         # 将新列移动到第三列位置
         .after = Levels)
# 步骤 3: 将数据框中所有"Point.Estimate", "Lower", "Upper"以外的列的 NA 替换为空字符串
dt2 <- dt2 %>%
  mutate(across(-c(Point_Estimate, Lower, Upper), ~ tidyr::replace_na(as.character(.), "")))
names(dt2)
# 步骤 4: 在第三和第四列之间插入一个空格列
dt2 <- add_column(dt2, "                         " = "                       ", .after = 3)
# 查看最终结果的列名和前几行
names(dt2)
# View(dt2)
str(dt2)
# 将`HR(95%CI)`值为"1.00(NA-NA)"的替换为"Reference"
dt2$`HR(95%CI)` <- ifelse(dt2$`HR(95%CI)` == "1.00 (NA-NA)", "Reference", dt2$`HR(95%CI)`)

# 绘制森林图
p2 <- forest(
    data = dt2[, c(1:4, 8)],
    lower = dt2$Lower,
    upper = dt2$Upper,
    est = dt2$`Point_Estimate`,
    ci_column = 4,
    ref_line = 1,
    xlim = c(-0.5, 5),
    theme = my_theme,
    fn_ci = my_fn_ci,  # 关键：指定自定义置信区间绘制函数
    ticks_at = c(-0.5, 1, 2, 5),
    # 在这里为自定义函数传递参数
    line_col = "#F7C1C1",      # 设置置信区间线的颜色
    point_col = "#cc5e5e", # 设置点的颜色
    point_pch = 18          # 设置点的形状 (18是菱形)
)
CairoPDF("workspace/projects/results/pictures/subgroup_forest/subgroup_forest_eating_frequency_cat.pdf", width = 8.5, height = 25, family = "Times New Roman")
print(p2)
dev.off()
# final_results_eating_time_window_cat.csv------------------------------------------
dt3 <- read.csv("workspace/projects/data/subgroup_analysis/final_results_eating_time_window_cat.csv")
# 示例：将所有列名中的 "." 替换为 "_"
dt3 <- dt3 %>% 
  rename_with(~ gsub("\\.", "_", .x))
names(dt3)
# 去掉“KM”列
dt3 <- dt3[, -which(colnames(dt3) == "KM")]
dt3 <- rename(.data = dt3, Subgroup = Variable)
names(dt3)
dt3 <- dt3[, -which(colnames(dt3) %in% c("Count", "Percent", "P_value"))]
str(dt3)
# 如果Point_Estimate列的值为Reference，则替换为1.00,然后将Point_Estimate列转换为数值型
dt3$Point_Estimate <- ifelse(dt3$Point_Estimate == "Reference", 1.00, dt3$Point_Estimate)
dt3$Point_Estimate <- as.numeric(dt3$Point_Estimate)
# 使用 dplyr 和 tidyr 来处理数据
dt3 <- dt3 %>%
  # 步骤 1: 创建 HR(95%CI) 列，格式化为两位小数
  mutate(`HR(95%CI)` = ifelse(is.na(Point_Estimate), "",
                              paste0(sprintf("%.2f", Point_Estimate), " (",
                                     sprintf("%.2f", Lower), "-",
                                     sprintf("%.2f", Upper), ")")),
         # 将新列移动到第三列位置
         .after = Levels)
# 步骤 3: 将数据框中所有"Point.Estimate", "Lower", "Upper"以外的列的 NA 替换为空字符串
dt3 <- dt3 %>%
  mutate(across(-c(Point_Estimate, Lower, Upper), ~ tidyr::replace_na(as.character(.), "")))
names(dt3)
# 步骤 4: 在第三和第四列之间插入一个空格列
dt3 <- add_column(dt3, "                         " = "                       ", .after = 3)
# 查看最终结果的列名和前几行
names(dt3)
# View(dt3)
str(dt3)
# 将`HR(95%CI)`值为"1.00(NA-NA)"的替换为"Reference"
dt3$`HR(95%CI)` <- ifelse(dt3$`HR(95%CI)` == "1.00 (NA-NA)", "Reference", dt3$`HR(95%CI)`)

# 绘制森林图
p3 <- forest(
    data = dt3[, c(1:4, 8)],
    lower = dt3$Lower,
    upper = dt3$Upper,
    est = dt3$`Point_Estimate`,
    ci_column = 4,
    ref_line = 1,
    xlim = c(-0.5, 20),
    theme = my_theme,
    fn_ci = my_fn_ci,  # 关键：指定自定义置信区间绘制函数
    ticks_at = c(1, 10, 20),
    # 在这里为自定义函数传递参数
    line_col = "#F9C780",      # 设置置信区间线的颜色
    point_col = "#ad690a", # 设置点的颜色
    point_pch = 18          # 设置点的形状 (18是菱形)
)
CairoPDF("workspace/projects/results/pictures/subgroup_forest/subgroup_forest_eating_time_window_cat.pdf", width = 8.5, height = 25, family = "Times New Roman")
print(p3)
dev.off()
# final_results_eating_window.csv---------------------------------------------
dt4 <- read.csv("workspace/projects/data/subgroup_analysis/final_results_eating_window.csv")

# 示例：将所有列名中的 "." 替换为 "_"
dt4 <- dt4 %>%
  rename_with(~ gsub("\\.", "_", .x))

names(dt4)
dt4 <- rename(.data = dt4, Subgroup = Variable)
names(dt4)
dt4 <- dt4[, -which(colnames(dt4) %in% c("Count", "Percent", "P_value"))]

# 使用 dplyr 和 tidyr 来处理数据
dt4 <- dt4 %>%
  # 步骤 1: 创建 HR(95%CI) 列，格式化为两位小数
  mutate(`HR(95%CI)` = ifelse(is.na(Point_Estimate), "",
                              paste0(sprintf("%.2f", Point_Estimate), " (",
                                     sprintf("%.2f", Lower), "-",
                                     sprintf("%.2f", Upper), ")")),
         # 将新列移动到第二列位置
         .after = Subgroup)

# 步骤 3: 将数据框中所有"Point.Estimate", "Lower", "Upper"以外的列的 NA 替换为空字符串
dt4 <- dt4 %>%
  mutate(across(-c(Point_Estimate, Lower, Upper), ~ tidyr::replace_na(as.character(.), "")))

# 步骤 4: 在第二和第三列之间插入一个空格列
dt4 <- add_column(dt4, "                       " = "                       ", .after = 2)

# 查看最终结果的列名和前几行
names(dt4)
# View(dt4)
str(dt4)
# 绘制森林图
# 自定义置信区间绘制函数
my_fn_ci <- function(est, lower, upper, sizes, xlim, pch, gp, t_height, nudge_y, 
                     line_col = "#93C8ED", point_col = "#1a5b8a", point_pch = 18, ...) {
  # 画线
  line_grob <- grid::segmentsGrob(
    x0 = unit((lower - xlim[1]) / diff(xlim), "npc"),
    x1 = unit((upper - xlim[1]) / diff(xlim), "npc"),
    y0 = unit(0.5 + nudge_y, "npc"),
    y1 = unit(0.5 + nudge_y, "npc"),
    gp = grid::gpar(col = line_col, lwd = 2, lty = 1)
  )
  # 画点
  point_grob <- grid::pointsGrob(
    x = unit((est - xlim[1]) / diff(xlim), "npc"),
    y = unit(0.5 + nudge_y, "npc"),
    pch = point_pch,
    size = unit(3, "mm"),
    gp = grid::gpar(col = point_col, fill = point_col)
  )
  grid::grobTree(line_grob, point_grob)
}

my_theme <- forest_theme(
  ci_lty = 1,
  ci_lwd = 2,
  ci_pch = 15,
  summary_col = "#1a5b8a",
  summary_fill = "#1a5b8a",
  refline_gp = gpar(col = "red", lwd = 0.3, lty = 2)
)


p4 <- forest(
    data = dt4[, c(1:3, 7)],
    lower = dt4$Lower,
    upper = dt4$Upper,
    est = dt4$`Point_Estimate`,
    ci_column = 3,
    ref_line = 1,
    xlim = c(-0.2, 1.5),
    theme = my_theme,
    fn_ci = my_fn_ci,  # 关键：指定自定义置信区间绘制函数
    ticks_at = c(-0.2, 1, 1.5)
)
# print(p4)
# 保存路径
dir <- "workspace/projects/results/pictures/subgroup_forest"
if (!dir.exists(dir)) {
  dir.create(dir, recursive = TRUE)
}
CairoPDF("workspace/projects/results/pictures/subgroup_forest/subgroup_forest_eating_time_window.pdf", width = 6, height = 11, family = "Times New Roman")
print(p4)
dev.off()
