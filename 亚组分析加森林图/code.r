# ----------------------
# 亚组分析+森林图
# ----------------------

rm(list = ls()) # 清空环境变量

# 加载必要的包 ------------------------------------------------
paks <- c(
    "dplyr", "jstable", "ggplot2", "forestplot", "survival", "grid", "forestploter"
)

lapply(paks, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
})
# 读取数据-------------------------
dt <- readRDS("亚组分析加森林图/employee_burnout_data.rds")
var_subgroup <- c(
    "age_group", "sex", "job_level", "marital", "income",
    "stress_index_group", "workload_group",
    "department", "caffeine", "alcohol", "insomnia", "anxiety"
)

var_cov <- c(
    "meeting_hours", "overtime_hours", "email_load",
    "physical_activity", "sleep_hours"
)

vars_pool <- unique(c(var_subgroup, var_cov))

results_list <- list()

for (strata in var_subgroup) {
    cat("Subgroup:", strata, "\n")
    res <- TableSubgroupCox(
        formula = Surv(follow_months, burnout_event) ~ screen_hours, # 这里的screen_hours是你要分析的主要暴露变量
        data = dt,               # 数据框
        var_subgroup = strata,   # 分层变量
        time_eventrate = 204,    # 这里的时间单位是月，事件率是每100人年多少事件
        var_cov = setdiff(vars_pool, strata), # 其他协变量，排除当前的分层变量
        decimal.hr = 2,          # HR的小数位数
        decimal.percent = 2,     # 百分比的小数位数
        decimal.pvalue = 3,      # P值的小数位数
        event = TRUE             # 是否显示事件数
    )
    results_list[[strata]] <- res
}

final_results <- do.call(rbind, results_list)
write.csv(final_results, "亚组分析加森林图/subgroup_screen_burnout.csv", row.names = FALSE)
# 绘制森林图 ------------------------------------------------
data <- read.csv("亚组分析加森林图/subgroup_screen_burnout.csv", fileEncoding = "utf-8") # Read the data
names(data)
View(data)
forest_plot_data <- data
cols_to_clean <- c("Variable", "Count", "Percent", "P.value", "P.for.interaction")
cols_to_clean <- intersect(cols_to_clean, colnames(forest_plot_data))
if (length(cols_to_clean) > 0) {
    forest_plot_data[cols_to_clean] <- lapply(forest_plot_data[cols_to_clean], function(x) {
        x[is.na(x)] <- ""
        x
    })
}
# View(forest_plot_data)
colnames(forest_plot_data)
# 设置列95%CI
forest_plot_data$`95%CI` <- sprintf(
    "%.2f (%.2f - %.2f)",
    suppressWarnings(as.numeric(forest_plot_data$Point.Estimate)),
    suppressWarnings(as.numeric(forest_plot_data$Lower)),
    suppressWarnings(as.numeric(forest_plot_data$Upper))
)

# 绘图列
forest_plot_data$`OR(95%CI)` <- "                                                    "
names(forest_plot_data)

# 调整列顺序，将95%CI调到Percent列下一列，OR(95%CI)调到95%CI下一列
forest_plot_data <- forest_plot_data[, c(
    "Variable", "Count", "Percent", "95%CI", "OR(95%CI)",
    "Point.Estimate", "Lower", "Upper", "P.for.interaction", "P.value"
)]
# 去掉“NA (NA - NA)”
forest_plot_data$`95%CI`[forest_plot_data$`95%CI` == "NA (NA - NA)"] <- ""
forest_plot_data <- rename(.data = forest_plot_data, `P for interaction` = `P.for.interaction`, `P value` = `P.value`)
View(forest_plot_data)
str(forest_plot_data)

# 自定义置信区间绘制函数-------------------------------------------
my_fn_ci <- function(est, lower, upper, sizes, xlim, pch, gp, t_height, nudge_y, ...) {
    # 画线（蓝色）
    line_grob <- grid::segmentsGrob(
        x0 = unit((lower - xlim[1]) / diff(xlim), "npc"), # 将lower转换为npc单位
        x1 = unit((upper - xlim[1]) / diff(xlim), "npc"), # 将upper转换为npc单位
        y0 = unit(0.5 + nudge_y, "npc"),                  # 将y位置设置为0.5加上nudge_y
        y1 = unit(0.5 + nudge_y, "npc"),                  # 将y位置设置为0.5加上nudge_y
        gp = grid::gpar(col = "#005BAC", lwd = 0.5, lty = 1) # 设置线条颜色、宽度和类型
    )
    # 画点（绿色）
    point_grob <- grid::pointsGrob(
        x = unit((est - xlim[1]) / diff(xlim), "npc"), # 将est转换为npc单位
        y = unit(0.5 + nudge_y, "npc"),                # 将y位置设置为0.5加上nudge_y
        pch = 23,                                      # 设置点的形状为菱形
        size = unit(2, "mm"),                          # 设置点的大小
        gp = grid::gpar(col = "#35B597", fill = "#35B597") # 设置点的边框颜色和填充颜色
    )
    grid::grobTree(line_grob, point_grob) # 将线和点组合成一个grob对象返回
}

my_theme <- forest_theme(
    ci_lty = 1,         # 置信区间线型
    ci_lwd = 0.5,        # 置信区间线宽
    ci_pch = 15,          # 点的形状
    summary_col = "#35B597", # 汇总行颜色
    summary_fill = "#35B597", # 汇总行填充色
    refline_gp = gpar(col = "red", lwd = 0.3, lty = 2) # 参考线样式
)
names(forest_plot_data)
p <- forest(
    data = forest_plot_data[, c(1:5, 9:10)], # 只选择需要显示的列
    lower = forest_plot_data$Lower,          # 指定置信区间下限，仅用于绘图
    upper = forest_plot_data$Upper,          # 指定置信区间上限，仅用于绘图
    est = forest_plot_data$`Point.Estimate`, # 指定点估计值，仅用于绘图
    ci_column = 5,                           # 森林图绘制在哪一列
    ref_line = 1,                            # 参考线位置
    xlim = c(0.5, 1.5),                      # x轴范围
    theme = my_theme,                        # 使用自定义主题
    fn_ci = my_fn_ci, # 关键：指定自定义置信区间绘制函数
    ticks_at = c(0.5, 1, 1.5)                # 指定刻度位置
)
Cairo::CairoPDF("亚组分析加森林图/forest_plot.pdf", width = 11, height = 11)
print(p)
dev.off()
