# ------------------------------------------------
# 题目：亚组分析（jstable实现，按“变量池 + 剔除当前分层变量”逻辑）
# 暴露：Eating_window、Eating_freq、Eating_frequency_cat、Eating_time_window_cat
# 结局：CVDmortality
# 分层变量：Age_group，DEI_group，Gender，Education，Marriage，Economic.status
# 协变量：Racial，BMI，Smoking，Drinking，HBP，DM，CVD，DEI, CARB_average、FAT_average、PROT_average、PA_total、sleep_time
# 时间变量：time
# 方法：Cox比例风险回归（jstable: TableSubgroupCox）
# 输出表头：Variable, Count, Percent, Point Estimate, Lower, Upper, P value, P for interaction
# ----------------------------------------------------

rm(list = ls())

setwd("D:/lianghao/孙莹数据分析20251215/")

paks <- c("survival", "dplyr", "readr", "jstable")
invisible(lapply(paks, require, character.only = TRUE))

# 读取数据
dt <- readRDS("workspace/projects/data/final_analysis_data.rds")
dt2 <- dt %>%
    mutate(across(c(mortstat, CVDmortality, CancerMortality), ~ as.numeric(as.character(.))))

# 年龄分组：<65 vs ≥65
dt2 <- dt2 %>%
    mutate(
        Age_group = case_when(
            Age < 65 ~ "<65",
            TRUE ~ "≥65"
        ),
        Age_group = factor(Age_group, levels = c("<65", "≥65"))
    )

# 将DEI按中位数分组，分为1,2两组
median_DEI <- median(dt2$DEI, na.rm = TRUE)
dt2 <- dt2 %>%
    mutate(
        DEI_group = case_when(
            DEI < median_DEI ~ "1",
            DEI >= median_DEI ~ "2"
        ),
        DEI_group = factor(DEI_group, levels = c("1", "2"))
    )

# 重设参考水平T2
dt2$Eating_frequency_cat <- relevel(dt2$Eating_frequency_cat, ref = "T2")
dt2$Eating_time_window_cat <- relevel(dt2$Eating_time_window_cat, ref = "T2")
# 分层变量池 / 协变量池 / 全调整变量池（分层变量池是变量池子集）
var_subgroup <- c("Age_group", "Gender", "Education", "Marriage", "Economic.status", "DEI_group", "BMI", "Racial", "Smoking", "Drinking", "HBP", "DM")
var_cov <- c("CARB_average", "FAT_average", "PROT_average", "PA_total", "sleep_time")
vars_pool <- unique(c(var_subgroup, var_cov))

# 确保变量存在
missing_vars <- setdiff(
    unique(c(
        "time", "CVDmortality", vars_pool,
        "Eating_window", "Eating_freq", "Eating_frequency_cat", "Eating_time_window_cat"
    )), names(dt2)
)
if (length(missing_vars) > 0) {
    stop(sprintf("数据缺少字段：%s", paste(missing_vars, collapse = ", ")))
}

# 亚组分析——Eating_window------------------------------------------------------------
results_list_eating_window <- list()
for (strata in var_subgroup) {
    cat("正在分析分层变量：", strata, "\n")
    res_subgroup <- TableSubgroupCox(
        formula = Surv(time, CVDmortality) ~ Eating_window,
        data = dt2,
        var_subgroup = strata,
        time_eventrate = 204,
        var_cov = setdiff(vars_pool, strata),
        decimal.hr = 2,
        decimal.percent = 2,
        decimal.pvalue = 3,
        event = TRUE
    )
    # 去掉第一行结果（总体）
    # res_subgroup <- res_subgroup[-1, ]
    results_list_eating_window[[strata]] <- res_subgroup
}
# 合并结果
final_results_eating_window <- do.call(rbind, results_list_eating_window)
# 将NA设置为""
final_results_eating_window[is.na(final_results_eating_window)] <- ""
# 保存结果
write.csv(final_results_eating_window, "workspace/projects/results/tables/subgroup_analysis/final_results_eating_window.csv", row.names = FALSE)
# 亚组分析——Eating_freq------------------------------------------------------------
results_list_eating_freq <- list()
for (strata in var_subgroup) {
    cat("正在分析分层变量：", strata, "\n")
    res_subgroup <- TableSubgroupCox(
        formula = Surv(time, CVDmortality) ~ Eating_freq,
        data = dt2,
        var_subgroup = strata,
        time_eventrate = 204,
        var_cov = setdiff(vars_pool, strata),
        decimal.hr = 2,
        decimal.percent = 2,
        decimal.pvalue = 3,
        event = TRUE
    )
    # 去掉第一行结果（总体）
    # res_subgroup <- res_subgroup[-1, ]
    results_list_eating_freq[[strata]] <- res_subgroup
}
# 合并结果
final_results_eating_freq <- do.call(rbind, results_list_eating_freq)
# 将NA设置为""
final_results_eating_freq[is.na(final_results_eating_freq)] <- ""
# 保存结果
write.csv(final_results_eating_freq, "workspace/projects/results/tables/subgroup_analysis/final_results_eating_freq.csv", row.names = FALSE)
# 亚组分析——Eating_frequency_cat----------------------------------------------------------
results_list_eating_frequency_cat <- list()
for (strata in var_subgroup) {
    cat("正在分析分层变量：", strata, "\n")
    res_subgroup <- TableSubgroupCox(
        formula = Surv(time, CVDmortality) ~ Eating_frequency_cat,
        data = dt2,
        var_subgroup = strata,
        time_eventrate = 204,
        var_cov = setdiff(vars_pool, strata),
        decimal.hr = 2,
        decimal.percent = 2,
        decimal.pvalue = 3,
        event = TRUE
    )
    # 去掉第一行结果（总体）
    # res_subgroup <- res_subgroup[-1, ]
    results_list_eating_frequency_cat[[strata]] <- res_subgroup
}
# 合并结果
final_results_eating_frequency_cat <- do.call(rbind, results_list_eating_frequency_cat)
# 将NA设置为""
final_results_eating_frequency_cat[is.na(final_results_eating_frequency_cat)] <- ""
# 保存结果
write.csv(final_results_eating_frequency_cat, "workspace/projects/results/tables/subgroup_analysis/final_results_eating_frequency_cat.csv", row.names = FALSE)

# 亚组分析——Eating_time_window_cat--------------------------------------------------------
results_list_eating_time_window_cat <- list()
for (strata in var_subgroup) {
    cat("正在分析分层变量：", strata, "\n")
    res_subgroup <- TableSubgroupCox(
        formula = Surv(time, CVDmortality) ~ Eating_time_window_cat,
        data = dt2,
        var_subgroup = strata,
        time_eventrate = 204,
        var_cov = setdiff(vars_pool, strata),
        decimal.hr = 2,
        decimal.percent = 2,
        decimal.pvalue = 3,
        event = TRUE
    )
    # 去掉第一行结果（总体）
    # res_subgroup <- res_subgroup[-1, ]
    results_list_eating_time_window_cat[[strata]] <- res_subgroup
}
# 合并结果
final_results_eating_time_window_cat <- do.call(rbind, results_list_eating_time_window_cat)
# 将NA设置为""
final_results_eating_time_window_cat[is.na(final_results_eating_time_window_cat)] <- ""
# 保存结果
write.csv(
    final_results_eating_time_window_cat,
    "workspace/projects/results/tables/subgroup_analysis/final_results_eating_time_window_cat.csv",
    row.names = FALSE
)