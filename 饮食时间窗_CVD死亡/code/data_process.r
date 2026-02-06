# ---------------------------------
# 题目：处理数据的暴露与结局
# 信息：
# SEQN：受访者序列号
# WTINT2YR：全样本访谈权重
# SDMVPSU：初级抽样单位代码
# First_time_avg ：平均首次进食时间
# Last_time_avg：平均末次进食时间
# combined_freq：合并的进餐频率
# Interval_avg：进食时间窗
# mortstat：全因死亡
# CVDmortality：心血管死亡
# CancerMortality：癌症死亡
# permth_int：生存时间（月）
# SDMVSTRA：分层变量代码
# MCQ220: 癌症
# 暴露变量：combined_freq、Interval_avg、Eating_time_window_cat、
# Eating_frequency_cat、Time_of_first_meal、Time_of_last_meal
# 连续变量：Age、BMXBMI、combined_kcal、permth_int、combined_freq、Interval_avg、CARB_average、FAT_average、PROT_average、PA_total、sleep_time
# 分类变量：Gender、Racial、Education、Marriage、Economic.status、BMI、Smoking
# Drinking、HBP、DM、CVD、mortstat、CVDmortality、CancerMortality、Eating_time_window_cat、
# Eating_frequency_cat、Time_of_first_meal、Time_of_last_meal
# ----------------------------------
rm(list = ls()) # 清空工作区
# setwd("D:/lianghao/孙莹数据分析20251215") # 设置工作目录
paks <- c("readr", "dplyr", "lubridate", "hms")
lapply(paks, library, character.only = TRUE)

# 读取数据
data_v1 <- readr::read_csv(
    "workspace\\projects\\data\\最终分析数据1发给梁豪.csv",
    na = c("", "NA", "Invalid Number")
)
data_v1_add <- readr::read_csv(
    "workspace\\projects\\data\\data_all.csv",
    na = c("", "NA", "Invalid Number")
)
names(data_v1_add)
# 合并数据
data_v1 <- data_v1 %>%
    left_join(
        data_v1_add,
        by = "SEQN"
    )
# View(data_v1)
colnames(data_v1)
# 时间格式处理------------------------------
# KCAL_50pct_Time_avg: 0.4562
# Before_50pct_Time_avg: 1899-12-31 09:15:00
# After_50pct_Time_avg: 1899-12-31 11:45:00
# First_time_avg: 1899-12-31 09:15:00
# Last_time_avg: 0.875
# earliest_time0: 9H 0M 0S
# latest_time0: 23H 0M 0S
# earliest_time1: 9H 30M 0S
# latest_time1: 19H 0M 0S
# earliest_time2: 19H 0M 0S
# latest_time2: 23H 0M 0S
# -------------------------------------------
# 检查时间格式
date_vars <- c("First_time_avg", "Last_time_avg", "Before_50pct_Time_avg", 
               "After_50pct_Time_avg")

lapply(date_vars, function(var) {
  print(var)
  print(class(data_v1[[var]]))
  print(head(data_v1[[var]]))
})

# -------------------------------------------
# 统一为 hms 时间格式
# -------------------------------------------
data_v1 <- data_v1 %>%
  mutate(
    # 将 POSIXct 类型转换为 hms 时间类型
    across(c("First_time_avg", "Before_50pct_Time_avg", "After_50pct_Time_avg"), as_hms),
    # 将代表一天中比例的数值转换为 hms 时间类型
    Last_time_avg = as_hms(round(Last_time_avg * 24 * 3600))
  )

# 再次检查处理后的数据格式
lapply(date_vars, function(var) {
  print(paste("---", var, "---"))
  print(class(data_v1[[var]]))
  print(head(data_v1[[var]]))
})

# 时间变量缺失值检查----------------
missing_summary <- sapply(data_v1[date_vars], function(x) sum(is.na(x)))
print("时间变量缺失值汇总：")
print(missing_summary)

# ------------------------------------------------
# 题目：处理暴露变量
# 第一餐时间（Time of first meal）：T1（08:00之前）、T2（08:00-10:00）和T3（10:00之后）
# 最后一餐的进食时间（Time of last meal）：T1（19:00之前）、T2（19:00-20:00）和T3（20:00之后）
# 进食频率（Eating frequency）：T1（少于4次）、T2（4至5次）和T3（超过5次）
# 进食时间窗：短时长（0-10小时/天）、中等时长（11-12小时/天）和长时长（≥13小时/天）（T1、T2、T3）
# -------------------------------------------------
# 第一餐时间：Time of first meal
data_v2 <- data_v1 %>%
    mutate(
        Time_of_first_meal = case_when(
            First_time_avg < as_hms("08:00:00") ~ "T1",
            First_time_avg >= as_hms("08:00:00") & First_time_avg <= as_hms("10:00:00") ~ "T2",
            First_time_avg > as_hms("10:00:00") ~ "T3",
            TRUE ~ NA_character_
        )
    ) %>%
    # 因子化
    mutate(
        Time_of_first_meal = factor(Time_of_first_meal, levels = c("T1", "T2", "T3"))
    )
# 检查
# 最后一餐进食时间：Time of last meal
data_v2 <- data_v2 %>%
    mutate(
        Time_of_last_meal = case_when(
            Last_time_avg < as_hms("19:00:00") ~ "T1",
            Last_time_avg >= as_hms("19:00:00") & Last_time_avg <= as_hms("20:00:00") ~ "T2",
            Last_time_avg > as_hms("20:00:00") ~ "T3",
            TRUE ~ NA_character_
        )
    ) %>%
    # 因子化
    mutate(
        Time_of_last_meal = factor(Time_of_last_meal, levels = c("T1", "T2", "T3"))
    )
# 检查
table(data_v2$Time_of_last_meal, useNA = "ifany")

# 进食频率：Eating frequency
data_v2 <- data_v2 %>%
    mutate(
        Eating_frequency_cat = case_when(
            combined_freq < 4 ~ "T1",
            combined_freq >= 4 & combined_freq <= 5 ~ "T2",
            combined_freq > 5 ~ "T3",
            TRUE ~ NA_character_
        )
    ) %>%
    # 因子化
    mutate(
        Eating_frequency_cat = factor(Eating_frequency_cat, levels = c("T1", "T2", "T3"))
    )
# 检查
table(data_v2$Eating_frequency_cat, useNA = "ifany")
sum(is.na(data_v2$combined_freq))

# 进食时间窗：Eating time window
data_v2 <- data_v2 %>%
    mutate(
        Eating_time_window_cat = case_when(
            Interval_avg <= 11 ~ "T1",
            Interval_avg > 11 & Interval_avg < 13 ~ "T2",
            Interval_avg >= 13 ~ "T3",
            TRUE ~ NA_character_
        )
    ) %>%
    # 因子化
    mutate(
        Eating_time_window_cat = factor(Eating_time_window_cat, levels = c("T1", "T2", "T3"))
    )
# 检查
class(data_v2$Interval_avg)
table(data_v2$Eating_time_window_cat, useNA = "ifany")
sum(is.na(data_v2$Interval_avg))

# 对分类变量进行因子化处理-----------------------------------------
# 确定分类变量
unique(data_v2$Age)    # 年龄
unique(data_v2$Gender)# 性别
unique(data_v2$Racial)# 种族
unique(data_v2$Education)# 教育水平
unique(data_v2$Marriage) # 婚姻状况
unique(data_v2$Economic.status) # 经济状况
unique(data_v2$BMXBMI) # BMI 连续
unique(data_v2$BMI) # BMI 分类
unique(data_v2$Smoking) # 吸烟状况
unique(data_v2$Drink.diet.) # 饮酒状况
unique(data_v2$HBP) # 高血压
unique(data_v2$DM) # 糖尿病
unique(data_v2$CVD) # 心血管疾病
unique(data_v2$combined_kcal) # 每日总能量摄入
unique(data_v2$mortstat) # 全因死亡
unique(data_v2$CVDmortality) # 心血管死亡
unique(data_v2$CancerMortality) # 癌症死亡
# 分类变量因子化------------------------------------------
# 处理BMI分类变量
unique(data_v2$BMI)
data_v2$BMI <- ifelse(
    data_v2$BMI %in% c(1, 2), 1, 
    ifelse(data_v2$BMI == 3, 2,
        data_v2$BMI)
)

# 定义需要因子化的分类变量列表
categorical_vars <- c(
    "Gender", "Racial", "Education", "Marriage", "Economic.status", 
    "BMI", "Smoking", "Drink.diet.", "HBP", "DM", "CVD", 
    "mortstat", "CVDmortality", "CancerMortality"
)

# 使用 across 和 factor 函数批量处理
data_v2 <- data_v2 %>%
    mutate(across(all_of(categorical_vars), ~ factor(., levels = sort(unique(.)))))

# (可选) 检查部分变量的因子化结果
print("--- Gender ---")
class(data_v2$Gender)
levels(data_v2$Gender)

print("--- BMI ---")
class(data_v2$BMI)
levels(data_v2$BMI)

print("--- mortstat ---")
class(data_v2$mortstat)
levels(data_v2$mortstat)

# 提取需要用到的变量，生成最终分析数据集------------------------
final_vars <- c(
    "SEQN", "WTINT2YR", "SDMVPSU", "SDMVSTRA",
    "Time_of_first_meal", "Time_of_last_meal", "Eating_frequency_cat",
    "permth_int",
    "combined_freq", "Interval_avg", "Eating_time_window_cat",
    "Age", "Gender", "Racial", "Education", "Marriage", "Economic.status", 
    "BMXBMI", "BMI", "Smoking", "Drink.diet.", "HBP", "DM", "CVD", 
    "combined_kcal", "mortstat", "CVDmortality", "CancerMortality",
    "CARB_average", "FAT_average", "PROT_average", "PA_total", "sleep_time", "MCQ220"
)

data_final <- data_v2 %>%
    select(all_of(final_vars))
str(data_final)
# 重命名保存最终数据集------------------------------
# 重命名映射表
rename_map <- c(
    time = "permth_int",
    Eating_freq = "combined_freq",
    Eating_window = "Interval_avg",
    DEI = "combined_kcal",
    Drinking = "Drink.diet."
)

# 使用 rename 函数和映射表重命名列
data_final <- data_final %>%
    rename(!!!rename_map)

# (可选) 检查重命名后的列名
colnames(data_final)
# 保存数据为rds格式----------------------------
saveRDS(data_final, "workspace/projects/data/final_analysis_data.rds")
# 协变量数据插补KNN方法-----------------------
rm(list = ls()) # 清空工作区
paks <- c("dplyr", "VIM")
lapply(paks, library, character.only = TRUE)

# 读取数据-------------------------------
data_final <- readRDS("workspace/projects/data/final_analysis_data.rds")
names(data_final)
# 创建 PA_total 的区间并生成频数分布表
# 将PA_total大于2000设为NA
data_final_2 <- data_final %>%
    mutate(
        PA_total = ifelse(PA_total > 1300, NA, PA_total)
    )
pa_breaks <- cut(data_final_2$PA_total, breaks = seq(0, 1300, by = 60), right = FALSE)
print("PA_total 频数分布表:")
table(pa_breaks, useNA = "ifany")
mean(data_final_2$PA_total, na.rm = TRUE)
summary(data_final_2$sleep_time)
summary(data_final_2$CARB_average)
summary(data_final_2$FAT_average)
summary(data_final_2$PROT_average)
unique(data_final_2$MCQ220)
unique(data_final_2$CVD)
table(data_final_2$CVD, useNA = "ifany")
table(data_final_2$MCQ220, useNA = "ifany")
# 剔除基线表CVD患病人群、剔除基线患癌症人群
data_final <- data_final_2 %>%
    filter(CVD != "1" | MCQ220 != 1) %>%
    as.data.frame()
nrow(data_final)-nrow(data_final_2) # 90
# 将睡眠时长在4-12小时范围外的设为NA
data_final <- data_final %>%
    mutate(
        sleep_time = ifelse(sleep_time < 4 | sleep_time > 12, NA, sleep_time)
    )
# 用用IQR法将CARB_average、FAT_average、PROT_average中的异常值设为NA
iqr_outlier_to_na <- function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    x[x < lower_bound | x > upper_bound] <- NA
    return(x)
}
data_final <- data_final %>%
    mutate(
        CARB_average = iqr_outlier_to_na(CARB_average),
        FAT_average = iqr_outlier_to_na(FAT_average),
        PROT_average = iqr_outlier_to_na(PROT_average)
    )
# 检查以上变量缺失量：PA_total、sleep_time、CARB_average、FAT_average、PROT_average
sapply(data_final[c("PA_total", "sleep_time", "CARB_average", "FAT_average", "PROT_average")], function(x) sum(is.na(x)))
View(data_final[c("PA_total", "sleep_time", "CARB_average", "FAT_average", "PROT_average")])
# 协变量列表
covariate_vars <- c(
        "Age", "Gender", "Racial", "Education", "Marriage", "Economic.status",
        "BMI", "Smoking", "Drinking", "HBP", "DM", "CVD", "DEI", "CARB_average", "FAT_average", "PROT_average", "PA_total", "sleep_time"
    )
# 检查数据缺失情况--------------------------------
sapply(data_final[covariate_vars], function(x) sum(is.na(x)))
# 插补数据
data_final <- kNN(data_final, variable = covariate_vars)

# 检查插补结果
sapply(data_final[covariate_vars], function(x) sum(is.na(x)))
# 保存插补后的数据
saveRDS(data_final, "workspace/projects/data/final_analysis_data.rds")