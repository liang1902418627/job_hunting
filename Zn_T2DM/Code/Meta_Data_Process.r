# ----------------------------------------------------------
# 处理元数据
# 暴露变量：血清锌
# 结局变量：T2DM、FPG、IFG or T2DM、IFG
# 协变量：年龄（age）、性别（gender）、BMI（anthrop_BMI）、婚姻状况（marital_status）、受教育程度（education）、家庭年收入（fam_income_year）、
#        吸烟（smk_smoking）、饮酒（diet_alc）、、家族糖尿病史（fam_hist_diabetes）、
#        膳食摄入Zn(Zn_intake)、日均能量摄入(energy_kcal)
# ------------------------------------------------------------------
# 清空环境变量
rm(list = ls())

# 载入必要的包
library(dplyr)

# 读取数据
md_v1 <- read.csv("Data/analysisdata.csv", fileEncoding = "gbk") # 元数据
# load("Data/gut_data.RData") # 读取肠道菌群数据,用来确定研究对象
metabolism <- read.csv("Data/element_cleaned.csv") # 读取代谢物数据，用于获取血清锌数据
energy <- read.csv("Data/Energy_intake.csv", na.strings = "#N/A") # 读取日均能量摄入数据
zn_intake <- read.delim("Data/n_2015 营养素数据.txt", fileEncoding = "GBK", sep = " ") # 读取膳食锌摄入数据 # nolint
f_a <- read.csv("Data/Final_analysis_data.csv", fileEncoding = "gbk") # 读取师姐给的final analysis数据
ids <- read.csv("Data/total7009.csv", fileEncoding = "gbk", row.names = 1) # 读取总的样本ID数据
h_o <- read.csv("Data/GGMP_HealthOutcome.csv", fileEncoding = "gbk", row.names = 1) # 读取健康结局数据
# View(metabolism)    # 代谢物数据
# View(energy)        # 日均能量摄入数据
# View(zn_intake)    # 膳食锌摄入数据
names(zn_intake)
names(h_o)
names(f_a)
names(energy)
names(zn_intake)
names(metabolism)
names(ids)
zn_intake$ivqid[1:5]
energy$id2[1:5]
names(md_v1)
names(metabolism)
summary(zn_intake$Mn)
# View(meta_data_v1) # 元数据

# 研究对象ID
# study_ids <- gut_data$alpha_diversity$SampleID

# 确定需要的变量----------------------------------------------------

exposure <- c("X66.Zn", "lnZn") # 血清锌变量
energy_kcal <- "energy_kcal" # 日均能量摄入变量
zn_variable <- "Zn" # 膳食锌摄入变量
# 匹配识别代码---------------------------------------------
md_v1 <- md_v1 %>%
    left_join(
        ids %>% select(SampleID, id),
        by = c("SampleID" = "SampleID")
    )
# View(meta_data_v1)
# 处理协变量数据------------------------------------------------------
# # 家庭月收入：
# <3000
# 3000-5000
# 5000-10000
# >=10000
# Declined to answer or unknown
# ---------------------------------------------------------------------
md_v2 <- md_v1 %>%
    mutate(family_income_m = ifelse(is.na(HB1M), HB1Y / 12, HB1M)) %>%
    select(-HB1M, -HB1Y) %>%
    mutate(
        family_income_cat = case_when(
            family_income_m < 3000 ~ "<3000",
            family_income_m >= 3000 & family_income_m < 5000 ~ "3000-5000",
            family_income_m >= 5000 & family_income_m < 10000 ~ "5000-10000",
            family_income_m >= 10000 ~ ">=10000",
            TRUE ~ "Declined to answer or unknown"
        ),
        family_income_cat = factor(family_income_cat,
            levels = c(
                "<3000",
                "3000-5000",
                "5000-10000",
                ">=10000",
                "Declined to answer or unknown"
            )
        )
    )
# summary(md_v2$family_income_m)
summary(md_v2$family_income_cat)
names(md_v2)
unique(md_v2$gender)
# 性别-----------------------------------------
md_v3 <- md_v2 %>%
    mutate(Gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")))
table(md_v3$Gender)

# 年龄------------------
md_v4 <- md_v3 %>%
    rename(., Age = age)
summary(md_v4$Age)

# BMI--------------------
summary(md_v4$BMI)

# Marital status---------------
table(md_v4$marry, useNA = "ifany")
md_v5 <- md_v4 %>%
    mutate(
        Marital_status = case_when(
            marry == 2 ~ "Married",
            TRUE ~ "Other"
        ),
        Marital_status = factor(Marital_status, levels = c("Married", "Other"))
    )
table(md_v5$Marital_status)

# Education----------------------------
# "Junior_high_school_or_below", "High_school", "Bachelor_degree_or_above"
# 1未接受正规学校教育
# 2小学未毕业
# 3小学毕业
# 4初中毕业
# 5高中/中专/技校
# 6大专毕业
# 7本科毕业
# 8研究生及以上
# ---------------------------------------------

table(md_v5$education, useNA = "ifany")
md_v6 <- md_v5 %>%
    mutate(
        Education = case_when(
            education <= 4 ~ "Junior_high_school_or_below",
            education == 5 ~ "High_school",
            education >= 6 ~ "Bachelor_degree_or_above",
            TRUE ~ NA_character_
        ),
        Education = factor(Education,
            levels = c(
                "Junior_high_school_or_below",
                "High_school",
                "Bachelor_degree_or_above"
            )
        )
    )
table(md_v6$Education, useNA = "ifany")

# Smoking status-------------------------------
# 1是的，每天吸
# 2是的，但不是每天吸
# 3以前吸，但现在不吸
# 4从不吸
# Nonsmokers
# Former smokers
# Current smokers
# ---------------------------------------------
table(md_v6$B1, useNA = "ifany")
md_v7 <- md_v6 %>%
    mutate(
        Smoking_status = case_when(
            B1 %in% c(1, 2) ~ "Current smokers",
            B1 == 3 ~ "Former smokers",
            B1 == 4 ~ "Nonsmokers",
            TRUE ~ NA_character_
        ),
        Smoking_status = factor(Smoking_status, levels = c("Nonsmokers", "Former smokers", "Current smokers"))
    )

table(md_v7$Smoking_status)

# Drinking status-------------------------------
# 1喝过，在30天内
# 2喝过，在过去30天以前
# 3没喝过
# Never drinking
# Drinking more than 30 days ago
# Drinking within the past 30 days
# C12
# ------------------------------------------
table(md_v7$C12, useNA = "ifany")
md_v8 <- md_v7 %>%
    mutate(
        Dringking_status = case_when(
            C12 == 3 ~ "Never drinking",
            C12 == 2 ~ "Drinking more than 30 days ago",
            C12 == 1 ~ "Drinking within the past 30 days",
            TRUE ~ NA_character_
        ),
        Dringking_status = factor(Dringking_status,
            levels = c(
                "Never drinking",
                "Drinking more than 30 days ago",
                "Drinking within the past 30 days"
            )
        )
    )

table(md_v8$Dringking_status)

# 是否进行高强度运动，用师姐给的final analysis数据-------------------------
md_v9 <- md_v8 %>%
    left_join(f_a %>% select(SampleID, act_sports_intense), by = c("SampleID")) %>%
    mutate(
        act_sports_intense = factor(
            act_sports_intense,
            levels = c("n", "y"),
            labels = c("No", "Yes")
        )
    )

table(md_v9$act_sports_intense)
# Family diabetes history------------------------------------
names(md_v9)
for (i in c("E8ga", "E8gb", "E8gc", "E8gd", "E8ge")) {
    if (!i %in% names(md_v9)) {
        cat(i, "不在数据集中", "\n")
    }
}

md_v10 <- md_v9 %>%
    mutate(
        Family_diabetes_history = case_when(
            E8ga == 1 | E8gb == 1 | E8gc == 1 | E8gd == 1 | E8ge == 1 ~ "Yes",
            TRUE ~ "No"
        )
    ) %>%
    mutate(
        Family_diabetes_history = factor(Family_diabetes_history, levels = c("No", "Yes"))
    )
table(md_v10$Family_diabetes_history)
# Energy intake----------------------------------------
# 取zn_intake中的energy2
md_v11 <- md_v10 %>%
    left_join(
        zn_intake %>% select(ivqid, energy2),
        by = c("ivqid" = "ivqid")
    ) %>%
    rename(Energy_intake = energy2)
summary(md_v11$Energy_intake)

# Zn intake---------------------------------------------------
md_v12 <- md_v11 %>%
    left_join(
        zn_intake %>% select(ivqid, Zn),
        by = c("ivqid" = "ivqid")
    ) %>%
    rename(Zn_intake = Zn)
summary(md_v12$Zn_intake)

# T2DM -------------------------------------------------------
md_v13 <- md_v12 %>%
    left_join(
        h_o %>% select(SampleID, diabetes),
        by = c("SampleID" = "SampleID")
    ) %>%
    mutate(
        T2DM = factor(diabetes,
            levels = c("no", "yes"),
            labels = c("No", "Yes")
        )
    )
table(md_v13$T2DM, useNA = "ifany")

# IFG------------------------------------------------------------
md_v14 <- md_v13 %>%
    mutate(
        IFG = case_when(
            T2DM == "Yes" ~ "T2DM",
            T2DM == "No" & !is.na(glu) & between(glu, 6.1, 7.0) ~ "Yes",
            T2DM == "No" ~ "No",
            TRUE ~ NA_character_
        ),
        IFG = factor(IFG, levels = c("No", "Yes", "T2DM"))
    )

table(md_v14$IFG, useNA = "ifany")

# IFG or T2DM------------------------------------------------------------
md_v15 <- md_v14 %>%
    mutate(
        IFG_or_T2DM = case_when(
            T2DM == "Yes" ~ "Yes",
            IFG == "Yes" ~ "Yes",
            IFG == "No" ~ "No",
            TRUE ~ NA_character_
        )
    ) %>%
    mutate(
        IFG_or_T2DM = factor(IFG_or_T2DM, levels = c("No", "Yes"))
    )

table(md_v15$IFG_or_T2DM, useNA = "ifany")

# FPG------------------------------------------------------------
md_v16 <- md_v15 %>%
    rename(FPG = glu)
summary(md_v16$FPG)

# 血清锌等变量----------------------------------------------------
md_v17 <- md_v16 %>%
    left_join(
        metabolism %>% select(
            SampleID, X66.Zn, lnZn, X51.V, X52.Cr, X55.Mn, X59.Co, X60.Ni,
            X63.Cu, X75.As, X82.Se, X95.Mo, X111.Cd, X111.Cd, X121.Sb, X205.Tl, X208.Pb
        ),
        by = c("SampleID" = "SampleID")
    ) %>%
    rename(
        Serum_Zn = X66.Zn,
        lnSerum_Zn = lnZn
    )

summary(md_v17$Serum_Zn)
summary(md_v17$lnSerum_Zn)
nrow(md_v17)

# 提取需要的变量--------------------------
names(md_v17)
vars <- c(
    "SampleID", "ivqid", "family_income_cat", "Gender", "Age",
    "BMI", "Marital_status", "Education", "Smoking_status", "Dringking_status",
    "act_sports_intense", "Family_diabetes_history", "Energy_intake",
    "Zn_intake", "T2DM", "IFG", "IFG_or_T2DM", "FPG", "Serum_Zn", "lnSerum_Zn",
    "X51.V", "X52.Cr", "X55.Mn", "X59.Co", "X60.Ni",
    "X63.Cu", "X75.As", "X82.Se", "X95.Mo", "X111.Cd", "X111.Cd", "X121.Sb", "X205.Tl", "X208.Pb"
)
md_v18 <- md_v17 %>%
    select(all_of(vars))
names(md_v18)
nrow(md_v18) # 7003人
# 纳入排除------------------------------
# 二型糖尿病缺失 64------------------------
sum(is.na(md_v18$T2DM))
md_v19 <- md_v18 %>%
    filter(!is.na(T2DM))

# 血清锌缺失 3957-----------------------
sum(is.na(md_v19$Serum_Zn))
md_v20 <- md_v19 %>%
    filter(!is.na(Serum_Zn))
summary(md_v20$Serum_Zn)

# 年龄，预设年龄范围18-90岁, 排除3人------------------------------------
# 1. 缺失比例
cat(sum(is.na(md_v20$Age)) / nrow(md_v20) * 100, "%缺失", "\n")
# 2. 异常值
sum(md_v20$Age < 18 | md_v20$Age > 90, na.rm = TRUE)

md_v21 <- md_v20 %>%
    filter(!is.na(Age)) %>%
    filter(between(Age, 18, 90))

# BMI(16-35), 排除32----------------------------------
# 1. 缺失比例
cat(sum(is.na(md_v21$BMI)) / nrow(md_v21) * 100, "%缺失", "\n")

# 2. 异常值
cat(sum(md_v21$BMI < 16 | md_v21$BMI > 35, na.rm = TRUE) / nrow(md_v21) * 100, "% 异常值", "\n")

md_v22 <- md_v21 %>%
    filter(!is.na(BMI)) %>%
    filter(between(BMI, 16, 35))

# Marital_status-------------------------------
cat(sum(is.na(md_v22$Marital_status)) / nrow(md_v22) * 100, "%缺失", "\n")

# Education-------------------------------
cat(sum(is.na(md_v22$Education)) / nrow(md_v22) * 100, "%缺失", "\n")

# family_income_cat-------------------------------
cat(sum(is.na(md_v22$family_income_cat)) / nrow(md_v22) * 100, "%缺失", "\n")

# Family_diabetes_history-------------------------------
cat(sum(is.na(md_v22$Family_diabetes_history)) / nrow(md_v22) * 100, "%缺失", "\n")

# Smoking_status-------------------------------
cat(sum(is.na(md_v22$Smoking_status)) / nrow(md_v22) * 100, "%缺失", "\n")

# Dringking_status-------------------------------
cat(sum(is.na(md_v22$Dringking_status)) / nrow(md_v22) * 100, "%缺失", "\n")

# act_sports_intense-------------------------------
cat(sum(is.na(md_v22$act_sports_intense)) / nrow(md_v22) * 100, "%缺失", "\n")

# Energy_intake,缺失率太高56%，考虑不作为协变量-------------------------------
cat(sum(is.na(md_v22$Energy_intake)) / nrow(md_v22) * 100, "%缺失", "\n")

# Zn_intake，缺失率56%，考虑不作为协变量-------------------------------
cat(sum(is.na(md_v22$Zn_intake)) / nrow(md_v22) * 100, "%缺失", "\n")

# IFG--------------------------------------
cat(sum(is.na(md_v22$IFG)) / nrow(md_v22) * 100, "%缺失", "\n")

# IFG_or_T2DM--------------------------------------
cat(sum(is.na(md_v22$IFG_or_T2DM)) / nrow(md_v22) * 100, "%缺失", "\n")

md_v22$ivqid <- format(md_v22$ivqid, scientific = FALSE)
str(md_v22$ivqid)

# 拿来分析的变量------------------------------------------------
vars_final <- c(
    "SampleID", "ivqid", "family_income_cat", "Gender", "Age",
    "BMI", "Marital_status", "Education", "Smoking_status", "Dringking_status", "Energy_intake", "Zn_intake",
    "act_sports_intense", "Family_diabetes_history", "T2DM", "IFG", "IFG_or_T2DM", "Serum_Zn", "lnSerum_Zn", "FPG",
    "X51.V", "X52.Cr", "X55.Mn", "X59.Co", "X60.Ni",
    "X63.Cu", "X75.As", "X82.Se", "X95.Mo", "X111.Cd", "X111.Cd", "X121.Sb", "X205.Tl", "X208.Pb"
)

# 根据肠道菌数据确定研究对象, 25人---------------------------------
gut_id <- readRDS("data/gut_data.rds")$alpha_diversity$SampleID
md_v23 <- md_v22 %>%
    filter(SampleID %in% gut_id) %>%
    select(all_of(vars_final))

nrow(md_v23) - nrow(md_v22)

# 缺失数据插补，随机森林多重插补--------------------------------
# 插补变量
impute <- unique(c(
    "family_income_cat", "Gender", "Age",
    "BMI", "Marital_status", "Education", "Smoking_status", "Dringking_status", "Energy_intake",
    "Zn_intake",
    "act_sports_intense", "Family_diabetes_history",
    "X51.V", "X52.Cr", "X55.Mn", "X59.Co", "X60.Ni",
    "X63.Cu", "X75.As", "X82.Se", "X95.Mo", "X111.Cd", "X121.Sb", "X205.Tl", "X208.Pb"
))

library(mice)
set.seed(123)

# 仅对需要插补的列进行插补
setdiff(impute, names(md_v23))
imp_data <- md_v23[, impute]

# 可选：预测矩阵与方法设置（全部使用随机森林）
pred <- make.predictorMatrix(imp_data)
diag(pred) <- 0 # 自己不预测自己
meth <- make.method(imp_data) # 默认方法
meth[impute] <- "rf"

# 运行多重插补（m=5），选择第1个完成数据集
imp <- mice(imp_data, m = 5, maxit = 50, method = meth, predictorMatrix = pred, seed = 500, printFlag = FALSE)
imp_once <- complete(imp, 1)

# 插补后的插补变量换掉md_v23中的对应变量
md_final <- md_v23
md_final[, impute] <- imp_once[, impute]

# 简单检查
sapply(md_final[, impute], function(x) sum(is.na(x)))

# 保存最终数据集
saveRDS(md_final, file = "Data/md_final.rds")
# write.csv(md_final, file = "Data/md_final.csv", row.names = FALSE, fileEncoding = "gbk")
# 整理暴露（血清锌）的各种类型--------------------------------
rm(list = ls())
setwd("C:/lianghao/lianghao/Zn_Gut_T2DM/")
library(dplyr)
md_final <- readRDS("Data/md_final.rds")
View(md_final)
names(md_final)
# 剔除血清锌在1000以下的个案, 6
sum( md_final$Serum_Zn < 1000)
md_final <- md_final %>%
  filter(Serum_Zn > 1000)
# Zn四分位数----------------------
# 1. 计算血清锌(Serum_Zn)的四个四分位点 (0%, 25%, 50%, 75%, 100%)
quartiles <- quantile(md_final$Serum_Zn, probs = seq(0, 1, 0.25), na.rm = TRUE)
print("四分位点数值：")
print(quartiles)
md_final_v2 <- md_final %>%
  mutate(
    Serum_Zn_Q4 = cut(Serum_Zn,
                      breaks = quartiles, # 使用计算出的四分位点作为断点
                      labels = c("Q1", "Q2", "Q3", "Q4"), # 设置组标签
                      include.lowest = TRUE, # 确保最小值被包含在第一个区间内
                      right = FALSE # 设置区间为左闭右开 [a, b)
    )
  )

# 3. 查看分组后的因子水平（即各组对应的区间范围）
print("各分组的区间定义：")
print(levels(md_final_v2$Serum_Zn_Q4))
table(md_final_v2$Serum_Zn_Q4)

# 四分位区间数值化--------------------------
md_final_v3 <- md_final_v2 %>%
  mutate(
    Serum_Zn_Q4_num = as.numeric(Serum_Zn_Q4)
  )
table(md_final_v3$Serum_Zn_Q4_num)
hist(md_final_v3$Serum_Zn)
# psd锌------------------------------
md_final_v4 <- md_final_v3 %>%
    mutate(
        psd_Serum_Zn = (Serum_Zn - mean(Serum_Zn, na.rm = TRUE)) / sd(Serum_Zn, na.rm = TRUE)
    )
# 保存数据
saveRDS(md_final_v4, file = "Data/md_final_v2.rds")
