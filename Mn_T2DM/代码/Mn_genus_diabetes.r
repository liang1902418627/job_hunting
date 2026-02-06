# ===========计算家庭经济，月收入=========
rm(list = ls())
# 加载包
library(tidyverse)

# 读取数据
meta_df <- as.data.frame(openxlsx::read.xlsx("数据/ggmp元数据.xlsx", sheet = 1))
colnames(meta_df)

# 提取家庭收入相关变量
income <- c("ivqid", "SampleID", "HB1", "HB1M", "HB1Y")

# 数据处理
meta_df <- meta_df %>%
    select(all_of(income)) %>%
    mutate(family_income_m = ifelse(is.na(HB1M), HB1Y / 12, HB1M)) %>%
    select(-HB1M, -HB1Y)

family_income_m <- meta_df
save(family_income_m, file = "数据/family_income_m.RData")

#------------日均能量摄入---------
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
energy_df <- as.data.frame(openxlsx::read.xlsx("数据/日均能量摄入.xlsx", sheet = 1))
# View(energy_df)
# colnames(energy_df)
# class(energy_df$energy_kcal)
# 缺失值
sum(is.na(energy_df$energy_kcal))

# 数据处理
energy_df <- energy_df %>%
    mutate(energy_kcal = ifelse(energy_kcal == 0, NA, energy_kcal))

# 保存数据
save(energy_df, file = "数据/energy_df.RData")
# ===========婚姻========================================
rm(list = ls())

# 加载r包
library(tidyverse)

# 读取数据
df <- openxlsx::read.xlsx("数据\\ggmp元数据.xlsx")

# 查看数据
colnames(df)

# 选择变量
names <- c("ivqid", "SampleID", "marry")

# 提取变量
sub_df <- df %>%
    select(all_of(names)) %>%
    mutate(marry_new = case_when(
        marry == 1 ~ "Never married",
        marry == 2 ~ "Married",
        TRUE ~ "Other"
    )) %>%
    mutate(marry_new = factor(marry_new, levels = c("Never married", "Married", "Other"))) %>%
    select(-marry) %>%
    rename("Marital_status" = "marry_new")

# 查看数据
head(sub_df)
marital_status <- sub_df
save(marital_status, file = "数据\\marital_status.RData")

#------------运动，饮酒，吸烟，性别，年龄，教育程度，BMI-----------
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
df <- as.data.frame(openxlsx::read.xlsx("数据/部分Mn协变量.xlsx", sheet = 1))
# colnames(df)

# 数据处理
df <- df %>%
    mutate(act_sport = factor(act_sports_intense, levels = c("n", "y"), labels = c("No", "Yes"))) %>%
    mutate(smoker = factor(as.character(smk_smoking), levels = c("1", "2", "3"), labels = c("Current_smokers", "Former_smokers", "Nonsmokers"))) %>%
    mutate(Alcohol_consumption = factor(as.character(diet_alc), levels = c("1", "2", "3"), labels = c("Drinking_within_the_past_30_days", "Drinking_more_than_30_days_ago", "Neverdrinking"))) %>%
    mutate(gender = factor(gender, levels = c("f", "m"), labels = c("Female", "Male"))) %>%
    mutate(education = factor(as.character(education), levels = c("1", "2", "3"), labels = c("Junior_high_school_or_below", "High_school", "Bachelor_degree_or_above"))) %>%
    rename("BMI" = "anthrop_BMI") %>%
    select(SampleID, age, gender, BMI, education, act_sport, smoker, Alcohol_consumption)

act_smo_alc <- df
save(act_smo_alc, file = "数据/act_smo_alc.RData")
#------------膳食锰摄入---------------------------------
rm(list = ls())

# 加载包
library(tidyverse)

# 载入数据
diet_Mn <- as.data.frame(openxlsx::read.xlsx("数据/ggmp_食物摄入锰.xlsx", sheet = 1))

colnames(diet_Mn)[3] <- "diet_Mn_mg"
colnames(diet_Mn)[1] <- "id"
diet_Mn <- diet_Mn[, c(1, 3)]
save(diet_Mn, file = "数据/diet_Mn.RData")
# ===========职业============================================
# 清除环境
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
df <- as.data.frame(openxlsx::read.xlsx("数据\\ggmp元数据.xlsx"))

# 查看数据
colnames(df)
class(df$occupation)
# 提取变量
names <- c("ivqid", "SampleID", "occupation")

# 提取数据
sub_df <- select(df, all_of(names)) %>%
    mutate(occupation_new = case_when(
        occupation %in% c(1, 2) ~ "Manual worker",
        occupation == 3 ~ "Business or service staff",
        occupation %in% c(4, 5) ~ "Government official and clerk",
        occupation == 6 ~ "Professionals",
        occupation == 10 ~ "Jobless",
        occupation == 12 ~ "Retiree",
        occupation %in% c(7:9, 11) ~ "Others"
    )) %>%
    select(-occupation) %>%
    rename("occupation" = "occupation_new")

occupation <- sub_df
print(occupation$occupation)

# 保存数据
save(occupation, file = "数据\\occupation.RData")
# ===========家族史============================================
# 清除环境
rm(list = ls())

# 加载r包
library(tidyverse)

# 读取数据
df <- as.data.frame(openxlsx::read.xlsx("数据\\ggmp元数据.xlsx"))

# 提取数据
names <- c("ivqid", "SampleID", "E8ga", "E8gb", "E8gc", "E8gd", "E8ge")

sub_df <- df %>%
    select(all_of(names)) %>%
    mutate(Family_history_of_diabetes_mellitus = ifelse(E8ga == 1 | E8gb == 1 | E8gc == 1 | E8gd == 1 | E8ge == 1, "Yes",
        "No"
    )) %>%
    mutate(Family_history_of_diabetes_mellitus = case_when(
        Family_history_of_diabetes_mellitus == "Yes" ~ Family_history_of_diabetes_mellitus,
        TRUE ~ "No"
    )) %>%
    select(-c(E8ga, E8gb, E8gc, E8gd, E8ge))

Family_history_of_diabetes_mellitus <- sub_df
save(Family_history_of_diabetes_mellitus, file = "数据\\Family_history_of_diabetes_mellitus.RData")
# ===========IFG 或 糖尿病=======================
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
df <- openxlsx::read.xlsx("数据\\ggmp元数据.xlsx")
colnames(df)

# 选取变量
sub_df <- df %>%
    select(ivqid, SampleID, glu) %>%
    mutate(IFG = case_when(
        glu >= 6.1 & glu <= 7.0 ~ "Yes",
        glu < 6.1 | glu > 7.0 ~ "No",
        TRUE ~ NA
    ))
colnames(sub_df)
# 读取糖尿病数据
load("数据\\ggmp_health_outcome.RData")
colnames(ggmp_health_outcome)
names <- c("SampleID", "diabetes")

# 合并数据
sub_df <- sub_df %>%
    left_join(ggmp_health_outcome[, names], by = "SampleID")

sub_df$`IFG_or_diabetes` <- ifelse(sub_df$IFG == "Yes" | sub_df$diabetes == "yes", "Yes",
    ifelse(sub_df$IFG == "No" & sub_df$diabetes == "no", "No", NA)
)

colnames(sub_df)
IFG_or_diabetes <- sub_df[, c("ivqid", "SampleID", "IFG_or_diabetes")]
save(IFG_or_diabetes, file = "数据\\IFG_or_diabetes.RData")
#------------暴露与结局------------------------------------
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
Serum_Mn_df <- read.csv("数据/Final_analysis_data.csv", row.names = 1)
colnames(Serum_Mn_df)

select <- c("SampleID", "X55.Mn", "lnMn")
Serum_Mn_df <- Serum_Mn_df[, select]

# 更改列名
colnames(Serum_Mn_df) <- c("SampleID", "Serum_Mn", "ln_Serum_Mn")

# 检查缺失值
sum(is.na(Serum_Mn_df$Serum_Mn))
sum(is.na(Serum_Mn_df$ln_Serum_Mn))

# 检查是否存在非有限值
sum(!is.finite(Serum_Mn_df$ln_Serum_Mn))

# 保存数据
save(Serum_Mn_df, file = "数据/Serum_Mn_df.RData")
# ===========空腹血糖和糖化血红蛋白====================
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
load("数据/ggmp_health_outcome.RData")

# 提取糖尿病相关健康结局数据
colnames(ggmp_health_outcome)

select <- c("SampleID", "FPG", "hba1c")

ggmp_health_outcome <- ggmp_health_outcome[, select]
FPG_hba1c <- ggmp_health_outcome

# 保存数据
save(FPG_hba1c, file = "数据/FPG_hba1c.RData")

# ============数据合并整理======================
rm(list = ls())

# 加载r包
library(tidyverse)

# 读取数据
load("数据\\act_smo_alc.RData") # 运动，饮酒，吸烟，性别，年龄，教育程度，BMI
load("数据\\marital_status.RData") # 婚姻
load("数据\\Family_history_of_diabetes_mellitus.RData") # 糖尿病家族史
load("数据\\occupation.RData") # 职业
load("数据\\family_income_m.RData") # 家庭月收入
load("数据\\energy_df.RData") # 日均能量摄入
load("数据\\diet_Mn.RData") # 膳食锰摄入
load("数据\\Serum_Mn_df.RData") # 血清锰
load("数据\\IFG_or_diabetes.RData") # 空腹血糖受损或糖尿病
load("数据\\FPG_hba1c.RData") # 空腹血糖，糖化血红蛋白
load("数据/alpha_gen.RData") # 属水平alpha多样性
genus <- read.csv("数据\\final_gm_genus.csv", header = T)
genus_Absolute <- read.csv("数据\\属水平绝对丰度数据.csv")
genus_Relative <- as.data.frame(openxlsx::read.xlsx("数据\\α、β多样性分析数据-相对丰度emo.xlsx"))
colnames(genus_Absolute)[c(2:57)] <- paste("Absolute", colnames(genus_Absolute)[c(2:57)], sep = "_")
colnames(genus_Relative)[c(2:ncol(genus_Relative))] <- paste("Relative", colnames(genus_Relative)[c(2:ncol(genus_Relative))], sep = "_")
# 查看数据
# colnames(act_smo_alc) # "SampleID"
# colnames(marital_status) # "ivqid","SampleID"
# colnames(Family_history_of_diabetes_mellitus) # "ivqid","SampleID" 
# colnames(occupation) # "ivqid","SampleID"
# colnames(family_income_m) # "ivqid","SampleID"
# colnames(energy_df) # "id","id2"
# colnames(diet_Mn) # "id"
# colnames(Serum_Mn_df) # "SampleID"
# colnames(IFG_or_diabetes) # "ivqid","SampleID"
# colnames(FPG_hba1c) # "SampleID"
# colnames(alpha_gen) # "SampleID"
# colnames(genus) # "SampleID"
# colnames(genus_Absolute) # "SampleID"
# colnames(genus_Relative) # "SampleID"
names <- c(
    "SampleID", "gender", "age", "BMI",
    "Marital_status", "education",
    "occupation", "HB1", "family_income_m",
    "Family_history_of_diabetes_mellitus",
    "smoker", "Alcohol_consumption", "act_sport",
    "energy_kcal", "diet_Mn_mg", "Serum_Mn", "ln_Serum_Mn",
    "IFG_or_diabetes", "FPG", "hba1c"
)

# 合并数据
df <- energy_df[, -2] %>%
    left_join(diet_Mn, by = "id") %>%
    right_join(marital_status, by = c("id" = "ivqid")) %>%
    right_join(act_smo_alc, by = "SampleID") %>%
    right_join(Family_history_of_diabetes_mellitus[, -1], by = "SampleID") %>%
    right_join(occupation[, -1], by = "SampleID") %>%
    right_join(family_income_m[, -1], by = "SampleID") %>%
    right_join(IFG_or_diabetes[, -1], by = "SampleID") %>%
    right_join(FPG_hba1c, by = "SampleID") %>%
    right_join(Serum_Mn_df, by = "SampleID") %>%
    select(all_of(names)) %>%
    left_join(alpha_gen, by = "SampleID") %>%
    left_join(genus[, c(1:57)], by = "SampleID") %>%
    left_join(genus_Absolute, by = "SampleID") %>%
    left_join(genus_Relative[, c(1, 91:146)], by = "SampleID")

print(colnames(df))

colnames(df)[10] <- "family_diabetes_history"
# ===============函数=========================
# Function to replace double underscores with single underscores in column names
replace_double_underscore <- function(df) {
    colnames(df) <- gsub("__", "_", colnames(df))
    return(df)
}
# Function to replace dots with empty strings in column names
replace_dots_in_colnames <- function(df) {
    colnames(df) <- gsub("\\.", "", colnames(df))
    return(df)
}

# Function to remove square brackets from column names
remove_brackets <- function(df) {
    colnames(df) <- gsub("\\[|\\]", "", colnames(df))
    return(df)
}

# =========================================
# Apply the function to our data frame
df <- replace_double_underscore(df)
df <- replace_dots_in_colnames(df)
df <- remove_brackets(df)
# 保存为dta文件用spss对数据进行整理，清洗
haven::write_dta(df, "数据\\清理前的数据.dta")
# ============计算alpha多样性指数================
rm(list = ls()) # 清除环境变量

# 加载r包
library(tidyverse) # 数据处理
library(vegan) # 计算多样性指数
# library(mice) # 多重插补
# 读取数据
load("数据\\imputed_df_v6.RData") # 读取之前保存的插补数据
load("数据\\genus_data.RData") # 读取肠道菌数据
# imputed_df_v6 <- imputed_df_v6[, -c(25:112)]

# 绝对丰度数据
genus_Absolute <- genus_data$absolute
# 计算alpha多样性指数
# Richness（物种丰富度，即菌属的数量）
richness <- specnumber(genus_Absolute)

# Shannon指数
shannon <- diversity(genus_Absolute, index = "shannon")

# Pielou’s evenness（均匀度）
# 需要先计算Shannon指数和物种丰富度
pielou <- shannon / log(richness)

# Simpson指数
simpson <- diversity(genus_Absolute, index = "simpson")

# 将结果整理成一个数据框
alpha_diversity <- data.frame(
    SampleID = rownames(genus_Absolute),
    Richness = richness,
    Shannon = shannon,
    Pielou = pielou,
    Simpson = simpson
)

# 将alpha多样性指数添加到imputed_df_v6中
imputed_df_v6 <- imputed_df_v6 %>%
    left_join(alpha_diversity, by = "SampleID")
View(imputed_df_v6)
colnames(imputed_df_v6)
save(imputed_df_v6, file = "数据\\imputed_df_v6.RData")
# ============合并糖尿病数据=============
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
load("数据\\ggmp_health_outcome.RData")
load("数据\\imputed_data.RData")

# 查看数据
colnames(ggmp_health_outcome)
colnames(imputed_data)

# 合并数据
df <- imputed_data[, c(1:21)] %>%
    left_join(ggmp_health_outcome[, c(1, 7)], by = "SampleID") %>%
    left_join(imputed_data[, c(1, 22:77)], by = "SampleID")

imputed_data <- df

save(imputed_data, file = "数据\\imputed_data.RData")
# =============根据FPG算的是否患糖尿病,diabetes2================
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
load("数据\\imputed_data.RData")

# 查看数据
colnames(imputed_data)
imputed_data2 <- imputed_data %>%
    mutate(diabetes2 = case_when(
        FPG >= 7.0 ~ "Yes",
        FPG < 7.0 ~ "No",
        TRUE ~ NA
    )) %>%
    mutate(diabetes2 = factor(diabetes2, levels = c("No", "Yes"))) %>%
    mutate(illness = case_when(
        diabetes2 == "Yes" ~ "diabetes",
        FPG >= 6.1 & FPG < 7.0 ~ "IFG",
        FPG < 6.1 ~ "normal",
        TRUE ~ NA
    )) %>%
    mutate(illness = factor(illness, levels = c("normal", "IFG", "diabetes"))) %>%
    mutate(
        ln_FPG = log(FPG),
        ln_hba1c = log(hba1c)
    )
save(imputed_data2, file = "数据\\imputed_data2.RData")
# ==========对imputed数据进行修改===============
rm(list = ls())

# 加载包
library(tidyverse)

# 加载数据
load("数据\\imputed_data.RData")
load("数据\\imputed_data2.RData")

# 查看数据
colnames(imputed_data)
colnames(imputed_data2)

# 合并数据
imputed_df <- imputed_data[, c(1:20)] %>%
    left_join(imputed_data2[, c(1, 81, 82)], by = "SampleID") %>%
    left_join(imputed_data2[, c(1, 80)], by = "SampleID") %>%
    left_join(imputed_data[, c(1, 21:78)], by = "SampleID") %>%
    mutate(Mn_quartiles = factor(Mn_quartiles, levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
    mutate(illness = case_when(
        diabetes == "yes" ~ "diabetes",
        TRUE ~ illness
    ))

# 保存数据
save(imputed_df, file = "数据\\imputed_df.RData")
write.csv(imputed_df, "数据\\imputed_df.csv", row.names = FALSE)
haven::write_dta(imputed_df, "数据\\imputed_df.dta")
# ========更改婚姻状况=========
rm(list = ls())
# 加载包
library(tidyverse)

# 读取数据
load("数据\\imputed_df.RData")

# 查看数据
colnames(imputed_df)

# 更改婚姻状况
df <- imputed_df %>%
    mutate(Marital_status = case_when(
        Marital_status == "Never married" ~ "Other",
        TRUE ~ Marital_status
    ))

imputed_df <- df

# 保存数据
save(imputed_df, file = "数据\\imputed_df.RData")
write.csv(imputed_df, "数据\\imputed_df.csv", row.names = FALSE)
haven::write_dta(imputed_df, "数据\\imputed_df.dta")
# ========去掉职业变量、将经济大于10000的合并为一类====================================== # nolint
rm(list = ls()) # 清空环境变量

# 加载自编函数及包
source("代码\\myfunctions.r", chdir = TRUE)

# 加载数据
load("数据\\imputed_df_v4.RData")
colnames(imputed_df_v4)

df <- imputed_df_v4
df_v1 <- df[, -which(colnames(df) == "occupation")]

# 查看经济变量
class(df_v1$family_income_cat)
df_v1$family_income_cat <- as.character(df_v1$family_income_cat)
levels(df_v1$family_income_cat)
table(df_v1$family_income_cat, useNA = "always")
df_v1$family_income_cat <- ifelse(df_v1$family_income_cat %in% c("10000-20000", ">=20000"),
    ">=10000", df_v1$family_income_cat
)
df_v1$family_income_cat <- factor(df_v1$family_income_cat,
    levels = c(
        "<3000", "3000-5000", "5000-10000",
        ">=10000", "Refuse to answer or don't know"
    )
)

imputed_df_v6 <- df_v1
save(imputed_df_v6, file = "数据\\imputed_df_v6.RData")
# ==========基线表====================
rm(list = ls())

# 加载包
library(dplyr) # tidyverse 包含 dplyr, tidyr 等数据处理包
library(summarytools) # summarytools 包用于生成数据摘要
library(gtsummary) # gtsummary 包用于生成表格

# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")
# 查看数据
sample_g_id <- rownames(genus_data$relative)
sample_m_id <- imputed_df_v6$SampleID
sample_id <- intersect(sample_g_id, sample_m_id)
imputed_df_v6 <- imputed_df_v6 %>%
    filter(SampleID %in% sample_id)
# save(imputed_df_v6, file = "数据\\imputed_df_v6.RData")

# 基线表
vars <- c(
    "gender", "age", "BMI", "Marital_status", "education",
    "family_income_cat", "smoker", "Alcohol_consumption", "act_sport",
    "family_diabetes_history",
    "energy_kcal", "diet_Mn_mg", "IFG_or_diabetes", "diabetes",
    "FPG", "hba1c", "Mn_quartiles", "Serum_Mn", "Richness", "Shannon", "Simpson", "Pielou"
)

# 创建基线表
table1 <- imputed_df_v6 %>%
    select(all_of(vars)) %>%
    tbl_summary(
        by = "Mn_quartiles", # 按血清锰四分位分组
        statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
        missing = "no", # 不显示缺失值
        percent = "column", # 计算列百分比
        digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2) # 设置小数位数
    ) %>%
    add_overall() %>% # 添加合计列
    modify_header(label = "**Variable**") %>%
    bold_labels()
print(table1)

table2 <- imputed_df_v6 %>%
    select(all_of(vars)) %>%
    tbl_summary(
        by = "Mn_quartiles", # 按血清锰四分位分组
        statistic = list(all_continuous() ~ "{median}[{p25},{p75}]", all_categorical() ~ "{n} ({p})"),
        missing = "no",
        percent = "column",
        digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2)
    ) %>%
    modify_header(label = "**Variable**") %>%
    bold_labels()
# print(table2)

# 保存表格
library(flextable) # flextable 包用于创建 Word 表格
library(officer) # officer 包用于操作 Word 文档

# 将表格转换为flextable对象
flextable1 <- as_flex_table(table1) # 将 gtsummary 表格转换为 flextable 对象

# 创建一个Word文档并添加表格
doc <- read_docx() %>% # 创建一个新的 Word 文档
    body_add_flextable(flextable1) # 将 flextable 添加到文档中

# 保存Word文档
print(doc, target = "主要结果/表格/基线表_均值.docx") # 保存文档

# 将表格转换为flextable对象
flextable2 <- as_flex_table(table2) # 将 gtsummary 表格转换为 flextable 对象

# 创建一个Word文档并添加表格
doc2 <- read_docx() %>% # 创建一个新的 Word 文档
    body_add_flextable(flextable2) # 将 flextable 添加到文档中

# 保存Word文档
print(doc2, target = "主要结果/表格/基线表_中位数.docx") # 保存数据

# 使用函数检查imputed_df_v6中的缺失值情况
colSums(is.na(imputed_df_v6))
# 交叉表
table(imputed_df_v6$IFG_or_diabetes, imputed_df_v6$Mn_quartiles)
table(imputed_df_v6$diabetes, imputed_df_v6$Mn_quartiles)

# 假设检验
# 趋势性卡方检验
library(vcd)
# 性别
tbl_gender <- table(imputed_df_v6$gender, imputed_df_v6$Mn_quartiles)
assocstats(tbl_gender)

# 婚姻
tbl_marry <- table(imputed_df_v6$gender, imputed_df_v6$Mn_quartiles)
assocstats(tbl_marry)

# 教育
tbl_edu <- table(imputed_df_v6$education, imputed_df_v6$Mn_quartiles)
assocstats(tbl_edu)
# 家庭收入
tbl_income <- table(imputed_df_v6$family_income_cat, imputed_df_v6$Mn_quartiles)
assocstats(tbl_income)

# 吸烟
tbl_smoker <- table(imputed_df_v6$smoker, imputed_df_v6$Mn_quartiles)
assocstats(tbl_smoker)

# 饮酒
tbl_alcohol <- table(imputed_df_v6$Alcohol_consumption, imputed_df_v6$Mn_quartiles)
assocstats(tbl_alcohol)

# 运动
tbl_sport <- table(imputed_df_v6$act_sport, imputed_df_v6$Mn_quartiles)
assocstats(tbl_sport)

# 家族史
tbl_family <- table(imputed_df_v6$family_diabetes_history, imputed_df_v6$Mn_quartiles)
assocstats(tbl_family)

# 空腹血糖受损或糖尿病
tbl_diabetes <- table(imputed_df_v6$IFG_or_diabetes, imputed_df_v6$Mn_quartiles)
assocstats(tbl_diabetes)

# 糖尿病
tbl_diabetes <- table(imputed_df_v6$diabetes, imputed_df_v6$Mn_quartiles)
assocstats(tbl_diabetes)
# 方差分析
age <- aov(age ~ Mn_quartiles, data = imputed_df_v6)
summary(age)

BMI <- aov(BMI ~ Mn_quartiles, data = imputed_df_v6)
summary(BMI)

energy <- aov(energy_kcal ~ Mn_quartiles, data = imputed_df_v6)
summary(energy)

manganese_intake <- aov(diet_Mn_mg ~ Mn_quartiles, data = imputed_df_v6)
summary(manganese_intake)

FPG <- aov(FPG ~ Mn_quartiles, data = imputed_df_v6)
summary(FPG)

Hba1c <- aov(hba1c ~ Mn_quartiles, data = imputed_df_v6)
summary(Hba1c)

serum_Mn <- aov(Serum_Mn ~ Mn_quartiles, data = imputed_df_v6)
summary(serum_Mn)

Richness <- aov(Richness ~ Mn_quartiles, data = imputed_df_v6)
summary(Richness)

Shannon <- aov(Shannon ~ Mn_quartiles, data = imputed_df_v6)
summary(Shannon)

Simpson <- aov(Simpson ~ Mn_quartiles, data = imputed_df_v6)
summary(Simpson)

Pielou <- aov(Pielou ~ Mn_quartiles, data = imputed_df_v6)
summary(Pielou)
# =======血清锰与糖尿病、空腹血糖的关联===============
# 1. 线性关联
rm(list = ls())

# 加载包
library(tidyverse)

# 自编函数
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData")
colnames(imputed_df_v6)
table(imputed_df_v6$IFG)
imputed_df_v4 <- imputed_df_v6
colnames(imputed_df_v4)
imputed_df_v4 <- imputed_df_v4 %>%
    mutate(Mn_Q_con = as.numeric(Mn_quartiles)) %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2"))

imputed_df_v4$sd_ln_Mn <- imputed_df_v4$ln_Serum_Mn /
    sd(imputed_df_v4$ln_Serum_Mn, na.rm = TRUE) # per_sd
imputed_df_v4$sd_ln_FPG <- imputed_df_v4$ln_FPG /
    sd(imputed_df_v4$ln_FPG, na.rm = TRUE) # per_sd
data <- imputed_df_v4
# ========函数==========================================
# 1. 数据框
df <- function(Model, Q2 = "", Q1 = "", Q3 = "", Q4 = "", `p for trend` = "") {
    data.frame(
        Model = Model,
        Q2 = Q2,
        Q1 = Q1,
        Q3 = Q3,
        Q4 = Q4,
        `p for trend` = `p for trend`
    )
}

# 2. 计算置信区间下限的函数
low_ci_log <- function(x, sd) {
    exp(x - 1.96 * sd)
}

# 3. 计算置信区间上限的函数
up_ci_log <- function(x, sd) {
    exp(x + 1.96 * sd)
}

# 2. 计算置信区间下限的函数
low_ci_linear <- function(x, sd) {
    x - 1.96 * sd
}

# 3. 计算置信区间上限的函数
up_ci_linear <- function(x, sd) {
    x + 1.96 * sd
}

# 4. 格式化为2位小数字符串的函数
format_2 <- function(x) {
    sprintf("%.2f", x)
}

# 5. 提取需要的值的函数
value_for_reg_log <- function(Estimate, sd) {
    low_ci <- low_ci_log(Estimate, sd)
    up_ci <- up_ci_log(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(exp(Estimate))
    Q <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    return(Q)
}

value_for_reg_linear <- function(Estimate, sd) {
    low_ci <- low_ci_linear(Estimate, sd)
    up_ci <- up_ci_linear(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(Estimate)
    Q <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    return(Q)
}

# 7. p for trend
p_for_trend1 <- function(data, formula) {
    # formula: 其中的自变量为四分位因子型变量转换回1,2,3,4数值型

    # 拟合模型
    model <- glm(formula, data = data, family = "binomial")

    # 提取p值
    p_value <- summary(model)$coefficients[2, 4]

    return(p_value)
}

# 7. p for trend
p_for_trend2 <- function(data, formula) {
    # formula: 其中的自变量为四分位因子型变量转换回1,2,3,4数值型

    # 拟合模型
    model <- glm(formula, data = data)

    # 提取p值
    p_value <- summary(model)$coefficients[2, 4]

    return(p_value)
}

# 8. logistic回归
logistic_regression <- function(data, formula1, formula2, model_name) {
    # formula1: 自变量为四分位因子型变量
    # formula2: 自变量为四分位因子型变量转换回1,2,3,4数值型
    # model_name: 为模型命名
    # 拟合模型
    fitted_model <- glm(formula1, data = data, family = "binomial")

    # 提取系数
    result <- as.data.frame(summary(fitted_model)$coefficients)

    # 提取需要的值
    Estimate2 <- result$Estimate[2]
    Estimate3 <- result$Estimate[3]
    Estimate4 <- result$Estimate[4]

    sd2 <- result$`Std. Error`[2]
    sd3 <- result$`Std. Error`[3]
    sd4 <- result$`Std. Error`[4]

    Q2 <- "(reference)"
    Q1 <- value_for_reg_log(Estimate2, sd2)
    Q3 <- value_for_reg_log(Estimate3, sd3)
    Q4 <- value_for_reg_log(Estimate4, sd4)

    # 计算p for trend
    p_for_trend <- p_for_trend1(data, formula = formula2)

    return(df(
        Model = model_name,
        Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4,
        `p for trend` = p_for_trend
    ))
}

# 9. 线性回归
linear_regression <- function(data, formula1, formula2, model_name) {
    # formula1: 自变量为四分位因子型变量
    # formula2: 自变量为四分位因子型变量转换回1,2,3,4数值型
    # model_name: 为模型命名
    # 拟合模型
    fitted_model <- lm(formula1, data = data)

    # 提取系数
    result <- as.data.frame(summary(fitted_model)$coefficients)

    # 提取需要的值
    Estimate2 <- result$Estimate[2]
    Estimate3 <- result$Estimate[3]
    Estimate4 <- result$Estimate[4]

    sd2 <- result$`Std. Error`[2]
    sd3 <- result$`Std. Error`[3]
    sd4 <- result$`Std. Error`[4]

    Q2 <- "(reference)"
    Q1 <- value_for_reg_linear(Estimate2, sd2)
    Q3 <- value_for_reg_linear(Estimate3, sd3)
    Q4 <- value_for_reg_linear(Estimate4, sd4)

    # 计算p for trend
    p_for_trend <- p_for_trend2(data, formula2)

    return(df(
        Model = model_name,
        Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4,
        `p for trend` = p_for_trend
    ))
}
# ================================================
# 准备
# 1. 协变量（性别、年龄）
covariate1 <- c("gender", "age")

# 2. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate2 <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "sd_ln_FPG")

# 4. 自变量1(因子型Mn)
exposure1 <- "Mn_quartiles"

# 5. 自变量2(数值型Mn四分位)
exposure2 <- "Mn_Q_con"

# 6. 模型名称
model <- c("model1", "model2", "model3")

result_df <- data.frame()
# 回归循环
for (j in outcome) {
    result_df <- rbind(result_df, df(Model = j))
    for (i in model) {
        if (i == "model1") {
            formula1 <- create_formula(j, exposure1)
            formula2 <- create_formula(j, exposure2)
        } else if (i == "model2") {
            formula1 <- create_formula(j, exposure1, covariate1)
            formula2 <- create_formula(j, exposure2, covariate1)
        } else if (i == "model3") {
            formula1 <- create_formula(j, exposure1, covariate2)
            formula2 <- create_formula(j, exposure2, covariate2)
        }

        if (class(data[[j]]) == "factor") {
            result <- logistic_regression(data, formula1, formula2, i)
        } else {
            result <- linear_regression(data, formula1, formula2, i)
        }

        result_df <- rbind(result_df, result)
    }
}

View(result_df)
write.csv(result_df, "主要结果\\表格\\血清锰_四分位数_糖尿病相关_回归分析.csv", row.names = FALSE)
# 当血清锰为连续性变量
rm(list = ls())

# 加载r包
library(tidyverse)

# 读取数据
load("数据\\imputed_df_v6.RData")
imputed_df_v4 <- imputed_df_v6
# 查看数据
colnames(imputed_df_v4)
imputed_df_v4$sd_ln_Mn <- imputed_df_v4$ln_Serum_Mn /
    sd(imputed_df_v4$ln_Serum_Mn, na.rm = TRUE)
imputed_df_v4$sd_ln_FPG <- imputed_df_v4$ln_FPG /
    sd(imputed_df_v4$ln_FPG, na.rm = TRUE)
data <- imputed_df_v4

# 创建储存结果的数据框（新增p值列）
reg_df <- data.frame(
    Dependent_variable = character(),
    Model_1 = character(), Model_1_p = numeric(),
    Model_2 = character(), Model_2_p = numeric(),
    Model_3 = character(), Model_3_p = numeric(),
    stringsAsFactors = FALSE
)

# ============函数=======================
# 1. 数据框行构造（包含p值）
df <- function(Dependent_variable, Model_1, Model_1_p, Model_2, Model_2_p, Model_3, Model_3_p) {
    return(
        data.frame(
            Dependent_variable = Dependent_variable,
            Model_1 = Model_1, Model_1_p = Model_1_p,
            Model_2 = Model_2, Model_2_p = Model_2_p,
            Model_3 = Model_3, Model_3_p = Model_3_p,
            stringsAsFactors = FALSE
        )
    )
}

# 2. 计算置信区间下限的函数
low_ci <- function(x, sd) {
    x - 1.96 * sd
}

# 3. 计算置信区间上限的函数
up_ci <- function(x, sd) {
    x + 1.96 * sd
}

# 4. 格式化为2位小数字符串的函数
format_2 <- function(x) {
    sprintf("%.2f", x)
}

# 5. 显著性标志
significance <- function(p) {
    if (is.na(p)) {
        return("")
    } else if (p < 0.01) {
        return("**")
    } else if (p < 0.05) {
        return("*")
    } else {
        return("")
    }
}

# 6. 创建公式
create_formula <- function(outcome, exposure, covariates = vector(length = 0)) {
    if (!is.character(outcome) || length(outcome) != 1) {
        stop("因变量(outcome)必须是长度为1的字符串")
    }
    if (!is.character(exposure) || length(exposure) != 1) {
        stop("自变量(exposure)必须是长度为1的字符串")
    }
    if (!is.character(covariates) && length(covariates) > 0) {
        stop("协变量(covariates)必须是字符串向量")
    }
    if (length(covariates) > 0) {
        formula_str <- paste(outcome, "~", exposure, "+", paste(covariates, collapse = " + "))
    } else {
        formula_str <- paste(outcome, "~", exposure)
    }
    return(as.formula(formula_str))
}

# 7. 提取需要的值的函数（返回格式化字符串；p值由回归函数返回）
value_for_reg <- function(Estimate, sd, p) {
    low_ci <- low_ci(Estimate, sd)
    up_ci <- up_ci(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(Estimate)
    Q_left <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    p_sig <- significance(p)
    Q <- paste(Q_left, p_sig, sep = "")
    return(Q)
}

# 8. logistic回归（返回列表：value与p）
logistic_regression <- function(data, formula) {
    fitted_model <- glm(formula, data = data, family = "binomial")
    result <- as.data.frame(summary(fitted_model)$coefficients)
    Estimate <- result$Estimate[2]
    sd <- result$`Std. Error`[2]
    p <- result$`Pr(>|z|)`[2]
    value <- value_for_reg(Estimate, sd, p)
    return(list(value = value, p = as.numeric(p)))
}

# 9. 线性回归（返回列表：value与p）
linear_regression <- function(data, formula) {
    fitted_model <- lm(formula, data = data)
    result <- as.data.frame(summary(fitted_model)$coefficients)
    Estimate <- result$Estimate[2]
    sd <- result$`Std. Error`[2]
    p <- result$`Pr(>|t|)`[2]
    value <- value_for_reg(Estimate, sd, p)
    return(list(value = value, p = as.numeric(p)))
}

# =========================================
# 准备
covariate1 <- c("gender", "age")
covariate2 <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)
outcome <- c("diabetes", "IFG_or_diabetes", "sd_ln_FPG")
exposure <- "sd_ln_Mn"
model <- c("model1", "model2", "model3")

# 回归循环（收集格式化结果与p值）
for (j in outcome) {
    model1_val <- model2_val <- model3_val <- ""
    model1_p <- model2_p <- model3_p <- NA_real_

    for (i in model) {
        if (i == "model1") {
            formula <- create_formula(outcome = j, exposure = exposure)
        } else if (i == "model2") {
            formula <- create_formula(j, exposure, covariate1)
        } else if (i == "model3") {
            formula <- create_formula(j, exposure, covariate2)
        }

        if (is.factor(data[[j]])) {
            res <- logistic_regression(data, formula)
        } else {
            res <- linear_regression(data, formula)
        }

        if (i == "model1") {
            model1_val <- res$value
            model1_p <- res$p
        } else if (i == "model2") {
            model2_val <- res$value
            model2_p <- res$p
        } else if (i == "model3") {
            model3_val <- res$value
            model3_p <- res$p
        }
    }

    reg_df <- rbind(
        reg_df,
        df(j, model1_val, model1_p, model2_val, model2_p, model3_val, model3_p)
    )
}

# 整理并保存
# View(reg_df)
colnames(reg_df)[1:7] <- c("Dependent variable", "Model 1", "Model 1 p", "Model 2", "Model 2 p", "Model 3", "Model 3 p")
write.csv(reg_df, "主要结果\\表格\\血清锰_连续_糖尿病相关_回归分析.csv", row.names = FALSE)
# =======血清锰与糖尿病、空腹血糖的关联，亚组分析===============
rm(list = ls()) # 清空工作空间

# 读取自编函数
source("代码\\myfunctions.r")
# 读取r包
# install.packages("jstable")
# install.packages("forestploter")
library(jstable) # jstable 包用于生成表格
library(forestploter) # forestploter 包用于绘制森林图
# 读取数据
load("数据\\imputed_df_v6.RData") # 读取数据

data <- imputed_df_v6 # 赋值数据

rm(imputed_df_v6) # 删除原数据，释放内存

# 处理数据
data <- data %>%
    mutate(Mn_Q_con = as.numeric(Mn_quartiles)) %>% # 锰四分位数设置为数值型
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2")) # 将锰四分位数的参考组设置为Q2

data$sd_ln_Mn <- data$ln_Serum_Mn /
    sd(data$ln_Serum_Mn, na.rm = TRUE) # per_sd
data$sd_ln_FPG <- data$ln_FPG /
    sd(data$ln_FPG, na.rm = TRUE) # per_sd

# 准备
# 1. 协变量（性别、年龄）
covariate1 <- c("gender", "age")

# 2. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate2 <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "sd_ln_FPG")

# 4. 自变量1(因子型Mn)
exposure1 <- "Mn_quartiles"

# 5. 自变量2(数值型Mn四分位)
exposure2 <- "Mn_Q_con"

# =========糖尿病、Q4、校正2=========================================================
# 进行亚组分析
res3 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(outcome[1], exposure1),
    # 指定哪些变量有亚组(因子型变量)
    var_subgroups = c(
        "gender", "Marital_status", "education",
        "family_income_cat",
        "family_diabetes_history", "smoker",
        "Alcohol_consumption", "act_sport"
    ),
    # 指定数据集
    data = data,

    # 指定协变量
    var_cov = covariate2
)
class(res3) # 查看数据类型
View(res3) # 查看结果
res3[, c(1:ncol(res3))][is.na(res3[, c(1:ncol(res3))])] <- "" # 将NA替换为空格
res3 <- as.data.frame(res3) # 转换为数据框

View(res3)
# 画森林图
plot_df <- res3
# 添加个空列用于显示可信区间
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")

# 把P值，可信区间这些列变为数值型
plot_df[, 5:9] <- apply(plot_df[, 5:9], 2, as.numeric) # 将数据转换为数值型
plot_df$`P value` <- ifelse(is.na(plot_df$`P value`), "", plot_df$`P value`) # 将NA替换为空格
# 画图
# pak::pkg_install("forestploter")
library(forestploter) # forestploter 包用于绘制森林图
library(grid) # grid 包用于绘图

View(plot_df)
p <- forest(
    data = plot_df[, c(1:4, 8, 10)], # 选择需要的列
    lower = plot_df$Lower, # 可信区间下限
    upper = plot_df$Upper, # 可信区间上限
    est = plot_df$`OR`, # 点估计
    ci_column = 6, # 置信区间列
    ref_line = 1, # 参考线
    xlim = c(0.5, 8.5) # x轴范围
)
ggsave("其他结果\\图片\\糖尿病-Q4-亚组分析-校正2.pdf", p, width = 18, height = 28, dpi = 300)
write.csv(res3, "主要结果\\表格\\糖尿病-Q4-亚组分析-校正2.csv", row.names = FALSE)
# =========IFG_or_diabetes、Q4、校正2=========================================================
# 进行亚组分析
res6 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(outcome[2], exposure1),
    # 指定哪些变量有亚组
    var_subgroups = c(
        "gender", "Marital_status", "education",
        "family_income_cat",
        "family_diabetes_history", "smoker",
        "Alcohol_consumption", "act_sport"
    ),
    # 指定数据集
    data = data,

    # 指定协变量
    var_cov = covariate2
)
class(res6) # 查看数据类型
res6[, c(1:ncol(res6))][is.na(res5[, c(1:ncol(res6))])] <- "" # 将NA替换为空格
res6 <- as.data.frame(res6) # 转换为数据框
write.csv(res6, "主要结果\\表格\\IFG_or_diabetes-Q4-亚组分析-校正2.csv", row.names = FALSE)
# View(res5)
# 画森林图
plot_df <- res6
# 添加个空列用于显示可信区间
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
# 把P值，可信区间这些列变为数值型
plot_df[, 5:9] <- apply(plot_df[, 5:9], 2, as.numeric) # 将数据转换为数值型
plot_df$`P value` <- ifelse(is.na(plot_df$`P value`), "", plot_df$`P value`) # 将NA替换为空格
# 画图
# library(forestploter)
library(grid)

p <- forest(
    data = plot_df[, c(1:4, 8, 10)],
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    est = plot_df$`OR`,
    ci_column = 6,
    ref_line = 1,
    xlim = c(0.5, 8.5)
)
ggsave("其他结果\\图片\\IFG糖尿病-Q4-亚组分析-校正2.pdf", p, width = 18, height = 28, dpi = 300)

# =========sd_ln_FPG、Q4、校正2=========================================================
# 进行亚组分析
res7 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(outcome[3], exposure1),
    # 指定哪些变量有亚组
    var_subgroups = c(
        "gender", "Marital_status", "education",
        "family_income_cat",
        "family_diabetes_history", "smoker",
        "Alcohol_consumption", "act_sport"
    ),
    # 指定数据集
    data = data,
    # 指定回归类型
    family = "gaussian",

    # 指定协变量
    var_cov = covariate2
)
class(res7) # 查看数据类型
res7[, c(1:ncol(res7))][is.na(res7[, c(1:ncol(res7))])] <- "" # 将NA替换为空格
res7 <- as.data.frame(res7) # 转换为数据框
write.csv(res7, "主要结果\\表格\\sd_ln_FPG-Q4-亚组分析-校正2.csv", row.names = FALSE)
# View(res7)
# 画森林图
plot_df <- res7
# 添加个空列用于显示可信区间
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
# 把P值，可信区间这些列变为数值型
plot_df[, 5:9] <- apply(plot_df[, 5:9], 2, as.numeric) # 将数据转换为数值型
plot_df$`P value` <- ifelse(is.na(plot_df$`P value`), "", plot_df$`P value`) # 将NA替换为空格
# 画图
# library(forestploter)
library(grid)

p <- forest(
    data = plot_df[, c(1:4, 8, 10)],
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    est = plot_df$`Point.Estimate`,
    ci_column = 6,
    ref_line = 0,
    xlim = c(-0.7, 0.9)
)
ggsave("其他结果\\图片\\sd_ln_FPG-Q4-亚组分析-校正2.pdf", p, width = 17, height = 27, dpi = 300)
# =========糖尿病、sd_ln_Mn、校正2=========================================================
# 进行亚组分析
res8 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(outcome[1], "sd_ln_Mn"),
    # 指定哪些变量有亚组
    var_subgroups = c(
        "gender", "Marital_status", "education",
        "family_income_cat",
        "family_diabetes_history", "smoker",
        "Alcohol_consumption", "act_sport"
    ),
    # 指定数据集
    data = data,
    # 指定回归类型
    family = "binomial",

    # 指定协变量
    var_cov = covariate2
)
class(res8) # 查看数据类型
res8[, c(1:ncol(res8))][is.na(res8[, c(1:ncol(res8))])] <- "" # 将NA替换为空格
res8 <- as.data.frame(res8) # 转换为数据框
write.csv(res8, "主要结果\\表格\\糖尿病-sd_ln_Mn-亚组分析-校正2.csv", row.names = FALSE)

# View(res8)
# 画森林图
plot_df <- res8
# 添加个空列用于显示可信区间
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
# 把P值，可信区间这些列变为数值型
plot_df[, c("OR", "Lower", "Upper")] <- apply(plot_df[, c("OR", "Lower", "Upper")], 2, as.numeric) # 将数据转换为数值型
# plot_df$`P value` <- ifelse(is.na(plot_df$`P value`), "", plot_df$`P value`) # 将NA替换为空格
# 画图
# library(forestploter)
library(grid)

p <- forest(
    data = plot_df[, c(1:3, 7, 9)],
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    est = plot_df$`OR`,
    ci_column = 5,
    ref_line = 1,
    xlim = c(0.5, 5)
)
ggsave("其他结果\\图片\\糖尿病-sd_ln_Mn-亚组分析-校正2.pdf", p, width = 9, height = 10, dpi = 300)
# ========IFG_or_diabetes、sd_ln_Mn、校正2=========================================================
# 进行亚组分析
res9 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(outcome[2], "sd_ln_Mn"),
    # 指定哪些变量有亚组
    var_subgroups = c(
        "gender", "Marital_status", "education",
        "family_income_cat",
        "family_diabetes_history", "smoker",
        "Alcohol_consumption", "act_sport"
    ),
    # 指定数据集
    data = data,
    # 指定回归类型
    family = "binomial",

    # 指定协变量
    var_cov = covariate2
)

# class(res9) # 查看数据类型
res9[, c(1:ncol(res9))][is.na(res9[, c(1:ncol(res9))])] <- "" # 将NA替换为空格
res9 <- as.data.frame(res9) # 转换为数据框
write.csv(res9, "主要结果\\表格\\IFG_or_diabetes-sd_ln_Mn-亚组分析-校正2.csv", row.names = FALSE)

# View(res9)
# 画森林图
plot_df <- res9
# 添加个空列用于显示可信区间
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
# 把P值，可信区间这些列变为数值型
plot_df[, c("OR", "Lower", "Upper")] <- apply(plot_df[, c("OR", "Lower", "Upper")], 2, as.numeric) # 将数据转换为数值型
plot_df$`P value` <- ifelse(is.na(plot_df$`P value`), "", plot_df$`P value`) # 将NA替换为空格

# 画图
# library(forestploter)
library(grid)

p <- forest(
    data = plot_df[, c(1:3, 7, 9)],
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    est = plot_df$`OR`,
    ci_column = 5,
    ref_line = 1,
    xlim = c(0.5, 3)
)
ggsave("其他结果\\图片\\IFG糖尿病-sd_ln_Mn-亚组分析-校正2.pdf", p, width = 9, height = 10, dpi = 300)
# ========sd_ln_FPG、sd_ln_Mn、校正2=========================================================
# 进行亚组分析
res10 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(outcome[3], "sd_ln_Mn"),
    # 指定哪些变量有亚组
    var_subgroups = c(
        "gender", "Marital_status", "education",
        "family_income_cat",
        "family_diabetes_history", "smoker",
        "Alcohol_consumption", "act_sport"
    ),
    # 指定数据集
    data = data,
    # 指定回归类型
    family = "gaussian",

    # 指定协变量
    var_cov = covariate2
)

class(res10) # 查看数据类型
res10[, c(1:ncol(res10))][is.na(res10[, c(1:ncol(res10))])] <- "" # 将NA替换为空格
res10 <- as.data.frame(res10) # 转换为数据框

write.csv(res10, "主要结果\\表格\\sd_ln_FPG-sd_ln_Mn-亚组分析-校正2.csv", row.names = FALSE)
# View(res10)

# 画森林图
plot_df <- res10
# 添加个空列用于显示可信区间
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
# 把P值，可信区间这些列变为数值型
plot_df[, c("Point.Estimate", "Lower", "Upper")] <- apply(plot_df[, c("Point.Estimate", "Lower", "Upper")], 2, as.numeric) # 将数据转换为数值型
# 将NA替换为空格
plot_df$`P value` <- ifelse(is.na(plot_df$`P value`), "", plot_df$`P value`) # 将NA替换为空格

# 画图
# library(forestploter)
library(grid)

p <- forest(
    data = plot_df[, c(1:3, 7, 9)],
    lower = plot_df$Lower,
    upper = plot_df$Upper,
    est = plot_df$`Point.Estimate`,
    ci_column = 5,
    ref_line = 0,
    xlim = c(-0.3, 0.2)
)
ggsave("其他结果\\图片\\sd_ln_FPG-sd_ln_Mn-亚组分析-校正2.pdf", p, width = 9, height = 10, dpi = 300)
# 逐一放入协变量（血清锰为四分位数）=====================
rm(list = ls())

# 加载包
library(tidyverse)

# 加载自编函数
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df.RData")

# 查看数据
colnames(imputed_df)

# ========函数==============================
# 1. 数据框
df <- function(x) {
    data.frame(
        Model = x,
        Q1 = "",
        Q2 = "",
        Q3 = "",
        Q4 = ""
    )
}

# 2. 提取需要的值的函数
value_for_reg <- function(Estimate, sd, p) {
    low_ci <- low_ci(Estimate, sd)
    up_ci <- up_ci(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(Estimate)
    Q_left <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    p <- significance(p)
    Q <- paste(Q_left, p, sep = "")
    return(Q)
}

# 3. logistic回归函数
logistic_regression <- function(data, outcome, exposure, covariate) {
    # 拟合模型
    model <- glm(data[[outcome]] ~ data[[exposure]] + data[[covariate]], data = data, family = "binomial")

    # 提取系数
    result <- as.data.frame(summary(model)$coefficients)

    # 提取需要的值
    Estimate2 <- result$Estimate[2]
    Estimate3 <- result$Estimate[3]
    Estimate4 <- result$Estimate[4]

    sd2 <- result$`Std. Error`[2]
    sd3 <- result$`Std. Error`[3]
    sd4 <- result$`Std. Error`[4]

    p2 <- result$`Pr(>|z|)`[2]
    p3 <- result$`Pr(>|z|)`[3]
    p4 <- result$`Pr(>|t|)`[4]

    Q1 <- "(reference)"
    Q2 <- value_for_reg(Estimate2, sd2, p2)
    Q3 <- value_for_reg(Estimate3, sd3, p3)
    Q4 <- value_for_reg(Estimate4, sd4, p4)

    return(data.frame(Model = covariate, Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4))
}

# 4. 线性回归函数
linear_regression <- function(data, outcome, exposure, covariate) {
    # 拟合模型
    model <- lm(data[[outcome]] ~ data[[exposure]] + data[[covariate]], data = data)

    # 提取系数
    result <- as.data.frame(summary(model)$coefficients)

    # 提取需要的值
    Estimate2 <- result$Estimate[2]
    Estimate3 <- result$Estimate[3]
    Estimate4 <- result$Estimate[4]

    sd2 <- result$`Std. Error`[2]
    sd3 <- result$`Std. Error`[3]
    sd4 <- result$`Std. Error`[4]

    p2 <- result$`Pr(>|t|)`[2]
    p3 <- result$`Pr(>|t|)`[3]
    p4 <- result$`Pr(>|t|)`[4]

    Q1 <- "(reference)"
    Q2 <- value_for_reg(Estimate2, sd2, p2)
    Q3 <- value_for_reg(Estimate3, sd3, p3)
    Q4 <- value_for_reg(Estimate4, sd4, p4)

    return(data.frame(Model = covariate, Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4))
}

# =======================================================
# 1. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "occupation", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "ln_FPG", "ln_hba1c")

# 4. 自变量(因子型Mn)
exposure <- "Mn_quartiles"

# 5. 数据
data <- imputed_df

# 创建储存结果的数据框
reg_df <- data.frame(
    Model = character(),
    Q1 = character(),
    Q2 = character(),
    Q3 = character(),
    Q4 = character()
)

# 初始化结果存储列表
result_list <- list()

# 遍历outcome
for (i in outcome) {
    # 获取当前outcome列的数据
    current_outcome <- data[[i]]

    # 执行df函数（假设df是一个函数，需要根据实际情况修改）
    reg_first <- df(i)
    result_list <- c(result_list, list(reg_first)) # 将结果添加到列表中

    # 遍历covariate
    for (j in covariate) {
        # 判断当前outcome列是否为因子型
        if (is.factor(current_outcome)) {
            # 执行逻辑回归
            model <- logistic_regression(data, i, exposure, j)
        } else {
            # 执行线性回归
            model <- linear_regression(data, i, exposure, j)
        }
        result_list <- c(result_list, list(model)) # 将模型结果添加到列表中
    }
}

# 将结果列表转换为数据框
reg_df <- do.call(rbind, result_list)

View(reg_df)
# 保存结果
write.csv(reg_df, "其他结果\\表格\\血清锰_四分位数_糖尿病相关_逐一调整协变量回归分析.csv", row.names = FALSE)

# 逐一放入协变量(血清锰为连续)===========================
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
load("数据\\imputed_df.RData")

# 读取函数
source("代码\\myfunctions.r")

# ===========函数================
# 1. 数据框
df <- function(
    Dependent_variable,
    Model_1, Model_2, Model_3, Model_4,
    Model_5, Model_6, Model_7, Model_8,
    Model_9, Model_10, Model_11, Model_12, Model_13) {
    return(
        data.frame(
            Dependent_variable = Dependent_variable,
            Model_1 = Model_1,
            Model_2 = Model_2,
            Model_3 = Model_3,
            Model_4 = Model_4,
            Model_5 = Model_5,
            Model_6 = Model_6,
            Model_7 = Model_7,
            Model_8 = Model_8,
            Model_9 = Model_9,
            Model_10 = Model_10,
            Model_11 = Model_11,
            Model_12 = Model_12,
            Model_13 = Model_13
        )
    )
}

# 2. 提取需要的值的函数
value_for_reg <- function(Estimate, sd, p) {
    low_ci <- low_ci(Estimate, sd)
    up_ci <- up_ci(Estimate, sd)
    low_ci <- format_2(low_ci)
    up_ci <- format_2(up_ci)
    Estimate <- format_2(Estimate)
    Q_left <- paste(Estimate, "(", low_ci, ", ", up_ci, ")", sep = "")
    p <- significance(p)
    Q <- paste(Q_left, p, sep = "")
    return(Q)
}

# 3. logistic回归
logistic_regression <- function(data, outcome, exposure, covariate) {
    # 拟合模型
    model <- glm(data[[outcome]] ~ data[[exposure]] + data[[covariate]], data = data, family = "binomial")

    # 提取系数
    result <- as.data.frame(summary(model)$coefficients)

    # 提取需要的值
    Estimate <- result$Estimate[2]
    sd <- result$`Std. Error`[2]
    p <- result$`Pr(>|z|)`[2]

    value <- value_for_reg(Estimate, sd, p)

    return(value)
}

# 4. 线性回归
linear_regression <- function(data, outcome, exposure, covariate) {
    # 拟合模型
    model <- lm(data[[outcome]] ~ data[[exposure]] + data[[covariate]], data = data)

    # 提取系数
    result <- as.data.frame(summary(model)$coefficients)

    # 提取需要的值
    Estimate <- result$Estimate[2]
    sd <- result$`Std. Error`[2]
    p <- result$`Pr(>|t|)`[2]

    value <- value_for_reg(Estimate, sd, p)
    return(value)
}
# ============================
# 创建储存结果的数据框
reg_df <- data.frame(Model = character(), Q1 = character(), Q2 = character(), Q3 = character(), Q4 = character())

# 1. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "occupation", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "ln_FPG", "ln_hba1c")

# 4. 自变量(因子型Mn)
exposure <- "scale_ln_Serum_Mn"

# 5. 数据
data <- imputed_df

# 逐一进行回归分析
for (i in outcome) {
    if (is.factor(data[[i]])) {
        model1 <- logistic_regression(data, i, exposure, covariate[1])
        model2 <- logistic_regression(data, i, exposure, covariate[2])
        model3 <- logistic_regression(data, i, exposure, covariate[3])
        model4 <- logistic_regression(data, i, exposure, covariate[4])
        model5 <- logistic_regression(data, i, exposure, covariate[5])
        model6 <- logistic_regression(data, i, exposure, covariate[6])
        model7 <- logistic_regression(data, i, exposure, covariate[7])
        model8 <- logistic_regression(data, i, exposure, covariate[8])
        model9 <- logistic_regression(data, i, exposure, covariate[9])
        model10 <- logistic_regression(data, i, exposure, covariate[10])
        model11 <- logistic_regression(data, i, exposure, covariate[11])
        model12 <- logistic_regression(data, i, exposure, covariate[12])
        model13 <- logistic_regression(data, i, exposure, covariate[13])
    } else {
        model1 <- linear_regression(data, i, exposure, covariate[1])
        model2 <- linear_regression(data, i, exposure, covariate[2])
        model3 <- linear_regression(data, i, exposure, covariate[3])
        model4 <- linear_regression(data, i, exposure, covariate[4])
        model5 <- linear_regression(data, i, exposure, covariate[5])
        model6 <- linear_regression(data, i, exposure, covariate[6])
        model7 <- linear_regression(data, i, exposure, covariate[7])
        model8 <- linear_regression(data, i, exposure, covariate[8])
        model9 <- linear_regression(data, i, exposure, covariate[9])
        model10 <- linear_regression(data, i, exposure, covariate[10])
        model11 <- linear_regression(data, i, exposure, covariate[11])
        model12 <- linear_regression(data, i, exposure, covariate[12])
        model13 <- linear_regression(data, i, exposure, covariate[13])
    }
    reg_df <- rbind(reg_df, df(i, model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, model11, model12, model13))
}
traceback()
View(reg_df)
write.csv(reg_df, "其他结果\\表格\\血清锰_连续_糖尿病相关_逐一调整协变量回归分析.csv", row.names = FALSE)

# ============table2 限制性立方样条模型==========================
rm(list = ls())
library(rms)
# pak::pkg_install("ggrcs")
library(ggrcs)
# 自编函数
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData")
data <- imputed_df_v6
# 查看数据
colnames(data)

# per sd
data$sd_ln_Mn <- data$ln_Serum_Mn /
    sd(data$ln_Serum_Mn, na.rm = TRUE)
data$sd_ln_FPG <- data$ln_FPG /
    sd(data$ln_FPG, na.rm = TRUE)
# 建立限制性立方样条模型并提取相关参数
# 创建 datadist 对象并指定
dd <- datadist(data)
options(datadist = "dd")

# 1. 因变量
outcome <- c("diabetes", "IFG_or_diabetes", "sd_ln_FPG")

# 协变量1 性别 年龄
covariate1 <- c("gender", "age")

# 2. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate2 <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 3. 自变量
exposure <- "rcs(ln_Serum_Mn, 3)"

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure, covariate1)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure, covariate2)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# IFG or diabetes
# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[2], exposure)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[2], exposure, covariate1)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[2], exposure, covariate2)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# FPG
# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[3], exposure)

# 使用限制性立方样条模型
fit <- lm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[3], exposure, covariate1)

# 使用限制性立方样条模型
fit <- lm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[3], exposure, covariate2)

# 使用限制性立方样条模型
fit <- lm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))
# ============限制性立方样条模型(绘图)==========================
rm(list = ls())

# 加载必要的包
# install.packages("pak")
# pak::pkg_install(c("ggplot2", "rms", "grid"))
library(ggplot2)
library(rms)
library(ggrcs)
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
exposure <- "rcs(ln_Serum_Mn, 3)"

##### diabetes
# 构建logistic回归模型公式
covarite_formula <- create_formula(outcome[1], exposure)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, ln_Serum_Mn, fun = exp, ref.zero = TRUE)
# View(OR)

# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(2.64)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

# 绘图
p_for_diabetes <- ggplot() +
    geom_line(
        data = OR, aes(ln_Serum_Mn, yhat),
        linetype = 1, linewidth = 0.2, alpha = 0.9, colour = "skyblue"
    ) +
    geom_ribbon(
        data = OR, aes(ln_Serum_Mn, ymin = lower, ymax = upper),
        alpha = 0.3, fill = "skyblue"
    ) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") +
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("lnMn = ", round(x_at_y_1, 2)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) +
    theme_bw(base_size = 9) + # 直接使用带边框的主题
    labs(x = "Manganese", y = "Diabetes, Adjusted OR (95% CI)") +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(an$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 2
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(an$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 2
    )


# 保存图片
# ggsave("主要结果/图片/RCS/Dose-response relationship between serum manganese and diabetes.pdf", p_for_diabetes, width = 2.57, height = 3, dpi = 300)
##### IFG or diabetes
#  自变量
exposure <- "ln_Serum_Mn"
# 构建logistic回归模型公式
covarite_formula <- formula_rcs(outcome[2], exposure, 3)

# 使用限制性立方样条模型
fit <- lrm(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit)))

# 生成预测值
OR <- Predict(fit, ln_Serum_Mn, fun = exp, ref.zero = TRUE)
# View(OR)
# 找到 y = 1 对应的 x 值（右边的那个）
x_at_y_1 <- c(2.64)
# 如果有多个 x 值对应 y = 1，选择右边的那个
if (length(x_at_y_1) > 1) {
    x_at_y_1 <- max(x_at_y_1)
}

# 绘图
p_for_IFG_or_diabetes <- ggplot() +
    geom_line(data = OR, aes(ln_Serum_Mn, yhat), linetype = 1, linewidth = 0.2, alpha = 0.9, colour = "skyblue") +
    geom_ribbon(data = OR, aes(ln_Serum_Mn, ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") + # 添加竖线
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("lnMn = ", round(x_at_y_1, 2)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) + # 添加标注
    theme_bw() +
    theme(
        text = element_text(size = 9)
    ) +
    labs(x = "Manganese", y = "IFG or diabetes, Adjusted OR (95%CI)") +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(an$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 2
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(an$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 2
    )

# 保存图片
# ggsave("主要结果/图片/RCS/Dose-response relationship between serum manganese and IFG or diabetes .pdf", p_for_IFG_or_diabetes, width = 2.57, height = 3, dpi = 300)

# ==============FPG==========================================
#  自变量
library(plotRCS) # plotRCS包用于绘制限制性立方样条图
# install.packages("remotes")
# pak::pkg_install("segmented")
# library(scitable)
# library(segmented)

exposure <- "ln_Serum_Mn" # 自变量
# 回归模型公式
covarite_formula <- formula_rcs(outcome[3], exposure, 3) # 回归模型公式

# 使用限制性立方样条模型
fit <- ols(covarite_formula, data = data)
AIC(fit)
(an <- as.data.frame(anova(fit))) # 获取p for non-linear

p_FPG <- rcsplot(
    data = data,
    outcome = "ln_FPG",
    exposure = "ln_Serum_Mn",
    # covariates = covariate,
    linecolor = "skyblue",
    fillcolor = "skyblue",
    pvalue.position = c(0.3, 0.95),
    knots.line = TRUE,
    ref.value = "k1",
    fontsize = 9,
    explain = TRUE,
    xlab = "Manganese",
    ylab = "FPG, β (95%)",
    alpha = 0.3
)
p_FPG <- p_FPG +
    theme_bw(base_size = 9) + # base_size 控制全局字体
    theme(
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2)
    )
names(p_FPG)
# Display the plot
p_FPG
ggsave("主要结果/图片/RCS/Dose-response relationship between serum manganese and FPG.pdf", p_FPG, width = 2.6, height = 3, dpi = 300)

# ============3．血清锰与alpha多样性的相关分析==========================
# 清除环境
rm(list = ls())

# 加载包
library(tidyverse)
library(ggpubr)
library(agricolae)
library(gridExtra)
# pak::pkg_install(c("ggprism", "ggsignif"))
# pak::pkg_install("FSA")
library(ggprism)
library(ggsignif)

# 自编函数
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData")
imputed_df_v4 <- imputed_df_v6
# 查看数据
colnames(imputed_df_v4)

data <- imputed_df_v4
p_alpha_richness <- plot_violin_kruskal_dunn(
    data = data,
    xvar = "Mn_quartiles",
    yvar = "Richness",
    fill_colors = c("#F2D6B4", "#94D5DB", "#7696BA", "#A9CBB8"),
    line_width = 0.5,
    sig_bar_area_ratio = 0.8
)

ggsave("主要结果\\图片\\alpha\\Manganese vs Richness.pdf", p_alpha_richness, width = 2.07, height = 2.07, dpi = 300)

p_alpha_Shannon <- plot_violin_kruskal_dunn(
    data = data,
    xvar = "Mn_quartiles",
    yvar = "Shannon",
    fill_colors = c("#F2D6B4", "#94D5DB", "#7696BA", "#A9CBB8"),
    line_width = 0.5,
    sig_bar_area_ratio = 0.8
)

ggsave("主要结果\\图片\\alpha\\Manganese vs Shannon.pdf", p_alpha_Shannon, width = 2.07, height = 2.07, dpi = 300)

p_Pielou <- plot_violin_kruskal_dunn(
    data = data,
    xvar = "Mn_quartiles",
    yvar = "Pielou",
    fill_colors = c("#F2D6B4", "#94D5DB", "#7696BA", "#A9CBB8"),
    line_width = 0.5,
    sig_bar_area_ratio = 0.8
)

ggsave("主要结果\\图片\\alpha\\Manganese vs Pielou.pdf", p_Pielou, width = 2.07, height = 2.07, dpi = 300)

p_alpha_Simpson <- plot_violin_kruskal_dunn(
    data = data,
    xvar = "Mn_quartiles",
    yvar = "Simpson",
    fill_colors = c("#F2D6B4", "#94D5DB", "#7696BA", "#A9CBB8"),
    line_width = 0.5,
    sig_bar_area_ratio = 0.8
)

ggsave("主要结果\\图片\\alpha\\Manganese vs Simpson.pdf", p_alpha_Simpson, width = 2.07, height = 2.07, dpi = 300)

# ============当血清锰为连续性变量时====================================
# 清除环境
rm(list = ls())

# 加载包
# 加载自编函数
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData")
data <- imputed_df_v6
# 查看数据
colnames(data)

# 定义因变量，自变量，协变量
outcome <- c("Richness", "Shannon", "Simpson", "Pielou")
exposure <- "scale_ln_Serum_Mn"

# 连续型协变量
cont_covars <- c(
    "age", "BMI", "energy_kcal",
    "diet_Mn_mg"
)

# 分类型协变量
cat_covars <- c(
    "gender", "education", "Marital_status",
    "family_income_cat", "family_diabetes_history", "act_sport"
)

# 储存结果的数据框
result_df <- data.frame(
    Var1 = character(), Var2 = character(), Method = character(),
    N = integer(), Gp = numeric(), Estimate = numeric(),
    Statistic = numeric(), P_value = numeric()
)

# 逐一进行偏相关分析
for (i in outcome) {
    result <- pcor_s(
        exposure, i, cont_covars, cat_covars, data,
        method = "spearman"
    )
    result_df <- rbind(
        result_df,
        ppcor_result_df(
            exposure, i, "spearman", result$n, result$gp,
            result$estimate, result$statistic, result$p.value
        )
    )
}

# 查看结果
# View(result_df)

# 多重校正
result_df$adjusted_p <- p.adjust(result_df$P_value, method = "BH") # Benjamini-Hochberg 校正
# colnames(result_df)

# 检查 result_df 中实际存在的列有哪些
column_names <- colnames(result_df) # 获取结果数据框的列名
print(column_names) # 打印列名以检查哪些列存在
# 使用结果数据框中存在的列名
names <- c("Var1", "Var2", "Estimate", "P_value", "Statistic", "adjusted_p")
# 请确保 names 中的所有列都在 result_df 中存在。
if (all(names %in% column_names)) {
    final_df <- result_df[, names]
} else {
    # Identify missing columns
    missing_cols <- names[!names %in% column_names]
    warning(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    # Select only available columns
    available_names <- names[names %in% column_names]
    final_df <- result_df[, available_names]
}
# 整理结果，形成最终表格
final_df <- result_df %>%
    #   select(all_of(names)) %>%
    mutate(
        Var1 = "Manganese"
    ) %>%
    rename(
        "Adjusted p-value" = "adjusted_p",
        "Coefficient" = "Estimate"
    )

colnames(final_df)
# write.csv(final_df, "主要结果\\表格\\血清锰_连续_微生物多样性相关_偏相关分析.csv", row.names = FALSE)
# 绘制热图
# 创建显著性标记
result_df$signif <- ""
result_df$signif[result_df$adjusted_p < 0.05] <- "*"
result_df$signif[result_df$adjusted_p < 0.01] <- "**"
result_df$signif[result_df$adjusted_p < 0.001] <- "***"

# 绘制热图并添加显著性标记
p <- ggplot(result_df, aes(x = Var1, y = Var2, fill = Estimate)) +
    geom_tile(width = 0.9, height = 0.9) + # 调整方块大小
    geom_text(aes(label = signif), color = "black", size = 4, vjust = 1.5) + # 显著性标注
    scale_fill_gradient2(
        low = "#0509f4", # 蓝色
        mid = "#FFFFFF", # 白色
        high = "#f80808", # 红色
        midpoint = 0, # 中间值
        limits = c(-0.05, 0.05), # 设置颜色范围
        name = "Estimate" # 图例标题
    ) +
    theme_minimal() + # 使用简洁主题
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # 标题样式
        axis.title = element_text(size = 14, face = "bold"), # 轴标题样式
        axis.text = element_text(size = 12), # 轴标签样式
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # x轴标签倾斜
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5), # y轴标签样式
        legend.title = element_text(size = 12, face = "bold"), # 图例标题样式
        legend.text = element_text(size = 10), # 图例文字样式
        legend.position = "right", # 图例位置
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2), # 添加网格线
        panel.grid.minor = element_blank(), # 去掉次要网格线
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # 添加边框
        panel.background = element_rect(fill = "white") # 背景颜色
    ) +
    labs(
        title = "", # 图标题
        x = "", # x轴标题
        y = "" # y轴标题
    ) +
    scale_x_discrete(labels = rep("Manganese", length(unique(result_df$Var1)))) # 修改x轴标签

ggsave(p, file = "主要结果\\图片\\Fig 9. alpha_heatmap.pdf", width = 4, height = 8, dpi = 300) # 保存图片
# ========Mn 与alpha多样性的rcs=================================
# 清除环境
rm(list = ls())
# 仅用rms + ggplot2实现"剂量-反应关系"（受限立方样条回归），协变量可调
# 无需plotRCS包，适用于CRAN可安装环境
# install.packages("plotRCS") # 如果未安装plotRCS包，请先安装
library(rms)
library(ggplot2)
library(plotRCS)
# 读取数据
load("数据/imputed_df_v6.RData")
source("代码/myfunctions.r") # 加载自编函数
data <- imputed_df_v6

# 设置数据分布对象（rms包要求）
dd <- datadist(data)
options(datadist = "dd")

# 协变量
covariates <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)
exposure <- "ln_Serum_Mn"

# 受限立方样条结点（可调）
knots <- 3 # 或者 c(0.25, 0.5, 0.75)分位点

# 画Richness
p_richness <- rcsplot(
    data = data,
    outcome = "Richness",
    exposure = exposure,
    covariates = covariates,
    knots = knot(3), # 3个结点，自动转为合适分位点
    linecolor = "skyblue",
    fillcolor = "skyblue",
    xlab = "Manganese",
    fontsize = 7,
    ylab = "Richness,β"
)

# 保存
pathway <- "主要结果\\图片\\alpha\\RCS\\Mn vs Richness rcs.pdf"
ggsave(pathway, p_richness, width = 2.07, height = 3, dpi = 300)

# 画Shannon
p_shannon <- rcsplot(
    data = data,
    outcome = "Shannon",
    exposure = exposure,
    covariates = covariates,
    knots = knot(3),
    linecolor = "skyblue",
    fillcolor = "skyblue",
    fontsize = 7,
    xlab = "Manganese",
    ylab = "Shannon,β"
)

# 保存
pathway <- "主要结果\\图片\\alpha\\RCS\\Mn vs Shannon rcs.pdf"
ggsave(pathway, p_shannon, width = 2.07, height = 3, dpi = 300)


# 画Simpson
p_simpson <- rcsplot(
    data = data,
    outcome = "Simpson",
    exposure = exposure,
    covariates = covariates,
    knots = knot(3), # 3个结点，自动转为合适分位点
    linecolor = "skyblue",
    fillcolor = "skyblue",
    fontsize = 7,
    xlab = "Manganese",
    ylab = "Simpson,β"
)

# 保存
pathway <- "主要结果\\图片\\alpha\\RCS\\Mn vs Simpson rcs.pdf"
ggsave(pathway, p_simpson, width = 2.07, height = 3, dpi = 300)

# 画Pielou
p_pielou <- rcsplot(
    data = data,
    outcome = "Pielou",
    exposure = "ln_Serum_Mn",
    covariates = covariates,
    knots = knot(3), # 3个结点，自动转为合适分位点
    linecolor = "skyblue",
    fillcolor = "skyblue",
    fontsize = 7,
    xlab = "Manganese",
    ylab = "Pielou,β",
)

# 保存
pathway <- "主要结果\\图片\\alpha\\RCS\\Mn vs Pielou rcs.pdf"
ggsave(pathway, p_pielou, width = 2.07, height = 3, dpi = 300)

# ========diabetes或IFG与alpha多样性=================================
# ===== 1. 清理环境 =====
rm(list = ls())

# ===== 2. 加载/安装必要包 =====
required_pkgs <- c("gtsummary", "tidyverse", "flextable", "officer")
for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
}
# pak::pkg_install("FSA")
# pak::pkg_install("ggsignif")
source("代码/myfunctions.r") # 加载自定义函数
# ===== 3. 加载数据 =====
load("数据/imputed_df_v6.RData") # 根据你的实际路径调整
data <- imputed_df_v6

# ===== 4. 检查和处理分组变量类型 =====
# 假设你的分组变量为 diabetes，建议强制为因子型
if (!is.factor(data$diabetes)) {
    data$diabetes <- as.factor(data$diabetes)
}

# ===== 5. gtsummary 分组统计（自动处理缺失，无需提前过滤） =====
table1 <- tbl_summary(
    data = data,
    by = "diabetes",
    include = c("Richness", "Shannon", "Simpson", "Pielou"), # 只统计这四个变量
    statistic = list(
        all_continuous() ~ "{mean}±{sd}",
        all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no", # 不显示缺失值
    percent = "column",
    digits = list(
        all_categorical() ~ c(0, 2), # 分类变量计数0位、百分比2位
        all_continuous() ~ 2 # 连续变量保留2位小数
    )
) %>%
    add_p(
        test = list(
            all_continuous() ~ "t.test", # 连续变量用t检验
            all_categorical() ~ "chisq.test" # 分类变量用卡方检验
        )
    ) %>%
    modify_header(label = "**Variable**") %>%
    bold_labels()

# ===== 6. 展示统计表 =====
table1

# ===== 7. 可选：导出为Word文档 =====
table1_ft <- as_flex_table(table1)
doc <- read_docx()
doc <- body_add_flextable(doc, value = table1_ft)
print(doc, target = "table1.docx")
# 创建表格
table2 <- data %>%
    # 显式处理缺失值
    filter(!is.na(IFG_or_diabetes)) %>%
    select(all_of(vars[c(1, 3:6)])) %>%
    tbl_summary(
        by = c("IFG_or_diabetes"),
        statistic = list(all_continuous() ~ "{mean}±{sd}", all_categorical() ~ "{n} ({p})"),
        missing = "no",
        percent = "column",
        digits = list(all_categorical() ~ c(0, 2), all_continuous() ~ 2)
    ) %>%
    add_p(test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test")) %>% # 添加t检验p值
    modify_header(label = "**Variable**") %>%
    bold_labels()
# print(table2)

# 水平合并表格
merged_table <- tbl_merge(
    tbls = list(table1, table2),
    tab_spanner = c("**Diabetes Status**", "**IFG or Diabetes Status**")
)

# Print merged table
# print(merged_table)
# 将表格转换为flextable对象
flextable1 <- as_flex_table(merged_table)

# 创建一个Word文档并添加表格
doc <- read_docx() %>%
    body_add_flextable(flextable1)

# 保存Word文档
print(doc, target = "其他结果\\表格\\多样性指数与糖尿病状态比较.docx")

# 糖尿病与alpha多样性箱式图
library(ggplot2) # 用于绘图
# library(ggpubr) # ggpubr 包用于绘制显著性标记
# library(patchwork) # patchwork 包用于组合多个图形

p_diabetes_rich <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Richness) & !is.na(data$diabetes), ],
    xvar = "diabetes",
    yvar = "Richness",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)
print(p_diabetes_rich)
ggsave("主要结果\\图片\\alpha\\小提琴图\\diabetes_vs_Richness.pdf", p_diabetes_rich, width = 2.07, height = 3, dpi = 300)

p_diabetes_shannon <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Shannon) & !is.na(data$diabetes), ],
    xvar = "diabetes",
    yvar = "Shannon",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)

# 保存
ggsave("主要结果\\图片\\alpha\\小提琴图\\diabetes_vs_Shannon.pdf", p_diabetes_shannon, width = 2.07, height = 3, dpi = 300)

p_diabetes_simpson <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Simpson) & !is.na(data$diabetes), ],
    xvar = "diabetes",
    yvar = "Simpson",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)

# 保存
ggsave("主要结果\\图片\\alpha\\小提琴图\\diabetes_vs_Simpson.pdf", p_diabetes_simpson, width = 2.07, height = 3, dpi = 300)

p_diabetes_pielou <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Pielou) & !is.na(data$diabetes), ],
    xvar = "diabetes",
    yvar = "Pielou",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)

# 保存
ggsave("主要结果\\图片\\alpha\\小提琴图\\diabetes_vs_Pielou.pdf", p_diabetes_pielou, width = 2.07, height = 3, dpi = 300)

# ========IFG_or_diabetes与alpha多样性箱式图=========
p_IFG_or_diabetes_rich <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Richness) & !is.na(data$IFG_or_diabetes), ],
    xvar = "IFG_or_diabetes",
    yvar = "Richness",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)

ggsave("主要结果\\图片\\alpha\\小提琴图\\IFG_or_diabetes_vs_Richness.pdf", p_IFG_or_diabetes_rich, width = 2.07, height = 3, dpi = 300)

p_IFG_or_diabetes_shannon <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Shannon) & !is.na(data$IFG_or_diabetes), ],
    xvar = "IFG_or_diabetes",
    yvar = "Shannon",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)
ggsave("主要结果\\图片\\alpha\\小提琴图\\IFG_or_diabetes_vs_Shannon.pdf", p_IFG_or_diabetes_shannon, width = 2.07, height = 3, dpi = 300)

p_IFG_or_diabetes_simpson <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Simpson) & !is.na(data$IFG_or_diabetes), ],
    xvar = "IFG_or_diabetes",
    yvar = "Simpson",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)

ggsave("主要结果\\图片\\alpha\\小提琴图\\IFG_or_diabetes_vs_Simpson.pdf", p_IFG_or_diabetes_simpson, width = 2.07, height = 3, dpi = 300)

p_IFG_or_diabetes_pielou <- plot_violin_kruskal_dunn(
    data = data[!is.na(data$Pielou) & !is.na(data$IFG_or_diabetes), ],
    xvar = "IFG_or_diabetes",
    yvar = "Pielou",
    fill_colors = c("#7EBFC9", "#BCDF7A")
)

ggsave("主要结果\\图片\\alpha\\小提琴图\\IFG_or_diabetes_vs_Pielou.pdf", p_IFG_or_diabetes_pielou, width = 2.07, height = 3, dpi = 300)
# ========重新计算肠道菌数据==================
rm(list = ls())

# 读取数据
load("数据\\imputed_df_v2.RData")
data <- imputed_df_v2
# 查看数据
colnames(data)
row.names(data) <- data$SampleID # 将样本ID设置为行名
df <- data[, c(26:77)] # 选择肠道菌数据

# 设置行为分类单元，列为样本id
df <- as.data.frame(t(df))

## 计算各样本总丰度
colSums(df)

## 转置
df <- data.frame(t(df), check.names = FALSE)

## 均一化/抽平
set.seed(123)
df <- vegan::rrarefy(df, min(rowSums(df)))
df <- data.frame(t(df), check.names = FALSE)

## 计算各样本总丰度
colSums(df)

## 由于总丰度减少，剔除在所有样本中均为0的tax
df <- df[rowSums(df) > 0, ]

## 输出均一化的丰度表
write.csv(df, "数据\\gut.micro_evenness.csv")

## 求每个tax的相对丰度
gut.micro <- df # 读入均一化后的丰度表
gut.micro1 <- data.frame(t(gut.micro), check.names = F) # 转置
gut.micro1 <- gut.micro1 / rowSums(gut.micro1) # 计算相对丰度
gut.micro1 <- data.frame(t(gut.micro1), check.names = F) # 转置

# 此时各样本的总丰度为1
colSums(gut.micro1)

# 输出相对丰度表
write.csv(gut.micro1, "数据\\gut.micro_relative.csv")

# 剔除在所有样本中平均相对丰度低于0.0001的tax
gut.micro1_selected <- gut.micro1[which(rowMeans(gut.micro1) >= 0.0001), ]
# 剔除出现频率低于10%的tax
gut.micro1_selected <- gut.micro1_selected[which(apply(gut.micro1_selected, 1, function(x) sum(x > 0)) > (ncol(gut.micro1_selected) / 10)), ]

write.csv(gut.micro1_selected, "数据\\gut.micro_cleaned.csv")

# =======进行对数转换==================
library(tidyverse)
df <- gut.micro1_selected
df <- as.data.frame(t(df))
# 对 df 的每一列进行操作，将值为 0 的元素替换为当前列中大于 0 的最小值的十分之一。
df <-
    df %>%
    mutate_all(., ~ replace(., . == 0, min(as.numeric(.[. > 0])) / 10))

df_log <- log(df)

# 对提取的数据框进行标准化转换
g_columns_standardized <- scale(df_log)

write.csv(g_columns_standardized, "数据\\gm_clean_genus.csv")
# =======将标准化后的肠道菌数据合并到总数据中=======
rm(list = ls())

# 加载包
library(tidyverse)

# 读取数据
load("数据\\imputed_df_v2.RData")
genus <- read.csv("数据\\gm_clean_genus.csv", row.names = 1)
genus$SampleID <- rownames(genus)

df <- imputed_df_v2 %>%
    left_join(genus, by = "SampleID")

print(colnames(df))

delete_columns <- c(
    "g_Devosia",
    "g_Eggerthella", "g_Halomonas", "g_Lachnobacterium",
    "g_Methylobacterium", "g_Pseudomonas", "g_Sphingomonas",
    "g_Synergistes", "g_Actinomyces", "g_Leptothrix"
)

df <- df %>%
    select(-all_of(delete_columns))

print(colnames(df))

imputed_df_v3 <- df
save(imputed_df_v3, file = "数据\\imputed_df_v3.RData")
# =========血清锰四分位PCoA分析=============================================
rm(list = ls())

# 加载必要的R包
# pak::pkg_install("vegan") # 安装vegan包
library(vegan) # vegan包用于生态数据分析
library(ggplot2) # ggplot2包用于数据可视化
# 自编函数
source("代码\\myfunctions.r")
# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")
data <- imputed_df_v6
names(genus_data)
genus_Relative <- genus_data$relative
data <- data[data$SampleID %in% rownames(genus_Relative), ]
nrow(data) # 查看数据行数

# 查看列名
row.names(data) <- data$SampleID # 将样本ID设置为行名

# 计算距离矩阵
bray_dist <- vegdist(genus_Relative, method = "bray") # 使用Bray-Curtis距离

# 计算主坐标分析（PCoA），并绘制图像，分组为data的Mn_quartiles列
# 执行主坐标分析（PCoA）
pcoa <- cmdscale(bray_dist, k = 2, eig = TRUE) # k=2表示提取前两个主坐标

# 提取PCoA坐标
pcoa_points <- data.frame(
    SampleID = rownames(genus_Relative),
    PCo1 = pcoa$points[, 1],
    PCo2 = pcoa$points[, 2]
)

# 计算解释的变异比例
explained_var <- round(pcoa$eig / sum(pcoa$eig) * 100, 2)

# 合并Mn_quartiles信息
pcoa_df <- merge(pcoa_points, data[, c("SampleID", "Mn_quartiles")], by.x = "SampleID", by.y = "SampleID")

# 绘制PCoA图函数
plot_pcoa <- function(pcoa_df, title) {
    p <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = Mn_quartiles)) +
        geom_point(size = 0.4, alpha = 0.9, stroke = 0.2) +
        stat_ellipse(aes(group = Mn_quartiles),
            level = 0.95,
            linetype = "solid", linewidth = 0.5
        ) +
        labs(
            title = title,
            x = paste0("PCo1 (", explained_var[1], "%)"),
            y = paste0("PCo2 (", explained_var[2], "%)"),
            color = "Mn_quartiles"
        ) +
        theme_bw(base_size = 7) +
        theme(
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            panel.grid.major = element_line(color = "gray95", linewidth = 0.25),
            panel.grid.minor = element_line(color = "gray95", linewidth = 0.10),
            plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
            axis.title = element_text(face = "bold", size = 7),
            axis.text = element_text(size = 6),
            legend.title = element_text(face = "bold", size = 7),
            legend.text = element_text(size = 6),
            legend.position = "right",
            legend.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
            legend.key.height = unit(0.22, "cm"), # 图例项高度更小
            legend.key.width = unit(0.22, "cm"), # 图例项宽度更小
            legend.spacing.y = unit(0, "cm"), # 图例项之间无额外竖直间距
            legend.margin = margin(2, 2, 2, 2), # 图例整体边距减小
            legend.box.margin = margin(0, 0, 0, 0) # 图例外部边距减小
        ) +
        guides(color = guide_legend(override.aes = list(size = 1))) + # 图例点更小
        scale_color_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8", "#984EA3"))
    return(p)
}

# 使用
p <- plot_pcoa(pcoa_df, "PCoA based on Bray-Curtis Distance")

ggsave("主要结果\\图片\\beta\\PCoA_Bray_Curtis_Mn_Q.pdf",
    plot = p,
    width = 4, height = 4, dpi = 300
)
# ============4．adonis贡献度分析（置换多元方差分析）===================================================
# 清除环境
rm(list = ls())

# 加载包
library(vegan) # vegan包用于生态数据分析
library(dplyr) # dplyr包用于数据处理

# 自编函数
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData") # 加载肠道菌属数据
data <- imputed_df_v6
names(genus_data)
genus_Relative <- genus_data$relative
data <- data[data$SampleID %in% rownames(genus_Relative), ]
nrow(data) # 查看数据行数

# 查看列名
row.names(data) <- data$SampleID # 将样本ID设置为行名

# df_input_metadata <- df

# 计算Bray-Curtis距离:评估群落组成差异
gm.BC <- vegdist(genus_Relative, method = "bray") # 使用Bray-Curtis距离

# 先对连续型变量进行标准化，以消除因量纲不同带来的影响
cont <- c("age", "BMI", "energy_kcal", "diet_Mn_mg")
data[, cont] <- scale(data[, cont])

# 创建一个空的数据框用于存储结果
result_df1 <- data.frame()
colnames(data) # 查看数据列名
# 循环遍历可能的影响因素
for (i in c(2:13, 23)) {
    # 获取当前列的名字
    current_col <- colnames(data)[i]

    # 运行adonis2分析
    formula_str <- paste("gm.BC ~", current_col)
    adonis_result <- adonis2(
        as.formula(formula_str),
        data = data,
        permutations = 999
    )
    # 提取结果，转换为数据框
    adonis_summary <- data.frame(adonis_result) # 从 adonis2 结果中提取分析表

    # 添加当前列名作为额外的列来标识
    adonis_summary$variable <- current_col

    # 合并当前结果到最终的结果数据框中
    result_df1 <- rbind(result_df1, adonis_summary)
}

# 保存最终结果
result_df1 <- result_df1 %>%
    filter(!is.na(F)) # 只保留模型结果第一行
write.csv(result_df1, "主要结果/表格/adonis2.csv")

# 手动excel处理变量名称
# 添加提示音
library(beepr)
beep(0)

# 读取结果
result_df1 <- read.csv("主要结果/表格/adonis2.csv")

# 绘图
data <- data.frame(result_df1) %>%
    mutate(
        text = case_when(
            Pr..F. >= 0.05 ~ paste(" "),
            Pr..F. >= 0.01 ~ paste("\n*"),
            Pr..F. > 0.001 ~ paste("\n**"),
            Pr..F. <= 0.001 ~ paste("\n***")
        )
    ) # 添加文本列，根据p值设置星号
# 计算每行的R平方值百分比
data$`Explanation of gut microbiota structure (%)` <- data$R2 * 100

# 绘制贡献度分析结果
# 设置因子顺序
data <- data %>%
    arrange(desc(`Explanation of gut microbiota structure (%)`)) # desc 函数用于降序排列, arrange 函数用于排序

data$label <- factor(data$variable, levels = rev(data$variable)) # 设置标签的因子水平顺序，使用 rev 函数反转顺序

# 添加 `color` 列，默认值为 "grey"
data$color <- "grey"

# 将Mn的行的color列标记为"blue"
data$color[data$label == "Mn quartiles"] <- "skyblue"
# View(data)

library(ggplot2)
# 绘图
p <- ggplot(
    data = data[1:13, ],
    aes(x = `Explanation of gut microbiota structure (%)`, y = label, fill = color)
) +
    geom_bar(
        stat = "identity",
        width = 0.7,
        position = position_dodge(width = 0.9)
    ) +
    scale_fill_manual(
        values = c(skyblue = "skyblue", grey = "#bebebefb"),
        name = "Group"
    ) +
    geom_text(
        aes(label = text),
        col = "black",
        size = 2, # 适当调整字体大小
        position = position_dodge(width = 0.9),
        vjust = 0.25,
        hjust = -0.2 # 调整水平对齐
    ) +
    xlim(0, 1) +
    labs(y = "") +
    theme_bw(base_size = 7) +
    theme(panel.grid = element_blank()) +
    theme(
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        plot.margin = margin(
            t = 10,
            r = 10,
            b = 5,
            l = 5
        )
    )

ggsave("主要结果\\图片\\beta\\Adonis2 analysis Contribution of variables to the differences in microbial communities.pdf", p, width = 3.5, height = 4, dpi = 300)
# ============5．识别与血清锰相关的差异特征菌（相对丰度）MaAsLin2===================================
# 清除环境
rm(list = ls())

# 加载包
library(tidyverse) # 数据处理和可视化
# install.packages("devtools")
# library("devtools")
# install_github("biobakery/Maaslin2")
# devtools::install_github("biobakery/Maaslin2")
library(Maaslin2) # Maaslin2包用于微生物组数据分析
# BiocManager::install(c("edgeR", "metagenomeSeq"))
# 读取数据
load("数据\\imputed_df_v6.RData")
table(imputed_df_v6$Marital_status) # 查看锰四分位数分布
# imputed_df_v6$Marital_status <- factor(imputed_df_v6$Marital_status, levels = c("Married", "Other")) # 设置婚姻状况的因子水平
# save(imputed_df_v6, file = "数据\\imputed_df_v6.RData") # 保存数据
imputed_df_v4 <- imputed_df_v6
# 菌群数据
load("数据\\genus_data.RData")
colnames(imputed_df_v6)
df_input_data <- genus_data$relative
df_input_metadata <- imputed_df_v6
df_input_data <- df_input_data + 1e-5 # 避免0值导致对数转换错误
# 检查数据
colnames(df_input_data)
colnames(df_input_metadata)
row.names(df_input_metadata) <- imputed_df_v6$SampleID
df_input_metadata <- df_input_metadata[df_input_metadata$SampleID %in% row.names(df_input_data), ]
df_input_metadata <- df_input_metadata %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2")) # 设置自变量参照

## Maaslin分析差异菌群
fixed_effects <- c(
    "scale_ln_Serum_Mn",
    "gender",
    "age",
    "BMI",
    "Marital_status",
    "education",
    "family_income_cat",
    "family_diabetes_history",
    "smoker",
    "Alcohol_consumption",
    "act_sport",
    "energy_kcal",
    "diet_Mn_mg"
) # 固定效应

# random_effects <- c(

# ) # 随机效应变量

# 检查固定效应变量是否存在于 df_input_metadata 中
print(fixed_effects[which(!fixed_effects %in% colnames(df_input_metadata))]) # 打印不存在的固定效应变量

# 1. 看数据矩阵里有没有 NA
any(is.na(df_input_data)) # TRUE → 有缺失
sum(is.na(df_input_data)) # 具体多少缺失值
# 2. 看 metadata 里有没有 NA（尤其是 fixed_effects 列）
df_input_metadata[fixed_effects] %>%
    summarise_all(~ sum(is.na(.))) # 每列 NA 计数
for (i in fixed_effects) {
    if (is.factor(df_input_metadata[[i]])) {
        a <- table(df_input_metadata[[i]]) # 打印每个固定效应变量的频数表
        cat("Variable:", i)
        print(a)
    }
}
str(df_input_data) # 查看数据结构
str(df_input_metadata[fixed_effects])
# 运行 Maaslin2
fit_data2 <- Maaslin2(
    input_data = df_input_data, # 菌群数据
    input_metadata = df_input_metadata, # 协变量数据
    min_prevalence = 0, # 最小流行率
    normalization = "CLR", # 标准化方法
    output = "主要结果/连续_maaslin2_output", # 输出目录
    fixed_effects = fixed_effects, # 固定效应
    # random_effects = random_effects, # 随机效应
    transform = "NONE", # 转换方法
    standardize = TRUE, # 标准化
    reference = c(
        "gender,Female", # 设置参照组
        "education,Junior_high_school_or_below",
        "family_income_cat,<3000",
        "family_diabetes_history,No",
        "smoker,Nonsmokers",
        "Alcohol_consumption,Neverdrinking",
        "act_sport,No"
    ),
    plot_heatmap = FALSE, # 先关掉
    plot_scatter = FALSE,
    max_significance = 0.25,
    correction = "BH"
)

# 将有意义的结果的表格转化成csv格式
sig <- read.delim("主要结果/连续_maaslin2_output/significant_results.tsv")
write.csv(sig, "主要结果/连续_maaslin2_output/significant_results.csv", row.names = FALSE)
# 提示音
library(beepr)
beep(0)
# 以锰的四分位数识别差异菌
# 固定效应
fixed_effects <- c(
    "Mn_quartiles",
    "gender",
    "age",
    "BMI",
    "Marital_status",
    "education",
    "family_income_cat",
    "family_diabetes_history",
    "smoker",
    "Alcohol_consumption",
    "act_sport",
    "energy_kcal",
    "diet_Mn_mg"
)
# 随机效应
# random_effects <- c(

# ) # 随机效应变量

# 运行 Maaslin2
fit_data2 <- Maaslin2(
    input_data = df_input_data, # 菌群相对丰度数据
    input_metadata = df_input_metadata, # 协变量数据
    min_prevalence = 0, # 最小流行率
    normalization = "CLR", # 标准化方法
    output = "主要结果/四分位数_maaslin2_output_v2", # 输出目录
    fixed_effects = fixed_effects, # 固定效应
    # random_effects = random_effects, # 随机效应
    transform = "NONE", # 转换方法
    standardize = TRUE, # 标准化
    reference = c(
        "Mn_quartiles,Q2",
        "gender,Female",
        "education,Junior_high_school_or_below",
        "family_income_cat,<3000",
        "family_diabetes_history,No",
        "smoker,Nonsmokers",
        "Alcohol_consumption,Neverdrinking",
        "act_sport,No"
    ), # 设置参照组
    plot_heatmap = FALSE
)
# 将有意义的结果的表格转化成csv格式
significant_result <- read.delim("主要结果\\四分位数_maaslin2_output_v2\\significant_results.tsv")
write.csv(significant_result, "主要结果\\四分位数_maaslin2_output_v2\\significant_results.csv", row.names = FALSE)
# 提示音
library(beepr)
beep(8)

# ============差异特征菌与糖尿病及相关指标的关联（根据四分位数maaslin结果）=============================
rm(list = ls())

# 加载自编函数及常用r包
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData") # metadata
load("数据\\genus_data.RData") # genus

# 合并数据
genus_data$transformed$SampleID <- row.names(genus_data$transformed)
imputed_df_v4 <- merge(imputed_df_v6, genus_data$transformed, by = "SampleID")

# 查看列名
colnames(imputed_df_v4)
View(imputed_df_v4)

data <- imputed_df_v4 %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2"))

# 设置暴露向量、结局向量、协变量向量
exposure_vars <- c(
    "g_Bradyrhizobium",
    "g_Ralstonia",
    "g_Devosia",
    "g_Lachnospira",
    "g_Sediminibacterium",
    "g_Acidaminococcus"
)

outcome_vars <- c(
    "diabetes",
    "IFG_or_diabetes"
)

covariate_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 进行回归
# 检查变量是否存在于 数据框 中
check_var_exist(exposure_vars, data)
check_var_exist(outcome_vars, data)
check_var_exist(covariate_vars, data)

# 回归分析
result_df <- reg_log_lin(
    exposure_vars = exposure_vars,
    outcome_vars = outcome_vars,
    covariate_vars = covariate_vars,
    data = data
)
# 合并数据框

names(result_df)
result_df <- result_df$result_df
write.csv(result_df, "主要结果\\表格\\四分位_genus_outcome_v2_Q2.csv", row.names = FALSE)
# 绘图
# 绘制热图并添加显著性标记
p <- ggplot(result_df, aes(x = bacterium, y = phenotype, fill = beta)) +
    geom_tile(width = 0.9, height = 0.9) + # 调整方块大小
    geom_text(aes(label = sig_p), color = "black", size = 4, vjust = 1.5) + # 显著性标注
    scale_fill_gradient2(
        low = "#0509f4", # 蓝色
        mid = "#FFFFFF", # 白色
        high = "#f80808", # 红色
        midpoint = 0, # 中间值
        limits = c(-0.3, 0.3), # 设置颜色范围
        name = "Estimate" # 图例标题
    ) +
    theme_minimal() + # 使用简洁主题
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # 标题样式
        axis.title = element_text(size = 14, face = "bold"), # 轴标题样式
        axis.text = element_text(size = 12), # 轴标签样式
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # x轴标签倾斜
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5), # y轴标签样式
        legend.title = element_text(size = 12, face = "bold"), # 图例标题样式
        legend.text = element_text(size = 10), # 图例文字样式
        legend.position = "right", # 图例位置
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2), # 添加网格线
        panel.grid.minor = element_blank(), # 去掉次要网格线
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # 添加边框
        panel.background = element_rect(fill = "white") # 背景颜色
    ) +
    labs(
        title = "", # 图标题
        x = "", # x轴标题
        y = "" # y轴标题
    )

ggsave(p, file = "主要结果\\图片\\Fig 4-a. Q_genus_diabetes_heatmap.pdf", width = 8, height = 4, dpi = 300) # 保存图片
# ============差异特征菌与糖尿病及相关指标的关联（根据连续性锰maaslin结果）=============================
rm(list = ls())

# 加载自编函数及常用r包
source("代码\\myfunctions.r")

# 读取数据
load("数据\\imputed_df_v6.RData") # metadata
load("数据\\genus_data.RData") # genus

# 合并数据
genus_data$transformed$SampleID <- row.names(genus_data$transformed)
imputed_df_v4 <- merge(imputed_df_v6, genus_data$transformed, by = "SampleID")

# 查看列名
colnames(imputed_df_v4)
# View(imputed_df_v4)

data <- imputed_df_v4 %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2"))

# 设置暴露向量、结局向量、协变量向量
exposure_vars <- c(
    "g_Prauseria",
    "g_Ralstonia",
    "g_Sediminibacterium",
    "g_Bulleidia",
    "g_Bradyrhizobium",
    "g_Acidaminococcus",
    "g_Devosia",
    "g_Ochrobactrum",
    "g_Methylobacterium",
    "g_Eggerthella",
    "g_Leptothrix",
    "g_Lacticigenium",
    "g_Odoribacter",
    "g_Oscillospira"
)

outcome_vars <- c(
    "diabetes",
    "IFG_or_diabetes"
)

covariate_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

tables <- reg_log_lin(exposure_vars, outcome_vars, covariate_vars, data)
names(tables)
result_df <- tables[[3]]
View(result_df)
write.csv(result_df, "主要结果\\表格\\连续血清锰_genus_outcome.csv", row.names = FALSE)
# 绘图
# 绘制热图并添加显著性标记
p <- ggplot(result_df, aes(x = bacterium, y = phenotype, fill = beta)) +
    geom_tile(width = 0.9, height = 0.9) + # 调整方块大小
    geom_text(aes(label = sig_p), color = "black", size = 4, vjust = 1.5) + # 显著性标注
    scale_fill_gradient2(
        low = "#0509f4", # 蓝色
        mid = "#FFFFFF", # 白色
        high = "#f80808", # 红色
        midpoint = 0, # 中间值
        limits = c(-0.25, 0.25), # 设置颜色范围
        name = "Estimate" # 图例标题
    ) +
    theme_minimal() + # 使用简洁主题
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # 标题样式
        axis.title = element_text(size = 14, face = "bold"), # 轴标题样式
        axis.text = element_text(size = 12), # 轴标签样式
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # x轴标签倾斜
        axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5), # y轴标签样式
        legend.title = element_text(size = 12, face = "bold"), # 图例标题样式
        legend.text = element_text(size = 10), # 图例文字样式
        legend.position = "right", # 图例位置
        panel.grid.major = element_line(color = "gray90", linewidth = 0.2), # 添加网格线
        panel.grid.minor = element_blank(), # 去掉次要网格线
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # 添加边框
        panel.background = element_rect(fill = "white") # 背景颜色
    ) +
    labs(
        title = "", # 图标题
        x = "", # x轴标题
        y = "" # y轴标题
    )

ggsave(p, file = "主要结果\\图片\\Fig 4-b. continute_genus_diabetes_heatmap.pdf", width = 8, height = 4, dpi = 300) # 保存图片
# ============7. 中介分析===================================
# 清除环境
rm(list = ls())

# 加载必要的包和函数
source("代码\\myfunctions.r")
library(tidyverse) # 数据处理和可视化
# pak::pkg_install("mediation") # 安装 mediation 包
library(mediation) # mediation 用于中介分析
# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")

genus_data$transformed$SampleID <- row.names(genus_data$transformed)

# 合并数据
data <- imputed_df_v6 %>%
    merge(., genus_data$transformed, by = "SampleID") %>%
    mutate(Mn_quartiles = relevel(Mn_quartiles, ref = "Q2"))

# ============基于四分位数===================
covariate <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)
# =====血清锰四分位数——菌——疾病中介效应===========
# 提取所有需要用到的变量
vars_needed <- c(
    "g_Sediminibacterium",
    "g_Bradyrhizobium",
    "g_Ralstonia",
    "g_Acidaminococcus",
    "Mn_quartiles", "IFG_or_diabetes", covariate
)

# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, which(colnames(data) %in% vars_needed)])
# save(complete_data, file = "其他数据\\complete_data.RData")

#########  血清锰四分位数——g_Sediminibacterium——IFG_or_diabetes的中介效应
# 自变量对中介变量的影响
formular1 <- create_formula("g_Sediminibacterium", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + g_Sediminibacterium", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles", # 自变量
    mediator = "g_Sediminibacterium", # 中介变量
    treat.value = "Q4", # 处理组
    control.value = "Q2", # 对照组
    boot = TRUE, # 启用自助法
    sims = 1000 # 自助法模拟次数
)
names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

########## 血清锰四分位数——g_Ralstonia——IFG_or_diabetes的中介效应
# 提取所有需要用到的变量
vars_needed <- c("g_Ralstonia", "Mn_quartiles", "IFG_or_diabetes", covariate)

# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, c("g_Ralstonia", "Mn_quartiles", "IFG_or_diabetes", covariate)])

# 自变量对中介变量的影响
formular1 <- create_formula("g_Ralstonia", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + g_Ralstonia", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)

# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "g_Ralstonia",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

########## 血清锰四分位数——g_Acidaminococcus——IFG_or_diabetes的中介效应
# 自变量对中介变量的影响
formular1 <- create_formula("g_Acidaminococcus", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + g_Acidaminococcus", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)

# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "g_Acidaminococcus",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

###### 血清锰四分位数——g_Bradyrhizobium——疾病的中介效应分析==========
# 自变量对中介变量的影响
formular1 <- create_formula("g_Bradyrhizobium", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + g_Bradyrhizobium", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)

# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "g_Bradyrhizobium",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间


####### ======血清锰四分位数——g_Acidaminococcus——疾病的中介效应分析==========
# 血清锰四分位数——Richness指数——IFG_or_diabetes的中介效应
# 提取所有需要用到的变量
vars_needed <- c("Richness", "Mn_quartiles", "IFG_or_diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Richness", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + Richness", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Richness",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间
# 血清锰四分位数——shannon指数——IFG_or_diabetes
# 提取所有需要用到的变量
vars_needed <- c("Shannon", "Mn_quartiles", "IFG_or_diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Shannon", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + Shannon", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Shannon",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间
# 血清锰四分位数——Pielou指数——IFG_or_diabetes
# 提取所有需要用到的变量
vars_needed <- c("Pielou", "Mn_quartiles", "IFG_or_diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Pielou", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + Pielou", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Pielou",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

# names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

# 血清锰四分位数——Simpson指数——IFG_or_diabetes
# 提取所有需要用到的变量
vars_needed <- c("Simpson", "Mn_quartiles", "IFG_or_diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Simpson", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("IFG_or_diabetes", "Mn_quartiles + Simpson", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("IFG_or_diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Simpson",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

# names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

# 血清锰四分位数——Richness指数——diabetes的中介效应
# 提取所有需要用到的变量
vars_needed <- c("Richness", "Mn_quartiles", "diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Richness", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("diabetes", "Mn_quartiles + Richness", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Richness",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

# 血清锰四分位数——Shannon指数——diabetes
# 提取所有需要用到的变量
vars_needed <- c("Shannon", "Mn_quartiles", "diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Shannon", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("diabetes", "Mn_quartiles + Shannon", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Shannon",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

# 血清锰四分位数——Pielou指数——diabetes
# 提取所有需要用到的变量
vars_needed <- c("Pielou", "Mn_quartiles", "diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Pielou", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("diabetes", "Mn_quartiles + Pielou", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Pielou",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

# 血清锰四分位数——Simpson指数——diabetes
# 提取所有需要用到的变量
vars_needed <- c("Simpson", "Mn_quartiles", "diabetes", covariate)
# 创建完整数据集（删除任何包含NA的行）
complete_data <- na.omit(data[, vars_needed])
# 自变量对中介变量的影响
formular1 <- create_formula("Simpson", "Mn_quartiles", covariate)
# 中介变量、自变量对因变量的影响
formular2 <- create_formula("diabetes", "Mn_quartiles + Simpson", covariate)
# 自变量对因变量的影响
formular3 <- create_formula("diabetes", "Mn_quartiles", covariate)

# 使用完整数据重新拟合模型
model.m <- lm(formular1, data = complete_data) # 拟合中介模型
model.y <- glm(formular2, data = complete_data, family = "binomial") # 拟合因果模型
model.t <- glm(formular3, data = complete_data, family = "binomial") # 拟合总效应模型
summary(model.m)
summary(model.y)
summary(model.t)
# 进行中介分析
# 指定 Q4 作为处理组，Q2 作为对照组进行比较
result <- mediate(model.m, model.y,
    treat = "Mn_quartiles",
    mediator = "Simpson",
    treat.value = "Q4",
    control.value = "Q2",
    boot = TRUE,
    sims = 1000
)

names(result)
summary(result) # 查看中介分析结果
summary(result)$d.avg.ci # 平均中介效应置信区间
summary(result)$z.avg.ci # 平均直接效应置信区间
summary(result)$n.avg.ci # 平均中介比例置信区间
summary(result)$tau.ci # 平均总效应置信区间

# =========血清锰四分位数，IFG_or_diabetes, g_Ralstonia的绝对丰度分布====================
# 清空当前环境中的所有变量
rm(list = ls())

# 加载所需的R包及自定义函数
source("代码\\myfunctions.r") # 加载自定义函数文件

# 加载必要的R库
library(summarytools) # 数据汇总工具
library(gtsummary) # 创建描述统计表
library(flextable) # 格式化表格
library(officer) # 生成Office文档
library(report) # 自动生成统计报告
# install.packages("dunn.test") # 如果未安装，请取消注释进行安装
library(dunn.test) # Dunn检验

# 加载数据
load("数据\\imputed_df_v6.RData") # 加载RData数据文件
data <- imputed_df_v6 # 将数据赋值给变量data
rm(imputed_df_v6) # 删除原始变量，释放内存
colnames(data) # 检查数据的列名

# 定义需要进行描述统计的变量
vars <- c(
    "IFG_or_diabetes", # 是否为糖尿病或糖调节受损
    "Mn_quartiles", # 血清锰四分位分组
    "g_Ralstonia.x", # 微生物g_Ralstonia.x
    "g_Sediminibacterium.x" # 微生物g_Sediminibacterium.x
)

# 创建描述统计表1（按血清锰四分位分组）
table1 <- data %>%
    select(all_of(vars[2:4])) %>% # 筛选需要的变量
    tbl_summary(
        by = "Mn_quartiles", # 按 "Mn_quartiles" 分组
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})", # 连续变量显示中位数及四分位数范围
            all_categorical() ~ "{n} ({p})" # 分类变量显示频数及百分比
        ),
        missing = "no", # 不显示缺失值
        percent = "column", # 百分比按列计算
        digits = list(
            all_categorical() ~ c(0, 2), # 分类变量显示0位小数（频数）和2位小数（百分比）
            all_continuous() ~ 2 # 连续变量保留2位小数
        )
    ) %>%
    modify_header(label = "**Variable**") %>% # 修改变量列的表头
    bold_labels() # 加粗标签
# print(table1) # 如果需要查看表格，可以取消注释

# 创建描述统计表2（按是否为糖尿病或糖调节受损分组）
table2 <- data %>%
    select(all_of(vars[c(1, 3, 4)])) %>% # 筛选需要的变量
    tbl_summary(
        by = "IFG_or_diabetes", # 按 "IFG_or_diabetes" 分组
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})", # 连续变量显示中位数及四分位数范围
            all_categorical() ~ "{n} ({p})" # 分类变量显示频数及百分比
        ),
        missing = "no", # 不显示缺失值
        percent = "column", # 百分比按列计算
        digits = list(
            all_categorical() ~ c(0, 2), # 分类变量显示0位小数（频数）和2位小数（百分比）
            all_continuous() ~ 2 # 连续变量保留2位小数
        )
    ) %>%
    modify_header(label = "**Variable**") %>% # 修改变量列的表头
    bold_labels() # 加粗标签
# print(table2) # 如果需要查看表格，可以取消注释

# 水平合并表格
merged_table <- tbl_merge(
    tbls = list(table1, table2),
    tab_spanner = c("**Mn quartiles**", "**IFG or Diabetes Status**")
)

# Print merged table
# print(merged_table)
# 将表格转换为flextable对象
flextable1 <- as_flex_table(merged_table)

# 创建一个Word文档并添加表格
doc <- read_docx() %>%
    body_add_flextable(flextable1)

# 保存Word文档
print(doc, target = "其他结果\\表格\\g_Ralstonia绝对丰度在血清锰或IFG_diabetes的分布.docx")
# =========检验===================================================================
# 从 IFG_or_diabetes 数据集中删除缺失值
data_clean1 <- data %>%
    filter(!is.na(IFG_or_diabetes))

# g_Ralstonia与Mn四分位数 Kruskal-Wallis 检验
# Kruskal-Wallis 检验
kruskal_result <- kruskal.test(g_Ralstonia.x ~ Mn_quartiles, data = data_clean1)
print(kruskal_result) # 打印结果

# 事后检验
dunn_result <- dunn.test(
    data_clean1$g_Ralstonia.x,
    data_clean1$Mn_quartiles,
    method = "bh", # 使用 Benjamini-Hochberg 方法进行多重比较校正
    alpha = 0.05, # 显著性水平
    list = TRUE # 返回结果列表
)
print(dunn_result) # 打印结果

# g_Sediminibacterium 与 Mn 四分位数的 Kruskal-Wallis 检验
kruskal_result_sediminibacterium <- kruskal.test(g_Sediminibacterium.x ~ Mn_quartiles, data = data_clean1)
print(kruskal_result_sediminibacterium)

# 事后检验
# 使用 Dunn 检验进行事后比较
dunn_result_sediminibacterium <- dunn.test(
    data_clean1$g_Sediminibacterium.x,
    data_clean1$Mn_quartiles,
    method = "bh",
    alpha = 0.05,
    list = TRUE
)

print(dunn_result_sediminibacterium)

# g_Ralstonia 与 IFG_or_diabetes 的 wilcoxon 检验
wilcoxon_result <- wilcox.test(g_Ralstonia.x ~ IFG_or_diabetes, data = data_clean1)
print(wilcoxon_result) # 打印结果

# g_Sediminibacterium 与 IFG_or_diabetes 的 wilcoxon 检验
wilcoxon_result_sediminibacterium <- wilcox.test(g_Sediminibacterium.x ~ IFG_or_diabetes, data = data_clean1)
print(wilcoxon_result_sediminibacterium) # 打印结果
# =========绘图====================================================================
# 绘制箱图
# 加载用于可视化和统计分析所需的库
library(ggplot2)
library(ggsci) # 关于科学色彩配色方案
library(ggpubr) # 关于添加 p 值及显著性水平的说明
library(dplyr)
library(rstatix) # 对于统计检验

# ==========箱线图 1：g_Ralstonia 依据 IFG 或糖尿病状况分类===============
# 为 IFG_or_diabetes 创建箱线图
p1 <- ggplot(data_clean1, aes(x = IFG_or_diabetes, y = g_Ralstonia.x, fill = IFG_or_diabetes)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
    scale_fill_nejm() + # NEJM color palette from ggsci
    labs(
        title = "Distribution of g_Ralstonia Abundance by Diabetes Status",
        x = "IFG or Diabetes Status",
        y = "g_Ralstonia Abundance"
    ) +
    theme_classic() +
    theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    geom_signif(
        comparisons = list(c(
            levels(factor(data_clean1$IFG_or_diabetes))[1],
            levels(factor(data_clean1$IFG_or_diabetes))[2]
        )),
        map_signif_level = TRUE,
        test = "t.test",
        textsize = 3.5,
        size = 0.5
    )

# 打印并保存该图表
print(p1)
ggsave("其他结果\\图片\\g_Ralstonia_by_diabetic_status.pdf", p1, width = 6, height = 8, units = "in", dpi = 300)

# 箱线图 2：锰分位数分类下的 Ralstonia 细菌群落分布
# 定义用于事后检验的两两比较标准
my_comparisons <- list(
    c("Q1", "Q2"), c("Q1", "Q3"), c("Q1", "Q4"),
    c("Q2", "Q3"), c("Q2", "Q4"), c("Q3", "Q4")
)
# 为 Mn_quartiles 创建箱线图
p2 <- ggplot(data, aes(x = Mn_quartiles, y = g_Ralstonia.x, fill = Mn_quartiles)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
    scale_fill_jco() + # JCO color palette from ggsci
    labs(
        title = "Distribution of g_Ralstonia Abundance by Manganese Quartiles",
        x = "Manganese Quartiles",
        y = "g_Ralstonia Abundance"
    ) +
    theme_classic() +
    theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    ) +
    # Add ANOVA p-value at the top
    stat_compare_means(method = "anova", label.y = max(data$g_Ralstonia.x, na.rm = TRUE) * 1.5, label.x = 1) +
    # Use geom_signif for pairwise comparisons
    geom_signif(
        comparisons = my_comparisons,
        map_signif_level = TRUE,
        test = "t.test",
        textsize = 3.5,
        size = 0.5,
        step_increase = 0.1
    )

# 打印并保存该图表
print(p2)
ggsave("其他结果\\图片\\g_Ralstonia_by_Mn_quartiles.pdf", p2, width = 8, height = 12, units = "in", dpi = 300)
# ==========处理成机器学习所需数据形式==================================
rm(list = ls()) # 清空工作空间

# 读取r包及自编函数
source("代码\\myfunctions.r")

# 读取数据
# ID\自变量\协变量\结局\绝对丰度\相对丰度-对数-标准化: imputed_df_v6
load("数据\\imputed_df_v6.RData")
colnames(imputed_df_v6)

# 去掉除IFG_or_diabetes之外的结局变量、多样性指数、绝对丰度: df1
names <- c(
    "FPG", "ln_FPG", "ln_hba1c", "illness", "diabetes", "hba1c",
    "Richness", "Shannon", "Pielou", "Simpson",
    "Serum_Mn", "ln_Serum_Mn"
)

df1 <- imputed_df_v6[, -which(colnames(imputed_df_v6) %in% names)]
# 去除绝对丰度
df1 <- df1[, -grep("\\.x$", colnames(df1))]
colnames(df1)
# 查看数据结构
str(df1)
# 变量类型转换
# 将连续型变量进行标准化
df1$age <- scale(df1$age)
df1$BMI <- scale(df1$BMI)
df1$energy_kcal <- scale(df1$energy_kcal)
df1$diet_Mn_mg <- scale(df1$diet_Mn_mg)

# 设置哑变量，并去掉第一个哑变量
categorical_vars <- c(
    "gender", "Marital_status", "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption", "act_sport"
)

# 转换为因子
# df1[categorical_vars] <- lapply(df1[categorical_vars], as.factor)

# 创建虚拟变量（默认情况下会删除每个因子的第一个水平）
dummies <- model.matrix(
    ~ gender + Marital_status + education + family_income_cat +
        family_diabetes_history + smoker + Alcohol_consumption + act_sport,
    data = df1
)
dummies <- dummies[, -1] # 删除截距列

# 在数据框中添加虚拟变量并删除原始的分类变量
df1 <- cbind(df1[, !colnames(df1) %in% categorical_vars], dummies)

# 删除列名中的“.y”后缀
colnames(df1) <- gsub("\\.y$", "", colnames(df1))

# 因变量转换为数字型
df1$IFG_or_diabetes <- as.numeric(df1$IFG_or_diabetes)
df1$Mn_quartiles <- as.numeric(df1$Mn_quartiles)
# 导出用于机器学习的数据，命名为"lasso机器学习数据.csv"
write.csv(df1, file = "数据/lasso机器学习数据.csv", row.names = FALSE, fileEncoding = "UTF-8")
# =======机器学习筛选出的特征菌与血清锰四分位数以及IFG_or_diabetes的回归分析========================================
rm(list = ls())

# 读取r包及自编函数
source("代码\\myfunctions.r", chdir = TRUE)

# 读取数据
load("数据\\imputed_df_v6.RData", envir = parent.frame(), verbose = FALSE)

# 读取机器学习识别的特征菌，df_genus_name
df_genus_name <- read.csv("数据\\ridge_与血清锰_IFG_or_diabetes相关的肠道菌名称.csv",
    header = TRUE, stringsAsFactors = FALSE, check.names = FALSE
)

# 给机器学习识别出的特征菌名称加上后缀".y"
df_genus_name$`Selected Microbes` <- paste0(df_genus_name$`Selected Microbes`, ".y")

# 查看列名
colnames(imputed_df_v6)

# 设置血清锰四分位数的参照水平为Q2
imputed_df_v6$Mn_quartiles <- relevel(imputed_df_v6$Mn_quartiles, ref = "Q2")

# 血清锰四分位数与机器学习识别出的特征菌之间的关联
# 设置因变量名称
y_names <- df_genus_name[, 1]

# 设置自变量名称
x_names <- "Mn_quartiles"

# 设置协变量名称
covariate <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

result <- reg_log_lin(
    exposure_vars = x_names,
    outcome_vars = y_names,
    covariate_vars = covariate,
    data = imputed_df_v6
)

names(result)
# View(result$result_df)
# 保存结果
write.csv(
    result$result_df,
    file = "主要结果\\表格\\血清锰四分位数与机器学习识别出的特征菌之间的关联.csv",
    row.names = FALSE, fileEncoding = "UTF-8"
)

# 机器学习识别出的特征菌与IFG_or_diabetes之间的关联
# 设置因变量名称
y_names <- c("IFG_or_diabetes", "diabetes")

# 设置自变量名称
x_names <- df_genus_name[, 1]

# 回归
result <- reg_log_lin(
    exposure_vars = x_names,
    outcome_vars = y_names,
    covariate_vars = covariate,
    data = imputed_df_v6
)

# View(result$result_df)
# 保存结果
write.csv(
    result$result_df,
    file = "主要结果\\表格\\机器学习识别出的特征菌与IFG_or_diabetes之间的关联.csv",
    row.names = FALSE, fileEncoding = "UTF-8"
)
# ========绘制抽样调查地图=================================
# rm(list = ls()) # 清空环境变量
# # 读取原始数据
# df_original <- read.csv("D:\\lianghao\\Documents\\流行病与卫生统计学研究生\\金属锰—肠道菌—疾病\\analysisdata.csv",
#     header = TRUE, sep = ",", fileEncoding = "GBK"
# )

# # 读取总数据
# load("数据\\imputed_df_v6.RData")
# data <- imputed_df_v6

# colnames(df_original)[1:10]
# head(df_original[, 1:10])
# names <- c("SampleID", "dspName", "Town_Name", "vil_name") # 有关地理位置的信息：省，市，县, 街道

# # 去除原始的地理信息
# # data <- data[, -which(names(data) %in% names[-1])]

# # 合并数据
# data <- merge(data, df_original[, names], by = "SampleID", all.x = TRUE)

# imputed_df_v6 <- data
# # 保存数据
# # save(imputed_df_v6, file = "数据\\imputed_df_v6.RData")

# # 拆分为省、市、县
# df_split <- data %>%
#     separate(dspName, into = c("省", "市", "县"), sep = "-", extra = "drop")
# # extra = "drop" # 如果有多余的部分，忽略它们

# # 查看拆分结果
# head(df_split)
# # 地理编码（百度API）
# library(httr) # httr包用于处理HTTP请求
# library(jsonlite) # jsonlite包用于处理JSON数据

# # 定义地理编码函数（百度API）
# baidu_geocode <- function(address, ak) {
#     url <- "http://api.map.baidu.com/geocoding/v3/" # 百度地理编码API的URL
#     # 确保地址是UTF-8编码
#     address_encoded <- enc2utf8(address) # 将地址编码为UTF-8
#     response <- GET(url, query = list(
#         address = address_encoded, # 使用UTF-8编码的地址
#         output = "json", # 返回格式为JSON
#         ak = ak # 替换为你的百度API密钥
#     )) # 发送GET请求到百度地理编码API
#     # 明确指定UTF-8编码
#     content <- fromJSON(content(response, "text", encoding = "UTF-8")) # 解析响应内容为JSON格式
#     if (content$status == 0) {
#         lng <- content$result$location$lng # 获取经度
#         lat <- content$result$location$lat # 获取纬度
#         return(c(lng, lat)) # 返回经纬度
#     } else {
#         return(c(NA, NA)) # 如果请求失败，返回NA
#     }
# }

# # 为每个社区生成完整地址并编码
# ak <- "CJKID8ElVNu4vEA27VMjDU9xFmITlKNE" # 替换为你的AK

# # 确保所有地址字段使用UTF-8编码
# df_split <- df_split %>%
#     mutate(
#         省 = enc2utf8(省), # 确保省字段是UTF-8编码
#         市 = enc2utf8(市), # 确保市字段是UTF-8编码
#         县 = enc2utf8(县), # 确保县字段是UTF-8编码
#         Town_Name = enc2utf8(Town_Name), # 确保Town_Name字段是UTF-8编码
#         vil_name = enc2utf8(vil_name), # 确保vil_name字段是UTF-8编码
#         完整地址 = paste(省, 市, 县, Town_Name, vil_name), # 生成完整地址
#         geocode = purrr::map(完整地址, ~ baidu_geocode(.x, ak)) # 使用purrr包的map函数对每个地址进行地理编码
#     ) %>%
#     unnest_wider(geocode, names_sep = "_") %>% # 将地理编码结果展开为两列
#     rename(经度 = geocode_1, 纬度 = geocode_2) # 重命名经纬度列

# # 查看结果（包含经纬度）
# df_split <- as.data.frame(df_split) # 确保是数据框格式
# write.csv(df_split, file = "数据\\df_split.csv", row.names = FALSE, fileEncoding = "UTF-8") # 保存拆分后的数据

# # 手动整理完数据后再读入
# df_split <- read.csv("数据\\df_split.csv", fileEncoding = "GBK") # 读取拆分后的数据
# colnames(df_split)[113:120]

# names <- c(
#     "SampleID", "省", "市", "县", "Town_Name", "vil_name", "完整地址",
#     "经度", "纬度"
# )

# # 提取地理信息
# df_split <- df_split[, names]
# data <- data[, -which(names(data) %in% c("Town_Name", "vil_name"))]
# # 合并数据
# data <- merge(data, df_split, by = "SampleID", all.x = TRUE)
# imputed_df_v6 <- data
# save(imputed_df_v6, file = "数据\\imputed_df_v6.RData")
# colnames(imputed_df_v6)

# # 数据可视化
# # 读取数据
# rm(list = ls())

# load("数据\\imputed_df_v6.RData")
# # 加载必要的库# 加载必要的库
# library(ggplot2) # ggplot2用于数据可视化
# library(sf) # sf用于处理空间数据
# library(tmap) # tmap用于制作地图
# library(jsonlite) # jsonlite用于处理JSON数据
# # library(rnaturalearth) # rnaturalearth用于获取自然地理数据
# # library(rnaturalearthhires) # rnaturalearthhires用于获取高分辨率自然地理数据
# library(ggspatial) # ggspatial用于添加空间元素到ggplot
# library(viridis) # viridis用于生成色标
# library(dplyr) # dplyr用于数据处理

# # 加载数据
# df_map <- imputed_df_v6 # 使用之前处理好的数据
# guangdong_shapefile <- st_read("数据\\广东省.json") # 读取广东省的地理数据

# # 转换数据为 sf 对象并设置投影（WGS84 未投影）
# data_sf <- st_as_sf(df_map, coords = c("经度", "纬度"), crs = 4326) # coords指定经纬度列，crs指定坐标参考系统为WGS84

# # 投影转换：阿尔伯斯等面积投影（适合中国）
# data_sf <- st_transform(data_sf, crs = "+proj=aea +lat_1=25 +lat_2=47 +lon_0=105")
# guangdong_shapefile <- st_transform(guangdong_shapefile, crs = "+proj=aea +lat_1=25 +lat_2=47 +lon_0=105")

# # 生成分县色标
# county_levels <- unique(df_count$县) # 确保县的唯一性
# county_colors <- setNames(viridis(length(county_levels)), county_levels) # 使用viridis生成颜色，确保颜色对比度良好

# # 定义美化后的地图
# p <- ggplot() +
#     geom_sf(data = guangdong_shapefile, fill = "white", color = "black") + # 绘制广东省边界
#     geom_sf(data = data_sf, aes(color = 县), size = 0.7, alpha = 0.8) + # 绘制样本点
#     scale_color_manual(values = county_colors) + # 使用自定义颜色
#     annotation_north_arrow( # 添加指北针
#         location = "tl", which_north = "true", # location在左上角, which_north为真北
#         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"), # pad_x和pad_y设置指北针位置偏移
#         style = north_arrow_fancy_orienteering, # style为样式
#         height = unit(1, "cm"), width = unit(1, "cm") # 设置指北针的高度和宽度
#     ) +
#     annotation_scale(location = "br", width_hint = 0.5) + # 添加比例尺
#     coord_sf(expand = FALSE) + # expand = FALSE指定地图应该精确地填充整个绘图区域，不添加额外的边距。
#     theme_minimal() + # 使用最小主题
#     theme(
#         panel.grid.major = element_line(color = "gray80", linetype = "dashed"), # 主网格线为浅灰色虚线
#         panel.grid.minor = element_blank(), # 去除次网格线
#         panel.background = element_rect(fill = "white", colour = "gray80"), # 面板背景为白色，边框为浅灰色
#         plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # 标题居中，大小为16，加粗
#         legend.position = "bottom", # 图例位置在底部
#         legend.title = element_blank(), # 图例标题为空
#         legend.text = element_text(size = 8) # 图例文本大小为8
#     ) +
#     labs(
#         title = "广东省抽样调查地图", # 地图标题
#         x = "经度", # x轴标签
#         y = "纬度" # y轴标签
#     )
# print(p)
# # 保存地图为PDF格式
# # if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext") # 如果未安装showtext包，则安装它
# library(showtext) # 加载showtext包以支持中文字体
# font_add("SimSun", "C:/Windows/Fonts/simsun.ttc") # 添加宋体字体
# showtext_auto() # 自动启用showtext

# ggsave(p,
#     file = "主要结果\\图片\\广东省抽样调查地图.pdf",
#     width = 8, height = 6, dpi = 300, device = cairo_pdf
# )

# showtext_auto(FALSE) # 关闭showtext自动启用

# # 绘制柱状图
# # 数据计数
# df_count <- df_map %>%
#     group_by(县) %>% # 按县分组
#     summarise(count = n()) %>% # 计算每个县的样本数量
#     arrange(desc(count)) # 按样本数量降序排列

# df_count <- as.data.frame(df_count) # 确保是数据框格式

# font_add("SimSun", "C:/Windows/Fonts/simsun.ttc") # 添加宋体字体
# showtext_auto() # 自动启用showtext

# # 建议先将县名设置为因子，保证顺序一致
# df_count$县 <- factor(df_count$县, levels = df_count$县[order(df_count$count, decreasing = TRUE)]) # 按照样本数量降序排列县名

# p_bar <- ggplot(df_count, aes(x = 县, y = count, fill = 县)) +
#     geom_bar(stat = "identity", width = 0.5, color = NA) + # width更小，条更细，间距更大
#     scale_fill_manual(values = county_colors) +
#     coord_flip(clip = "off") + # 防止标签被裁剪
#     labs(
#         title = "各县样本数量分布",
#         x = NULL, # Nature风格去除x/y标题
#         y = NULL
#     ) +
#     theme_classic() +
#     theme(
#         plot.background = element_rect(fill = "white", color = NA),
#         panel.background = element_rect(fill = "white", color = NA),
#         panel.grid.major.y = element_blank(), # 去横向主网格
#         panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_line(color = "gray90", size = 0.3), # x主网格浅色
#         axis.text.x = element_text(size = 11, color = "black"),
#         axis.text.y = element_text(size = 11, color = "black"),
#         plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
#         legend.position = "none",
#         plot.margin = margin(10, 20, 10, 10) # 适度留白
#     )

# # 可选：添加数量标签
# p_bar <- p_bar +
#     geom_text(aes(label = count), hjust = -0.15, size = 3.5, color = "black")

# # 推荐宽高比（Nature常用4:3或更窄）：宽8cm高6cm
# ggsave(p_bar, file = "主要结果/图片/广东省样本分布_bar_nature.pdf", width = 8, height = 6, units = "cm", dpi = 600)
# warnings()
# print(p_bar)
# cope with the map data
rm(list = ls())
library(tidyverse)
# Install sqldf if not already installed
if (!requireNamespace("sqldf", quietly = TRUE)) {
    install.packages("sqldf")
}
library(sqldf)

# load the analysis data
load("数据\\imputed_df_v7.RData")
colnames(imputed_df_v7)

df <- sqldf("select 市 as name, count(*) as count
             from imputed_df_v7
             group by 市")

View(df)
write.csv(df, "数据\\广东省各市样本数量.csv", row.names = FALSE, fileEncoding = "UTF-8")
# ==========中介菌与IFG_or_diabetes的RCS========================
# 清空工作空间
rm(list = ls())

# 加载必要的包和函数
source("代码\\myfunctions.r", chdir = TRUE)
library(rms)
library(ggplot2)
# library(ggrcs)
# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData") # 假设 genus_data 包含了 g_Ralstonia 和 g_Sediminibacterium 的数据

# 数据处理
data <- imputed_df_v6
row.names(data) <- data$SampleID # 设置 SampleID 为行名
rm(imputed_df_v6) # 删除原始数据以释放内存

genus_data$relative$SampleID <- row.names(genus_data$relative) # 设置 SampleID 为行名
# 合并数据
data <- merge(data, genus_data$relative, by = "SampleID")

# 创建 datadist 对象
dd <- datadist(data)
options(datadist = "dd")

# 定义协变量和因变量
covariates <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)
response <- "IFG_or_diabetes"

##### g_Sediminibacterium
# 构建模型公式并拟合模型
formula <- create_formula(response, "rcs(g_Sediminibacterium, 3)", covariates)
model <- lrm(formula, data = data)

# 模型结果
anova_result <- anova(model)
anova_df <- as.data.frame(anova_result)

# 生成预测值（调整为 OR）
OR <- Predict(model, g_Sediminibacterium, fun = exp, ref.zero = TRUE)
View(OR)
# 找到特定点（y = 1）对应的 x 值
x_at_y_1 <- c(0.0054) # 可修改为动态计算

# 绘图
p <- ggplot() +
    # 绘制预测曲线
    geom_line(data = OR, aes(g_Sediminibacterium, yhat), linetype = 1, linewidth = 0.3, alpha = 0.9, colour = "skyblue") +
    # 添加置信区间
    geom_ribbon(data = OR, aes(g_Sediminibacterium, ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    # 添加水平线 y = 1
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    # 添加竖线 x = x_at_y_1
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") +
    # 标注竖线位置
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("g_Sediminibacterium = ", round(x_at_y_1, 3)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) +
    # 设置主题
    theme_bw(base_size = 9) +
    # 添加坐标轴标签
    labs(x = "g_Sediminibacterium", y = "IFG or diabetes, Adjusted OR (95%CI)") +
    # 添加显著性标注
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(anova_df$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 2
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(anova_df$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 2
    )

# 显示图形
print(p)
ggsave(
    filename = "主要结果\\图片\\RCS\\RCS_g_Sediminibacterium vs IFG_or_diabetes.pdf",
    plot = p, width = 2.54, height = 3, units = "in", dpi = 300, device = cairo_pdf
)

####### g_Bradyrhizobium
# 构建模型公式并拟合模型
formula <- create_formula(response, "rcs(g_Bradyrhizobium, 3)", covariates)
model <- lrm(formula, data = data)

# 模型结果
# summary(model)
anova_result <- anova(model)
anova_df <- as.data.frame(anova_result)

# 生成预测值（调整为 OR）
OR <- Predict(model, g_Bradyrhizobium, fun = exp, ref.zero = TRUE)
View(OR)
# 找到特定点（y = 1）对应的 x 值
x_at_y_1 <- c(0.0021) # 可修改为动态计算

# 绘图
p <- ggplot() +
    # 绘制预测曲线
    geom_line(data = OR, aes(g_Bradyrhizobium, yhat), linetype = 1, linewidth = 1, alpha = 0.9, colour = "#005BAC") +
    # 添加置信区间
    geom_ribbon(data = OR, aes(g_Bradyrhizobium, ymin = lower, ymax = upper), alpha = 0.1, fill = "skyblue") +
    # 添加水平线 y = 1
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    # 添加竖线 x = x_at_y_1
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") +
    # 标注竖线位置
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("g_Bradyrhizobium = ", round(x_at_y_1, 3)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) +
    # 设置主题
    theme_bw(base_size = 9) +
    # 添加坐标轴标签
    labs(x = "g_Bradyrhizobium", y = "Adjusted OR (95% CI)") +
    # 添加显著性标注
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(anova_df$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 5
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(anova_df$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 5
    )

# 显示图形
print(p)
ggsave(
    filename = "主要结果\\图片\\RCS\\g_Bradyrhizobium vs IFG_or_diabetes.pdf",
    plot = p, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

##### g_Ralstonia
# 构建模型公式并拟合模型
formula <- create_formula(response, "rcs(g_Ralstonia, 3)", covariates)
model <- lrm(formula, data = data)

# 模型结果
# summary(model)
anova_result <- anova(model)
anova_df <- as.data.frame(anova_result)

# 生成预测值（调整为 OR）
OR <- Predict(model, g_Ralstonia, fun = exp, ref.zero = TRUE)
View(OR)
# 找到特定点（y = 1）对应的 x 值
x_at_y_1 <- c(0.0128) # 可修改为动态计算

# 绘图
p <- ggplot() +
    # 绘制预测曲线
    geom_line(data = OR, aes(g_Ralstonia, yhat), linetype = 1, linewidth = 0.3, alpha = 0.9, colour = "skyblue") +
    # 添加置信区间
    geom_ribbon(data = OR, aes(g_Ralstonia, ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    # 添加水平线 y = 1
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2) +
    # 添加竖线 x = x_at_y_1
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.2, colour = "red") +
    # 标注竖线位置
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("g_Ralstonia = ", round(x_at_y_1, 3)),
        hjust = -0.1, vjust = 1.1, size = 2, colour = "red"
    ) +
    # 设置主题
    theme_bw(base_size = 9) +
    # 添加坐标轴标签
    labs(x = "g_Ralstonia", y = "IFG or diabetes, Adjusted OR (95% CI)") +
    # 添加显著性标注
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(anova_df$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 2
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(anova_df$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 2
    )

# 显示图形
print(p)
ggsave(
    filename = "主要结果\\图片\\RCS\\g_Ralstonia vs IFG_or_diabetes.pdf",
    plot = p, width = 2.54, height = 3, units = "in", dpi = 300, device = cairo_pdf
)

##### g_Acidaminococcus
# 构建模型公式并拟合模型
formula <- create_formula(response, "rcs(g_Acidaminococcus, 3)", covariates)
model <- lrm(formula, data = data)

# 模型结果
anova_result <- anova(model)
anova_df <- as.data.frame(anova_result)

# 生成预测值（调整为 OR）
OR <- Predict(model, g_Acidaminococcus, fun = exp, ref.zero = TRUE)
View(OR)

# 找到特定点（y = 1）对应的 x 值
# x_at_y_1 <- c(0.0003) # 可修改为动态计算

# 绘图
p <- ggplot() +
    # 绘制预测曲线
    geom_line(data = OR, aes(g_Acidaminococcus, yhat), linetype = 1, linewidth = 1, alpha = 0.9, colour = "#005BAC") +
    # 添加置信区间
    geom_ribbon(data = OR, aes(g_Acidaminococcus, ymin = lower, ymax = upper), alpha = 0.3, fill = "#005BAC") +
    # 添加水平线 y = 1
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.5) +
    # 添加竖线 x = x_at_y_1
    geom_vline(xintercept = x_at_y_1, linetype = 2, linewidth = 0.5, colour = "red") +
    # 标注竖线位置
    annotate("text",
        x = x_at_y_1, y = 1,
        label = paste0("g_Acidaminococcus = ", round(x_at_y_1, 2)),
        hjust = -0.1, vjust = 1.1, size = 4, colour = "red"
    ) +
    # 设置主题
    theme_classic() +
    theme(
        text = element_text(size = 15),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    ) +
    # 添加坐标轴标签
    labs(x = "g_Acidaminococcus", y = "Adjusted OR (95% CI)") +
    # 添加显著性标注
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for overall: " ~ .(format(anova_df$`P`[1], digits = 2))),
        hjust = 1.1, vjust = 2, size = 5
    ) +
    annotate("text",
        x = Inf, y = Inf,
        label = bquote(italic("P") ~ " for non-linear: " ~ .(format(anova_df$`P`[2], digits = 2))),
        hjust = 1.1, vjust = 3.5, size = 5
    )

# 显示图形
print(p)
ggsave(
    filename = "主要结果\\图片\\RCS\\g_Acidaminococcus vs IFG_or_diabetes.pdf",
    plot = p, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

# ==========中介菌在是否患IFG or diabetes患者中的差异========
# 清空工作空间
rm(list = ls())

# 加载必要的包
library(ggplot2)
library(ggpubr)
library(agricolae)
library(gridExtra)
library(ggprism)
library(ggsignif)
source("代码\\myfunctions.r", chdir = TRUE)
# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData") # 假设 genus_data 包含了 g_Ralstonia 和 g_Sediminibacterium 的数据
data <- imputed_df_v6
rm(imputed_df_v6) # 删除原始数据以释放内存
names(genus_data)
genus_data$transformed$SampleID <- row.names(genus_data$transformed) # 设置 SampleID 为行名
data <- merge(data, genus_data$transformed, by = "SampleID")


# 定义要分析的菌群变量
bacteria_vars <- c(
    "g_Sediminibacterium",
    "g_Bradyrhizobium",
    "g_Ralstonia",
    "g_Acidaminococcus"
)

# 定义分组变量
group_var <- "IFG_or_diabetes"

# 检查数据完整性
data <- data[complete.cases(data[, c(group_var, bacteria_vars)]), ]

# 差异性分析函数
perform_analysis <- function(var, group_var, data) {
    # 进行 Mann-Whitney U 检验
    test_result <- wilcox.test(data[[var]] ~ data[[group_var]], data = data)

    # 获取两个组的唯一值
    groups <- unique(data[[group_var]])

    # 返回结果，使用小写的 group1 和 group2 列名
    return(data.frame(
        Variable = var,
        group1 = groups[1], # 注意是小写的 group1
        group2 = groups[2], # 注意是小写的 group2
        p = test_result$p.value, # p 值列名也需要是 p，而不是 P_Value
        y.position = max(data[[var]], na.rm = TRUE) * 1.1 # 直接添加 y.position
    ))
}

# 对每个菌群变量进行差异性分析
analysis_results <- do.call(rbind, lapply(bacteria_vars, perform_analysis, group_var = group_var, data = data))

# 创建一个带有大写列名的版本用于输出到 CSV
analysis_results_csv <- data.frame(
    Variable = analysis_results$Variable,
    Group1 = analysis_results$group1,
    Group2 = analysis_results$group2,
    P_Value = analysis_results$p
)

# 输出分析结果到 CSV 文件
# write.csv(analysis_results_csv, "genus_IFG_diabetes.csv", row.names = FALSE)
## g_Sediminibacterum
p_g_Sediminibacterium <- plot_violin_kruskal_dunn(
    data,
    xvar = "IFG_or_diabetes",
    yvar = "g_Sediminibacterium"
)

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Sediminibacterium vs IFG_or_diabetes.pdf",
    plot = p_g_Sediminibacterium, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)
## g_bradyrhizobium
p_g_Bradyrhizobium <- plot_violin_kruskal_dunn(
    data,
    xvar = "IFG_or_diabetes",
    yvar = "g_Bradyrhizobium"
)

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Bradyrhizobium vs IFG_or_diabetes.pdf",
    plot = p_g_Bradyrhizobium, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

## g_Ralstonia
p_g_Ralstonia <- plot_violin_kruskal_dunn(
    data,
    xvar = "IFG_or_diabetes",
    yvar = "g_Ralstonia"
)

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Ralstonia vs IFG_or_diabetes.pdf",
    plot = p_g_Ralstonia, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

## g_Acidaminococcus
p_g_Acidaminococcus <- plot_violin_kruskal_dunn(
    data,
    xvar = "IFG_or_diabetes",
    yvar = "g_Acidaminococcus"
)

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Acidaminococcus vs IFG_or_diabetes.pdf",
    plot = p_g_Acidaminococcus, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)
# ===========中介菌在血清锰四分位数中的分布差异===================
# 清空工作空间
rm(list = ls())

# 读取r包及自编函数
source("代码\\myfunctions.r", chdir = TRUE)
library(ggplot2)
library(ggpubr)
library(agricolae)
library(gridExtra)
# pak::pkg_install("ggprism")
library(ggprism)
library(ggsignif)
library(tidyverse)
# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")
data <- imputed_df_v6
rm(imputed_df_v6) # 删除原始数据以释放内存
genus_Absolute <- genus_data$absolute
genus_Relative <- genus_data$relative
genus_Relative$SampleID <- rownames(genus_Relative) # 添加 SampleID 列
genus_Absolute$SampleID <- rownames(genus_Absolute) # 添加 SampleID 列
genus_data$transformed$SampleID <- rownames(genus_data$transformed) # 添加 SampleID 列
# 定义要分析的菌群变量
bacteria_vars <- c(
    "g_Sediminibacterium",
    "g_Bradyrhizobium",
    "g_Ralstonia",
    "g_Acidaminococcus"
)

# 定义分组变量
group_var <- "Mn_quartiles"

# 检查数据完整性
data <- data[data$SampleID %in% genus_data$transformed$SampleID, ]

# 根据转换后的数据绘制小提琴图
p_g_Sediminibacterium <- data %>%
    merge(genus_data$transformed, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Sediminibacterium",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Sediminibacterium vs Mn_quartiles.pdf",
    plot = p_g_Sediminibacterium, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

p_g_Bradyrhizobium <- data %>%
    merge(genus_data$transformed, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Bradyrhizobium",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Bradyrhizobium vs Mn_quartiles.pdf",
    plot = p_g_Bradyrhizobium, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

p_g_Ralstonia <- data %>%
    merge(genus_data$transformed, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Ralstonia",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Ralstonia vs Mn_quartiles.pdf",
    plot = p_g_Ralstonia, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

p_g_Acidaminococcus <- data %>%
    merge(genus_data$transformed, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Acidaminococcus",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Acidaminococcus vs Mn_quartiles.pdf",
    plot = p_g_Acidaminococcus, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

# 根据相对丰度绘制小提琴图
p_g_Sediminibacterium_rel <- data %>%
    merge(genus_Relative, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Sediminibacterium",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Sediminibacterium_rel vs Mn_quartiles.pdf",
    plot = p_g_Sediminibacterium_rel, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

p_g_Bradyrhizobium_rel <- data %>%
    merge(genus_Relative, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Bradyrhizobium",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Bradyrhizobium_rel vs Mn_quartiles.pdf",
    plot = p_g_Bradyrhizobium_rel, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

p_g_Ralstonia_rel <- data %>%
    merge(genus_Relative, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Ralstonia",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Ralstonia_rel vs Mn_quartiles.pdf",
    plot = p_g_Ralstonia_rel, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)

p_g_Acidaminococcus_rel <- data %>%
    merge(genus_Relative, by = "SampleID", all.x = TRUE) %>%
    plot_violin_kruskal_dunn(
        .,
        xvar = "Mn_quartiles",
        yvar = "g_Acidaminococcus",
    )

ggsave(
    filename = "主要结果\\图片\\Diff_genus\\violin_plot\\g_Acidaminococcus_rel vs Mn_quartiles.pdf",
    plot = p_g_Acidaminococcus_rel, width = 6, height = 8, units = "in", dpi = 300, device = cairo_pdf
)
# ===========中介菌与血清锰及疾病的差异性分析（基于绝对丰度）===========
rm(list = ls()) # 清除环境变量

## 加载必要的包


## 加载数据
load("数据/imputed_df_v6.RData") # 加载数据
load("数据/genus_data.RData") # 加载菌群数据

## 数据处理
data <- imputed_df_v6
genus_data$absolute$SampleID <- row.names(genus_data$absolute)
data <- merge(data, genus_data$absolute, by = "SampleID")

## 定义要分析的菌群变量
bacteria_vars <- c(
    "g_Sediminibacterium",
    "g_Bradyrhizobium",
    "g_Ralstonia",
    "g_Acidaminococcus"
)

group_var <- c("Mn_quartiles", "IFG_or_diabetes") # 定义分组变量

data_v2 <- data[, which(colnames(data) %in% c(group_var, bacteria_vars))] # 选择相关变量
write.csv(data_v2, "绝对丰度菌群与血清锰及疾病.csv", row.names = FALSE, fileEncoding = "UTF-8")
# ===========补充：空腹血糖受损与血清锰的关联===========
rm(list = ls()) # 清除环境变量

# pak::pkg_install(c("epiDisplay", "janitor"))
library(tidyverse) # 进行数据处理
library(epiDisplay) # 查看or值及95%置信区间
library(janitor) # 交叉表

source("代码/myfunctions.r", chdir = TRUE) # 加载自定义函数

load("数据/imputed_df_v6.RData") # 加载数据
colnames(imputed_df_v6)

# 选择子集
table(imputed_df_v6$IFG_or_diabetes)
sum(is.na(imputed_df_v6$IFG_or_diabetes))
df1 <- imputed_df_v6 %>%
    filter(diabetes != "yes") %>% # 只保留非糖尿病患者
    mutate(
        IFG = ifelse(is.na(IFG_or_diabetes), NA,
            ifelse(IFG_or_diabetes == "Yes" & diabetes == "no", 1,
                ifelse(IFG_or_diabetes == "No", 0, NA)
            )
        )
    ) %>%
    mutate(IFG = factor(as.character(IFG), levels = c("0", "1"), labels = c("No", "Yes"))) %>%
    mutate(Mn_Q_con = as.numeric(Mn_quartiles)) %>% # 将锰四分位数字化
    mutate(per_sd_lnMn = ln_Serum_Mn / (sd(ln_Serum_Mn, na.rm = TRUE))) # 计算per—sd
# 更改Mn_quartiles的参照水平为Q1
df1$Mn_quartiles <- relevel(df1$Mn_quartiles, ref = "Q1")
# 查看IFG的分布
table(df1$IFG)
tabpct(df1$IFG, df1$Mn_quartiles) # 交叉表，百分比显示两位小数

# IFG与血清锰的趋势性卡方检验
successes <- tapply(df1$IFG == "Yes", df1$Mn_quartiles, sum, na.rm = TRUE)
totals    <- tapply(!is.na(df1$IFG),  df1$Mn_quartiles, sum)

trend_res <- prop.trend.test(successes, totals, score = 1:4)
trend_res  # 查看卡方统计量与 p 值

print("IFG 与血清锰四分位数的交叉表:")
df1 %>%
    tabyl(IFG, Mn_quartiles) %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns() %>%
    print()
# 1. 协变量（性别、年龄）
covariate1 <- c("gender", "age")

# 2. 协变量（性别、年龄、BMI、婚姻、教育、职业、家庭月收入、糖尿病家族史、吸烟、饮酒、运动、能量、膳食锰）
covariate2 <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 公式
formula1 <- create_formula("IFG", "Mn_quartiles", covariate1)
formula2 <- create_formula("IFG", "Mn_quartiles", covariate2)

# 模型
model.1 <- glm(
    IFG ~ Mn_quartiles,
    data = df1,
    family = binomial(link = "logit")
)

model.2 <- glm(
    formula1,
    data = df1,
    family = binomial(link = "logit")
)

model.3 <- glm(
    formula2,
    data = df1,
    family = binomial(link = "logit")
)

# or值及95区间
logistic.display(model.1)
logistic.display(model.2)
exp(cbind(OR = coef(model.3), confint(model.3))) # 因为变量名称太长，模型3无法用logistic.display函数

# per_sd_lnMn
formula3 <- create_formula("IFG", "per_sd_lnMn", covariate1)
formula4 <- create_formula("IFG", "per_sd_lnMn", covariate2)

# model
model.4 <- glm(
    IFG ~ per_sd_lnMn,
    data = df1,
    family = binomial(link = "logit")
)

model.5 <- glm(
    formula3,
    data = df1,
    family = binomial(link = "logit")
)

model.6 <- glm(
    formula4,
    data = df1,
    family = binomial(link = "logit")
)
# or及95%置信区间
logistic.display(model.4)
logistic.display(model.5)
summary(model.6)
cbind(OR = coef(model.6), confint(model.6)) %>%
    exp() # 因为变量名称太长，模型6无法用logistic.display函数

# p for trend
formula5 <- create_formula("IFG", "Mn_Q_con", covariate1)
formula6 <- create_formula("IFG", "Mn_Q_con", covariate2)

# model
model.7 <- glm(
    IFG ~ Mn_Q_con,
    data = df1,
    family = binomial(link = "logit")
)

model.8 <- glm(
    formula5,
    data = df1,
    family = binomial(link = "logit")
)

model.9 <- glm(
    formula6,
    data = df1,
    family = binomial(link = "logit")
)

# p
summary(model.7)
summary(model.8)
summary(model.9)

# RCS限制性立方样条模型
# 创建 datadist 对象并指定
dd <- datadist(df1)
options(datadist = "dd")

# 自变量
exposure <- "rcs(ln_Serum_Mn, 3)"

# 构建logistic回归模型公式
formula7 <- create_formula("IFG", exposure) # 只包含自变量
formula8 <- create_formula("IFG", exposure, covariate1) # 包含协变量1
formula9 <- create_formula("IFG", exposure, covariate2) # 包含协变量2

# 模型拟合
model.10 <- lrm(formula7, data = df1)
model.11 <- lrm(formula8, data = df1)
model.12 <- lrm(formula9, data = df1)

# p for non-linear
(anova(model.10))
(anova(model.11))
(anova(model.12))
# =========误差条形图数据前处理============
rm(list = ls()) # 清除环境变量

library(tidyverse)     # 数据处理与绘图
# 可选：用于格式化p值（若未安装不会影响主流程）
suppressWarnings({
  if (!requireNamespace("rstatix", quietly = TRUE)) {
    message("提示：未安装 rstatix 包，将使用基础格式显示 p 值。")
  }
})
# 可选：用于Dunn事后检验
has_FSA <- requireNamespace("FSA", quietly = TRUE)

# 加载数据
load("数据/imputed_df_v6.RData")
load("数据/genus_data.RData")

genus_data$relative$SampleID <- row.names(genus_data$relative)
data <- merge(imputed_df_v6, genus_data$relative, by = "SampleID")

# 定义变量
bacteria_vars <- c("g_Sediminibacterium","g_Bradyrhizobium","g_Ralstonia","g_Acidaminococcus")
group_var <- c("Mn_quartiles", "IFG_or_diabetes")

# 去除缺失值，仅保留所需列
data <- data[complete.cases(data[, c(group_var, bacteria_vars)]), ]
data <- data[, c(group_var, bacteria_vars)]

# 计算中位数、四分位数下限和上限
data_summary <- data %>%
  pivot_longer(cols = all_of(bacteria_vars), names_to = "Bacteria", values_to = "Value") %>%
  group_by(Mn_quartiles, IFG_or_diabetes, Bacteria) %>%
  summarise(
    Median = median(Value, na.rm = TRUE),
    Q1     = quantile(Value, 0.25, na.rm = TRUE),
    Q3     = quantile(Value, 0.75, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  mutate(IQR = Q3 - Q1)

# 构建长表（原始值，用于统计检验）
data_long <- data %>%
  pivot_longer(cols = all_of(bacteria_vars), names_to = "Bacteria", values_to = "Value") %>%
  mutate(
    Mn_quartiles    = as.factor(Mn_quartiles),
    IFG_or_diabetes = as.factor(IFG_or_diabetes)
  )

# 1) Wilcoxon 秩和检验（每个 Mn_quartiles 层级内比较 IFG_or_diabetes 组间差异）
wilcox_by_quartile <- data_long %>%
  group_by(Bacteria, Mn_quartiles) %>%
  summarise(
    n_group = dplyr::n_distinct(IFG_or_diabetes),
    p_value = tryCatch({
      if (n_group == 2) {
        stats::wilcox.test(Value ~ IFG_or_diabetes)$p.value
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_),
    .groups = "drop"
  ) %>%
  group_by(Bacteria) %>%
  mutate(p_adj_BH = p.adjust(p_value, method = "BH")) %>%  # 按每个菌分层做FDR校正
  ungroup()

# 生成图上标注所需的 y 位置和标签
# 对于每个 Bacteria x Mn_quartiles，寻找最大 Q3 作为柱顶参考
y_positions <- data_summary %>%
  group_by(Bacteria, Mn_quartiles) %>%
  summarise(
    ymax = max(Q3, na.rm = TRUE),
    # 计算一个适度的上移量，避免与误差线重叠；若数值过小，给一个最小偏移
    y_pos = ymax + pmax(0.1 * ymax, 1e-4),
    .groups = "drop"
  )

# 合并 p 值以生成标注文本
annot_ifg_within_quartile <- wilcox_by_quartile %>%
  left_join(y_positions, by = c("Bacteria", "Mn_quartiles")) %>%
  mutate(
    label = if (!requireNamespace("rstatix", quietly = TRUE)) {
      # 基础格式
      paste0("p(adj)=", ifelse(is.na(p_adj_BH), "NA", formatC(p_adj_BH, format = "e", digits = 2)))
    } else {
      # 若安装了 rstatix，可用其格式化（含显著性星号）
      rstatix::p_format(p_adj_BH, add.p = TRUE)
    }
  )

# 2) Kruskal-Wallis 检验（每个 IFG_or_diabetes 层级内比较 Mn_quartiles 组间差异）
kw_by_ifg <- data_long %>%
  group_by(Bacteria, IFG_or_diabetes) %>%
  summarise(
    k_groups = dplyr::n_distinct(Mn_quartiles),
    p_kw = tryCatch({
      if (k_groups >= 2) {
        stats::kruskal.test(Value ~ Mn_quartiles)$p.value
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_),
    .groups = "drop"
  ) %>%
  group_by(Bacteria) %>%
  mutate(p_kw_adj_BH = p.adjust(p_kw, method = "BH")) %>%  # 按每个菌分层做FDR校正
  ungroup()

# 2.1) 若可用 FSA 包且 Kruskal 显著，则进行 Dunn 事后检验（BH校正）
dunn_results <- NULL
if (has_FSA) {
  dunn_list <- list()
  cnt <- 1L
  for (bact in unique(data_long$Bacteria)) {
    for (grp in unique(data_long$IFG_or_diabetes)) {
      subdat <- data_long %>% filter(Bacteria == bact, IFG_or_diabetes == grp)
      if (n_distinct(subdat$Mn_quartiles) >= 2) {
        # 仅对显著的进行 Dunn，也可以不判断直接做
        p_kw_adj <- kw_by_ifg %>% filter(Bacteria == bact, IFG_or_diabetes == grp) %>% pull(p_kw_adj_BH)
        if (is.na(p_kw_adj) || p_kw_adj >= 0.05) next
        # Dunn test
        dt <- FSA::dunnTest(Value ~ Mn_quartiles, data = subdat, method = "bh")
        out <- dt$res %>%
          mutate(
            Bacteria = bact,
            IFG_or_diabetes = grp
          ) %>%
          select(Bacteria, IFG_or_diabetes, Comparison, Z = Z, P.unadj = P.unadj, P.adj = P.adj)
        dunn_list[[cnt]] <- out
        cnt <- cnt + 1L
      }
    }
  }
  if (length(dunn_list) > 0) {
    dunn_results <- bind_rows(dunn_list)
  }
} else {
  message("提示：未安装 FSA 包，跳过 Dunn 事后检验。")
}

# 创建输出目录并保存统计检验结果
dir.create("其他数据/统计检验", recursive = TRUE, showWarnings = FALSE)

# 汇总并导出 Wilcoxon 与 Kruskal 结果
wilcox_out <- wilcox_by_quartile %>%
  rename(
    Grouping = Mn_quartiles,
    p_raw = p_value,
    p_adj = p_adj_BH
  ) %>%
  mutate(Test = "Wilcoxon (IFG within Mn quartile)")

kw_out <- kw_by_ifg %>%
  rename(
    Grouping = IFG_or_diabetes,
    p_raw = p_kw,
    p_adj = p_kw_adj_BH
  ) %>%
  mutate(Test = "Kruskal-Wallis (Mn quartiles within IFG)")

stats_results <- bind_rows(wilcox_out, kw_out) %>%
  select(Test, Bacteria, Grouping, p_raw, p_adj)

write.csv(stats_results, "其他数据/统计检验/genus_nonparametric_tests.csv", row.names = FALSE, fileEncoding = "UTF-8")

if (!is.null(dunn_results)) {
  write.csv(dunn_results, "其他数据/统计检验/genus_dunn_posthoc.csv", row.names = FALSE, fileEncoding = "UTF-8")
}

# 绘图（在每个菌的图上标注 Wilcoxon FDR校正后p值）
bacteria_names <- unique(data_summary$Bacteria)

for (bacteria in bacteria_names) {
  data_bacteria <- data_summary %>% filter(Bacteria == bacteria)
  ann_bacteria  <- annot_ifg_within_quartile %>% filter(Bacteria == bacteria)

  p <- ggplot(data_bacteria, aes(x = Mn_quartiles, y = Median, fill = as.factor(IFG_or_diabetes))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7, color = "black") +
    geom_errorbar(aes(ymin = Q1, ymax = Q3),
                  width = 0.2, position = position_dodge(0.7), color = "black") +
    # 在每个四分位类别的两根柱子上方居中标注 p 值（FDR 校正后）
    geom_text(
      data = ann_bacteria,
      aes(x = Mn_quartiles, y = y_pos, label = label),
      inherit.aes = FALSE,
      size = 3
    ) +
    scale_fill_manual(values = c("#AFD3DF", "#BAE0CD")) +
    theme_minimal() +
    theme_bw(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification = c("right", "top"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 7),
      legend.background = element_rect(fill = alpha("white", 0.85), color = "black", linewidth = 0.5),
      legend.margin = margin(4, 8, 4, 8),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      strip.text = element_text(face = "bold"),
      strip.background = element_blank(),
      strip.placement = "outside"
    ) +
    labs(
      x = "Mn Quartiles",
      y = "Median Relative Abundance",
      fill = "IFG or Diabetes",
      title = paste(bacteria)
    ) +
    scale_x_discrete(labels = function(x) gsub("_", " ", x))

  # 保存图
  out_dir <- "主要结果/图片/clustered_error_bar_plot"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(out_dir, paste0(bacteria, "_clustered_error_bar_plot.pdf")),
    plot = p, width = 4, height = 5, device = "pdf"
  )
}

message("完成：已计算非参数检验并输出至 其他数据/统计检验，且在图中标注了每个四分位组内的IFG差异（FDR校正p值）。")
# ========中介菌与Mn四分位和IFG_or_diabetes的差异性分析===========
rm(list = ls())
library(tidyverse)
library(dunn.test)

# 数据读取
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")

genus_data$relative$SampleID <- row.names(genus_data$relative)
data <- merge(imputed_df_v6, genus_data$relative, by = "SampleID")

# 要分析的变量
bacteria_vars <- c(
  "g_Sediminibacterium",
  "g_Bradyrhizobium",
  "g_Ralstonia",
  "g_Acidaminococcus"
)
group_var <- c("Mn_quartiles", "IFG_or_diabetes")
table(data$IFG_or_diabetes)
table(data$Mn_quartiles)

# 统一的科学计数法格式化函数
# 可通过 digits 调整有效位数（比如 3 或 4）
fmt <- function(x, digits = 3) {
  # 允许向量化输入并保留 NA
  out <- ifelse(is.na(x), NA_character_, formatC(x, format = "e", digits = digits))
  return(out)
}

# 字母标注函数（根据 dunn.test 输出自动分组）
get_group_letters <- function(dt_result, groups) {
  # 生成两两比较 p 值矩阵
  mat <- matrix(1,
                nrow = length(groups), ncol = length(groups),
                dimnames = list(groups, groups))
  for (i in 1:length(dt_result$comparisons)) {
    cs <- unlist(strsplit(dt_result$comparisons[i], " - "))
    mat[cs[1], cs[2]] <- dt_result$P.adjusted[i]
    mat[cs[2], cs[1]] <- dt_result$P.adjusted[i]
  }
  # 用 multcompView 包生成分组字母
  if (!requireNamespace("multcompView", quietly = TRUE)) install.packages("multcompView")
  library(multcompView)
  letters <- multcompView::multcompLetters(mat < 0.05)$Letters
  return(letters[groups])
}

summary_table <- function(data, bacteria_vars, group_var) {
  result_list <- list()
  for (bact in bacteria_vars) {
    # -------- Mn_quartiles 分组 --------
    group1 <- data %>%
      group_by(!!sym(group_var[1])) %>%
      summarise(
        n = n(),
        median = median(.data[[bact]], na.rm = TRUE),
        Q1 = quantile(.data[[bact]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[bact]], 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # 先把数值格式化为科学计数法，再拼接 IQR 文本
      mutate(
        median_s = fmt(median),
        Q1_s = fmt(Q1),
        Q3_s = fmt(Q3),
        iqr = paste0(median_s, " [", Q1_s, ", ", Q3_s, "]")
      )

    group_names <- as.character(group1[[group_var[1]]])

    # Kruskal-Wallis
    kw_p <- kruskal.test(data[[bact]] ~ data[[group_var[1]]])$p.value

    # Dunn's test，获得字母
    dt <- dunn.test::dunn.test(data[[bact]], data[[group_var[1]]], method = "bonferroni", kw = FALSE, label = TRUE)
    letters <- get_group_letters(dt, group_names)
    group1$letter <- letters
    group1$iqr_letter <- paste0(group1$iqr, group1$letter)

    # -------- IFG_or_diabetes 分组 --------
    group2 <- data %>%
      group_by(!!sym(group_var[2])) %>%
      summarise(
        n = n(),
        median = median(.data[[bact]], na.rm = TRUE),
        Q1 = quantile(.data[[bact]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[bact]], 0.75, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        median_s = fmt(median),
        Q1_s = fmt(Q1),
        Q3_s = fmt(Q3),
        iqr = paste0(median_s, " [", Q1_s, ", ", Q3_s, "]")
      )

    # Wilcoxon
    wil_p <- wilcox.test(data[[bact]] ~ data[[group_var[2]]])$p.value

    # 汇总一行（p 值也转为科学计数法字符串）
    res <- tibble(
      Variable = bact,
      `Q1` = group1$iqr_letter[group1[[group_var[1]]] == "Q1"],
      `Q2` = group1$iqr_letter[group1[[group_var[1]]] == "Q2"],
      `Q3` = group1$iqr_letter[group1[[group_var[1]]] == "Q3"],
      `Q4` = group1$iqr_letter[group1[[group_var[1]]] == "Q4"],
      `P value (quartile)` = fmt(kw_p),
      `No` = group2$iqr[group2[[group_var[2]]] == "No"],
      `Yes` = group2$iqr[group2[[group_var[2]]] == "Yes"],
      `P value (IFG/DM)` = fmt(wil_p)
    )
    result_list[[bact]] <- res
  }
  result <- bind_rows(result_list)
  return(result)
}

# 调用
res_table <- summary_table(data, bacteria_vars, group_var)
print(res_table)
write.csv(res_table, "其他结果\\表格\\genus_T2DM_Mn_Q_result.csv", row.names = FALSE)
# ======更正：alpha多样性指数在Diabetes Status及IFG or diabetes中的分布=============
rm(list = ls())
# 加载数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")

# 加载r包
packages <- c("dplyr", "knitr", "kableExtra")
lapply(packages, require, character.only = TRUE)

# 检查数据
names(imputed_df_v6)
dim(imputed_df_v6)
names(genus_data$alpha_diversity)
dt <- merge(imputed_df_v6, genus_data$alpha_diversity, by = "SampleID")

# 需要分析的变量
alpha_vars <- c("Richness", "Shannon", "Pielou", "Simpson")

# 创建结果数据框
results <- data.frame(
    Variable = alpha_vars,
    stringsAsFactors = FALSE
)

# 计算diabetes的统计量
for (var in alpha_vars) {
    # 获取数据
    no_diabetes <- dt[dt$diabetes == "no", var]
    yes_diabetes <- dt[dt$diabetes == "yes", var]

    # 计算均值和标准差
    no_mean <- mean(no_diabetes, na.rm = TRUE)
    no_sd <- sd(no_diabetes, na.rm = TRUE)
    yes_mean <- mean(yes_diabetes, na.rm = TRUE)
    yes_sd <- sd(yes_diabetes, na.rm = TRUE)

    # 计算样本量
    no_n <- sum(!is.na(no_diabetes))
    yes_n <- sum(!is.na(yes_diabetes))

    # 进行Welch Two Sample t-test
    t_test <- t.test(yes_diabetes, no_diabetes, var.equal = FALSE)
    p_value <- t_test$p.value

    # 格式化结果
    results[results$Variable == var, "No_diabetes"] <- paste0(sprintf("%.2f", no_mean), " ± ", sprintf("%.2f", no_sd))
    results[results$Variable == var, "Yes_diabetes"] <- paste0(sprintf("%.2f", yes_mean), " ± ", sprintf("%.2f", yes_sd))
    results[results$Variable == var, "p_value_diabetes"] <- sprintf("%.3f", p_value)
    results[results$Variable == var, "No_diabetes_n"] <- no_n
    results[results$Variable == var, "Yes_diabetes_n"] <- yes_n
}

# 计算IFG_or_diabetes的统计量
for (var in alpha_vars) {
    # 获取数据
    no_ifg_diabetes <- dt[dt$IFG_or_diabetes == "No", var]
    yes_ifg_diabetes <- dt[dt$IFG_or_diabetes == "Yes", var]

    # 计算均值和标准差
    no_mean <- mean(no_ifg_diabetes, na.rm = TRUE)
    no_sd <- sd(no_ifg_diabetes, na.rm = TRUE)
    yes_mean <- mean(yes_ifg_diabetes, na.rm = TRUE)
    yes_sd <- sd(yes_ifg_diabetes, na.rm = TRUE)

    # 计算样本量
    no_n <- sum(!is.na(no_ifg_diabetes))
    yes_n <- sum(!is.na(yes_ifg_diabetes))

    # 进行Welch Two Sample t-test
    t_test <- t.test(yes_ifg_diabetes, no_ifg_diabetes, var.equal = FALSE)
    p_value <- t_test$p.value

    # 格式化结果
    results[results$Variable == var, "No_IFG_diabetes"] <- paste0(sprintf("%.2f", no_mean), " ± ", sprintf("%.2f", no_sd))
    results[results$Variable == var, "Yes_IFG_diabetes"] <- paste0(sprintf("%.2f", yes_mean), " ± ", sprintf("%.2f", yes_sd))
    results[results$Variable == var, "p_value_IFG_diabetes"] <- sprintf("%.3f", p_value)
    results[results$Variable == var, "No_IFG_diabetes_n"] <- no_n
    results[results$Variable == var, "Yes_IFG_diabetes_n"] <- yes_n
}

# 创建最终的表格（用于显示）
final_table_display <- data.frame(
    Variable = results$Variable,
    No_diabetes = paste0(results$No_diabetes, " (N = ", results$No_diabetes_n, ")"),
    Yes_diabetes = paste0(results$Yes_diabetes, " (N = ", results$Yes_diabetes_n, ")"),
    p_value_diabetes = results$p_value_diabetes,
    No_IFG_diabetes = paste0(results$No_IFG_diabetes, " (N = ", results$No_IFG_diabetes_n, ")"),
    Yes_IFG_diabetes = paste0(results$Yes_IFG_diabetes, " (N = ", results$Yes_IFG_diabetes_n, ")"),
    p_value_IFG_diabetes = results$p_value_IFG_diabetes
)

# 为CSV保存创建更清晰的表格格式
final_table_csv <- data.frame(
    Variable = results$Variable,
    Diabetes_Status_No_Mean_SD = results$No_diabetes,
    Diabetes_Status_No_N = results$No_diabetes_n,
    Diabetes_Status_Yes_Mean_SD = results$Yes_diabetes,
    Diabetes_Status_Yes_N = results$Yes_diabetes_n,
    Diabetes_Status_p_value = results$p_value_diabetes,
    IFG_or_Diabetes_No_Mean_SD = results$No_IFG_diabetes,
    IFG_or_Diabetes_No_N = results$No_IFG_diabetes_n,
    IFG_or_Diabetes_Yes_Mean_SD = results$Yes_IFG_diabetes,
    IFG_or_Diabetes_Yes_N = results$Yes_IFG_diabetes_n,
    IFG_or_Diabetes_p_value = results$p_value_IFG_diabetes
)

# 修改显示表格的列名
colnames(final_table_display) <- c("Variable", "No", "Yes", "p-value", "No", "Yes", "p-value")

# 打印结果
print(final_table_display)

# 保存为CSV文件
write.csv(final_table_csv, "主要结果/表格/alpha_diversity_analysis_results.csv", row.names = FALSE)

# 创建一个包含表格标题的CSV文件
# 先创建标题行
header_info <- data.frame(
    Info = c(
        "Analysis: Alpha diversity indices across diabetes and IFG status groups",
        "Date: 2025-09-02",
        "Method: Welch Two Sample t-test",
        "Data format: mean ± SD",
        "Abbreviations: IFG = impaired fasting glucose",
        ""
    )
)

# 写入标题信息
write.table(header_info, "主要结果/表格/alpha_diversity_analysis_results_with_header.csv",
    sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE
)

# 追加数据表格
write.table(final_table_csv, "主要结果/表格/alpha_diversity_analysis_results_with_header.csv",
    sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE, append = TRUE
)

# 如果需要更美观的表格输出，可以使用kable
library(knitr)
kable(final_table_display, caption = "Alpha diversity indices across diabetes and IFG status groups")

# 单独输出统计摘要
cat("Summary Statistics:\n")
cat("Diabetes Status:\n")
for (i in 1:nrow(results)) {
    cat(paste0(
        results$Variable[i], ": No diabetes (N=", results$No_diabetes_n[i], ") ",
        results$No_diabetes[i], ", Yes diabetes (N=", results$Yes_diabetes_n[i], ") ",
        results$Yes_diabetes[i], ", p-value = ", results$p_value_diabetes[i], "\n"
    ))
}

cat("\nIFG or Diabetes Status:\n")
for (i in 1:nrow(results)) {
    cat(paste0(
        results$Variable[i], ": No IFG/diabetes (N=", results$No_IFG_diabetes_n[i], ") ",
        results$No_IFG_diabetes[i], ", Yes IFG/diabetes (N=", results$Yes_IFG_diabetes_n[i], ") ",
        results$Yes_IFG_diabetes[i], ", p-value = ", results$p_value_IFG_diabetes[i], "\n"
    ))
}

cat("\nNotes:\n")
cat("Data are presented as mean ± SD.\n")
cat("Welch Two Sample t-test was used to calculate p-values.\n")
cat("Abbreviations: IFG, impaired fasting glucose.\n")

# 提示保存成功
cat("\n=== 文件保存信息 ===\n")
cat("结果已保存为以下CSV文件:\n")
cat("1. alpha_diversity_analysis_results.csv - 基础数据表格\n")
cat("2. alpha_diversity_analysis_results_with_header.csv - 包含标题信息的完整表格\n")
cat("文件保存在当前工作目录中。\n")
# ======补充分析：Mn与alpha多样性指数的回归分析及RCS非线性分析======================
rm(list = ls())

# 加载必要的r包
lb <- c("dplyr", "rms")

install_and_load <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}

install_and_load(lb)

# 加载数据
load("数据/imputed_df_v6.RData")
load("数据/genus_data.RData")

names(imputed_df_v6)
names(genus_data)

# 匹配alpha多样性数据
# names(genus_data$alpha_diversity)
df <- merge(imputed_df_v6, genus_data$alpha_diversity, by = "SampleID")
names(df)

# 目的：
# - 对 4 个 Alpha 多样性指标（Richness、Shannon、Pielou、Simpson）分别做多重线性回归
# - 提取 ln_Serum_Mn 的“标准化回归系数（标准化 X 与 Y）”、95%CI 与 p 值（线性模型）
# - 使用限制性立方样条（RCS；Hmisc::rcspline.eval 构造基函数），在候选结点数 k 内通过 AIC 选择最佳模型
# - 用嵌套模型 F 检验对“非线性样条项”做联合检验，得到非线性 p 值（RCS 模型）
# - 输出两张表：Mn_alpha_linear.csv（线性模型），Mn_alpha_rcs.csv（RCS 最优结点数、最小 AIC、非线性 p 值）
# - 保存路径：主要结果/表格/Mn与alpha的回归及RCS
#
# 前提：
# - 当前 R 会话中已有数据框 df，且包含：
#   暴露：ln_Serum_Mn
#   因变量：Richness, Shannon, Pielou, Simpson
#   协变量：gender, age, BMI, Marital_status, education, family_income_cat,
#           family_diabetes_history, smoker, Alcohol_consumption, act_sport,
#           energy_kcal, diet_Mn_mg
# 最终没选择这部分代码得到的RCS的结果，用了上面代码生成的图片中的RCS结果

# 1) 加载/安装所需 R 包 -------------------------------------------------------
packages <- c("dplyr", "tidyr", "tibble", "readr", "stats", "Hmisc")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

# 2) 用户可配置参数 ------------------------------------------------------------
outcomes   <- c("Richness", "Shannon", "Pielou", "Simpson")   # 因变量列表
exposure   <- "ln_Serum_Mn"                                   # 暴露变量
covariates <- c(                                              # 协变量（混杂因素）
  "gender", "age", "BMI", "Marital_status",
  "education", "family_income_cat",
  "family_diabetes_history", "smoker", "Alcohol_consumption",
  "act_sport", "energy_kcal", "diet_Mn_mg"
)
rcs_k_candidates <- 3:7  # RCS 结点数的候选集合（k>=3）；可改为 3:5 或 3:6 等

# 3) 将分类协变量转为 factor 类型（若尚未为 factor）----------------------------
factor_vars <- c("gender", "Marital_status", "education", "family_income_cat",
                 "family_diabetes_history", "smoker", "Alcohol_consumption", "act_sport")

df <- df %>%
  dplyr::mutate(across(any_of(factor_vars), ~ as.factor(.)))

# 4) 工具函数 ------------------------------------------------------------------

# 4.1 计算“标准化回归系数（标准化 X 与 Y）”、其 95%CI 与 p 值（线性 lm 模型）
# 思路：在原始 OLS 模型中获得未标准化系数与其 CI，然后按 beta_std = beta * sd(X)/sd(Y) 做线性变换；
#      CI 同样按比例变换；p 值来自 t 检验。
compute_std_beta <- function(lm_fit, data_cc, y, x) {
  sm <- summary(lm_fit)$coefficients
  stopifnot(x %in% rownames(sm))
  beta   <- sm[x, "Estimate"]
  pval   <- sm[x, "Pr(>|t|)"]
  ci_mat <- suppressWarnings(confint(lm_fit))
  ci_raw <- ci_mat[x, ]

  sdx <- sd(data_cc[[x]], na.rm = TRUE)
  sdy <- sd(data_cc[[y]], na.rm = TRUE)
  beta_std <- beta * sdx / sdy
  ci_std   <- ci_raw * sdx / sdy

  list(beta_std = as.numeric(beta_std),
       ci_lo_std = as.numeric(ci_std[1]),
       ci_hi_std = as.numeric(ci_std[2]),
       p = as.numeric(pval))
}

# 4.2 生成 RCS 结点（分位数法）
# - 为保证与常用实践一致，k=3 时用 5%、50%、95%；k>=4 时用 5%~95% 等距分位
rcs_knots_by_quantile <- function(xvec, k) {
  xvec <- xvec[is.finite(xvec)]
  if (length(unique(xvec)) < 4) return(NA)  # 数据变异太小
  if (k == 3) {
    probs <- c(0.05, 0.50, 0.95)
  } else {
    probs <- seq(0.05, 0.95, length.out = k)
  }
  knots <- as.numeric(quantile(xvec, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
  knots <- sort(unique(knots))
  if (length(knots) < 3) return(NA)
  knots
}

# 4.3 基于 AIC 选择 RCS 结点数，并用嵌套 F 检验提取“非线性 p 值”
# - 线性模型：y ~ x + 协变量
# - RCS 模型（候选）：y ~ x + RCS_nonlin_terms + 协变量（RCS_nonlin 由 Hmisc::rcspline.eval inclx=FALSE 生成）
# - AIC 最小者记为 best_k；非线性 p 值来自 anova(linear, rcs_best, test="F") 的增量检验
compute_rcs_best_k_and_p <- function(data_cc, y, x, covars, k_candidates = 3:7) {
  # 线性模型
  lin_formula <- as.formula(paste(y, "~", paste(c(x, covars), collapse = " + ")))
  fit_lin <- tryCatch(lm(lin_formula, data = data_cc), error = function(e) NULL)
  if (is.null(fit_lin)) {
    return(list(best_k = NA_integer_, aic_min = NA_real_, nonlin_p = NA_real_, best_knots = NA))
  }

  # 遍历候选 k：构造 RCS 非线性基并拟合
  aic_map <- list()
  fit_map <- list()
  knots_map <- list()

  k_candidates <- k_candidates[k_candidates >= 3]
  for (k in k_candidates) {
    knots <- rcs_knots_by_quantile(data_cc[[x]], k)
    if (all(is.na(knots))) {
      aic_map[[as.character(k)]] <- NA_real_
      fit_map[[as.character(k)]] <- NULL
      knots_map[[as.character(k)]] <- NA
      next
    }

    # 仅生成“非线性”基（不含线性分量），避免与原始 x 共线
    bmat <- tryCatch(Hmisc::rcspline.eval(data_cc[[x]], knots = knots, inclx = FALSE),
                     error = function(e) NULL)
    if (is.null(bmat)) {
      aic_map[[as.character(k)]] <- NA_real_
      fit_map[[as.character(k)]] <- NULL
      knots_map[[as.character(k)]] <- NA
      next
    }
    bmat <- as.matrix(bmat)
    if (ncol(bmat) == 0 || any(!is.finite(bmat))) {
      aic_map[[as.character(k)]] <- NA_real_
      fit_map[[as.character(k)]] <- NULL
      knots_map[[as.character(k)]] <- NA
      next
    }

    colnames(bmat) <- paste0(x, "_rcs", seq_len(ncol(bmat)))
    data_aug <- cbind(data_cc, as.data.frame(bmat))

    rhs <- paste(c(x, covars, colnames(bmat)), collapse = " + ")
    form_rcs <- as.formula(paste(y, "~", rhs))

    fit_rcs <- tryCatch(lm(form_rcs, data = data_aug), error = function(e) NULL)

    aic_val <- if (!is.null(fit_rcs)) suppressWarnings(AIC(fit_rcs)) else NA_real_
    aic_map[[as.character(k)]] <- aic_val
    fit_map[[as.character(k)]] <- fit_rcs
    knots_map[[as.character(k)]] <- knots
  }

  aic_vec <- unlist(aic_map)
  if (length(aic_vec) == 0 || all(is.na(aic_vec))) {
    return(list(best_k = NA_integer_, aic_min = NA_real_, nonlin_p = NA_real_, best_knots = NA))
  }

  best_k <- as.integer(names(which.min(aic_vec)))
  best_fit <- fit_map[[as.character(best_k)]]
  best_knots <- knots_map[[as.character(best_k)]]
  aic_min <- suppressWarnings(min(aic_vec, na.rm = TRUE))

  # 用嵌套模型 F 检验提取“非线性 p 值”：比较线性模型 vs 最优 RCS 模型
  nonlin_p <- NA_real_
  if (!is.null(best_fit)) {
    cmp <- tryCatch(anova(fit_lin, best_fit, test = "F"), error = function(e) NULL)
    if (!is.null(cmp) && nrow(cmp) >= 2) {
      p_col <- c("Pr(>F)", "Pr(>Chi)", "Pr(>Chisq)")
      p_col <- intersect(p_col, colnames(cmp))
      if (length(p_col) > 0) {
        nonlin_p <- suppressWarnings(as.numeric(cmp[2, p_col[1]]))
      }
    }
  }

  list(best_k = best_k, aic_min = as.numeric(aic_min), nonlin_p = nonlin_p, best_knots = best_knots)
}

# 5) 主分析：分别生成线性模型表与 RCS 模型表 -----------------------------------
linear_results <- list()
rcs_results <- list()

for (y in outcomes) {
  # 仅保留该模型所需变量，并做 complete-case（全变量非缺失）分析集
  vars_needed <- c(y, exposure, covariates)
  data_cc <- df %>% dplyr::select(all_of(vars_needed)) %>% tidyr::drop_na()
  n_cc <- nrow(data_cc)
  if (n_cc < 10) {
    warning(sprintf("因变量 %s 的 complete-case 样本量偏小（n=%d），请检查数据。", y, n_cc))
  }

  # 5.1 线性回归（多重线性回归）
  lin_formula <- as.formula(paste(y, "~", paste(c(exposure, covariates), collapse = " + ")))
  fit_lm <- lm(lin_formula, data = data_cc)

  # 5.2 计算 ln_Serum_Mn 的标准化系数、95%CI、p 值
  std_stats <- compute_std_beta(fit_lm, data_cc, y = y, x = exposure)

  linear_results[[y]] <- tibble::tibble(
    outcome = y,
    n = n_cc,
    beta_std = std_stats$beta_std,
    ci_lower_std = std_stats$ci_lo_std,
    ci_upper_std = std_stats$ci_hi_std,
    p_value = std_stats$p
  )

  # 5.3 RCS：AIC 选择最佳结点数，并提取非线性 p 值
  rcs_sel <- compute_rcs_best_k_and_p(
    data_cc, y = y, x = exposure, covars = covariates, k_candidates = rcs_k_candidates
  )

  # 将最佳 knots 也存下来（便于复现）
  knots_str <- if (all(is.na(rcs_sel$best_knots))) NA_character_ else paste(signif(rcs_sel$best_knots, 5), collapse = "; ")

  rcs_results[[y]] <- tibble::tibble(
    outcome = y,
    n = n_cc,
    rcs_best_k = rcs_sel$best_k,
    rcs_knots = knots_str,
    rcs_aic_min = rcs_sel$aic_min,
    rcs_nonlin_p = rcs_sel$nonlin_p
  )
}

linear_results <- dplyr::bind_rows(linear_results)
rcs_results    <- dplyr::bind_rows(rcs_results)
View(linear_results)
View(rcs_results)
# 6) 保存结果与方法学说明 ------------------------------------------------------
out_dir <- file.path("主要结果", "表格", "Mn与alpha的回归及RCS")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 6.1 分别保存 CSV
out_csv_linear <- file.path(out_dir, "Mn_alpha_linear.csv")
out_csv_rcs    <- file.path(out_dir, "Mn_alpha_rcs.csv")
readr::write_csv(linear_results, out_csv_linear)
readr::write_csv(rcs_results, out_csv_rcs)

# 6.2 英文方法学说明（RCS + AIC 选 k + 嵌套 F 检验）
methods_text <- "---
Title: Associations of ln(Serum Manganese) with Alpha-Diversity Indices: Linear and Restricted Cubic Spline Models
---

Statistical analysis
- Outcomes: We analyzed four alpha-diversity indices as continuous outcomes: Richness, Shannon, Pielou, and Simpson.
- Exposure: ln-transformed serum manganese (ln_Serum_Mn).
- Covariates: We adjusted for gender, age, BMI, marital status, education, family income category, family diabetes history, smoking status, alcohol consumption, physical activity (sport), total energy intake (kcal), and dietary manganese intake (mg). Categorical covariates were modeled as factors; continuous covariates were modeled linearly.

Linear regression
- For each outcome, we fit multivariable ordinary least squares (OLS) regression models with ln_Serum_Mn as the exposure and all covariates included as adjustment variables.
- We reported the standardized regression coefficient (β_std) for ln_Serum_Mn (both exposure and outcome standardized). The 95% confidence interval (CI) for β_std was obtained by linearly transforming the model-based 95% CI for the unstandardized coefficient using SDs computed on the analysis dataset. p-values were obtained from t-tests. Complete-case analysis was performed for each outcome-specific model.

Restricted cubic spline (RCS) modeling and nonlinearity test
- We modeled ln_Serum_Mn using restricted cubic spline (RCS) basis functions constructed with Hmisc::rcspline.eval (linear tails).
- Candidate numbers of knots K ∈ {3, 4, 5, 6, 7} were evaluated. Knots were placed at empirical quantiles: for K=3 at the 5th, 50th, and 95th percentiles; for K≥4 at equally spaced percentiles between the 5th and 95th (inclusive). The number of knots was selected by minimizing the Akaike Information Criterion (AIC).
- For the selected K, nonlinearity was assessed using a nested-model F test comparing the linear model (ln_Serum_Mn as a linear term) with the RCS model (ln_Serum_Mn modeled via RCS basis). The p-value corresponds to the joint test that all nonlinear spline coefficients equal zero (i.e., deviation from linearity).
- All models included the same set of covariates as in the linear models.

Handling of missing data
- We conducted complete-case analyses separately for each outcome, using observations with non-missing values for the outcome, exposure, and all covariates included in that model.

Outputs
- Two tables were generated: (1) Mn_alpha_linear.csv for standardized coefficients from linear models; and (2) Mn_alpha_rcs.csv for the selected number of spline knots, the knot locations, the minimum AIC, and the p-value for nonlinearity from RCS models.

Software
- Analyses were performed in R using the stats, Hmisc, dplyr, tidyr, tibble, and readr packages."

methods_md <- file.path(out_dir, "Methods_Mn_alpha.md")
writeLines(methods_text, con = methods_md, useBytes = TRUE)

message(sprintf("线性模型结果 CSV 已保存至: %s", normalizePath(out_csv_linear, mustWork = FALSE)))
message(sprintf("RCS 模型结果 CSV 已保存至: %s", normalizePath(out_csv_rcs, mustWork = FALSE)))
message(sprintf("方法学说明已保存至: %s", normalizePath(methods_md, mustWork = FALSE)))
# ======以中介菌做亚组分析================
rm(list = ls())

# 加载必要的R包
lb <- c("dplyr", "jstable")

install_and_load <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}

install_and_load(lb)
# 加载自编函数
source("代码/myfunctions.r", chdir = TRUE)
# 读取数据
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")

genus_data$relative$SampleID <- row.names(genus_data$relative)
data <- merge(imputed_df_v6, genus_data$relative, by = "SampleID")

data$Sed_cat <- ifelse(data$g_Sediminibacterium >= median(data$g_Sediminibacterium), "high", "low")
data$Sed_cat <- factor(data$Sed_cat, levels = c("low", "high"))
table(data$Sed_cat)

data$Ral_cat <- ifelse(data$g_Ralstonia >= median(data$g_Ralstonia), "high", "low")
data$Ral_cat <- factor(data$Ral_cat, levels = c("low", "high"))
table(data$Ral_cat)
# 检查自变量因子水平
table(data$Mn_quartiles)
# 设置自变量参考水平
data$Mn_quartiles <- relevel(data$Mn_quartiles, ref = "Q2")
# 设置协变量
c_vars <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg", "Sed_cat", "Ral_cat"
)

# 设置自变量
x_var <- "Mn_quartiles"

# 设置因变量
y_var <- "IFG_or_diabetes"

# 进行亚组分析
res3 <- TableSubgroupMultiGLM(
    # 指定公式
    formula = create_formula(y_var, x_var),
    # 指定哪些变量有亚组(因子型变量)
    var_subgroups = c("Sed_cat", "Ral_cat"),
    # 指定数据集
    data = data,

    # 指定协变量
    var_cov = c_vars
)
View(res3)
write.csv(res3, "其他结果\\表格\\genus_T2DM_Mn_subgroup_result.csv", row.names = FALSE)

# 换一种思路，分组拟合模型
rm(list = ls())

# 1) 加载必要R包 ---------------------------------------------------------------
lb <- c("dplyr", "jstable")

install_and_load <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}

install_and_load(lb)

# 2) 加载自编函数（如有需要） ---------------------------------------------------
# 若 myfunctions.r 中包含你其他需要的工具函数，可继续加载
# 注意：本脚本不再使用 TableSubgroupMultiGLM，而是直接用 glm 拟合逻辑回归
source("代码/myfunctions.r", chdir = TRUE)

# 3) 读取数据 -------------------------------------------------------------------
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")

# 4) 数据准备 -------------------------------------------------------------------
genus_data$relative$SampleID <- row.names(genus_data$relative)
data <- merge(imputed_df_v6, genus_data$relative, by = "SampleID")

# 亚组变量：按中位数二分，并设置为因子（显式处理缺失）
data$Sed_cat <- ifelse(data$g_Sediminibacterium >= median(data$g_Sediminibacterium, na.rm = TRUE), "high", "low")
data$Sed_cat <- factor(data$Sed_cat, levels = c("low", "high"))

data$Ral_cat <- ifelse(data$g_Ralstonia >= median(data$g_Ralstonia, na.rm = TRUE), "high", "low")
data$Ral_cat <- factor(data$Ral_cat, levels = c("low", "high"))

# 自变量（暴露）分位数因子与参考水平
data$Mn_quartiles <- factor(data$Mn_quartiles)
if (!("Q2" %in% levels(data$Mn_quartiles))) {
  stop("在 data$Mn_quartiles 的水平中未发现 'Q2'，请检查变量取值。")
}
data$Mn_quartiles <- stats::relevel(data$Mn_quartiles, ref = "Q2")

# 5) 设定变量 -------------------------------------------------------------------
# 主模型协变量：明确排除两个菌的分类变量
c_vars_main <- c(
  "gender", "age", "BMI", "Marital_status",
  "education", "family_income_cat",
  "family_diabetes_history", "smoker", "Alcohol_consumption",
  "act_sport", "energy_kcal", "diet_Mn_mg"
)

# 自变量与因变量
x_var <- "Mn_quartiles"
y_var <- "IFG_or_diabetes"

# 6) 工具函数：拟合与提取 OR -----------------------------------------------------
# 根据 glm 模型提取指定自变量的 OR、95%CI、P 值，并附加 N 与事件数
extract_or_for_x <- function(mod, x_var, model_name, subgroup_var = "", subgroup_level = "") {
  sm <- summary(mod)$coefficients
  term_names <- rownames(sm)

  # 针对分类自变量：系数名以 "x_var" 开头；针对数值型：恰好等于 "x_var"
  idx <- grepl(paste0("^", x_var), term_names) | term_names == x_var
  if (!any(idx)) {
    return(data.frame(
      model = model_name,
      subgroup_var = subgroup_var,
      subgroup_level = subgroup_level,
      term = character(0),
      OR = numeric(0),
      LCL = numeric(0),
      UCL = numeric(0),
      p_value = numeric(0),
      N = integer(0),
      events = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  est <- sm[idx, , drop = FALSE]
  beta <- est[, "Estimate"]
  se <- est[, "Std. Error"]
  pval <- est[, "Pr(>|z|)"]

  OR <- exp(beta)
  LCL <- exp(beta - 1.96 * se)
  UCL <- exp(beta + 1.96 * se)

  # 计算 N 与事件数（基于当前模型框架中的响应）
  mf <- model.frame(mod)
  yvec <- model.response(mf)
  # 事件数：若为因子，取第二层为“成功”；若为数值/逻辑，取==1
  if (is.factor(yvec)) {
    if (nlevels(yvec) < 2) {
      events <- NA_integer_
    } else {
      events <- sum(yvec == levels(yvec)[2], na.rm = TRUE)
    }
  } else {
    events <- sum(yvec == 1, na.rm = TRUE)
  }
  N <- nrow(mf)

  # 生成暴露条目的可读标签
  term_full <- rownames(est)
  term_label <- ifelse(
    term_full == x_var, x_var,
    sub(paste0("^", x_var), "", term_full)
  )
  # 去掉可能多余的分隔符（如无分隔符则保持原样）
  term_label <- gsub("^[:]*", "", term_label)

  data.frame(
    model = model_name,
    subgroup_var = subgroup_var,
    subgroup_level = subgroup_level,
    term = term_label,
    OR = as.numeric(OR),
    LCL = as.numeric(LCL),
    UCL = as.numeric(UCL),
    p_value = as.numeric(pval),
    N = N,
    events = as.integer(events),
    stringsAsFactors = FALSE
  )
}

# 安全拟合 glm（二分类逻辑回归），返回模型；若样本过少或问题数据，返回 NULL
safe_glm_binomial <- function(formula, data) {
  # 移除含 NA 的行（与 glm 默认一致），并检查样本量与事件
  mf <- model.frame(formula, data = data, na.action = na.omit)
  if (nrow(mf) < 5) return(NULL)  # 样本太少直接跳过
  yvec <- model.response(mf)
  # 检查是否存在双类且均有样本
  if (is.factor(yvec)) {
    if (nlevels(yvec) < 2) return(NULL)
    if (all(table(yvec) == 0)) return(NULL)
  } else {
    # 数值/逻辑：需要既有 0 又有 1
    if (!(any(yvec == 0, na.rm = TRUE) && any(yvec == 1, na.rm = TRUE))) return(NULL)
  }
  # 拟合
  tryCatch(
    glm(formula, data = data, family = binomial()),
    warning = function(w) {
      # 保留警告但继续返回模型
      message("glm warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      message("glm error: ", conditionMessage(e))
      NULL
    }
  )
}

# 构造公式的便捷函数
make_formula <- function(y, x, covars = NULL) {
  rhs <- c(x, covars)
  as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
}

# 7) 构建并拟合各模型 -----------------------------------------------------------
# 主模型（不分亚组；协变量不含 Sed_cat 与 Ral_cat）
fml_main <- make_formula(y_var, x_var, c_vars_main)
mod_main <- safe_glm_binomial(fml_main, data)

# 分层：Sed_cat = low / high
data_sed_low  <- subset(data, Sed_cat == "low")
data_sed_high <- subset(data, Sed_cat == "high")

mod_sed_low  <- safe_glm_binomial(fml_main, data_sed_low)
mod_sed_high <- safe_glm_binomial(fml_main, data_sed_high)

# 分层：Ral_cat = low / high
data_ral_low  <- subset(data, Ral_cat == "low")
data_ral_high <- subset(data, Ral_cat == "high")

mod_ral_low  <- safe_glm_binomial(fml_main, data_ral_low)
mod_ral_high <- safe_glm_binomial(fml_main, data_ral_high)

# 8) 汇总主要结果参数（仅暴露 Mn_quartiles 的效应） ------------------------------
res_list <- list()

if (!is.null(mod_main)) {
  res_list[["Model 1: Main"]] <- extract_or_for_x(mod_main, x_var, "Model 1: Main")
} else {
  message("主模型拟合失败。")
}

if (!is.null(mod_sed_low)) {
  res_list[["Model 2: Sed_cat=low"]] <- extract_or_for_x(mod_sed_low, x_var, "Model 2: Sed_cat strata", "Sed_cat", "low")
} else {
  message("Sed_cat=low 模型拟合失败。")
}

if (!is.null(mod_sed_high)) {
  res_list[["Model 3: Sed_cat=high"]] <- extract_or_for_x(mod_sed_high, x_var, "Model 3: Sed_cat strata", "Sed_cat", "high")
} else {
  message("Sed_cat=high 模型拟合失败。")
}

if (!is.null(mod_ral_low)) {
  res_list[["Model 4: Ral_cat=low"]] <- extract_or_for_x(mod_ral_low, x_var, "Model 4: Ral_cat strata", "Ral_cat", "low")
} else {
  message("Ral_cat=low 模型拟合失败。")
}

if (!is.null(mod_ral_high)) {
  res_list[["Model 5: Ral_cat=high"]] <- extract_or_for_x(mod_ral_high, x_var, "Model 5: Ral_cat strata", "Ral_cat", "high")
} else {
  message("Ral_cat=high 模型拟合失败。")
}

res_all <- dplyr::bind_rows(res_list)

# 9) 导出结果 -------------------------------------------------------------------
# 创建目录（若不存在）
out_dir <- "其他结果/表格"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(out_dir, "Mn_quartiles_main_and_subgroups.csv")
if (nrow(res_all) > 0) {
  write.csv(res_all, out_file, row.names = FALSE)
  message("结果已保存到：", out_file)
} else {
  message("没有可导出的结果行，请检查模型拟合是否成功。")
}

# 可选：在查看器中查看
# View(res_all)
print(res_all)
# ======将中介菌按Q1-Q4分组做回归====================
rm(list = ls())

# 1) 加载必要R包 ---------------------------------------------------------------
lb <- c("dplyr", "jstable")

install_and_load <- function(pkgs) {
  missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing)
  invisible(lapply(pkgs, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}

install_and_load(lb)

# 2) 加载自编函数（如有需要） ---------------------------------------------------
# 本脚本用基础 glm 拟合，不依赖 TableSubgroupMultiGLM
if (file.exists("代码/myfunctions.r")) {
  source("代码/myfunctions.r", chdir = TRUE)
}

# 3) 读取数据 -------------------------------------------------------------------
load("数据\\imputed_df_v6.RData")
load("数据\\genus_data.RData")

# 4) 数据准备 -------------------------------------------------------------------
genus_data$relative$SampleID <- row.names(genus_data$relative)
data <- merge(imputed_df_v6, genus_data$relative, by = "SampleID")

# 自变量（暴露）分位数因子与参考水平
data$Mn_quartiles <- factor(data$Mn_quartiles)
if (!("Q2" %in% levels(data$Mn_quartiles))) {
  stop("在 data$Mn_quartiles 的水平中未发现 'Q2'，请检查变量取值。")
}
data$Mn_quartiles <- stats::relevel(data$Mn_quartiles, ref = "Q2")

# 5) 将菌分为 Q1-Q4（稳健切分函数） ---------------------------------------------
make_quartile_factor <- function(x, labels = c("Q1","Q2","Q3","Q4")) {
  x_num <- as.numeric(x)
  # 先尝试按分位点 cut（最符合“分位数”定义）
  br <- quantile(x_num, probs = seq(0, 1, 0.25), na.rm = TRUE, type = 7)
  br_u <- unique(br)
  if (length(br_u) >= 5) {
    q <- cut(x_num, breaks = br, include.lowest = TRUE, labels = labels)
  } else {
    # 若分位点不唯一（大量相同值），退回到等频分箱 ntile
    q_num <- rep(NA_integer_, length(x_num))
    not_na <- !is.na(x_num)
    q_num[not_na] <- dplyr::ntile(x_num[not_na], 4)
    q <- factor(paste0("Q", q_num), levels = labels)
  }
  q
}

# 为两种菌生成四分位分组
data$Sed_q <- make_quartile_factor(data$g_Sediminibacterium)
data$Ral_q <- make_quartile_factor(data$g_Ralstonia)

# 6) 设定变量 -------------------------------------------------------------------
# 主模型协变量：明确排除菌的分类变量（这里不把 Sed_q、Ral_q 放入协变量）
c_vars_main <- c(
  "gender", "age", "BMI", "Marital_status",
  "education", "family_income_cat",
  "family_diabetes_history", "smoker", "Alcohol_consumption",
  "act_sport", "energy_kcal", "diet_Mn_mg"
)

x_var <- "Mn_quartiles"
y_var <- "IFG_or_diabetes"

# 7) 工具函数：拟合与提取 OR -----------------------------------------------------
extract_or_for_x <- function(mod, x_var, model_name, subgroup_var = "", subgroup_level = "") {
  sm <- summary(mod)$coefficients
  term_names <- rownames(sm)

  idx <- grepl(paste0("^", x_var), term_names) | term_names == x_var
  if (!any(idx)) {
    return(data.frame(
      model = model_name,
      subgroup_var = subgroup_var,
      subgroup_level = subgroup_level,
      term = character(0),
      OR = numeric(0),
      LCL = numeric(0),
      UCL = numeric(0),
      p_value = numeric(0),
      N = integer(0),
      events = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  est <- sm[idx, , drop = FALSE]
  beta <- est[, "Estimate"]
  se <- est[, "Std. Error"]
  pval <- est[, "Pr(>|z|)"]

  OR <- exp(beta)
  LCL <- exp(beta - 1.96 * se)
  UCL <- exp(beta + 1.96 * se)

  mf <- model.frame(mod)
  yvec <- model.response(mf)
  if (is.factor(yvec)) {
    events <- if (nlevels(yvec) >= 2) sum(yvec == levels(yvec)[2], na.rm = TRUE) else NA_integer_
  } else {
    events <- sum(yvec == 1, na.rm = TRUE)
  }
  N <- nrow(mf)

  term_full <- rownames(est)
  term_label <- ifelse(term_full == x_var, x_var, sub(paste0("^", x_var), "", term_full))
  term_label <- gsub("^[:]*", "", term_label)

  data.frame(
    model = model_name,
    subgroup_var = subgroup_var,
    subgroup_level = subgroup_level,
    term = term_label,
    OR = as.numeric(OR),
    LCL = as.numeric(LCL),
    UCL = as.numeric(UCL),
    p_value = as.numeric(pval),
    N = N,
    events = as.integer(events),
    stringsAsFactors = FALSE
  )
}

safe_glm_binomial <- function(formula, data) {
  mf <- model.frame(formula, data = data, na.action = na.omit)
  if (nrow(mf) < 5) return(NULL)
  yvec <- model.response(mf)
  if (is.factor(yvec)) {
    if (nlevels(yvec) < 2) return(NULL)
  } else {
    if (!(any(yvec == 0, na.rm = TRUE) && any(yvec == 1, na.rm = TRUE))) return(NULL)
  }
  tryCatch(
    glm(formula, data = data, family = binomial()),
    warning = function(w) {
      message("glm warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      message("glm error: ", conditionMessage(e))
      NULL
    }
  )
}

make_formula <- function(y, x, covars = NULL) {
  rhs <- c(x, covars)
  as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
}

# 8) 构建公式（全模型与分层模型共享同一公式） -----------------------------------
fml_main <- make_formula(y_var, x_var, c_vars_main)

# 9) 拟合主模型 ------------------------------------------------------------------
mod_main <- safe_glm_binomial(fml_main, data)

# 10) 按 Sed_q 四分位分层拟合（Q1~Q4） -----------------------------------------
res_list <- list()

if (!is.null(mod_main)) {
  res_list[["Model 1: Main"]] <- extract_or_for_x(mod_main, x_var, "Model 1: Main")
} else {
  message("主模型拟合失败。")
}

for (lvl in levels(data$Sed_q)) {
  d_sub <- subset(data, Sed_q == lvl)
  mod <- safe_glm_binomial(fml_main, d_sub)
  key <- paste0("Model_Sed_", lvl)
  if (!is.null(mod)) {
    res_list[[key]] <- extract_or_for_x(mod, x_var,
                                        model_name = paste0("Model (Sed_q=", lvl, ")"),
                                        subgroup_var = "Sed_q",
                                        subgroup_level = lvl)
  } else {
    message("Sed_q=", lvl, " 模型拟合失败。")
  }
}

# 11) 按 Ral_q 四分位分层拟合（Q1~Q4） -----------------------------------------
for (lvl in levels(data$Ral_q)) {
  d_sub <- subset(data, Ral_q == lvl)
  mod <- safe_glm_binomial(fml_main, d_sub)
  key <- paste0("Model_Ral_", lvl)
  if (!is.null(mod)) {
    res_list[[key]] <- extract_or_for_x(mod, x_var,
                                        model_name = paste0("Model (Ral_q=", lvl, ")"),
                                        subgroup_var = "Ral_q",
                                        subgroup_level = lvl)
  } else {
    message("Ral_q=", lvl, " 模型拟合失败。")
  }
}

res_all <- dplyr::bind_rows(res_list)

# 12) 导出结果 -------------------------------------------------------------------
out_dir <- "其他结果/表格"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "Mn_quartiles_main_and_bacteria_quartiles.csv")

if (nrow(res_all) > 0) {
  write.csv(res_all, out_file, row.names = FALSE)
  message("结果已保存到：", out_file)
} else {
  message("没有可导出的结果行，请检查模型拟合是否成功。")
}

# 可选查看
print(res_all)
# View(res_all)

# ======== 不同的菌分层下的RCS（受限立方样条）分析与作图 ========
# -------------------------- 0) 清理与依赖 --------------------------
rm(list = ls())

suppressPackageStartupMessages({
  library(ggplot2)
  library(rms)
})

# 可选：加载你的工具函数（如存在）
if (file.exists(file.path("代码", "myfunctions.r"))) {
  source(file.path("代码", "myfunctions.r"))
}

# -------------------------- 1) 通用与绘图工具 --------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b
pt_to_size <- function(pt) pt / 2.845276
ref_x <- function(x) stats::median(x, na.rm = TRUE)

alpha_col <- function(col, alpha = 0.18) {
  if (requireNamespace("scales", quietly = TRUE)) return(scales::alpha(col, alpha))
  rgb_val <- col2rgb(col) / 255
  rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha = alpha)
}

theme_clean_compact <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.3),
      axis.ticks = element_line(color = "grey40", linewidth = 0.3),
      axis.title = element_text(margin = margin(t = 2, r = 2, b = 2, l = 2), size = base_size),
      axis.text  = element_text(size = base_size),
      legend.position = "top",
      legend.title = element_text(size = base_size),
      legend.text  = element_text(size = base_size),
      legend.margin = margin(b = 0),
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size),
      plot.caption = element_text(size = base_size, hjust = 1, color = "grey40")
    )
}

col_single <- "#005BAC"
col_pair   <- c(low = "#35B597", high = "#005BAC")

a4_w_in  <- 8.27
fig_w_in <- a4_w_in * 0.4
fig_h_in <- fig_w_in * 1.2

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) return("P<1e-4")
  sprintf("P=%.3f", p)
}

# -------------------------- 2) datadist 保障 --------------------------

.ensure_datadist <- function(df) {
  dd_opt <- getOption("datadist", default = NULL)
  if (!is.null(dd_opt) && is.character(dd_opt) && exists(dd_opt, envir = .GlobalEnv, inherits = FALSE)) {
    return(FALSE)
  }
  dd <- datadist(df)
  assign("dd", dd, envir = .GlobalEnv)
  options(datadist = "dd")
  TRUE
}

.cleanup_datadist <- function(set_by_me) {
  if (isTRUE(set_by_me)) {
    options(datadist = NULL)
    if (exists("dd", envir = .GlobalEnv, inherits = FALSE)) {
      rm("dd", envir = .GlobalEnv)
    }
  }
}

# -------------------------- 3) AIC 选择最佳 K --------------------------

select_best_knots <- function(df, y, x, covars = NULL, strata = NULL, k_candidates = 3:6) {
  set_by_me <- .ensure_datadist(df)
  on.exit(.cleanup_datadist(set_by_me), add = TRUE)

  aics <- data.frame(K = integer(), AIC = numeric(), stringsAsFactors = FALSE)

  for (K in k_candidates) {
    if (is.null(strata)) {
      rhs <- paste(c(sprintf("rcs(%s, %d)", x, K), covars), collapse = " + ")
    } else {
      cov_adj <- setdiff(covars %||% character(), c(strata, "Sed_cat", "Ral_cat"))
      rhs <- paste(c(sprintf("rcs(%s, %d) * %s", x, K, strata), cov_adj), collapse = " + ")
    }
    fml <- as.formula(sprintf("%s ~ %s", y, rhs))

    fit <- tryCatch(
      lrm(fml, data = df, x = TRUE, y = TRUE, na.action = na.omit),
      error = function(e) {
        message(sprintf("[warn] K=%d 拟合失败：%s", K, conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(fit)) {
      aic_val <- tryCatch(AIC(fit), error = function(e) NA_real_)
      if (is.finite(aic_val)) aics <- rbind(aics, data.frame(K = K, AIC = aic_val))
    }
  }

  if (!nrow(aics)) stop("AIC 选择失败：请检查数据、结局变量是否为二分类、或协变量是否存在。")
  aics <- aics[order(aics$AIC), , drop = FALSE]
  list(best_k = aics$K[1], table = aics)
}

# -------------------------- 4) anova(fit) 稳健提取 --------------------------

.parse_p <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- as.character(x)[1]
  if (grepl("^\\s*<", x)) {
    num <- suppressWarnings(as.numeric(gsub("[^0-9.eE-]", "", x)))
    if (is.finite(num)) return(num / 2)
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x))
}

.parse_anova_text <- function(fit) {
  txt <- capture.output(print(anova(fit)))
  if (!length(txt)) return(NULL)

  hdr_i <- which(grepl("\\bd\\.?f\\.?\\b", txt, ignore.case = TRUE) & grepl("\\bP\\b", txt))
  if (!length(hdr_i)) return(NULL)
  start <- hdr_i[length(hdr_i)] + 1

  lines <- txt[start:length(txt)]
  lines <- lines[!grepl("^\\s*-{2,}\\s*$", lines)]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(NULL)

  eff <- stat <- ddf <- pp <- character(0)
  for (ln in lines) {
    if (grepl("^\\s*TOTAL", ln, ignore.case = TRUE)) next
    m <- regexec("^\\s*(.*?)\\s+([0-9\\.eE+-]+)\\s+([0-9\\.eE+-]+)\\s+([<> =0-9\\.eE+-]+)\\s*$", ln)
    res <- regmatches(ln, m)[[1]]
    if (length(res) == 5) {
      eff  <- c(eff,  trimws(res[2]))
      stat <- c(stat, res[3])
      ddf  <- c(ddf,  res[4])
      pp   <- c(pp,   res[5])
    }
  }
  if (!length(eff)) return(NULL)

  df <- data.frame(
    Effect = eff,
    Stat   = suppressWarnings(as.numeric(stat)),
    df     = suppressWarnings(as.numeric(ddf)),
    P      = sapply(pp, .parse_p),
    stringsAsFactors = FALSE
  )
  rownames(df) <- df$Effect
  df
}

.get_anova_df <- function(fit) {
  an_df <- suppressWarnings(try(as.data.frame(anova(fit)), silent = TRUE))
  if (!inherits(an_df, "try-error") && is.data.frame(an_df) && nrow(an_df) > 0) {
    rn <- rownames(an_df)
    p_idx    <- which(tolower(names(an_df)) %in% c("p", "p value", "p-value", "pr(>chi)"))[1]
    stat_idx <- grep("(wald|chi|chi[- ]?square|lr|score)", names(an_df), ignore.case = TRUE)[1]
    df_idx   <- grep("^(d\\.?f\\.?|df)$", names(an_df), ignore.case = TRUE)[1]
    if (is.na(stat_idx)) stat_idx <- match("Wald", names(an_df))
    if (is.na(stat_idx)) stat_idx <- match("Chi-Square", names(an_df))
    if (is.na(df_idx))   df_idx   <- match("d.f.", names(an_df))
    if (is.na(p_idx))    p_idx    <- match("P", names(an_df))

    ok <- all(!is.na(c(stat_idx, df_idx)))
    if (ok) {
      out <- data.frame(
        Effect = rn,
        Stat   = suppressWarnings(as.numeric(an_df[[stat_idx]])),
        df     = suppressWarnings(as.numeric(an_df[[df_idx]])),
        P      = if (!is.na(p_idx)) sapply(an_df[[p_idx]], .parse_p) else NA_real_,
        stringsAsFactors = FALSE
      )
      rownames(out) <- out$Effect
      return(out)
    }
  }

  .parse_anova_text(fit)
}

# -------------------------- 5) P 值提取器 --------------------------

.match_effect_row <- function(effects, x, allow_interaction = FALSE, strata = NULL) {
  idx <- which(effects == x)
  if (length(idx)) return(idx[1])

  x_esc <- gsub("([\\W])", "\\\\\\1", x, perl = TRUE)
  idx <- grep(paste0("rcs\\s*\\(\\s*", x_esc, "\\b"), effects, ignore.case = TRUE)
  if (length(idx)) return(idx[1])

  idx <- setdiff(grep(paste0("(^|[^A-Za-z0-9_])", x_esc, "([^A-Za-z0-9_]|$)"),
                      effects, perl = TRUE, ignore.case = TRUE),
                 grep("(\\*|:)", effects))
  if (length(idx)) return(idx[1])

  if (isTRUE(allow_interaction) && !is.null(strata)) {
    s_esc <- gsub("([\\W])", "\\\\\\1", strata, perl = TRUE)
    pats <- c(
      paste0("(?i)", x_esc, "\\s*(\\*|:)\\s*", s_esc),
      paste0("(?i)", s_esc, "\\s*(\\*|:)\\s*", x_esc)
    )
    for (pat in pats) {
      idx <- grep(pat, effects, perl = TRUE)
      if (length(idx)) return(idx[1])
    }
  }

  integer(0)
}

.find_nonlinear_after <- function(an, i, max_scan = 6) {
  if (i >= nrow(an)) return(NA_integer_)
  j_range <- seq(i + 1, min(nrow(an), i + max_scan))
  nl_idx <- j_range[grepl("Nonlinear", an$Effect[j_range], ignore.case = TRUE)]
  if (length(nl_idx)) nl_idx[1] else NA_integer_
}

extract_p_overall_nonlin <- function(fit, x) {
  an <- .get_anova_df(fit)
  if (is.null(an) || !nrow(an)) return(list(p_overall = NA_real_, p_nonlin = NA_real_))

  i <- .match_effect_row(an$Effect, x, allow_interaction = FALSE)
  if (!length(i)) return(list(p_overall = NA_real_, p_nonlin = NA_real_))

  p_overall <- an$P[i]
  if (is.na(p_overall) && is.finite(an$Stat[i]) && is.finite(an$df[i]) && an$df[i] > 0) {
    p_overall <- 1 - pchisq(an$Stat[i], an$df[i])
  }

  j <- .find_nonlinear_after(an, i, max_scan = 6)
  p_nonlin <- NA_real_
  if (!is.na(j)) {
    p_nonlin <- an$P[j]
    if (is.na(p_nonlin) && is.finite(an$Stat[j]) && is.finite(an$df[j]) && an$df[j] > 0) {
      p_nonlin <- 1 - pchisq(an$Stat[j], an$df[j])
    }
  }

  list(p_overall = p_overall, p_nonlin = p_nonlin)
}

extract_p_interaction <- function(fit, strata, x = NULL) {
  an <- .get_anova_df(fit)
  if (is.null(an) || !nrow(an)) return(NA_real_)

  if (is.null(x)) {
    s_esc <- gsub("([\\W])", "\\\\\\1", strata, perl = TRUE)
    idx <- grep(paste0("(^|.*)(\\*|:)\\s*", s_esc, "($|.*)"), an$Effect, perl = TRUE, ignore.case = TRUE)
    if (!length(idx)) return(NA_real_)
    j <- idx[1]
  } else {
    j <- .match_effect_row(an$Effect, x, allow_interaction = TRUE, strata = strata)
    if (!length(j)) return(NA_real_)
  }

  p_int <- an$P[j]
  if (is.na(p_int) && is.finite(an$Stat[j]) && is.finite(an$df[j]) && an$df[j] > 0) {
    p_int <- 1 - pchisq(an$Stat[j], an$df[j])
  }
  p_int
}

extract_p_nonlinear_main <- function(fit, x) {
  an <- .get_anova_df(fit)
  if (is.null(an) || !nrow(an)) return(NA_real_)

  i <- .match_effect_row(an$Effect, x, allow_interaction = FALSE)
  if (!length(i)) return(NA_real_)
  j <- .find_nonlinear_after(an, i, max_scan = 6)
  if (is.na(j)) return(NA_real_)

  p <- an$P[j]
  if (is.na(p) && is.finite(an$Stat[j]) && is.finite(an$df[j]) && an$df[j] > 0) {
    p <- 1 - pchisq(an$Stat[j], an$df[j])
  }
  p
}

# 每个分层内单独拟合 rcs 模型，提取该分层自身的 P_nonlinear（使用与交互模型相同的 K）
p_nonlinear_by_stratum <- function(df, y, x, strata, covars = NULL, K) {
  levs <- levels(df[[strata]])
  cov_adj <- setdiff(covars %||% character(), c(strata, "Sed_cat", "Ral_cat"))

  res <- setNames(rep(NA_real_, length(levs)), levs)
  for (s in levs) {
    df_s <- df[df[[strata]] == s, , drop = FALSE]
    vars_need <- unique(c(y, x, cov_adj))
    df_s <- na.omit(df_s[, vars_need, drop = FALSE])
    if (nrow(df_s) < 10) next

    dd_set <- .ensure_datadist(df_s)
    on.exit(.cleanup_datadist(dd_set), add = TRUE)

    rhs <- paste(c(sprintf("rcs(%s, %d)", x, K), cov_adj), collapse = " + ")
    fml <- as.formula(sprintf("%s ~ %s", y, rhs))

    fit_s <- tryCatch(
      lrm(fml, data = df_s, x = TRUE, y = TRUE, na.action = na.omit),
      error = function(e) NULL
    )
    if (is.null(fit_s)) next

    pvals <- extract_p_overall_nonlin(fit_s, x)
    res[s] <- pvals$p_nonlin
  }
  res
}

# 调试打印
print_anova_debug <- function(fit) {
  cat("--------- anova(fit) 原始打印 ---------\n")
  print(anova(fit))
  cat("\n--------- 标准化后的 anova 数据框 ---------\n")
  an <- .get_anova_df(fit)
  if (is.null(an)) {
    cat("(标准化失败)\n")
  } else {
    print(an, row.names = TRUE)
  }
  invisible(an)
}

# -------------------------- 6) 作图函数 --------------------------

rcs_plot_or_single <- function(df, y, x, covars = NULL, k_candidates = 3:6,
                               out_path = "figs/rcs_or_single.pdf",
                               x_lab = "Serum Mn (log)", y_lab = "Adjusted OR (95% CI)",
                               title = "Dose-response") {

  vars_need <- unique(c(y, x, covars))
  df_use <- na.omit(df[, vars_need, drop = FALSE])

  dd_set <- .ensure_datadist(df_use)
  on.exit(.cleanup_datadist(dd_set), add = TRUE)

  sel <- select_best_knots(df_use, y, x, covars, strata = NULL, k_candidates)
  K <- sel$best_k

  rhs <- paste(c(sprintf("rcs(%s, %d)", x, K), covars), collapse = " + ")
  fml <- as.formula(sprintf("%s ~ %s", y, rhs))
  fit <- lrm(fml, data = df_use, x = TRUE, y = TRUE, na.action = na.omit)

  x0 <- ref_x(df_use[[x]])
  pr <- eval(parse(text = sprintf("Predict(fit, %s, fun = exp, ref.zero = TRUE)", x)))
  pr <- as.data.frame(pr)

  pvals <- extract_p_overall_nonlin(fit, x)

  p <- ggplot() +
    geom_ribbon(data = pr, aes(x = .data[[x]], ymin = lower, ymax = upper),
                fill = alpha_col(col_single, 0.18), color = NA) +
    geom_line(data = pr, aes(x = .data[[x]], y = yhat),
              linewidth = 0.7, color = col_single) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.25, color = "grey40") +
    geom_vline(xintercept = x0, linetype = 3, linewidth = 0.25, color = "red") +
    annotate("text", x = x0, y = 1, label = sprintf("ref=%.2f", x0),
             hjust = -0.05, vjust = 1.2, size = pt_to_size(10), color = "red") +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("%s; %s", fmt_p(pvals$p_overall), fmt_p(pvals$p_nonlin)),
             hjust = 1.05, vjust = 1.8, size = pt_to_size(10)) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_clean_compact(base_size = 10)

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  grDevices::pdf(out_path, width = fig_w_in, height = fig_h_in, useDingbats = FALSE)
  print(p)
  grDevices::dev.off()

  list(fit = fit, best_k = K, aic_table = sel$table,
       plot = p, predict_df = pr, file = out_path,
       p_overall = pvals$p_overall, p_nonlinear = pvals$p_nonlin)
}

rcs_plot_or_stratified <- function(df, y, x, strata, covars = NULL, k_candidates = 3:6,
                                   out_path = "figs/rcs_or_stratified.pdf",
                                   x_lab = "Serum Mn (log)", y_lab = "Adjusted OR (95% CI)",
                                   title = "Dose-response by stratum",
                                   legend_title = "Stratum",
                                   level_labels = c(low = "Low", high = "High")) {

  vars_need <- unique(c(y, x, covars, strata))
  df_use <- na.omit(df[, vars_need, drop = FALSE])
  if (!is.factor(df_use[[strata]])) df_use[[strata]] <- factor(df_use[[strata]])

  dd_set <- .ensure_datadist(df_use)
  on.exit(.cleanup_datadist(dd_set), add = TRUE)

  # 1) 交互模型：用于绘图和 P_interaction
  sel <- select_best_knots(df_use, y, x, covars, strata, k_candidates)
  K <- sel$best_k

  cov_adj <- setdiff(covars %||% character(), c(strata, "Sed_cat", "Ral_cat"))
  rhs <- paste(c(sprintf("rcs(%s, %d) * %s", x, K, strata), cov_adj), collapse = " + ")
  fml <- as.formula(sprintf("%s ~ %s", y, rhs))
  fit <- lrm(fml, data = df_use, x = TRUE, y = TRUE, na.action = na.omit)

  # 2) 预测曲线（各分层 ref.zero = TRUE）
  levs <- levels(df_use[[strata]])
  preds <- lapply(levs, function(s) {
    call_txt <- sprintf("Predict(fit, %s, %s = '%s', fun = exp, ref.zero = TRUE)", x, strata, s)
    pr <- eval(parse(text = call_txt))
    d <- as.data.frame(pr)
    d[[strata]] <- s
    d
  })
  pr <- do.call(rbind, preds)

  # 3) 交互 P 值（整体）
  p_int <- extract_p_interaction(fit, strata, x)

  # 4) 分层内各自非线性 P 值（使用相同 K 的分层内模型）
  p_by_stratum <- p_nonlinear_by_stratum(df_use, y, x, strata, covars = covars, K = K)
  # 显示标签使用传入的 level_labels 名称映射
  lbl_map <- setNames(unname(level_labels[levs]), levs)

  # 参考竖线（总体中位数）
  x0 <- ref_x(df_use[[x]])

  # 5) 绘图
  p <- ggplot(pr, aes(x = .data[[x]], y = yhat, color = .data[[strata]], fill = .data[[strata]])) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = unname(col_pair[levs]),
                       labels = unname(level_labels[levs]),
                       name = legend_title) +
    scale_fill_manual(values = unname(sapply(col_pair[levs], alpha_col, alpha = 0.18)),
                      labels = unname(level_labels[levs]),
                      name = legend_title) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.25, color = "grey40") +
    geom_vline(xintercept = x0, linetype = 3, linewidth = 0.25, color = "red") +
    annotate("text", x = x0, y = 1, label = sprintf("ref=%.2f", x0),
             hjust = -0.05, vjust = 1.2, size = pt_to_size(10), color = "red") +
    annotate("text", x = Inf, y = Inf,
             label = paste0(
               "P_nonlinear(", lbl_map[levs[1]], "): ", fmt_p(p_by_stratum[levs[1]]), "\n",
               "P_nonlinear(", lbl_map[levs[2]], "): ", fmt_p(p_by_stratum[levs[2]]), "\n",
               "P_interaction: ", fmt_p(p_int)
             ),
             hjust = 1.05, vjust = 2.2, size = pt_to_size(10)) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_clean_compact(base_size = 10)

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  grDevices::pdf(out_path, width = fig_w_in, height = fig_h_in, useDingbats = FALSE)
  print(p)
  grDevices::dev.off()

  list(
    fit = fit, best_k = K, aic_table = sel$table,
    plot = p, predict_df = pr, file = out_path,
    p_nonlinear_by_stratum = p_by_stratum,
    p_interaction = p_int
  )
}

# -------------------------- 7) 示例调用（可按需注释） --------------------------

if (file.exists("数据\\imputed_df_v6.RData") && file.exists("数据\\genus_data.RData")) {
  load("数据\\imputed_df_v6.RData")
  load("数据\\genus_data.RData")

  genus_data$relative$SampleID <- row.names(genus_data$relative)
  data <- merge(imputed_df_v6, genus_data$relative, by = "SampleID")

  data$Sed_cat <- ifelse(data$g_Sediminibacterium >= median(data$g_Sediminibacterium, na.rm = TRUE), "high", "low")
  data$Sed_cat <- factor(data$Sed_cat, levels = c("low", "high"))

  data$Ral_cat <- ifelse(data$g_Ralstonia >= median(data$g_Ralstonia, na.rm = TRUE), "high", "low")
  data$Ral_cat <- factor(data$Ral_cat, levels = c("low", "high"))

  covariates <- c(
    "gender", "age", "BMI", "Marital_status",
    "education", "family_income_cat",
    "family_diabetes_history", "smoker", "Alcohol_consumption",
    "act_sport", "energy_kcal", "diet_Mn_mg"
  )

  # 单曲线
  res_single <- rcs_plot_or_single(
    df = data,
    y = "IFG_or_diabetes",
    x = "ln_Serum_Mn",
    covars = covariates,
    k_candidates = 3:6,
    out_path = "主要结果/图片/RCS/OR_diabetes.pdf",
    x_lab = "ln(Mn, μg/L)",
    y_lab = "Adjusted OR (95% CI)",
    title = "Serum manganese and IFG/T2DM"
  )

  # 分层曲线（Sed_cat）
  if ("Sed_cat" %in% names(data)) {
    res_sed <- rcs_plot_or_stratified(
      df = data,
      y = "IFG_or_diabetes",
      x = "ln_Serum_Mn",
      strata = "Sed_cat",
      covars = covariates,
      k_candidates = 3:6,
      out_path = "主要结果/图片/RCS/OR_IFG_or_T2DM_by_Sed_cat.pdf",
      x_lab = "ln(Mn, μg/L)",
      y_lab = "Adjusted OR (95% CI)",
      title = "Serum manganese and IFG/T2DM by Sediminibacterium",
      legend_title = "Level",
      level_labels = c(low = "Low", high = "High")
    )
  }

  # 分层曲线（Ral_cat）
  if ("Ral_cat" %in% names(data)) {
    res_ral <- rcs_plot_or_stratified(
      df = data,
      y = "IFG_or_diabetes",
      x = "ln_Serum_Mn",
      strata = "Ral_cat",
      covars = covariates,
      k_candidates = 3:6,
      out_path = "主要结果/图片/RCS/OR_IFG_or_T2DM_by_Ral_cat.pdf",
      x_lab = "ln(Mn, μg/L)",
      y_lab = "Adjusted OR (95% CI)",
      title = "Serum manganese and IFG/T2DM by Ralstonia",
      legend_title = "Level",
      level_labels = c(low = "Low", high = "High")
    )
  }

} else {
  message("未找到数据文件，已跳过示例调用。请检查路径：数据\\imputed_df_v6.RData 与 数据\\genus_data.RData")
}
