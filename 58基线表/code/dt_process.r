rm(list = ls())

# 加载r包
packages <- c("dplyr", "gtsummary", "summarytools", "flextable", "officer", "cards","cardx") # 需要加载的R包
# 设计函数，如果不存在上述r包，则指出哪个r包不存在，并安装后加载；如果存在，则直接加载
load_packages <- function(packages) {
  # 找出未安装的包
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  # 加载并返回加载结果（隐式返回）
  loaded <- vapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Package not available after installation: ", pkg)
      return(FALSE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
    TRUE
  }, logical(1))
  loaded
}
library(cards)
library(cardx)
load_packages(packages)
# 读取数据
cleveland <- read.csv("data\\processed.cleveland.data", header = FALSE, stringsAsFactors = FALSE, na.strings = "?")
hungarian <- read.csv("data\\processed.hungarian.data", header = FALSE, stringsAsFactors = FALSE, na.strings = "?")
switzerland <- read.csv("data\\processed.switzerland.data", header = FALSE, stringsAsFactors = FALSE, na.strings = "?")
va <- read.csv("data\\processed.va.data", header = FALSE, stringsAsFactors = FALSE, na.strings = "?")
# View(cleveland)
# View(hungarian)
# View(switzerland)
# View(va)

heart_disease_attributes <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
df_list <- list(cleveland, hungarian, switzerland, va)
merge_dt <- do.call(rbind, df_list)
colnames(merge_dt) <- heart_disease_attributes
# View(merge_dt)

merge_dt_v2 <- merge_dt %>%
  mutate(
    # 数值型变量（连续变量）- 缺失值保持NA
    age = as.numeric(age),
    trestbps = as.numeric(trestbps),
    chol = as.numeric(chol),
    thalach = as.numeric(thalach),
    oldpeak = as.numeric(oldpeak),
    ca = as.numeric(ca),
    
    # 因子型变量
    sex = factor(sex, 
                 levels = c(0, 1), 
                 labels = c("Female", "Male")),
    
    fbs = factor(fbs, 
                 levels = c(0, 1), 
                 labels = c("False", "True")),
    
    exang = factor(exang, 
                   levels = c(0, 1), 
                   labels = c("No", "Yes")),
    
    cp = factor(cp, 
                levels = c(1, 2, 3, 4),
                labels = c("Typical Angina",
                           "Atypical Angina",
                           "Non-anginal Pain",
                           "Asymptomatic")),
    
    restecg = factor(restecg,
                     levels = c(0, 1, 2),
                     labels = c("Normal",
                                "ST-T Abnormality",
                                "Left Ventricular Hypertrophy")),
    
    slope = factor(slope,
                   levels = c(1, 2, 3),
                   labels = c("Upsloping", "Flat", "Downsloping"),
                   ordered = TRUE),
    
    thal = factor(thal,
                  levels = c(3, 6, 7),
                  labels = c("Normal", "Fixed Defect", "Reversible Defect")),
    
    # 目标变量（二分类）
    num = case_when(
        num %in% c(1:4) ~ 1, # 将num值为2、3、4的记录归为1（表示有心脏病）
        num == 0 ~ 0,        # 将num值为0的记录归为0（表示无心脏病）
        TRUE ~ NA_real_      # 其他情况（如缺失值）保持NA
    ),
    num = factor(num, 
                 levels = c(0, 1), 
                 labels = c("No Disease", "Disease"))
  )
View(merge_dt_v2)
# 缺失值，缺失比例检查-----------------
miss <- sapply(merge_dt_v2, function(x) sum(is.na(x)))
miss_percent <- round(miss / nrow(merge_dt_v2) * 100, 2)
missing_data_summary <- data.frame(
  Variable = names(miss),
  Missing_Count = miss,
  Missing_Percent = miss_percent
)
View(missing_data_summary)
var_need <- c(
    "age",
    "sex",
    "cp",
    "trestbps",
    "chol",
    "fbs",
    "restecg",
    "thalach",
    "exang",
    "oldpeak",
    "num"
)

# 缺失值多重插补，随机选择其中一个结果--------------
library(mice)
merge_dt_v3 <- merge_dt_v2 %>%
  select(all_of(var_need))

# 设置mice参数
mice_imputed <- mice(merge_dt_v3, m = 5, method = "rf", seed = 123)

# 从mice结果中提取第一个完整数据集
merge_dt_v4 <- complete(mice_imputed, 1)
View(merge_dt_v4)

saveRDS(merge_dt_v4, "data/heart_disease_data.rds")
