# ---------------------------------------------------------
# 生成符合回归分析需求的虚拟数据（n = 1500）
# 保存路径: project/data/meta_data_clean.rds
# ---------------------------------------------------------
rm(list = ls())
paks <- c("dplyr", "MASS")
lapply(X = paks, FUN = library, character.only = TRUE)

set.seed(123) # 可重复

n <- 1500

# 1. 生成协变量（符合现实分布）
data_sim <- data.frame(
  id = 1:n,
  age = round(rnorm(n, mean = 45, sd = 15)),           # 年龄 18-80
  sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)),
  race = sample(c("White", "Black", "Hispanic", "Asian"), 
                n, replace = TRUE, prob = c(0.6, 0.15, 0.15, 0.10))
)

# 年龄截断到合理范围
data_sim$age <- pmax(18, pmin(85, data_sim$age))

# 吸烟：男性略高，年龄越大越可能吸烟（但非线性）
smoking_prob <- ifelse(data_sim$sex == "Male", 0.25, 0.20)
data_sim$smoking_status <- rbinom(n, 1, smoking_prob)
data_sim$smoking_status <- factor(data_sim$smoking_status, levels = 0:1, labels = c("No", "Yes"))

# 糖尿病：随年龄增加，约 10% 患病率
dm_prob <- pmin(0.3, pmax(0.02, (data_sim$age - 30) / 150))
data_sim$diabetes <- rbinom(n, 1, dm_prob)
data_sim$diabetes <- factor(data_sim$diabetes, levels = 0:1, labels = c("No", "Yes"))

# 心血管疾病：年龄、糖尿病、吸烟相关
cvd_logit <- -4 + 0.05 * data_sim$age + 
  0.8 * (data_sim$diabetes == "Yes") + 
  0.5 * (data_sim$smoking_status == "Yes")
cvd_prob <- plogis(cvd_logit)
data_sim$cardiovascular_disease <- rbinom(n, 1, cvd_prob)
data_sim$cardiovascular_disease <- factor(data_sim$cardiovascular_disease, levels = 0:1, labels = c("No", "Yes"))

# 2. 生成屏幕时间（daily_screen_hours）
# 假设平均 ~3.5 小时，右偏（用 gamma 或对数正态）
screen_hours <- rgamma(n, shape = 2, scale = 1.8)  # 均值 ≈ 3.6
screen_hours <- pmin(screen_hours, 12)             # 截断上限
data_sim$daily_screen_hours <- round(screen_hours, 1)

# 创建分组：低 (<2), 中 (2-4), 高 (>4)
data_sim$screen_time_group <- cut(data_sim$daily_screen_hours,
                                  breaks = c(-Inf, 2, 4, Inf),
                                  labels = c("Low", "Medium", "High"),
                                  right = FALSE)
data_sim$screen_time_group <- factor(data_sim$screen_time_group, 
                                     levels = c("Low", "Medium", "High"))

# 标准化屏幕时间（z-score）
data_sim$screen_time_zscore <- scale(data_sim$daily_screen_hours)[,1]

# 四分位（用于趋势检验）
quartiles <- quantile(data_sim$daily_screen_hours, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
data_sim$screen_time_quartile <- as.numeric(cut(data_sim$daily_screen_hours, breaks = quartiles, include.lowest = TRUE, labels = 1:4))

# 3. 生成结局变量：phq9_score（0-27）
# 假设：屏幕时间↑ → 抑郁评分↑；女性更高；吸烟、慢性病也增加风险
linear_pred <- 
  5.0 + 
  1.2 * (data_sim$screen_time_zscore) +          # 屏幕时间效应
  1.5 * (data_sim$sex == "Female") +             # 女性更高
  0.8 * (data_sim$smoking_status == "Yes") +     # 吸烟者更高
  1.0 * (data_sim$diabetes == "Yes") +           # 慢性病影响
  1.2 * (data_sim$cardiovascular_disease == "Yes") +
  0.02 * data_sim$age                            # 轻微年龄效应

# 加入随机误差
phq9_score <- rnorm(n, mean = linear_pred, sd = 3.5)
# 截断到 0-27（PHQ-9 范围）
data_sim$phq9_score <- pmax(0, pmin(27, round(phq9_score, 1)))

# 4. 保留所需变量（按新命名）
meta_data <- data_sim %>%
  dplyr::select(
    screen_time_group,
    daily_screen_hours,
    screen_time_zscore,
    screen_time_quartile,
    phq9_score,
    age,
    sex,
    race,
    smoking_status,
    diabetes,
    cardiovascular_disease
  )

# 查看数据结构
str(meta_data)
head(meta_data)

# 保存虚拟数据（可选）
saveRDS(meta_data, "project/data/meta_data_clean.rds")