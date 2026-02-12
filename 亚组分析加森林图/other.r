# 基本用法示例
library(survival)
library(dplyr)

# 准备数据
lung_prepared <- lung %>%
  mutate(
    status = as.integer(status == 2),  # 将status转换为0/1
    sex = factor(sex, labels = c("Male", "Female")),
    age_group = factor(ifelse(age > 65, ">65", "≤65")),
    performance = factor(ifelse(ph.karno >= 80, "Good", "Poor"))
  )

# Cox模型亚组分析
result <- TableSubgroupMultiCox(
  formula = Surv(time, status) ~ sex,
  var_subgroups = c("age_group", "performance"), 
  data = lung_prepared,
  line = TRUE,
  decimal = 2
)

print(result)

# 逻辑回归亚组分析示例
library(dplyr)

# 使用mtcars数据创建示例
data_prepared <- mtcars %>%
  mutate(
    vs = factor(vs, labels = c("V-shaped", "Straight")),
    am = factor(am, labels = c("Automatic", "Manual")),
    cyl_group = factor(ifelse(cyl > 6, "High", "Low")),
    hp_group = factor(ifelse(hp > 150, "High", "Low"))
  )

# 逻辑回归亚组分析
result_glm <- TableSubgroupMultiGLM(
  formula = vs ~ am,
  var_subgroups = c("cyl_group", "hp_group"),
  data = data_prepared,
  family = "binomial",
  line = TRUE,
  decimal = 3
)

print(result_glm)

# 线性回归亚组分析示例
result_lm <- TableSubgroupMultiGLM(
  formula = mpg ~ wt,
  var_subgroups = c("am", "cyl_group"),
  data = data_prepared,
  family = "gaussian",
  line = FALSE
)

print(result_lm)