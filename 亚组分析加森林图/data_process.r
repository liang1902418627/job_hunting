rm(list = ls()) # 清空环境变量
library(dplyr)
set.seed(2026)
n <- 1500

dt2 <- data.frame(
  follow_months = rexp(n, 0.05),
  burnout_event = rbinom(n, 1, 0.25),

  age = round(rnorm(n, 38, 8)),
  sex = factor(sample(c("Male","Female"), n, TRUE)),
  job_level = factor(sample(c("Junior","Mid","Senior"), n, TRUE)),
  marital = factor(sample(c("Married","Single"), n, TRUE)),
  income = factor(sample(c("Low","Mid","High"), n, TRUE)),

  stress_index = rnorm(n, 50, 10),
  workload = rnorm(n, 6, 2),
  department = factor(sample(c("IT","HR","Sales","Finance"), n, TRUE)),

  caffeine = factor(sample(c("Yes","No"), n, TRUE)),
  alcohol = factor(sample(c("Yes","No"), n, TRUE)),
  insomnia = factor(sample(c(0,1), n, TRUE)),
  anxiety = factor(sample(c(0,1), n, TRUE)),

  screen_hours = rnorm(n, 4, 1.2),
  screen_group = factor(sample(c("T1","T2","T3"), n, TRUE)),
  sleep_shift_cat = factor(sample(c("T1","T2","T3"), n, TRUE)),

  meeting_hours = rnorm(n, 15, 5),
  overtime_hours = rnorm(n, 10, 6),
  email_load = rnorm(n, 80, 30),
  physical_activity = rnorm(n, 150, 60),
  sleep_hours = rnorm(n, 6.5, 1.2)
)

# 年龄分组
dt2 <- dt2 %>%
  mutate(
    age_group = ifelse(age < 40, "<40", "≥40"),
    age_group = factor(age_group, levels = c("<40","≥40"))
  )

# stress_index 按中位数分组
med_stress <- median(dt2$stress_index, na.rm = TRUE)
dt2 <- dt2 %>%
  mutate(
    stress_index_group = ifelse(stress_index < med_stress, "Low","High"),
    stress_index_group = factor(stress_index_group, levels = c("Low","High"))
  )

# workload 分组
med_work <- median(dt2$workload)
dt2 <- dt2 %>%
  mutate(
    workload_group = ifelse(workload < med_work, "Low","High"),
    workload_group = factor(workload_group, levels = c("Low","High"))
  )

saveRDS(dt2, file = "亚组分析加森林图/employee_burnout_data.rds")
