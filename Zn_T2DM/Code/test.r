rm(list = ls())
library(dplyr)
dt <- readRDS("Data/md_final_v2.rds")

# 1. 计算血清锌(Serum_Zn)的四个四分位点 (0%, 25%, 50%, 75%, 100%)
quartiles <- quantile(dt$Serum_Zn, probs = seq(0, 1, 0.25), na.rm = TRUE)
print("四分位点数值：")
print(quartiles)
md_final_v2 <- dt %>%
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
md_final_v3 <- dt %>%
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