# ----------------------------------------------------------
# 确定研究对象
# ----------------------------------------------------------

rm(list = ls())

library(dplyr)

# 读取数据
data <- read.csv("Data/element_cleaned.csv", row.names = 1)

names(data)
samples <- data[which(!is.na(data$X66.Zn)), "SampleID"]
length(samples)
samples_with_Zn <- unique(samples)
save(samples_with_Zn, file = "Data/samples_with_Zn.RData")
