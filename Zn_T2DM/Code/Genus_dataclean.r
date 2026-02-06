# ----------------------------------------------------------
# 处理属水平肠道菌群数据
# 计算alpha多样性指数
# ----------------------------------------------------------

# 清空环境变量
rm(list = ls())

# 载入必要的包
library(dplyr)
library(vegan)
source("Code/myfunctions.r") # 加载自编函数

# 读取属水平肠道菌群数据
genus_data <- read.csv("Data/OTU_Genus_7009.csv")
str(genus_data)
View(genus_data)
names(genus_data)
# 数据转换
gut_data <- rarefy_transform_gut_microbiota(
    df_microbiota = genus_data,
    sample_id_col = "SampleID"
)
names(gut_data)
dim(gut_data$absolute)

# 计算alpha多样性指数------------------------------------------------
# 绝对丰度数据
genus_Absolute <- gut_data$absolute

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

# 将alpha多样性指数添加到gut_data中
gut_data$alpha_diversity <- alpha_diversity

saveRDS(gut_data, file = "data/gut_data.rds")
