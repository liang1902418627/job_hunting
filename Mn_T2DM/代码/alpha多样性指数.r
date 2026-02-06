rm(list = ls()) # 清空工作空间

# 加载r包
# pak::pkg_install(c("dplyr", "tidyr", "vegan"))
# 创建一个包含要加载的 R 包名称的字符向量
packages <- c("dplyr", "tidyr", "vegan")

# 使用 lapply() 批量加载 R 包
lapply(packages, library, character.only = TRUE)

# 加载数据
load("数据/genus_data.RData")

##### 计算alpha多样性指数
# 绝对丰度数据
genus_Absolute <- genus_data$absolute

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

View(alpha_diversity)

genus_data[["alpha_diversity"]] <- alpha_diversity
save(genus_data, file = "数据/genus_data.RData")
