# --------------------------------------------------------
# 题目：绘制MaAsLin2(Mn 与 IFG_or_diabetes)森林图以及Gut 与 IFG_or_diabetes森林图
# 1. Mn 与 Gut森林图
# 2. Gut 与 IFG_or_diabetes森林图
# ----------------------------------------------------------
rm(list = ls())
setwd("D:/lianghao/金属锰—肠道菌—疾病/血清锰-肠道菌-糖尿病相关")

# 加载必要的包
paks <- c("ggplot2", "extrafont", "dplyr", "tidyr", "reshape2", "patchwork")
lapply(paks, library, character.only = TRUE)

# 加载字体到 Windows 设备（每次启动 R 后需要运行一次）
loadfonts(device = "win")
# 设置数据表------------------------------------
base_data <- list(
    Mn_Gut = list(
        g_Bradyrhizobium = list(
            coef = 0.002,
            stderr = 0.001
        ),
        g_Ralstonia = list(
            coef = -0.008,
            stderr = 0.003
        ),
        g_Devosia = list(
            coef = 0.005,
            stderr = 0.002
        ),
        g_Lachnospira = list(
            coef = 0.006,
            stderr = 0.001
        ),
        g_Sediminibacterium = list(
            coef = 0.007,
            stderr = 0.003
        ),
        g_Acidaminococcus = list(
            coef = 0.002,
            stderr = 0.001
        )
    ),
    Gut_IFG_or_diabetes = list(
        g_Sediminibacterium = list(
            coef = 1.158,
            low_ci = 1.037,
            up_ci = 1.294
        ),
        g_Bradyrhizobium = list(
            coef = 1.138,
            low_ci = 1.022,
            up_ci = 1.268
        ),
        g_Acidaminococcus = list(
            coef = 1.113,
            low_ci = 1.007,
            up_ci = 1.227
        ),
        g_Devosia = list(
            coef = 0.994,
            low_ci = 0.862,
            up_ci = 1.137
        ),
        g_Ralstonia = list(
            coef = 1.124,
            low_ci = 1.006,
            up_ci = 1.258
        ),
        g_Lachnospira = list(
            coef = 0.625,
            low_ci = 0.440,
            up_ci = 0.810
        )
    )
)

# 设置根据标准误计算置信区间函数---------------------------------
get_ci_from_se <- function(coef, se, z = 1.96) {
    low_ci <- coef - z * se
    up_ci  <- coef + z * se
    return(c(low_ci = low_ci, up_ci = up_ci))
}

# 计算 Mn_Gut 数据的置信区间---------------------------------
for (taxa in names(base_data$Mn_Gut)) {
    coef <- base_data$Mn_Gut[[taxa]]$coef
    se   <- base_data$Mn_Gut[[taxa]]$stderr
    ci   <- get_ci_from_se(coef, se)
    base_data$Mn_Gut[[taxa]]$low_ci <- ci["low_ci"]
    base_data$Mn_Gut[[taxa]]$up_ci  <- ci["up_ci"]
}
# View(base_data$Mn_Gut)

# 去掉列表中的标准误元素， 不是赋值为缺失---------------------------------------
for (taxa in names(base_data$Mn_Gut)) {
    base_data$Mn_Gut[[taxa]]$stderr <- NULL
}
# View(base_data)

# 将数据列表转换为数据框，列名分别为group, coef, low_ci, up_ci------------------------------
# group指的是：Mn_Gut，Gut_IFG_or_diabetes
df <- melt(base_data)
# View(df)
colnames(df) <- c("value", "value_name", "taxa", "group")
# View(df)

# 转换为宽格式
df_wide <- df %>%
  pivot_wider(
    names_from = value_name,  # 列名来自哪个列
    values_from = value    # 值来自哪个列
  )
# View(df_wide)
unique(df_wide$taxa)

# 添加门信息
phylum_mapping <- data.frame(
  taxa = c("g_Bradyrhizobium", "g_Ralstonia", "g_Devosia", 
           "g_Lachnospira", "g_Sediminibacterium", "g_Acidaminococcus"),
  phylum = c("p_Proteobacteria", "p_Proteobacteria", "p_Proteobacteria",
             "p_Firmicutes", "p_Bacteroidetes", "p_Firmicutes")
)
df_final <- merge(df_wide, phylum_mapping, by = "taxa")

# 按门和属排序，并设置因子水平以在 ggplot 中保持顺序
df_final <- df_final %>%
  arrange(phylum, taxa) %>%
  mutate(taxa = factor(taxa, levels = unique(taxa)))

# View(df_final)

# 配色设置--------------------------------------------
phylum_colors <- c(
    "p_Proteobacteria" = "#984EA3",  # 蓝色
    "p_Firmicutes"     = "#4DAF4A",  # 橙色
    "p_Bacteroidetes"  = "#377EB8"   # 绿色
)

# 绘制森林图--------------------------------------------
# 分面：group
# 填色：phylum
# 插入竖线：x=0（Mn_Gut），x=1（Gut_IFG_or_diabetes）
# -------------------------------------------------------
p <- ggplot(df_final, aes(x = taxa, y = coef, color = phylum)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = low_ci, ymax = up_ci), width = 0.2, position = position_dodge(width = 0.5)) +
    facet_wrap(~ group, scales = "free_x") +
    geom_hline(data = data.frame(group = c("Mn_Gut", "Gut_IFG_or_diabetes"), hline = c(0, 1)),
               aes(yintercept = hline), linetype = "dashed", color = "#E41A1C") +
    scale_color_manual(values = phylum_colors) +
    labs(x = "Taxa", y = "Coefficient (95% CI)", color = "Phylum") +
    theme_bw() +
    theme(
        text = element_text(family = "Times New Roman", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    coord_flip()

# 加载 patchwork 包
library(patchwork)

# 1. 为 Mn_Gut 创建图
p_mn_gut <- df_final %>%
  filter(group == "Mn_Gut") %>%
  ggplot(aes(x = taxa, y = coef, color = phylum)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low_ci, ymax = up_ci), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#E41A1C") +
  scale_color_manual(values = phylum_colors, guide = "none") + # 隐藏此图的图例
  labs(x = NULL, y = "Coefficient (95% CI)", title = "Mn_Gut") +
  coord_flip() +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman", size = 13), # 基础字体，影响未单独设置的元素
    plot.title = element_text(hjust = 0.5, size = 13),      # 图标题
    axis.title = element_text(size = 13),                     # 坐标轴标题
    axis.text.y = element_text(size = 13, color = "black")    # Y轴标签（属名）
  )

# 2. 为 Gut_IFG_or_diabetes 创建图
p_gut_diabetes <- df_final %>%
  filter(group == "Gut_IFG_or_diabetes") %>%
  ggplot(aes(x = taxa, y = coef, color = phylum)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = low_ci, ymax = up_ci), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#E41A1C") +
  scale_color_manual(values = phylum_colors) +
  labs(x = NULL, y = "OR (95% CI)", title = "Gut_IFG_or_diabetes", color = "Phylum") +
  coord_flip() + # 翻转坐标轴
  theme_bw() + # 使用白色背景主题
  theme(
    text = element_text(family = "Times New Roman", size = 13), # 基础字体
    plot.title = element_text(hjust = 0.5, size = 13),      # 图标题
    axis.title = element_text(size = 13),                     # 坐标轴标题
    legend.title = element_text(size = 13),                   # 图例标题
    legend.text = element_text(size = 13),                    # 图例文字
    axis.text.y = element_blank(), # 隐藏y轴标签，避免重复
    axis.ticks.y = element_blank()
  )

# 3. 使用 patchwork 组合两个图
combined_plot <- p_mn_gut + p_gut_diabetes + plot_layout(guides = 'collect') # 共享图例

print(combined_plot)

