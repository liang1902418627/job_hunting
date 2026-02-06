# ------------------------------------------
# 题目：T2DM与肠道菌总体的关系分析
# 箱图, PCoA, Adonis
# -------------------------------------------
rm(list = ls())
setwd("C:\\lianghao\\lianghao\\Zn_Gut_T2DM")

# 加载所需要的r包
paks <- c("data.table", "ggplot2", "dplyr", "vegan", "rstatix", "readr", "broom", "extrafont", "ggpubr")

for (pak in paks) {
    if (!requireNamespace(pak, quietly = TRUE)) {
        install.packages(pak)
    }
    library(pak, character.only = TRUE)
}
# 加载字体到 Windows 设备（每次启动 R 后需要运行一次）
loadfonts(device = "win")
# 读取数据
md <- readRDS("Data/md_final_v2.rds")
gut <- readRDS("Data/gut_data.rds")
alpha <- read.csv("Data/Mn_T2DM_alpha.csv") %>%
    select(SampleID, Richness, Shannon, Pielou, Simpson)
names(alpha)

# 检查数据
names(md)
names(gut)
names(gut$alpha_diversity)

# 合并数据
md_v2 <- md %>%
    left_join(alpha, by = "SampleID")
nrow(md_v2)

# 缺失值检查
# 去除含有NA值的行
md_v3 <- md_v2 %>%
    drop_na(Richness, Shannon, Pielou, Simpson)
nrow(md_v3)
# 数据选择-----------
# T2DM，"Richness" "Shannon"  "Pielou"   "Simpson"
md_t2dm <- md_v2 %>%
    select(SampleID, T2DM, Richness, Shannon, Pielou, Simpson)

# 宽数据转长数据
md_t2dm_long <- md_t2dm %>%
    tidyr::pivot_longer(
        cols = c(Richness, Shannon, Pielou, Simpson),
        names_to = "Alpha_Diversity",
        values_to = "Value"
    )
# View(md_t2dm_long)

# 箱线图绘制-----------
# 分组变量： T2DM
# 分面变量： Alpha_Diversity
# 值： Value

# 设置比较组
group <- levels(factor(md_t2dm$T2DM))
comp <- combn(group, 2, simplify = FALSE) # <- 改成列表，每个元素是长度为2的向量

# 配置箱线图参数
base_size <- 15

# ggplot2: element_text(size=) 的单位是 pt
# geom_text/stat_compare_means 的 size 单位是 mm，需要 pt -> mm 的换算：mm = pt / ggplot2::.pt
anno_size_mm <- base_size / ggplot2::.pt

# 绘图
plot_list <- list(
    outcomes = c("Richness", "Shannon", "Pielou", "Simpson"),
    group_var = "T2DM",
    group_labels = c("No", "Yes"),
    y_labels = c("Richness", "Shannon", "Pielou", "Simpson"),
    box_colors = c("#93C8ED", "#F7C1C1"),
    plot_title = "Boxplot of Alpha Diversity by T2DM Status",
    out_path = "Results\\T2DM_gut_总体\\figures",
    data = md_t2dm,
    theme = theme_classic(base_family = "Times New Roman", base_size = base_size)
)

# 检查并创建输出目录
if (!dir.exists(plot_list$out_path)) {
    dir.create(plot_list$out_path, recursive = TRUE)
}

# 绘制箱线图并存储在列表中
p_list <- list()
for (i in 1:length(plot_list$outcomes)) {
    ycol <- plot_list$outcomes[i]

    # 动态计算y轴位置（避免全NA导致 -Inf）
    max_y <- suppressWarnings(max(plot_list$data[[ycol]], na.rm = TRUE))
    if (!is.finite(max_y)) max_y <- 0
    label_y_pos <- max_y * 1.05

    # ——箱线图：刻度标签纯黑——
    p <- ggboxplot(
        plot_list$data,
        x = plot_list$group_var, y = ycol,
        color = plot_list$group_var,
        fill = NA,
        palette = plot_list$box_colors,
        ylab = plot_list$y_labels[i],
        xlab = "T2DM"
    ) +
        scale_x_discrete(labels = plot_list$group_labels) +
        stat_compare_means(
            method = "t.test",
            label = "p.format",
            label.y = label_y_pos,
            size = anno_size_mm,
            family = "Times New Roman"
        ) +
        plot_list$theme +
        theme(
            legend.position = "none",
            legend.title = element_blank(),

            axis.text.x = element_text(size = base_size, family = "Times New Roman", color = "black"),
            axis.text.y = element_text(size = base_size, family = "Times New Roman", color = "black"),
            axis.title.x = element_text(
                size = base_size, family = "Times New Roman",
                margin = margin(t = 10, r = 0, b = 0, l = 0)
            ),
            axis.title.y = element_text(
                size = base_size, family = "Times New Roman",
                margin = margin(t = 0, r = 25, b = 0, l = 0)
            )
        )

    p_list[[ycol]] <- p
}
print(p_list$Richness)
print(p_list$Shannon)
print(p_list$Pielou)
print(p_list$Simpson)
# 保存图片4.83*5.19inch；pdf
# --------- 逐个保存子图为 PDF ---------
pdf_w <- 4.83
pdf_h <- 5.19

# 保存每个单独箱线图
for (nm in names(p_list)) {
    ggplot2::ggsave(
        filename = file.path(plot_list$out_path, paste0(nm, ".pdf")),
        plot = p_list[[nm]],
        width = pdf_w, height = pdf_h, units = "in",
        device = grDevices::cairo_pdf
    )
}

# PCoA 图绘制及Adonis分析--------------------------------
# 数据整理，以相对丰度计算距离矩阵
rel <- gut$relative %>%
    filter(row.names(.) %in% md_v2$SampleID)
nrow(rel)
# 数据检查
str(rel)
# 计算 Bray-Curtis 距离矩阵
bray_dist <- vegdist(rel, method = "bray") # 使用Bray-Curtis距离

# adonis 分析
formula_str <- paste("bray_dist ~", "T2DM")
adonis_result <- adonis2(
    as.formula(formula_str),
    data = md_v2,
    permutations = 999
)

# 提取R值和p值
table <- tidy(adonis_result)
print(table)
print(adonis_result)
R <- table[1, "R2"]
p_value <- table[1, "p.value"]

# PCoA分析----------------------------------------------
pcoa <- cmdscale(bray_dist, k = 2, eig = TRUE) # k=2表示提取前两个主坐标

# 提取PCoA坐标
pcoa_points <- data.frame(
    SampleID = rownames(rel),
    PCo1 = pcoa$points[, 1],
    PCo2 = pcoa$points[, 2]
)

# 计算解释的变异比例
explained_var <- round(pcoa$eig / sum(pcoa$eig) * 100, 2)

# 合并Zn信息
pcoa_df <- merge(pcoa_points, md_v2[, c("SampleID", "T2DM")], by.x = "SampleID", by.y = "SampleID")

# ——PCoA：刻度标签纯黑——
plot_pcoa <- function(pcoa_df, title) {

    pcoa_base_size <- 12
    pcoa_anno_size_mm <- pcoa_base_size / ggplot2::.pt

    p <- ggplot(pcoa_df, aes(x = PCo1, y = PCo2, color = T2DM)) +
        geom_point(size = 0.4, alpha = 0.9, stroke = 0.2) +
        stat_ellipse(aes(group = T2DM),
            level = 0.95,
            linetype = "solid", linewidth = 0.5
        ) +
        labs(
            title = title,
            x = paste0("PCo1 (", explained_var[1], "%)"),
            y = paste0("PCo2 (", explained_var[2], "%)"),
            color = "T2DM"
        ) +
        theme_bw(base_family = "Times New Roman", base_size = pcoa_base_size) +
        theme(
            panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
            panel.grid.major = element_line(color = "gray95", linewidth = 0.25),
            panel.grid.minor = element_line(color = "gray95", linewidth = 0.10),

            plot.title = element_text(hjust = 0.5, face = "bold", size = pcoa_base_size, family = "Times New Roman"),
            axis.title = element_text(face = "bold", size = pcoa_base_size, family = "Times New Roman"),
            axis.text  = element_text(size = pcoa_base_size, family = "Times New Roman", color = "black"),

            legend.title = element_text(face = "bold", size = pcoa_base_size, family = "Times New Roman"),
            legend.text  = element_text(size = pcoa_base_size, family = "Times New Roman"),

            legend.position = "right",
            legend.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
            legend.key.height = unit(0.22, "cm"),
            legend.key.width  = unit(0.22, "cm"),
            legend.spacing.y  = unit(0, "cm"),
            legend.margin = margin(2, 2, 2, 2),
            legend.box.margin = margin(0, 0, 0, 0)
        ) +
        guides(color = guide_legend(override.aes = list(size = 1))) +
        scale_color_manual(values = c("#93C8ED", "#F7C1C1")) +
        annotate("text", x = Inf, y = Inf,
            label = paste0("Adonis R² = ", round(R, 4), "\n", "p = ", signif(p_value, 3)),
            hjust = 1.1, vjust = 1.1,
            size = pcoa_anno_size_mm,
            family = "Times New Roman"
        )

    return(p)
}

p <- plot_pcoa(pcoa_df, "PCoA based on Bray-Curtis Distance")
# print(p)
ggsave(
    filename = file.path(plot_list$out_path, "PCoA_T2DM.pdf"),
    plot = p,
    width = 6.10, height = 4.70, units = "in",
    device = grDevices::cairo_pdf
)
