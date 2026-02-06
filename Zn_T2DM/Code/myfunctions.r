# 加载包
# packages <- c("tidyverse","ppcor")
# for (pkg in packages) {
#     if (!requireNamespace(pkg, quietly = TRUE)) {
#         install.packages(pkg)
#     }
#     library(pkg, character.only = TRUE)
# }
a <- c(
    "注意：有一些大的函数，与其他函数可能不是独立的"
)
# =====创建公式=======================
create_formula <- function(outcome, exposure, covariates = vector(length = 0)) {
    if (!is.character(outcome) || length(outcome) != 1) {
        stop("因变量(outcome)必须是长度为1的字符串")
    }
    if (!is.character(exposure) || length(exposure) != 1) {
        stop("自变量(exposure)必须是长度为1的字符串")
    }
    if (!is.character(covariates) && length(covariates) > 0) {
        stop("协变量(covariates)必须是字符串向量")
    }
    if (length(covariates) > 0) {
        formula_str <- paste(outcome, "~", exposure, "+", paste(covariates, collapse = " + "))
    } else {
        formula_str <- paste(outcome, "~", exposure)
    }
    return(as.formula(formula_str))
}
# =====加载r包================
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
# =====计算置信区间下限的函数==============
low_ci <- function(x, sd) {
    x - 1.96 * sd
}

# =====计算置信区间上限的函数==============
up_ci <- function(x, sd) {
    x + 1.96 * sd
}

# =====格式化为2位小数字符串的函数===================
format_2 <- function(x) {
    sprintf("%.2f", x)
}

# =====显著性标志================
significance <- function(p) {
    if (length(p) == 0 || is.na(p)) {
        return("")
    } else if (p < 0.01) {
        return("**")
    } else if (p < 0.05) {
        return("*")
    } else {
        return("")
    }
}
# =====显著性标志================
significance_for_vector <- function(p) {
    # 首先检查p是否为空或NULL
    if (is.null(p) || length(p) == 0) {
        return(character(0)) # 返回一个空的字符向量
    }

    # 创建一个与p相同长度的字符向量
    result <- character(length(p))

    # 为每个p值分配显著性标记
    for (i in seq_along(p)) {
        if (is.na(p[i])) {
            result[i] <- ""
        } else if (p[i] < 0.001) {
            result[i] <- "***"
        } else if (p[i] < 0.01) {
            result[i] <- "**"
        } else if (p[i] < 0.05) {
            result[i] <- "*"
        } else {
            result[i] <- ""
        }
    }

    return(result)
}
# =====构建rcs回归模型方程========================
formula_rcs <- function(outcome, exposure, nodes = 3, covariates = character()) {
    # 检查参数
    if (!is.character(outcome) || length(outcome) != 1) stop("outcome 必须是长度为1的字符串")
    if (!is.character(exposure) || length(exposure) != 1) stop("exposure 必须是长度为1的字符串")
    if (!is.numeric(nodes) || length(nodes) < 1) stop("nodes 必须是数值（结点数或结点位置向量）")
    if (!is.character(covariates)) stop("covariates 必须是字符向量")

    # 结点数或结点向量
    knot_str <- if (length(nodes) == 1) as.character(nodes) else paste0("c(", paste(nodes, collapse = ", "), ")")

    # 右侧表达式
    rhs <- paste0("rcs(", exposure, ", ", knot_str, ")")
    if (length(covariates) > 0) {
        rhs <- paste(rhs, paste(covariates, collapse = " + "), sep = " + ")
    }

    as.formula(paste(outcome, "~", rhs))
}

# =====储存偏相关分析结果的数据框=================
ppcor_result_df <- function(var1, var2, method, n, gp, estimate, statistic, p_value) {
    data.frame(
        Var1 = var1,
        Var2 = var2,
        Method = method,
        N = n,
        Gp = gp,
        Estimate = estimate,
        Statistic = statistic,
        P_value = p_value
    )
}

# =====偏相关分析函数=================
# 2. 偏相关分析
pcor_s <- function(x1, x2, cor_s_con, cor_s_fac, data, method = "spearman") {
    # 删除相关列中缺失值所在的行
    complete_idx <- complete.cases(data[, c(x1, x2, cor_s_con, cor_s_fac)])
    data <- data[complete_idx, ]
    formula <- as.formula(paste("~", paste(cor_s_fac, collapse = " + "), "- 1"))
    covariates <- cbind(
        data[, cor_s_con],
        model.matrix(formula, data = data)[, -1]
    )
    result <- ppcor::pcor.test(
        data[, x1],
        data[, x2],
        covariates,
        method = method
    ) # 进行偏相关分析，已为分类变量创建虚拟变量并纳入协变量

    return(result)
}
# =====检查向量长度==========================
check_vector_length <- function(df) {
    # 使用sapply计算每列的长度
    lengths <- sapply(df, length)
    # 打印每个变量的名称及其对应的长度
    print(lengths)
    # 返回包含每个变量长度的结果向量
    return(lengths)
}
# =====函数定义：检查数据框中哪些列存在缺失值=========================
check_missing_values <- function(data) {
    # 计算每列NA值的数量
    na_counts <- sapply(data, function(x) sum(is.na(x)))

    # 找出有缺失值的列
    missing_cols <- which(na_counts > 0)

    if (length(missing_cols) == 0) {
        return("数据框中没有缺失值")
    } else {
        # 创建包含列索引、列名和NA数量的数据框
        result <- data.frame(
            column_index = missing_cols,
            column_name = names(missing_cols),
            na_count = na_counts[missing_cols]
        )

        return(result)
    }
}

# =====创建回归结果表格=======================
df <- function(
    reg_type, bacterium,
    phenotype, beta, low_ci,
    up_ci, t_or_z, p_value) {
    data.frame(
        reg_type = reg_type,
        bacterium = bacterium,
        phenotype = phenotype,
        beta = beta,
        low_ci = low_ci,
        up_ci = up_ci,
        t_or_z = t_or_z,
        p_value = p_value
    )
}

# =====从回归模型中提取值===========
extract_values <- function(model) {
    # 提取回归模型的系数、置信区间、t值和p值
    beta <- coef(model)[2]
    ci <- confint(model)[2, ]
    low_ci <- ci[1]
    up_ci <- ci[2]
    t_or_z <- summary(model)$coefficients[2, 3]
    p_value <- summary(model)$coefficients[2, 4]

    return(c(beta, low_ci, up_ci, t_or_z, p_value))
}
# =====检查变量是否存在于 数据框 中==================
check_var_exist <- function(vars, df) {
    # vars: 变量名的字符向量
    for (var in vars) {
        if (!var %in% colnames(df)) {
            # 如果变量不存在于 df 中，print出来
            print(paste("变量: ", var, "不存在于数据框中"))
        }
    }
}

# =====回归循环的表格（包含logistic回归和线性回归，自变量为连续型变量或因子型变量）==================
#' 回归分析函数：支持逻辑回归与线性回归，自动跳过拟合失败的情况
#'
#' @description
#' 对指定暴露变量、结果变量、协变量进行逐一回归分析，自动判断因变量类型选择逻辑回归或线性回归。
#' 如果某次模型拟合失败（如数据异常或收敛问题），则自动跳过该模型，并输出警告信息。
#'
#' @param exposure_vars 暴露变量名向量（character vector）
#' @param outcome_vars 结果变量名向量（character vector）
#' @param covariate_vars 协变量名向量（character vector）
#' @param data 数据框（data.frame）
#'
#' @return 返回一个列表，包含逻辑回归结果数据框 `result_df_log`，线性回归结果数据框 `result_df_lin`，以及合并后的结果数据框 `result_df`
#'
#' @examples
#' reg_log_lin(
#'     exposure_vars = c("A", "B"),
#'     outcome_vars = c("Y"),
#'     covariate_vars = c("C1", "C2"),
#'     data = dat
#' )
#'
reg_log_lin <- function(exposure_vars, outcome_vars, covariate_vars, data) {
    # 完成时播放声音
    if (!requireNamespace("beepr", quietly = TRUE)) {
        message("提示：请安装 'beepr' 包以便完成时播放提示音：install.packages('beepr')")
        install.packages("beepr")
        library(beepr)
    } else {
        library(beepr)
    }
    # 检查暴露变量、结果变量和协变量是否存在于数据框中
    check_var_exist(exposure_vars, data)
    check_var_exist(outcome_vars, data)
    check_var_exist(covariate_vars, data)

    # 初始化结果数据框
    result_df_log <- data.frame()
    result_df_lin <- data.frame()

    # 遍历结果变量向量
    for (i in outcome_vars) {
        if (is.factor(data[[i]])) { # 如果结果变量是因子型变量，进行逻辑回归
            for (j in exposure_vars) {
                formula <- create_formula(i, j, covariate_vars) # 创建公式
                fit <- tryCatch(
                    glm(formula, data = data, family = "binomial"),
                    error = function(e) {
                        message(sprintf("逻辑回归拟合失败：phenotype=%s, bacterium=%s，已跳过。错误：%s", i, j, e$message))
                        return(NULL)
                    }
                )
                if (is.null(fit)) next

                # 检查暴露变量是否为具有多个层级的因子
                if (is.factor(data[[j]]) && length(levels(data[[j]])) > 1) {
                    coef_summary <- summary(fit)$coefficients
                    coef_names <- rownames(coef_summary)

                    factor_levels <- levels(data[[j]])
                    ref_level <- factor_levels[1]

                    for (level in factor_levels[-1]) {
                        coef_name <- paste0(j, level)
                        if (coef_name %in% coef_names) {
                            ci <- tryCatch(confint(fit)[coef_name, ], error = function(e) c(NA, NA))
                            beta <- coef_summary[coef_name, "Estimate"]
                            t_or_z <- coef_summary[coef_name, "z value"]
                            p_value <- coef_summary[coef_name, "Pr(>|z|)"]
                            result_log <- data.frame(
                                reg_type = "logistic",
                                bacterium = paste0(j, "[", level, " vs ", ref_level, "]"),
                                phenotype = i,
                                beta = beta,
                                low_ci = ci[1],
                                up_ci = ci[2],
                                t_or_z = t_or_z,
                                p_value = p_value
                            )
                            result_df_log <- rbind(result_df_log, result_log)
                        }
                    }
                } else {
                    ci <- tryCatch(confint(fit)[2, ], error = function(e) c(NA, NA))
                    beta <- coef(fit)[2]
                    t_or_z <- summary(fit)$coefficients[2, "z value"]
                    p_value <- summary(fit)$coefficients[2, "Pr(>|z|)"]
                    result_log <- data.frame(
                        reg_type = "logistic",
                        bacterium = j,
                        phenotype = i,
                        beta = beta,
                        low_ci = ci[1],
                        up_ci = ci[2],
                        t_or_z = t_or_z,
                        p_value = p_value
                    )
                    result_df_log <- rbind(result_df_log, result_log)
                }
            }
        } else { # 如果结果变量是连续型变量，进行线性回归分析
            for (j in exposure_vars) {
                formula <- create_formula(i, j, covariate_vars) # 创建公式
                fit <- tryCatch(
                    glm(formula, data = data),
                    error = function(e) {
                        message(sprintf("线性回归拟合失败：phenotype=%s, bacterium=%s，已跳过。错误：%s", i, j, e$message))
                        return(NULL)
                    }
                )
                if (is.null(fit)) next

                if (is.factor(data[[j]]) && length(levels(data[[j]])) > 1) {
                    coef_summary <- summary(fit)$coefficients
                    coef_names <- rownames(coef_summary)
                    factor_levels <- levels(data[[j]])
                    ref_level <- factor_levels[1]
                    for (level in factor_levels[-1]) {
                        coef_name <- paste0(j, level)
                        if (coef_name %in% coef_names) {
                            ci <- tryCatch(confint(fit)[coef_name, ], error = function(e) c(NA, NA))
                            beta <- coef_summary[coef_name, "Estimate"]
                            t_or_z <- coef_summary[coef_name, "t value"]
                            p_value <- coef_summary[coef_name, "Pr(>|t|)"]
                            result_lin <- data.frame(
                                reg_type = "linear",
                                bacterium = paste0(j, "[", level, " vs ", ref_level, "]"),
                                phenotype = i,
                                beta = beta,
                                low_ci = ci[1],
                                up_ci = ci[2],
                                t_or_z = t_or_z,
                                p_value = p_value
                            )
                            result_df_lin <- rbind(result_df_lin, result_lin)
                        }
                    }
                } else {
                    ci <- tryCatch(confint(fit)[2, ], error = function(e) c(NA, NA))
                    beta <- coef(fit)[2]
                    t_or_z <- summary(fit)$coefficients[2, "t value"]
                    p_value <- summary(fit)$coefficients[2, "Pr(>|t|)"]
                    result_lin <- data.frame(
                        reg_type = "linear",
                        bacterium = j,
                        phenotype = i,
                        beta = beta,
                        low_ci = ci[1],
                        up_ci = ci[2],
                        t_or_z = t_or_z,
                        p_value = p_value
                    )
                    result_df_lin <- rbind(result_df_lin, result_lin)
                }
            }
        }
    }

    # BH 校正
    if (nrow(result_df_log) > 0) {
        result_df_log$FDR_p <- p.adjust(result_df_log$p_value, method = "BH")
        result_df_log$sig_p <- significance_for_vector(result_df_log$p_value)
        result_df_log$sig_FDR_p <- significance_for_vector(result_df_log$FDR_p)
    }
    if (nrow(result_df_lin) > 0) {
        result_df_lin$FDR_p <- p.adjust(result_df_lin$p_value, method = "BH")
        result_df_lin$sig_p <- significance_for_vector(result_df_lin$p_value)
        result_df_lin$sig_FDR_p <- significance_for_vector(result_df_lin$FDR_p)
    }
    result_df <- rbind(result_df_log, result_df_lin)

    # 完成时播放声音
    if (!requireNamespace("beepr", quietly = TRUE)) {
        message("提示：请安装 'beepr' 包以便完成时播放提示音：install.packages('beepr')")
        install.packages("beepr")
        library(beepr)
    } else {
        beepr::beep(8)
    }
    return(list(result_df_log = result_df_log, result_df_lin = result_df_lin, result_df = result_df))
}
# =====肠道菌群数据抽平、相对丰度、相对丰度对数化后标准化的函数===========
#' 对肠道菌群丰度表进行抽平、筛选、对数化与标准化处理
#'
#' @param df_microbiota 含样本ID的肠道菌群绝对丰度表，行为样本，列为菌，包含样本id列
#' @param sample_id_col 样本ID列名（字符串）
#' @param rel_abund_threshold 平均相对丰度过滤阈值，默认0.0001
#' @param occ_threshold 最低检出比例（0-1），默认0.1，表示保留在10%以上样本中检出的菌
#' @param log_add 常数，用于对数变换避免log(0)，默认1e-4
#' @param seed 随机种子，保证可重复性，默认123
#'
#' @return 列表，包含absolute（抽平后绝对丰度数据框）、relative（相对丰度数据框）、transformed（对数化标准化后的数据框）
#' @importFrom vegan rrarefy
#' @export
#'
#' @examples
#' result <- rarefy_transform_gut_microbiota(df, sample_id_col = "SampleID")
#' abs_df <- result$absolute
#' rel_df <- result$relative
#' trans_df <- result$transformed
rarefy_transform_gut_microbiota <- function(df_microbiota,
                                            sample_id_col,
                                            rel_abund_threshold = 0.0001,
                                            occ_threshold = 0.1,
                                            log_add = 1e-4,
                                            seed = 123,
                                            min_total_reads = 1000) {
    if (!requireNamespace("vegan", quietly = TRUE)) {
        stop("Please install the 'vegan' package.")
    }
    set.seed(seed)
    # 以样本ID设置行名，并移除ID列
    rownames(df_microbiota) <- df_microbiota[[sample_id_col]]
    df_microbiota <- df_microbiota[, setdiff(names(df_microbiota), sample_id_col), drop = FALSE]

    # 先过滤：去除总绝对丰度低于阈值的样本
    keep_samples <- rowSums(df_microbiota) >= min_total_reads
    df_microbiota <- df_microbiota[keep_samples, , drop = FALSE]

    # 过滤后为空的健全性检查
    if (nrow(df_microbiota) == 0) {
        stop(sprintf("过滤后没有样本。请降低 min_total_reads（当前=%s）或检查原始reads是否为整数计数。", min_total_reads))
    }

    # 类型与整数计数检查
    if (!all(sapply(df_microbiota, is.numeric))) {
        stop("All columns except sample ID must be numeric!")
    }
    if (!all(sapply(df_microbiota, function(x) all(is.finite(x)) && all(x == floor(x))))) {
        stop("抽平(rrarefy)需要整数计数（非小数、非NA/Inf）。请确保输入为原始reads。")
    }

    # 稀释深度
    min_depth <- min(rowSums(df_microbiota), na.rm = TRUE)
    if (!is.finite(min_depth) || min_depth <= 0) {
        stop(sprintf("稀释深度非法(min_depth=%s)。请检查输入数据与过滤阈值。", min_depth))
    }

    # 抽平
    df_rarefied <- vegan::rrarefy(df_microbiota, sample = min_depth)

    # 移除全为0的菌
    df_abs <- df_rarefied[, colSums(df_rarefied) > 0, drop = FALSE]

    # 相对丰度
    df_rel <- sweep(df_abs, 1, rowSums(df_abs), FUN = "/")

    # 按平均相对丰度与出现频率筛选
    keep1 <- colMeans(df_rel) > rel_abund_threshold
    df_rel <- df_rel[, keep1, drop = FALSE]
    keep2 <- colSums(df_rel > 0) / nrow(df_rel) > occ_threshold
    df_rel <- df_rel[, keep2, drop = FALSE]

    # 匹配绝对丰度
    df_abs <- df_abs[, colnames(df_rel), drop = FALSE]

    # 对数变换+标准化
    df_log <- log(df_rel + log_add)
    df_trans <- scale(df_log)

    list(
        absolute = as.data.frame(df_abs),
        relative = as.data.frame(df_rel),
        transformed = as.data.frame(df_trans)
    )
}
# =====分组为多组的小提琴图函数===========
#' @title 绘制多组小提琴图（多分类分组, Kruskal-Wallis检验+Dunn两两比较）
#' @description 绘制适用于多分类分组变量的小提琴图，并添加箱线图、整体p值（Kruskal-Wallis检验）及两两比较（Dunn检验）星号标注。显著性横杠和星号的间隔可自动根据数据和比较数调整，也可自定义。横杠粗细可通过line_width参数调整。
#' @param data 数据框
#' @param xvar x轴分组变量名（多分类）
#' @param yvar y轴变量名
#' @param fill_colors 填充颜色向量
#' @param text_size 文本大小
#' @param sig_bar_area_ratio 显著性标注区高度比例（相对于y最大值），默认0.3。数值大则横杠和星号间隔更大。
#' @param line_width 横杠和星号连线的粗细，默认0.6，数值越小线越细
#' @return ggplot对象
plot_violin_kruskal_dunn <- function(data, xvar, yvar, fill_colors = NULL, text_size = 10, sig_bar_area_ratio = 0.3, line_width = 0.05) {
    library(ggplot2)
    library(FSA) # for dunnTest
    library(ggsignif) # for adding significance bars

    # 检查分组
    group_levels <- as.character(sort(unique(data[[xvar]])))
    k <- length(group_levels)
    if (is.null(fill_colors)) {
        fill_colors <- scales::hue_pal()(k)
    } else {
        # 自动补全#号
        fill_colors <- ifelse(substr(fill_colors, 1, 1) == "#", fill_colors, paste0("#", fill_colors))
    }

    # 根据分组变量的类别数选择检验方法
    if (k == 2) {
        # 二分类变量，使用Wilcoxon秩和检验
        formula <- as.formula(paste(yvar, "~", xvar))
        wilcoxon_res <- wilcox.test(formula, data = data)
        p_value <- wilcoxon_res$p.value

        # 由于是二分类，不需要Dunn检验
        dunn_table <- NULL
        signif_labels <- NULL
    } else {
        # 多分类变量，使用Kruskal-Wallis检验
        formula <- as.formula(paste(yvar, "~", xvar))
        kruskal_res <- kruskal.test(formula, data = data)
        p_value <- kruskal_res$p.value

        # Dunn检验，两两比较
        dunn_res <- FSA::dunnTest(formula, data = data, method = "bh")
        dunn_table <- dunn_res$res
        # 提取pair和p值
        pairwise_comparisons <- strsplit(dunn_table$Comparison, " - ")
        pairs <- do.call(rbind, pairwise_comparisons)
        stars <- function(p) {
            if (p < 0.001) {
                return("***")
            }
            if (p < 0.01) {
                return("**")
            }
            if (p < 0.05) {
                return("*")
            }
            return("ns")
        }
        signif_labels <- sapply(dunn_table$P.adj, stars)
    }

    # 计算每组最大y用于横杠放置，并合理设置显著性标注间隔
    agg_max <- aggregate(data[[yvar]], by = list(data[[xvar]]), FUN = max, na.rm = TRUE)
    y_max <- max(agg_max$x, na.rm = TRUE)
    n_pw <- if (is.null(dunn_table)) 0 else nrow(dunn_table)
    y_bar_area <- y_max * sig_bar_area_ratio * 1.2 # 横杠/星号区高度可调，推荐0.2~0.4
    y_start <- y_max * 1.05 # 横杠起始高度（略高于数据最大值）
    y_gap <- y_bar_area / max(1, n_pw) # 横杠之间的间隔（自动根据比较数调整）

    # 绘图主体
    p <- ggplot(data, aes(x = as.factor(.data[[xvar]]), y = .data[[yvar]])) +
        geom_violin(aes(fill = as.factor(.data[[xvar]])), alpha = 0.8, trim = FALSE) +
        geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.8, outlier.shape = NA) +
        scale_fill_manual(values = fill_colors) +
        labs(x = xvar, y = yvar, title = "") +
        theme_classic() +
        theme(
            text = element_text(color = "gray30", size = text_size),
            axis.line = element_line(linewidth = 0.6, color = "gray30"),
            axis.ticks = element_line(linewidth = 0.6, color = "gray30"),
            axis.ticks.length = unit(1.5, units = "mm"),
            plot.margin = unit(rep(1, 4), units = "lines"),
            legend.position = "none"
        )

    # 添加横杠和星号
    if (!is.null(dunn_table)) {
        for (i in seq_len(n_pw)) {
            vs <- pairs[i, ]
            vs_num <- match(vs, group_levels)
            y_bar <- y_start + (i - 1) * y_gap
            p <- p +
                annotate("segment", x = vs_num[1], xend = vs_num[2], y = y_bar, yend = y_bar, linewidth = line_width, color = "black", size = text_size*0.25) +
                annotate("segment", x = vs_num[1], xend = vs_num[1], y = y_bar, yend = y_bar - y_gap * 0.3, linewidth = line_width, color = "black", size = text_size*0.25) +
                annotate("segment", x = vs_num[2], xend = vs_num[2], y = y_bar, yend = y_bar - y_gap * 0.3, linewidth = line_width, color = "black", size = text_size*0.25) +
                annotate("text", x = mean(vs_num), y = y_bar + y_gap * 0.25, label = signif_labels[i], size = text_size * 0.25, color = "black")
        }
    }

    # 添加整体p值
    if (k == 2) {
        p <- p +
            annotate(
                "text",
                x = (k + 1) / 2, y = y_start + n_pw * y_gap + y_gap * 0.5,
                label = paste0("Wilcoxon p = ", formatC(p_value, digits = 3, format = "f")),
                size = 3
            )
    } else {
        p <- p +
            annotate(
                "text",
                x = (k + 1) / 2, y = y_start + n_pw * y_gap + y_gap * 0.5,
                label = paste0("Kruskal-Wallis p = ", formatC(p_value, digits = 3, format = "f")),
                size = 3
            )
    }
    return(p)
}
