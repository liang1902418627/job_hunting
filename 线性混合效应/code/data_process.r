# 数据处理，匹配结局---------------------
rm(list = ls())
setwd("D:\\梁豪文件\\谢佳欣20260202中小学\\")
library(dplyr)
meta_data <- data.table::fread("data/meta_data_v9.csv", data.table = F)
names(meta_data)

str(meta_data)
# # 本来是数值型变量，转换为数值型
# numeric_vars <- c(
#   "Birth_weight",
#   "birth",
#   "Height",
#   "Weight",
#   "Waist",
#   "TC",
#   "TG",
#   "Serum_vA",
#   "PM2.5_mean",
#   "Temp_mean",
#   "Humidity_mean"
# )
# for (var in numeric_vars) {
#   meta_data[[var]] <- as.numeric(meta_data[[var]])
# }
# 将除了SampleID列之外的character列转换为因子---------------
for (col in names(meta_data)[-1]) {
    if (is.character(meta_data[[col]])) {
        meta_data[[col]] <- as.factor(meta_data[[col]])
    }
}

str(meta_data)

# 读取结局数据---------------------
outcome <- data.table::fread("data/servey_2021_v3.csv", data.table = F)
names(outcome)

vars <- c(
    "SampleID", "Obesity_status", "bmi_zscore", "WHtR", "central_obesity", "OWOB"
)

# 分类结局因子化---------------------

outcome_sub <- outcome[, vars]

outcome_v2 <- outcome_sub %>%
    mutate(
        Obesity_status = factor(
            Obesity_status,
            levels = c(0, 1),
            labels = c("No", "Yes")
        )
    ) %>%
    mutate(
        central_obesity = factor(
            central_obesity,
            levels = c(0, 1),
            labels = c("No", "Yes")
        )
    ) %>%
    mutate(
        OWOB = factor(
            OWOB,
            levels = c(0, 1),
            labels = c("No", "Yes")
        )
    )

# 合并数据--------------------------
data_merge <- meta_data %>%
    select(-c("Obesity_status")) %>%
    inner_join(outcome_v2, by = "SampleID")
nrow(data_merge)
table(data_merge$Carbonated_beverage)
# 处理软饮分类--------------------------------
data_merge_v1 <- data_merge %>%
  # 先处理NA值，将NA视为"No"
  mutate(across(
    c(Carbonated_beverage, Colored_fruit_flavored_beverage, Sports_drinks,
      Fruit_and_vegetable_juice, Dairy_based_beverages, Plant_protein_beverages,
      Tea_beverages, Brewed_tea_or_coffee, Other_beverages),
    ~ coalesce(., "No")
  )) %>%
  # 计算"Yes"的数量
  mutate(
    yes_count = rowSums(across(
      c(Carbonated_beverage, Colored_fruit_flavored_beverage, Sports_drinks,
        Fruit_and_vegetable_juice, Dairy_based_beverages, Plant_protein_beverages,
        Tea_beverages, Brewed_tea_or_coffee, Other_beverages)
    ) == "Yes"),
    
    # 识别具体的饮料类型
    soft_drink_category = case_when(
      yes_count == 0 ~ "No",
      yes_count == 1 ~ case_when(
        Carbonated_beverage == "Yes" ~ "Carbonated_beverage",
        Colored_fruit_flavored_beverage == "Yes" ~ "Colored_fruit_flavored_beverage",
        Sports_drinks == "Yes" ~ "Sports_drinks",
        Fruit_and_vegetable_juice == "Yes" ~ "Fruit_and_vegetable_juice",
        Dairy_based_beverages == "Yes" ~ "Dairy_based_beverages",
        Plant_protein_beverages == "Yes" ~ "Plant_protein_beverages",
        Tea_beverages == "Yes" ~ "Tea_beverages",
        Brewed_tea_or_coffee == "Yes" ~ "Brewed_tea_or_coffee",
        Other_beverages == "Yes" ~ "Other_beverages",
        TRUE ~ "Error"  # 理论上不会出现，用于调试
      ),
      yes_count > 1 ~ "Mixed"
    )
  ) %>%
  # 移除辅助列
  select(-yes_count) %>%
  # 转换为因子
  mutate(
    soft_drink_category = factor(
      soft_drink_category,
      levels = c("No", "Carbonated_beverage", "Colored_fruit_flavored_beverage",
                 "Sports_drinks", "Fruit_and_vegetable_juice", "Dairy_based_beverages",
                 "Plant_protein_beverages", "Tea_beverages", "Brewed_tea_or_coffee",
                 "Other_beverages", "Mixed")
    )
  )

# 查看结果分布
table(data_merge_v1$soft_drink_category)

saveRDS(data_merge_v1, file = "data/analysis_data_v1.rds")
