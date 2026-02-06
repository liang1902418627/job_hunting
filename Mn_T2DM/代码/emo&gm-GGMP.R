setwd("/Users/huangshiqi/Desktop/GGMP-何老师")
####================================数据清理===============================####
library(dplyr);library(colSum)
### 数据准备
## 读入测序数据
gut.micro_tax <- read.csv("GGMP_closedref_even10k_s700.biom (949OTUS).csv")
## 剔除叶绿体、线粒体(具体名称需要根据tax注释表进行修改，不同测序名称可能存在区别)
gut.micro_tax <- gut.micro_tax[!grepl('Chloroplast',gut.micro_tax$taxname),] #剔除叶绿体tax
gut.micro_tax <- gut.micro_tax[!grepl('Mitochondria',gut.micro_tax$taxname),] #剔除线粒体tax

## 根据情况选择是否剔除古细菌/细菌
gut.micro_tax <- gut.micro_tax[!grepl('Archaea',gut.micro_tax$taxname),] #剔除古细菌tax
#gut.micro_tax <- gut.micro_tax[!grepl('Bacteria',gut.micro_tax$taxname),] #剔除细菌tax

## 对丰度数值进行处理，首先剔除非数值的列
rownames(gut.micro_tax) <- gut.micro_tax$taxID # 设置行名为ID
gut.micro_tax <-gut.micro_tax[,3:7011] # 选择数据列

## 计算各样本总丰度
colSums(gut.micro_tax)

## 转置
gut.micro_tax1 <- data.frame(t(gut.micro_tax),check.names = FALSE)

## 均一化/抽平
set.seed(123)
gut.micro_tax1 <- vegan::rrarefy(gut.micro_tax1,min(rowSums(gut.micro_tax1)))
gut.micro_tax1 <- data.frame(t(gut.micro_tax1),check.names = FALSE)

## 计算各样本总丰度
colSums(gut.micro_tax1)

## 由于总丰度减少，剔除在所有样本中均为0的tax
gut.micro_tax1 <-gut.micro_tax1[rowSums(gut.micro_tax1)>0,]

## 输出均一化的丰度表
write.csv(gut.micro_tax1,"gut.micro_evenness.csv")

## 求每个tax的相对丰度
gut.micro <- gut.micro_tax1 #读入均一化后的丰度表
gut.micro1 <- data.frame(t(gut.micro),check.names = F) #转置
gut.micro1 <- gut.micro1/rowSums(gut.micro1) #计算相对丰度
gut.micro1 <- data.frame(t(gut.micro1),check.names = F) #转置

# 此时各样本的总丰度为1
colSums(gut.micro1)

# 输出相对丰度表
write.csv(gut.micro1,"gut.micro_relative.csv")

# 剔除在所有样本中平均相对丰度低于0.0001的tax
gut.micro1_selected <- gut.micro1[which(rowMeans(gut.micro1)>=0.0001),]
# 剔除出现频率低于10%的tax
gut.micro1_selected <- gut.micro1_selected[which(apply(gut.micro1_selected,1,function(x) sum(x>0))>(ncol(gut.micro1_selected)/10)),]

write.csv(gut.micro1_selected, "gut.micro_cleaned.csv")


####===================根据丰度表计算不同分类水平物种相对丰度==============####
gut.micro<-read.csv("gut.micro_cleaned.csv") ##先在上面步骤输出的数据中，taxID列增加上列名。
taxonomy<- read_excel("taxname(1)-GGMP.xlsx")


# 合并测序表和分类注释表
gm_tax <- dplyr::left_join(gut.micro,taxonomy)

# 获取样本列名
sample_columns<- colnames(gut.micro)[-1]

# 按界汇总丰度
Kingdom_abundance <- gm_tax %>%
  group_by(Kingdom) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算界分类水平的相对丰度(百分比表示)
relative_abundance<- Kingdom_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"kin_relative_abundance.csv")


# 按门汇总丰度
phylum_abundance <- gm_tax %>%
  group_by(Phylum) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算门分类水平的相对丰度(百分比表示)
relative_abundance<- phylum_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"phy_relative_abundance.csv")

# 按纲汇总丰度
class_abundance <- gm_tax %>%
  group_by(Class) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算纲分类水平的相对丰度(百分比表示)
relative_abundance<- class_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"cla_relative_abundance.csv")

# 按目汇总丰度
Order_abundance <- gm_tax %>%
  group_by(Order) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算目分类水平的相对丰度(百分比表示)
relative_abundance<- Order_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"ord_relative_abundance.csv")


# 按科汇总丰度
Family_abundance <- gm_tax %>%
  group_by(Family) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算科分类水平的相对丰度(百分比表示)
relative_abundance<- Family_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"fam_relative_abundance.csv")


# 按属汇总丰度
Genus_abundance <- gm_tax %>%
  group_by(Genus) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算属分类水平的相对丰度(百分比表示)
relative_abundance<- Genus_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"gen_relative_abundance.csv")


# 按种汇总丰度
Species_abundance <- gm_tax %>%
  group_by(Species) %>%
  summarise(across(all_of(sample_columns),sum,.names = "sum_{.col}"))

# 计算种分类水平的相对丰度(百分比表示)
relative_abundance<- Species_abundance %>%
  mutate(across(starts_with("sum_"),~./sum(.)*100))

# 输出结果
write.csv(relative_abundance,"spe_relative_abundance.csv")


## 将每个水平上的相对丰度结果合并成一个大表
Kin_abundance <- read.csv("kin_relative_abundance.csv")
Phy_abundance <- read.csv("phy_relative_abundance.csv")
Cla_abundance <- read.csv("cla_relative_abundance.csv")
Ord_abundance <- read.csv("ord_relative_abundance.csv")
Fam_abundance <- read.csv("fam_relative_abundance.csv")
Gen_abundance <- read.csv("gen_relative_abundance.csv")
Spe_abundance <- read.csv("spe_relative_abundance.csv")


Kin_abundance <- Kin_abundance %>%
  mutate(Kingdom = paste0("k_", Kingdom)) %>%
  rename(taxname = Kingdom)  # 将 Kingdom 列名改为 taxname
Phy_abundance <- Phy_abundance %>%
  mutate(Phylum = paste0("p_", Phylum)) %>%
  rename(taxname = Phylum)  # 将 Phylum 列名改为 taxname
Cla_abundance <- Cla_abundance %>%
  mutate(Class = paste0("c_", Class)) %>%
  rename(taxname = Class)  # 将 Class 列名改为 taxname
Ord_abundance <- Ord_abundance %>%
  mutate(Order = paste0("o_", Order)) %>%
  rename(taxname = Order)  # 将 Order 列名改为 taxname
Fam_abundance <- Fam_abundance %>%
  mutate(Family = paste0("f_", Family)) %>%
  rename(taxname = Family)  # 将 Family 列名改为 taxname
Gen_abundance <- Gen_abundance %>%
  mutate(Genus = paste0("g_", Genus)) %>%
  rename(taxname = Genus)  # 将 Genus 列名改为 taxname
Spe_abundance <- Spe_abundance %>%
  mutate(Species = paste0("s_", Species)) %>%
  rename(taxname = Species)  # 将 Species 列名改为 taxname

All_Tax<-rbind(Kin_abundance,Phy_abundance,Cla_abundance,Ord_abundance,Fam_abundance,Gen_abundance,Spe_abundance)
All_Tax<-All_Tax[,-1]

rownames(All_Tax)<-All_Tax$taxname
All_Tax<-All_Tax[,-1]
All_Tax<-as.data.frame(t(All_Tax))

write.csv(All_Tax,"All_tax_relative_abundance.csv")
####数据格式转换####
##肠道微生物组##
gut.micro<-read.csv("All_tax_relative_abundance.csv")
rownames(gut.micro) <- gut.micro$SampleID #设置行名为SampleID
## 选择所有数据列
data <- gut.micro[, 2:175]
# 对除了SampleID以外的所有列进行除以100的归一化操作（原数据的物种类别和为100）
gut.micro_modified <- gut.micro %>%
  mutate(across(-SampleID, ~ . / 100))

# 输出数据
write.csv(gut.micro_modified,"gut.micro_cleaned.csv")


##选取genus水平数据，进行数据转换，用于后续差异性分析
gut.micro<-read.csv("All_tax_relative_abundance.csv")
rownames(gut.micro)<-gut.micro$SampleID #设置列名

# 提取genus水平数据
# 1. 提取以 "g_" 开头的列
g_columns <- gut.micro[, grep("^g_", colnames(gut.micro))]

# 2. 对提取的数据框进行对数转换
# 对 data 的每一列进行操作，将值为 0 的元素替换为当前列中大于 0 的最小值的十分之一。
g_columns  <- 
  g_columns  %>% 
  mutate_all(., ~ replace(., . == 0, min(as.numeric(.[.>0]))/10))

g_columns_log <-log(g_columns)

# 3. 对提取的数据框进行标准化转换
g_columns_standardized <- scale(g_columns_log)

write.csv(g_columns_standardized,"gm_clean_genus.csv")

# 加入协变量数据
gm<-read.csv("gm_clean_genus.csv")
Covariates<-read.csv("GGMP协变量6992.csv")

# 合并测序数据与问卷数据
gm_cov <- dplyr::left_join(gm,Covariates)

gm_cov$health_emo_prob_days <- as.numeric(gm_cov$health_emo_prob_days)
gm_cov$ethnics <- as.numeric(gm_cov$ethnics)
gm_cov$education <- as.numeric(gm_cov$education)
gm_cov$marital_status <- as.numeric(gm_cov$marital_status)
gm_cov$occupation <- as.numeric(gm_cov$occupation)
gm_cov$tm <- as.numeric(gm_cov$tm)
gm_cov$rh <- as.numeric(gm_cov$rh)

#删除 health_emo_prob_days、温湿度列为 NA 的行
gm_cov <- gm_cov[!is.na(gm_cov$health_emo_prob_days), ]#-16
gm_cov <- gm_cov[!is.na(gm_cov$tm), ]#-1

# 新增一列 emo_group
gm_cov$emo_group <- NA  # 初始化emo_group列


# 为emo_group列赋值
gm_cov$emo_group[gm_cov$health_emo_prob_days >= 0] <- 0
gm_cov$emo_group[gm_cov$health_emo_prob_days >= 1] <- 1

# 插补协变量
#install.packages("mice")
library(mice)

# 对含有缺失值的数据框 df 进行多重插补
imputed_data <- mice(gm_cov, m = 5, method = 'pmm', seed = 123)

# 获取插补后的数据框（选择第一个插补数据集）
gm_cov_imputed <- complete(imputed_data, 1)

# 输出最终分析数据
write.csv(gm_cov_imputed,"final_gm_genus.csv")



####贡献度分析####
# 载入必要的包
library(vegan)
# 读入数据
gm <- read.csv("final_gm_genus.csv")
# 计算Bray-Curtis距离:评估群落组成差异
## 剔除NA的行
gm <- na.omit(gm)
gm.BC <- vegdist(gm[,2:57], method = 'euclidean')  

# 创建一个空的数据框用于存储结果
result_df1 <- data.frame()

# 循环遍历可能的影响因素
for (i in 58:69) {
  
  # 获取当前列的名字
  current_col <- colnames(gm)[i]
  
  # 替换temp变量，运行adonis2分析
  adonis_result <- adonis2(gm.BC ~ gm[[i]],  # 修改为 gut_micro_cg[[i]]
                           data = gm, 
                           permutations = 999)
  
  # 提取结果，转换为数据框
  adonis_summary <- data.frame(adonis_result) # 从 adonis2 结果中提取分析表
  
  # 添加当前列名作为额外的列来标识
  adonis_summary$variable <- current_col
  
  # 合并当前结果到最终的结果数据框中
  result_df1 <- rbind(result_df1, adonis_summary)
}


# 保存最终结果
result_df1 <- result_df1 %>%
  filter(!is.na(F)) #只保留有意义的行
write.csv(result_df1,"gm_special_R2.csv")

##绘图
library(dplyr);library(ggplot2)

data <- data.frame(result_df1) %>%
  mutate(
    text = case_when(
      Pr..F. >= 0.05 ~ paste(" "),
      Pr..F. >= 0.01 ~ paste("\n*"),
      Pr..F. > 0.001 ~ paste("\n**"),
      Pr..F. <= 0.001 ~ paste("\n***")
    )
  )


# 对数据进行整理
data$feature <- data$variable


# 添加 `color` 列，默认值为 "grey"
data$color <- "grey"

# 计算每行的R平方值百分比
data$`Explanation of gut microbiota structure (%)` <- data$R2 * 100

# 比较前两行的R2值，选择更大的那个
# max_R2_row <- data[which.max(data$R2[1:2]), ]

# 将emo的行的color列标记为"red"
data$color[data$feature == "health_emo_prob_days"] <- "red"
data$color[data$feature == "emo_group"] <- "red"

# 1. 找到 feature == "emo_group" 的行
emo_group_row <- data[data$feature == "emo_group", ]

# 2. 删除原先的符合条件的行
data <- data[data$feature != "emo_group", ]

# 3. 将 emo_group_row 插入到数据框的第二行
# 使用 rbind 将数据框拆分成两部分，然后将 "emo_group" 行插入到第二行的位置
data <- rbind(data[1, , drop = FALSE], emo_group_row, data[2:nrow(data), , drop = FALSE])

# 设置因子顺序
data$feature <- factor(data$feature, levels = rev(data$feature))

# 绘图
ggplot(data = data[1:8, ],
       aes(x = `Explanation of gut microbiota structure (%)`, y = feature, fill = color)) +
  geom_bar(stat = "identity",
           width = 0.7,
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c(red = "#FF3300", grey = "#999999"),
                    name = "Group") +
  geom_text(
    aes(label = text),
    col = "black",
    size = 6,
    position = position_dodge(width = 0.9),
    vjust = 0.25,
    hjust = -0.2
  ) +
  xlim(0, 1) +
  labs(y = "") +
  theme_bw() + theme(panel.grid = element_blank()) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14, color = "black"),
    legend.position = "none",
    plot.margin = margin(
      t = 20,
      r = 20,
      b = 15,
      l = 10
    )
  )


####差异菌群分析(湿度干旱)####
# 读入数据
gut.micro<-read.csv("final_gm_genus.csv")
gut.micro.transf<-gut.micro


#构建phyloseq对象
library(phyloseq)
library(MicrobiotaProcess)
library(dplyr)
#otu
names(gut.micro.transf)
otu_gm <- otu_table(as.matrix(gut.micro.transf[,2:57]), taxa_are_rows = F)

#sample
gut.micro.transf$hot <- ifelse(gut.micro.transf$hot == 0, "No", "Yes")
sample_da <- gut.micro.transf[,c(1:57,67)]
sample_da <-sample_data(sample_da)

#tax
tax_bac <- read.csv("tax_apart_genus.csv")
tax_bac_phy <-  phyloseq::tax_table(as.matrix(tax_bac))


phy.gm <- phyloseq::phyloseq(otu_gm,
                             sample_da,
                             tax_bac)
phy.gm

####差异菌分析diff_analysis####
set.seed(555)
diffres <- diff_analysis(
  obj = phy.gm,
  class = "hot",
  mlfun = "lda",
  firstcomfun = "kruskal.test",
  secondcomfun = "wilcox.test",
  alltax = F,
  type = "others",
  firstalpha = 1,
  secondalpha = 1,
  ldascore = 0
)
diffres
gm.diff.all <- diffres@result

gm.diff.all$f <- as.character(gm.diff.all$f)

df <- group_by(gut.micro.transf[,c(2:57,67)], hot) %>% summarise(across(g_Acidaminococcus:g_.Ruminococcus., mean))
df <- as.data.frame(df)
rownames(df) <- df$hot
df <- as.data.frame(t(df[,-1]))
df <- df %>% 
  tibble::rownames_to_column("f")
df$Enrichment <- ifelse(df$No < df$Yes, "Yes","No")  ##根据 Low 和 High 列的比较结果，生成一个新的列 Enrichment

gm.diff.all <- merge(df,gm.diff.all, 
                     by = "f")

gm.diff.sig <- gm.diff.all[gm.diff.all$fdr < 0.1 & gm.diff.all$LDAmean > 2,]  ##从合并后的结果中筛选出假发现率（FDR）小于 0.1 且 LDA 均值大于 2 的行，生成 gm.diff.sig 数据框，表示显著差异的结果

#### gm.diff.sig ols ####
library(dplyr);library(purrr)

y <- gm.diff.sig$f
##定义线性回归函数
linear_reg.1=function(y){
  coef(summary(lm(y~hot,data = gut.micro.transf)))[2,c(1,2,4)]
}

# result=map(y,linear_reg.1) 原代码有问题
result = map(y, function(col_name) {
  linear_reg.1(gut.micro.transf[[col_name]])
}) %>% set_names(y)

result=as.data.frame(result)

result <- as.data.frame(t(result)) %>% 
  tibble::rownames_to_column("f")
result$FDR <- p.adjust(result$`Pr(>|t|)`, method = "BH", n = length(result$`Pr(>|t|)`))
result$beta.lwr <- result$Estimate - 1.96 * result$`Std. Error`
result$beta.upr <- result$Estimate + 1.96 * result$`Std. Error`
gm.lm1 <- result


linear_reg.2 = function(y) {
  coef(summary(
    lm(
      y ~ hot +
        age + gender + ethnics +
        education+
        marital_status + occupation,
      data = gut.micro.transf
    )
  ))[2, c(1, 2, 4)]
}

#result=map(y,linear_reg.2)
result = map(y, function(col_name) {
  linear_reg.2(gut.micro.transf[[col_name]])
}) %>% set_names(y)

result=as.data.frame(result)

result <- as.data.frame(t(result)) %>% 
  tibble::rownames_to_column("f")
result$FDR <- p.adjust(result$`Pr(>|t|)`, method = "BH", n = length(result$`Pr(>|t|)`))
result$beta.lwr <- result$Estimate - 1.96 * result$`Std. Error`
result$beta.upr <- result$Estimate + 1.96 * result$`Std. Error`
gm.lm2 <- result

#模型3:需要添加空气污染物作为协变量
library(splines)
linear_reg.3 = function(y) {
  coef(summary(
    lm(
      y ~ temp +
        age + sex + BMI +
        antibiotic_use +
        energy_intake + sumMET +
        smok + drink +
        incom_3 + edu_3 +
        ns(pm25_lag7, df = 3) + ns(rh_lag7, df = 3),
      data = gut.micro.transf
    )
  ))[2, c(1, 2, 4)]
}

#result=map(y,linear_reg.3)
result = map(y, function(col_name) {
  linear_reg.3(gut.micro.transf[[col_name]])
}) %>% set_names(y)

result=as.data.frame(result)

result <- as.data.frame(t(result)) %>% 
  tibble::rownames_to_column("feature")
result$FDR <- p.adjust(result$`Pr(>|t|)`, method = "BH", n = length(result$`Pr(>|t|)`))
result$beta.lwr <- result$Estimate - 1.96 * result$`Std. Error`
result$beta.upr <- result$Estimate + 1.96 * result$`Std. Error`

gm.lm3 <- result


#integrate
names(gm.lm1) <- paste("lm1", colnames(gm.lm1), sep = ":")
names(gm.lm2) <- paste("lm2", colnames(gm.lm2), sep = ":")
names(gm.lm3) <- paste("lm3", colnames(gm.lm3), sep = ":")

colnames(gm.lm1)[1] <- "f.taxa"
colnames(gm.lm2)[1] <- "f.taxa"
colnames(gm.lm3)[1] <- "f.taxa"

colnames(gm.diff.sig)[1] <- "f.taxa"

gm.res.lm <- Reduce(left_join,
                    list(gm.diff.sig,
                         gm.lm1,
                         gm.lm2))
write.csv(gm.res.lm,"gm.res.lm-ggmp(hot90%).csv")

bio.gm <- gm.res.lm[gm.res.lm$`lm2:FDR`<0.1,]

#### bac LDA 绘图####
bac.codebook.phy <- read_excel("Phylum&genus_tax.xlsx")
data <- merge(gm.res.lm,bac.codebook.phy, by = "f.taxa", all.x = T)
data <- data[data$f.taxa %in% bio.gm$f.taxa,] 

data$ldadir <- ifelse(data$Enrichment=="No",0-data$LDAmean, data$LDAmean)

data <- data[order(data$Enrichment,-data$ldadir),]

data$y <- factor(data$f.taxa, levels = c(data$f.taxa))

library(RColorBrewer)
brewer.pal(8,"Set2")
brewer.pal(9,"Paired")


p1 <- ggplot(data, aes(x = ldadir, y = y, fill = phylum, )) +
  geom_bar(stat = "identity", width = .7) +
  scale_fill_manual(
    values = c(
      p_Actinobacteria = "#80B1D3",
      p_Bacteroidetes = "#8DD3C7",
      p_Firmicutes = "#FDB462",
      p_Proteobacteria = "#FCCDE5",
      p_Verrucomicrobia = "#BEBADA",
      p_Synergistetes = "#FB8072"
    )
  ) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.8) +
  xlab("LDA Score (Log10)") +
  ylab("") +
  theme_bw() +
  theme(
    axis.ticks.length = unit(.4, "lines", "Bold"),
    axis.ticks = element_line(linewidth = 0.6),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 14)
  )

p1


p <-
  ggplot(data, aes(`lm1:Estimate`, y, col = phylum)) # 不同形状shape= Factor

p2 <- p + geom_point(size = 2, aes(col = phylum)) +
  
  geom_errorbarh(aes(xmax = `lm1:beta.lwr`, xmin = `lm1:beta.upr`, col =
                       phylum),
                 height = 0.4) +
  
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.8) +
  
  scale_color_manual(
    values = c(
      p_Actinobacteria = "#80B1D3",
      p_Bacteroidetes = "#8DD3C7",
      p_Firmicutes = "#FDB462",
      p_Proteobacteria = "#FCCDE5",
      p_Verrucomicrobia = "#BEBADA",
      p_Synergistetes = "#FB8072"
    )
  ) +
  
  xlab('SMD (95%CI)') + ylab("") + theme_bw() + theme(legend.position =
                                                        "top") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.text = element_text(size = 11),
    axis.ticks.length = unit(.4, "lines"),
    axis.ticks = element_line(linewidth = 0.8),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 14),
    # 修改图例方向为纵向（上到下）
    legend.direction = "vertical"
  )


p2


p <-
  ggplot(data, aes(`lm2:Estimate`, y, col = phylum)) # 不同形状shape= Factor

p3 <- p + geom_point(size = 2, aes(col = phylum)) +
  
  geom_errorbarh(aes(xmax = `lm2:beta.lwr`, xmin = `lm2:beta.upr`, col =
                       phylum),
                 height = 0.4) +
  
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.8) +
  
  scale_color_manual(
    values = c(
      p_Actinobacteria = "#80B1D3",
      p_Bacteroidetes = "#8DD3C7",
      p_Firmicutes = "#FDB462",
      p_Proteobacteria = "#FCCDE5",
      p_Verrucomicrobia  = "#BEBADA",
      p_Synergistetes = "#FB8072"
    )
  ) +
  
  xlab('SMD (95%CI)') + ylab("") + theme_bw() + theme(legend.position =
                                                        "top") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 11),
    axis.ticks.length = unit(.4, "lines"),
    axis.ticks = element_line(linewidth = 0.8),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 14),
    # 修改图例方向为纵向（上到下）
    legend.direction = "vertical"
  )

p3

library(aplot)

p1 %>%
  insert_right(p2)%>%
  insert_right(p3)

####差异菌群分析(湿度干旱)####
# 读入数据
gut.micro<-read.csv("final_gm_genus.csv")
gut.micro.transf<-gut.micro


#构建phyloseq对象
library(phyloseq)
library(MicrobiotaProcess)
library(dplyr)
#otu
names(gut.micro.transf)
otu_gm <- otu_table(as.matrix(gut.micro.transf[,2:57]), taxa_are_rows = F)

#sample
gut.micro.transf$dry <- ifelse(gut.micro.transf$dry == 0, "No", "Yes")
sample_da <- gut.micro.transf[,c(1:57,68)]
sample_da <-sample_data(sample_da)

#tax
tax_bac <- read.csv("tax_apart_genus.csv")
tax_bac_phy <-  phyloseq::tax_table(as.matrix(tax_bac))


phy.gm <- phyloseq::phyloseq(otu_gm,
                             sample_da,
                             tax_bac)
phy.gm

####差异菌分析diff_analysis####
set.seed(555)
diffres <- diff_analysis(
  obj = phy.gm,
  class = "dry",
  mlfun = "lda",
  firstcomfun = "kruskal.test",
  secondcomfun = "wilcox.test",
  alltax = F,
  type = "others",
  firstalpha = 1,
  secondalpha = 1,
  ldascore = 0
)
diffres
gm.diff.all <- diffres@result

gm.diff.all$f <- as.character(gm.diff.all$f)

df <- group_by(gut.micro.transf[,c(2:57,68)], dry) %>% summarise(across(g_Acidaminococcus:g_.Ruminococcus., mean))
df <- as.data.frame(df)
rownames(df) <- df$dry
df <- as.data.frame(t(df[,-1]))
df <- df %>% 
  tibble::rownames_to_column("f")
df$Enrichment <- ifelse(df$No < df$Yes, "Yes","No")  ##根据 Low 和 High 列的比较结果，生成一个新的列 Enrichment

gm.diff.all <- merge(df,gm.diff.all, 
                     by = "f")

gm.diff.sig <- gm.diff.all[gm.diff.all$fdr < 0.1 & gm.diff.all$LDAmean > 2,]  ##从合并后的结果中筛选出假发现率（FDR）小于 0.1 且 LDA 均值大于 2 的行，生成 gm.diff.sig 数据框，表示显著差异的结果

#### gm.diff.sig ols ####
library(dplyr);library(purrr)

y <- gm.diff.sig$f
##定义线性回归函数
linear_reg.1=function(y){
  coef(summary(lm(y~dry,data = gut.micro.transf)))[2,c(1,2,4)]
}

# result=map(y,linear_reg.1) 原代码有问题
result = map(y, function(col_name) {
  linear_reg.1(gut.micro.transf[[col_name]])
}) %>% set_names(y)

result=as.data.frame(result)

result <- as.data.frame(t(result)) %>% 
  tibble::rownames_to_column("f")
result$FDR <- p.adjust(result$`Pr(>|t|)`, method = "BH", n = length(result$`Pr(>|t|)`))
result$beta.lwr <- result$Estimate - 1.96 * result$`Std. Error`
result$beta.upr <- result$Estimate + 1.96 * result$`Std. Error`
gm.lm1 <- result


linear_reg.2 = function(y) {
  coef(summary(
    lm(
      y ~ dry +
        age + gender + ethnics +
        education+
        marital_status + occupation,
      data = gut.micro.transf
    )
  ))[2, c(1, 2, 4)]
}

#result=map(y,linear_reg.2)
result = map(y, function(col_name) {
  linear_reg.2(gut.micro.transf[[col_name]])
}) %>% set_names(y)

result=as.data.frame(result)

result <- as.data.frame(t(result)) %>% 
  tibble::rownames_to_column("f")
result$FDR <- p.adjust(result$`Pr(>|t|)`, method = "BH", n = length(result$`Pr(>|t|)`))
result$beta.lwr <- result$Estimate - 1.96 * result$`Std. Error`
result$beta.upr <- result$Estimate + 1.96 * result$`Std. Error`
gm.lm2 <- result

#模型3:需要添加空气污染物作为协变量
library(splines)
linear_reg.3 = function(y) {
  coef(summary(
    lm(
      y ~ temp +
        age + sex + BMI +
        antibiotic_use +
        energy_intake + sumMET +
        smok + drink +
        incom_3 + edu_3 +
        ns(pm25_lag7, df = 3) + ns(rh_lag7, df = 3),
      data = gut.micro.transf
    )
  ))[2, c(1, 2, 4)]
}

#result=map(y,linear_reg.3)
result = map(y, function(col_name) {
  linear_reg.3(gut.micro.transf[[col_name]])
}) %>% set_names(y)

result=as.data.frame(result)

result <- as.data.frame(t(result)) %>% 
  tibble::rownames_to_column("feature")
result$FDR <- p.adjust(result$`Pr(>|t|)`, method = "BH", n = length(result$`Pr(>|t|)`))
result$beta.lwr <- result$Estimate - 1.96 * result$`Std. Error`
result$beta.upr <- result$Estimate + 1.96 * result$`Std. Error`

gm.lm3 <- result


#integrate
names(gm.lm1) <- paste("lm1", colnames(gm.lm1), sep = ":")
names(gm.lm2) <- paste("lm2", colnames(gm.lm2), sep = ":")
names(gm.lm3) <- paste("lm3", colnames(gm.lm3), sep = ":")

colnames(gm.lm1)[1] <- "f.taxa"
colnames(gm.lm2)[1] <- "f.taxa"
colnames(gm.lm3)[1] <- "f.taxa"

colnames(gm.diff.sig)[1] <- "f.taxa"

gm.res.lm <- Reduce(left_join,
                    list(gm.diff.sig,
                         gm.lm1,
                         gm.lm2))
write.csv(gm.res.lm,"gm.res.lm-ggmp(dry10%).csv")

bio.gm <- gm.res.lm[gm.res.lm$`lm2:FDR`<0.1,]

#### bac LDA 绘图####
bac.codebook.phy <- read_excel("Phylum&genus_tax.xlsx")
data <- merge(gm.res.lm,bac.codebook.phy, by = "f.taxa", all.x = T)
data <- data[data$f.taxa %in% bio.gm$f.taxa,] 
data<-data[data$f.taxa!="g_NA",]
data$ldadir <- ifelse(data$Enrichment=="No",0-data$LDAmean, data$LDAmean)

data <- data[order(data$Enrichment,-data$ldadir),]

data$y <- factor(data$f.taxa, levels = c(data$f.taxa))

library(RColorBrewer)
brewer.pal(8,"Set2")
brewer.pal(9,"Paired")


p1 <- ggplot(data, aes(x = ldadir, y = y, fill = phylum, )) +
  geom_bar(stat = "identity", width = .7) +
  scale_fill_manual(
    values = c(
      p_Actinobacteria = "#80B1D3",
      p_Bacteroidetes = "#8DD3C7",
      p_Firmicutes = "#FDB462",
      p_Proteobacteria = "#FCCDE5",
      p_Verrucomicrobia = "#BEBADA",
      p_Synergistetes = "#FB8072"
    )
  ) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.8) +
  xlab("LDA Score (Log10)") +
  ylab("") +
  theme_bw() +
  theme(
    axis.ticks.length = unit(.4, "lines", "Bold"),
    axis.ticks = element_line(linewidth = 0.6),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 14)
  )

p1


p <-
  ggplot(data, aes(`lm1:Estimate`, y, col = phylum)) # 不同形状shape= Factor

p2 <- p + geom_point(size = 2, aes(col = phylum)) +
  
  geom_errorbarh(aes(xmax = `lm1:beta.lwr`, xmin = `lm1:beta.upr`, col =
                       phylum),
                 height = 0.4) +
  
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.8) +
  
  scale_color_manual(
    values = c(
      p_Actinobacteria = "#80B1D3",
      p_Bacteroidetes = "#8DD3C7",
      p_Firmicutes = "#FDB462",
      p_Proteobacteria = "#FCCDE5",
      p_Verrucomicrobia = "#BEBADA",
      p_Synergistetes = "#FB8072"
    )
  ) +
  
  xlab('SMD (95%CI)') + ylab("") + theme_bw() + theme(legend.position =
                                                        "top") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.text = element_text(size = 11),
    axis.ticks.length = unit(.4, "lines"),
    axis.ticks = element_line(linewidth = 0.8),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 14),
    # 修改图例方向为纵向（上到下）
    legend.direction = "vertical"
  )


p2


p <-
  ggplot(data, aes(`lm2:Estimate`, y, col = phylum)) # 不同形状shape= Factor

p3 <- p + geom_point(size = 2, aes(col = phylum)) +
  
  geom_errorbarh(aes(xmax = `lm2:beta.lwr`, xmin = `lm2:beta.upr`, col =
                       phylum),
                 height = 0.4) +
  
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.8) +
  
  scale_color_manual(
    values = c(
      p_Actinobacteria = "#80B1D3",
      p_Bacteroidetes = "#8DD3C7",
      p_Firmicutes = "#FDB462",
      p_Proteobacteria = "#FCCDE5",
      p_Verrucomicrobia  = "#BEBADA",
      p_Synergistetes = "#FB8072"
    )
  ) +
  
  xlab('SMD (95%CI)') + ylab("") + theme_bw() + theme(legend.position =
                                                        "top") +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 11),
    axis.ticks.length = unit(.4, "lines"),
    axis.ticks = element_line(linewidth = 0.8),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(size = 14),
    # 修改图例方向为纵向（上到下）
    legend.direction = "vertical"
  )

p3

library(aplot)

p1 %>%
  insert_right(p2)%>%
  insert_right(p3)



library(vegan)
library(ggplot2)
library(ggsignif)
library(customLayout)

asv_data <- t(physeq@otu_table) %>% as.data.frame()
identical(physeq@sam_data$SampleID, rownames(asv_data))
Demo <- physeq@sam_data #创建Demo数据框，从physeq对象中提取样本数据，并计算每个样本的丰富度、Shannon多样性指、Simpson多样性指数，以及Pielou均匀度指数
Demo$richness <- specnumber(asv_data) 
Demo$shannon <- diversity(asv_data, index = "shannon")
Demo$simpson <- diversity(asv_data, index = "simpson")
Demo$Pielou <- Demo$shannon/log(Demo$richness,exp(1))
Demo <- cbind(Demo, asv_data)

#### MaAsLin2 ####,'edu_3','marry_2','occupation'
library(Maaslin2)
MaAsLin = Maaslin2( # (take mian analysis for example) 运行MaAsLin2分析，用于鉴定微生物丰度与其他因素的关联关系
  input_data = Demo[,c('SampleID', rownames(physeq@otu_table))], #指定MaAsLin2分析的输入数据，包括样本ID和微生物丰度数据。
  input_metadata = Demo[,c("SampleID",'hot','age', 'BMI')], #指定MaAsLin2分析的元数据，包括样本ID、吸烟状态、年龄和BMI等。
  output = paste0('C:/Users/OMEN/Desktop/GNHS_maaslin_female_var2'), 
  min_abundance = 0.0, #设置微生物丰度的最小阈值，低于此阈值的微生物将被过滤掉
  min_prevalence = 0.1, #设置微生物的最小普遍性，低于此值的微生物将被过滤掉
  min_variance = 0.0, #设置微生物丰度的最小方差，低于此值的微生物将被过滤掉
  normalization = "NONE", #TSS, CLR, CSS, NONE, TMM
  transform = "LOG",         #LOG, LOGIT, AST, NONE  设置数据的转换方法，此处选择对数转换
  analysis_method = "LM",   #LM, CPLM, NEGBIN, ZINB 选择线性模型
  max_significance = 0.05, #设置显著性水平的阈值，低于此值的结果将被认为是显著的
  fixed_effects = c('hot','age', 'BMI'), #指定用作固定效应的变量，包括吸烟状态、年龄和BMI
  correction = "BH", #设置多重比较校正方法，此处选择Benjamini-Hochberg方法
  standardize = TRUE,
  plot_heatmap = TRUE,
  plot_scatter = TRUE,
  heatmap_first_n = 20,
  reference = c('hot,No')) #指定用作参考组的吸烟状态，此处选择非吸烟者作为参考组。
