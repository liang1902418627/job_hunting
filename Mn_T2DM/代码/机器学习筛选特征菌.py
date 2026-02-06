# ====L1 正则化===============================================================
import numpy as np   #表示将NumPy库导入到程序中,并用别名`np` ，安装包使用命令为 pip install +“包”
import pandas as pd 
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold

# =======多分类Lasso逻辑回归======================================
# 以血清锰四分位数为因变量筛选差异特征菌, 数据：df
# 读取数据
df = pd.read_csv(r"数据\lasso机器学习数据.csv")

# 提取协变量数据和微生物数据
covariates = df[['age', 'BMI', 'energy_kcal', 'diet_Mn_mg', 
                 'genderMale','Marital_statusOther','educationHigh_school',
                 'educationBachelor_degree_or_above','family_income_cat3000-5000',
                 'family_income_cat5000-10000','family_income_cat>=10000',
                 "family_income_catRefuse to answer or don't know",
                 'family_diabetes_historyYes','smokerFormer_smokers','smokerCurrent_smokers',
                 'Alcohol_consumptionDrinking_more_than_30_days_ago','Alcohol_consumptionDrinking_within_the_past_30_days',
                 'act_sportYes']]

# 找出“g_Eubacterium”这一列的索引号
eubacterium_idx = df.columns.get_loc("g_Eubacterium") if "g_Eubacterium" in df.columns else None
print(f"Index of g_Eubacterium: {eubacterium_idx}")
# 肠道菌相对丰度-对数-标准化
microbes_std = df.iloc[:, 8:50]

# 检查列名是否全部包含肠道菌数据
for col in microbes_std.columns:
    print(f"column: {col}")

# 设置特征数据集
X = pd.concat([covariates, microbes_std], axis=1)

# 设置分类标签（血清锰四分位数）
y = df['Mn_quartiles']  # 分类标签

# 设置正则化权重（协变量不惩罚）
penalty_factor = np.array([0]*covariates.shape[1] + [1]*microbes_std.shape[1])

# 多分类Lasso（弹性网中alpha=1等价于Lasso）
model = LogisticRegression(penalty='l1', solver='saga', multi_class='multinomial',
                          max_iter=10000, class_weight='balanced')

# 交叉验证选择λ
param_grid = {'C': np.logspace(-4, 2, 50)}  # C=1/λ
# 创建带随机种子的交叉验证分割器
cv = KFold(n_splits=5, shuffle=True, random_state=42)
grid = GridSearchCV(model, param_grid, cv=cv, scoring='accuracy')
grid.fit(X * penalty_factor, y)  # 通过权重矩阵实现分层惩罚

# 提取非零系数
best_model = grid.best_estimator_
selected_microbes = X.columns[best_model.coef_[0] != 0]
selected_microbes.shape  # 查看选中的微生物特征数量
print("Selected microbes:", selected_microbes.tolist())

# 将选中的微生物特征保存到CSV文件中
selected_microbes_df = pd.DataFrame(selected_microbes, columns=['Selected Microbes'])
selected_microbes_df.to_csv(r"数据\lasso_与血清锰四分位数相关的肠道菌名称.csv", index=False)

# 以IFG_or_diabetes为因变量筛选差异特征菌, 数据：df
# 手动清除终端
import numpy as np   #表示将NumPy库导入到程序中,并用别名`np` ，安装包使用命令为 pip install +“包”
import pandas as pd 
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold

# 读取数据
df = pd.read_csv(r"数据\lasso机器学习数据.csv")
# 清除IFG_or_diabetes含有缺失值的行
df = df.dropna(subset=['IFG_or_diabetes'])
# 提取协变量数据和微生物数据
covariates = df[['age', 'BMI', 'energy_kcal', 'diet_Mn_mg', 
                 'genderMale','Marital_statusOther','educationHigh_school',
                 'educationBachelor_degree_or_above','family_income_cat3000-5000',
                 'family_income_cat5000-10000','family_income_cat>=10000',
                 "family_income_catRefuse to answer or don't know",
                 'family_diabetes_historyYes','smokerFormer_smokers','smokerCurrent_smokers',
                 'Alcohol_consumptionDrinking_more_than_30_days_ago','Alcohol_consumptionDrinking_within_the_past_30_days',
                 'act_sportYes']]

# 读取与血清锰四分位数相关的肠道菌名称df2
df2 = pd.read_csv(r"数据\lasso_与血清锰四分位数相关的肠道菌名称.csv")

# 肠道菌相对丰度-对数-标准化数据
microbes_std = df[df2['Selected Microbes'].values]

# 检查列名是否全部包含肠道菌数据
for col in microbes_std.columns:
    print(f"column: {col}")

# 设置特征数据集
X = pd.concat([covariates, microbes_std], axis=1)

# 设置分类标签（IFG_or_diabetes）
y = df['IFG_or_diabetes']  # 分类标签
# 设置正则化权重（协变量不惩罚）
penalty_factor = np.array([0]*covariates.shape[1] + [1]*microbes_std.shape[1])

# 多分类Lasso（弹性网中alpha=1等价于Lasso）
model = LogisticRegression(penalty='l1', solver='saga', multi_class='multinomial',
                          max_iter=10000, class_weight='balanced')

# 交叉验证选择λ
param_grid = {'C': np.logspace(-4, 2, 50)}  # C=1/λ
# 创建带随机种子的交叉验证分割器
cv = KFold(n_splits=5, shuffle=True, random_state=42)
grid = GridSearchCV(model, param_grid, cv=cv, scoring='accuracy')
grid.fit(X * penalty_factor, y)  # 通过权重矩阵实现分层惩罚

# 提取非零系数
best_model = grid.best_estimator_
selected_microbes = X.columns[best_model.coef_[0] != 0]
selected_microbes.shape  # 查看选中的微生物特征数量
print("Selected microbes:", selected_microbes.tolist())
# ==========L2 正则化=================================================
# 手动清除终端
import numpy as np
import pandas as pd 
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import KFold

# =======多分类Ridge逻辑回归======================================
# 以血清锰四分位数为因变量筛选差异特征菌, 数据：df
# 读取数据
df = pd.read_csv(r"数据\lasso机器学习数据.csv")

# 提取协变量数据和微生物数据
covariates = df[['age', 'BMI', 'energy_kcal', 'diet_Mn_mg', 
                 'genderMale','Marital_statusOther','educationHigh_school',
                 'educationBachelor_degree_or_above','family_income_cat3000-5000',
                 'family_income_cat5000-10000','family_income_cat>=10000',
                 "family_income_catRefuse to answer or don't know",
                 'family_diabetes_historyYes','smokerFormer_smokers','smokerCurrent_smokers',
                 'Alcohol_consumptionDrinking_more_than_30_days_ago','Alcohol_consumptionDrinking_within_the_past_30_days',
                 'act_sportYes']]

# 找出"g_Eubacterium"这一列的索引号
eubacterium_idx = df.columns.get_loc("g_Eubacterium") if "g_Eubacterium" in df.columns else None
print(f"Index of g_Eubacterium: {eubacterium_idx}")
# 肠道菌相对丰度-对数-标准化
microbes_std = df.iloc[:, 8:50]

# 检查列名是否全部包含肠道菌数据
for col in microbes_std.columns:
    print(f"column: {col}")

# 设置特征数据集
X = pd.concat([covariates, microbes_std], axis=1)

# 设置分类标签（血清锰四分位数）
y = df['Mn_quartiles']  # 分类标签

# 设置正则化权重（协变量不惩罚）
penalty_factor = np.array([0]*covariates.shape[1] + [1]*microbes_std.shape[1])

# 多分类Ridge（L2正则化）
model = LogisticRegression(penalty='l2', solver='lbfgs', multi_class='multinomial',
                          max_iter=10000, class_weight='balanced')

# 交叉验证选择λ
param_grid = {'C': np.logspace(-4, 2, 50)}  # C=1/λ
# 创建带随机种子的交叉验证分割器
cv = KFold(n_splits=5, shuffle=True, random_state=42)
grid = GridSearchCV(model, param_grid, cv=cv, scoring='accuracy')
grid.fit(X * penalty_factor, y)  # 通过权重矩阵实现分层惩罚

# 提取重要特征（L2可能不会产生完全为零的系数，所以考虑系数绝对值较大的）
best_model = grid.best_estimator_
coef_abs = np.abs(best_model.coef_)
# 选择每个类别中绝对值大于阈值的特征
threshold = np.percentile(coef_abs, 75)  # 例如选择系数绝对值最大的前25%
selected_microbes = X.columns[np.any(coef_abs > threshold, axis=0)]
print("Selected microbes:", selected_microbes.tolist())

# 将选中的微生物特征保存到CSV文件中
selected_microbes_df = pd.DataFrame(selected_microbes, columns=['Selected Microbes'])
selected_microbes_df.to_csv(r"数据\ridge_与血清锰四分位数相关的肠道菌名称.csv", index=False)

# 以IFG_or_diabetes为因变量筛选差异特征菌, 数据：df
# 读取数据
df = pd.read_csv(r"数据\lasso机器学习数据.csv")
# 清除IFG_or_diabetes含有缺失值的行
df = df.dropna(subset=['IFG_or_diabetes'])
# 提取协变量数据和微生物数据
covariates = df[['age', 'BMI', 'energy_kcal', 'diet_Mn_mg', 
                 'genderMale','Marital_statusOther','educationHigh_school',
                 'educationBachelor_degree_or_above','family_income_cat3000-5000',
                 'family_income_cat5000-10000','family_income_cat>=10000',
                 "family_income_catRefuse to answer or don't know",
                 'family_diabetes_historyYes','smokerFormer_smokers','smokerCurrent_smokers',
                 'Alcohol_consumptionDrinking_more_than_30_days_ago','Alcohol_consumptionDrinking_within_the_past_30_days',
                 'act_sportYes']]

# 读取与血清锰四分位数相关的肠道菌名称df2
df2 = pd.read_csv(r"数据\ridge_与血清锰四分位数相关的肠道菌名称.csv")

# 肠道菌相对丰度-对数-标准化数据
microbes_std = df[df2['Selected Microbes'].values]

# 检查列名是否全部包含肠道菌数据
for col in microbes_std.columns:
    print(f"column: {col}")

# 设置特征数据集
X = pd.concat([covariates, microbes_std], axis=1)

# 设置分类标签（IFG_or_diabetes）
y = df['IFG_or_diabetes']  # 分类标签
# 设置正则化权重（协变量不惩罚）
penalty_factor = np.array([0]*covariates.shape[1] + [1]*microbes_std.shape[1])

# 多分类Ridge（L2正则化）
model = LogisticRegression(penalty='l2', solver='lbfgs', multi_class='multinomial',
                          max_iter=10000, class_weight='balanced')

# 交叉验证选择λ
param_grid = {'C': np.logspace(-4, 2, 50)}  # C=1/λ
# 创建带随机种子的交叉验证分割器
cv = KFold(n_splits=5, shuffle=True, random_state=42)
grid = GridSearchCV(model, param_grid, cv=cv, scoring='accuracy')
grid.fit(X * penalty_factor, y)  # 通过权重矩阵实现分层惩罚

# 提取重要特征
best_model = grid.best_estimator_
coef_abs = np.abs(best_model.coef_)
threshold = np.percentile(coef_abs, 75)
selected_microbes = X.columns[np.any(coef_abs > threshold, axis=0)]
print("Selected microbes:", selected_microbes.tolist())

# 将选中的微生物特征保存到CSV文件中
selected_microbes_df = pd.DataFrame(selected_microbes, columns=['Selected Microbes'])
selected_microbes_df.to_csv(r"数据\ridge_与血清锰_IFG_or_diabetes相关的肠道菌名称.csv", index=False)
selected_microbes_df.shape  # 查看选中的微生物特征数量