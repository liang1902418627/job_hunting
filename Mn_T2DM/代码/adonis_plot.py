import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.patches import Patch

# 读取数据
data = pd.read_csv("主要结果/表格/adonis2.csv")
data.columns
# 添加文本列，根据p值设置星号
conditions = [
    data['Pr..F.'] >= 0.05,
    data['Pr..F.'] >= 0.01,
    data['Pr..F.'] > 0.001,
    data['Pr..F.'] <= 0.001
]
choices = [" ", "\n*", "\n**", "\n***"]

data['text'] = np.select(conditions, choices, default=" ")

# 计算每行的R平方值百分比
data['Explanation of gut microbiota structure (%)'] = data['R2'] * 100

# 升序排列
data = data.sort_values(by='Explanation of gut microbiota structure (%)', ascending=True)

# 设置标签的因子水平顺序，使用反转顺序
data['label'] = pd.Categorical(data['variable'], categories=data['variable'][::-1], ordered=False)

# 添加 color 列
data['color'] = 'grey'
data.loc[data['label'] == 'Mn quartiles', 'color'] = 'blue'

# 只取前13行
plot_data = data.iloc[:13, :].copy()

# 画图
plt.figure(figsize=(7, 8))
bar_colors = plot_data['color'].map({'blue': "#0072B2", 'grey': "#bebebefb"})

bars = plt.barh(
    plot_data['label'],
    plot_data['Explanation of gut microbiota structure (%)'] , 
    color=bar_colors,
    height=0.7
)

# 添加星号文本
for i, (bar, txt) in enumerate(zip(bars, plot_data['text'])):
    plt.text(
        bar.get_width() + 0.01, # 水平位置
        bar.get_y() + bar.get_height() / 2,
        txt,
        va='center',
        ha='left',
        color='black',
        fontsize=14
    )


# 或者根据数据量动态调整高度
height = max(8, len(plot_data) * 0.6)  # 根据条形数量动态调整高度
plt.xlim(0, 1)
plt.xlabel("Explanation of gut microbiota structure (%)", fontsize=10, fontweight='normal')
plt.ylabel("")
plt.yticks(fontsize=12, color='black')
plt.xticks(fontsize=12, color='black')
plt.gca().spines['top'].set_linewidth(1.2)
plt.gca().spines['right'].set_linewidth(1.2)
plt.gca().spines['left'].set_linewidth(1.2)
plt.gca().spines['bottom'].set_linewidth(1.2)
plt.grid(False)
plt.tight_layout()

# 不显示图例（如需可自行加legend）

plt.savefig(
    "主要结果/图片/beta/Adonis2_analysis_Contribution_of_variables_to_the_differences_in_microbial_communities.pdf",
    format="pdf", dpi=300
)
# plt.show()
plt.close()