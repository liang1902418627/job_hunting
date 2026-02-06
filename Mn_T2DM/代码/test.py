from pathlib import Path

import geopandas as gpd

# 输入/输出路径（相对当前脚本所在目录）
in_path = Path(r"数据\地图数据-中国\中华人民共和国.geojson")
out_path = Path(r"数据\地图数据-中国\中华人民共和国.shp")

gdf = gpd.read_file(in_path)

# 把字段 name 改为 name1（只有存在时才改）
if "name" in gdf.columns:
    gdf = gdf.rename(columns={"name": "name1"})

# 确保输出目录存在
out_path.parent.mkdir(parents=True, exist_ok=True)

# 写出前删除旧的 Shapefile 全套文件，避免残留/缓存导致看到旧字段
for ext in [".shp", ".shx", ".dbf", ".prj", ".cpg", ".qix", ".fix"]:
    p = out_path.with_suffix(ext)
    if p.exists():
        p.unlink()

# 写出 Shapefile
gdf.to_file(out_path, driver="ESRI Shapefile", encoding="utf-8")

print("完成：", out_path.resolve())
print("写出前字段：", list(gdf.columns))

# 读回验证（确认磁盘上的字段名）
gdf2 = gpd.read_file(out_path)
print("读回字段：", list(gdf2.columns))