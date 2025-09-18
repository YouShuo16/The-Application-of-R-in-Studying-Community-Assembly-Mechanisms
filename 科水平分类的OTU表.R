setwd("D:/A重要文件/向花 组会汇报/数据/1型/实验结果")
#install.packages("RColorBrewer")
#install.packages("extrafont")

# 加载必要的包
library(ggplot2)
library(dplyr)
library(forcats) # 用于高效处理因子型数据
library(scales)
library(RColorBrewer)
library(extrafont)

# 1. 读取数据
relative_data <- read.csv("family水平分类-11_相对丰度.csv", stringsAsFactors = FALSE)

# 2. 数据预处理 - 核心步骤：定义主要菌科并合并其余
# 计算每个科在所有样本中的平均丰度
family_summary <- relative_data %>%
  group_by(Species) %>%
  summarise(Mean_Abundance = mean(Relative_Abundance), .groups = 'drop')

# 按平均丰度降序排列
top_families <- family_summary %>%
  arrange(desc(Mean_Abundance)) %>%
  pull(Species)

# 选择前N个最丰富的科，其余的归为“Other Families”
# 这个数字（例如8-12）可以根据您想突出的主要菌科数量调整
num_top_families <- 10
families_to_keep <- top_families[1:num_top_families]
actual_samples <- sort(unique(relative_data$Samples))
print("实际样本名称:")
print(actual_samples)

# 按照数字顺序排序样本
sample_levels <- rev(actual_samples[order(as.numeric(gsub("S", "", actual_samples)))])
print("排序后的样本顺序:")
print(sample_levels)

plot_data <- relative_data %>%
  mutate(
    # 根据上述列表创建新的分组变量
    Species_Grouped = if_else(Species %in% families_to_keep, 
                             Species, 
                             "Other"),
    # 确保样本是因子，并可以按需要排序（这里按原始顺序）
    Samples = factor(Samples, levels = unique(sample_levels))
  )

# 3. 对分组后的数据按样本和菌科进行汇总
plot_data_final <- plot_data %>%
  group_by(Samples, Species_Grouped) %>%
  summarise(Relative_Abundance = sum(Relative_Abundance, na.rm = TRUE), .groups = 'drop')

# 4. 为菌科排序（按整体丰度从高到低），让图例和堆叠顺序更合理
# 计算“Species_Grouped”级别的总丰度来确定顺序
family_order <- plot_data_final %>%
  group_by(Species_Grouped) %>%
  summarise(Total_Abundance = sum(Relative_Abundance)) %>%
  arrange(desc(Total_Abundance)) %>%
  pull(Species_Grouped)
#将“Other”放在最后
final_family_order <- c(family_order, "Other")
plot_data_final <- plot_data_final %>%
  mutate(Species_Grouped = factor(Species_Grouped, levels = family_order))

# 5. 创建颜色方案
# 为主要菌科分配颜色，为“Other Families”分配灰色
n_colors_needed <- length(families_to_keep)
# 使用Set3调色板，确保不超过其最大颜色数(12)
family_colors <- c(brewer.pal(min(n_colors_needed, 12), "Set3")[1:n_colors_needed], "#d3d3d3") 
names(family_colors) <- c(families_to_keep, "Other")

# 确保因子水平顺序正确
final_family_order <- c(families_to_keep, "Other")
plot_data_final <- plot_data_final %>%
  mutate(Species_Grouped = factor(Species_Grouped, levels = final_family_order))

# 6. 绘制横向堆叠柱状图
p <- ggplot(plot_data_final, 
            aes(x = Samples, 
                y = Relative_Abundance, 
                fill = Species_Grouped)) +
  geom_col(width = 0.75, color = NA) + # 使用geom_col()代替geom_bar(stat="identity"), 去除边框线
  scale_fill_manual(values = family_colors) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = expansion(mult = c(0, 0.01))) + # Y轴从0开始，顶部稍留空间
  coord_flip() + # 翻转坐标轴，变成横向图
  labs(
    x = NULL, # 样本名称在Y轴，通常不需要再标“Sampling Site”
    y = "Relative Abundance (%)",
    fill = "Family" # 图例标题更简洁
  ) +
  theme_minimal(base_size = 14) + # 增大基础字体
  theme(
    panel.grid = element_blank(), # 去除所有网格线
    axis.line = element_line(color = "black"), # 添加所有坐标轴线
    axis.ticks = element_line(color = "black"), # 添加所有刻度线
    axis.text.y = element_text(face = "bold"), # 加粗样本名称
    legend.position = "right", # 图例在右侧
    legend.title = element_text(face = "bold", size = 12, hjust = 0.5), # 图例标题居中
    legend.text = element_text(size = 10),
    # 整体去除不必要的元素，更接近目标图的简洁风格
    panel.border = element_blank(),
    plot.background = element_blank(),
    text = element_text(family = "Times New Roman"), # 设置全局文本为Times New Roman
    axis.text = element_text(family = "Times New Roman") # 确保坐标轴刻度文字也是
  ) +
  guides(fill = guide_legend(ncol = 1, keyheight = unit(8, "pt")))
# 7. 显示图形
print(p)

# 8. 保存为高质量图片（推荐矢量图）
ggsave("Microbial_Community_Family.pdf", 
       plot = p, 
       width = 10, 
       height = 6, 
       family = "Times New Roman", # 指定字体家族
       device = cairo_pdf) # 使用cairo_pdf引擎保存，兼容性更好

ggsave("Microbial_Community_Family.png", 
       plot = p, 
       width = 10, 
       height = 6, 
       dpi = 600)

