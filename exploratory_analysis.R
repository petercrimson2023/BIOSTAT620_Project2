# EDA part version 2

setwd(here::here())
load("complete_data3.RData")
Sys.setenv(languages = "en")

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(readxl)

summarize_and_pivot <- function(data, value_col, common_dates) {
  data %>%
    select(pseudo_ID, Date, !!sym(value_col)) %>%
    group_by(pseudo_ID, Date) %>%
    summarise("Total" = sum(!!sym(value_col), na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = pseudo_ID, values_from = Total, values_fill = list(Total = NA)) %>%
    right_join(data.frame(Date = common_dates), by = "Date") %>%
    arrange(Date)
}


plot_time_series_with_means_highlight <- function(data_set, category_info, metric_name, category_variable, 
                                                  highlight_period = FALSE, highlight_dates = NULL) {
  # 获取相应的数据集，这里假设数据集已经是全局环境中的一个对象
  data <- get(data_set, envir = globalenv())
  
  # 转换数据为长格式
  data <- pivot_longer(data, cols = -Date, names_to = "pseudo_ID", values_to = "value")
  
  # 获取分类信息，假设这也是一个全局数据框
  category_data <- get(category_info, envir = globalenv()) %>%
    select(pseudo_ID, !!sym(category_variable)) %>%
    distinct(pseudo_ID, .keep_all = TRUE)
  
  # 将分类信息合并到数据中
  data <- left_join(data, category_data, by = "pseudo_ID")
  
  # 计算分类特定均值
  mean_data <- data %>%
    group_by(Date, !!sym(category_variable)) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # 开始构建图形对象
  plot <- ggplot(data, aes(x = Date, y = value)) +
    geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5) +  # 所有人的线条作为背景
    geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2)  # 分类均值线条
  
  # 根据 highlight_period 决定是否高亮显示特定日期
  if (highlight_period && !is.null(highlight_dates)) {
    
    highlight_start <- as.Date(highlight_dates[1])
    highlight_end <- as.Date(highlight_dates[2])
    
    plot <- ggplot(data, aes(x = Date, y = value)) +
      geom_rect(xmin = highlight_start, xmax = highlight_end,  # 先绘制高亮区域
                ymin = -Inf, ymax = Inf,
                fill = "green", alpha = 0.5) +
      geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5) +  # 然后绘制所有人的线条
      geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2)  # 最后绘制分类均值线条
    
  }
  
  # 添加图形的其他元素和样式
  plot <- plot +
    scale_color_manual(values = c("1" = "blue", "0" = "red"), labels = c("1" = "Male", "0" = "Female")) +
    labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),  # 标题居中并设定大小
      legend.position = "right",
      legend.title = element_text(size = 12),  # 图例标题放大
      legend.text = element_text(size = 12)    # 图例文本放大
    )
  # 
  # plot <- plot +
  #   scale_color_manual(values = c("A" = "blue", "B" = "red"), labels = c("A" = "Trt A", "B" = "Trt B")) +
  #   labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
  #   theme_bw() +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, size = 14),  # 标题居中并设定大小
  #     legend.position = "right",
  #     legend.title = element_text(size = 12),  # 图例标题放大
  #     legend.text = element_text(size = 12)    # 图例文本放大
  #   )
  # 
  return(plot)
}

#_______________________baseline description_____________________________________


temp_data = complete_data %>% select(pseudo_ID,Treatment,sex,pets,devices,
                                     procrastination_score) %>% distinct()

sheet2 = read_excel("Fulldata_620W24_Project2(2).xlsx",sheet=2)


library(e1071)
stats_summary <- sheet2 %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    n_na_sex = sum(is.na(sex)),
    mean_sex = mean(sex, na.rm = TRUE),
    sd_sex = sd(sex, na.rm = TRUE),
    median_sex = median(sex, na.rm = TRUE),
    min_sex = min(sex, na.rm = TRUE),
    max_sex = max(sex, na.rm = TRUE),
    skew_sex = skewness(sex, na.rm = TRUE),
    kurtosis_sex = kurtosis(sex, na.rm = TRUE),
    
    n_na_pets = sum(is.na(pets)),
    mean_pets = mean(pets, na.rm = TRUE),
    sd_pets = sd(pets, na.rm = TRUE),
    median_pets = median(pets, na.rm = TRUE),
    min_pets = min(pets, na.rm = TRUE),
    max_pets = max(pets, na.rm = TRUE),
    skew_pets = skewness(pets, na.rm = TRUE),
    kurtosis_pets = kurtosis(pets, na.rm = TRUE),
    
    n_na_devices = sum(is.na(devices)),
    mean_devices = mean(devices, na.rm = TRUE),
    sd_devices = sd(devices, na.rm = TRUE),
    median_devices = median(devices, na.rm = TRUE),
    min_devicesx = min(devices, na.rm = TRUE),
    max_devices = max(devices, na.rm = TRUE),
    skew_devices = skewness(devices, na.rm = TRUE),
    kurtosis_devices = kurtosis(devices, na.rm = TRUE),
    
    n_na_age = sum(is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    skew_age= skewness(age, na.rm = TRUE),
    kurtosis_age = kurtosis(age, na.rm = TRUE),
    
    n_naprocrastination_score = sum(is.na(procrastination_score)),
    mean_procrastination_score = mean(procrastination_score, na.rm = TRUE),
    sd_procrastination_score = sd(procrastination_score, na.rm = TRUE),
    median_procrastination_score = median(procrastination_score, na.rm = TRUE),
    min_procrastination_score = min(procrastination_score, na.rm = TRUE),
    max_procrastination_score = max(procrastination_score, na.rm = TRUE),
    skew_procrastination_score = skewness(procrastination_score, na.rm = TRUE),
    kurtosis_procrastination_score = kurtosis(procrastination_score, na.rm = TRUE),
    
  )
stats_summary 

#_______________________Summary Statistics_____________________________________

complete_data %>% group_by(pseudo_ID) %>% 
  summarise(
    Total_mean = mean(Total.ST.min, na.rm = TRUE),
    Social_mean = mean(Social.ST.min, na.rm = TRUE),
    Pickups_mean = mean(Pickups, na.rm = TRUE),
    Proportion_mean = mean(Proportion.ST, na.rm = TRUE)
  ) %>% distinct() -> summary_stats


p1 = ggplot(summary_stats, aes(x = Total_mean)) +
  geom_histogram(fill = "blue", color = "black", bins = 15) +
  labs(title = "Total Screen Time", x = "Total Screen Time (minutes)", y = "Frequency") +
  theme_minimal()

p2 = ggplot(summary_stats, aes(x = Social_mean)) +
  geom_histogram(fill = "red", color = "black", bins = 15) +
  labs(title = "Social Screen Time", x = "Social Screen Time (minutes)", y = "Frequency") +
  theme_minimal()

p3 = ggplot(summary_stats, aes(x = Pickups_mean)) +
  geom_histogram(fill = "green", color = "black", bins = 30) +
  labs(title = "Pickups", x = "Number of Pickups", y = "Frequency") +
  theme_minimal()

p4 = ggplot(summary_stats, aes(x = Proportion_mean)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Proportion of Screen Time", x = "Proportion of Screen Time", y = "Frequency") +
  theme_minimal()


#dir.create("plots", showWarnings = FALSE)

#(p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)) %>% ggsave("plots/summary_stats.png",.)

#对上述四个变量，使用ggplot2，绘制箱线图

p5 = ggplot(summary_stats, aes(x = "", y = Total_mean)) +
  geom_boxplot(fill = "blue", color = "black",coef = 1.1) +
  labs(title = "Total Screen Time", x = "", y = "Total Screen Time (minutes)") +
  theme_minimal()

p6 = ggplot(summary_stats, aes(x = "", y = Social_mean)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Social Screen Time", x = "", y = "Social Screen Time (minutes)") +
  theme_minimal()

p7 = ggplot(summary_stats, aes(x = "", y = Pickups_mean)) +
  geom_boxplot(fill = "green", color = "black",coef=1.2) +
  labs(title = "Pickups", x = "", y = "Number of Pickups") +
  theme_minimal()

p8 = ggplot(summary_stats, aes(x = "", y = Proportion_mean)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Proportion of Screen Time", x = "", y = "Proportion of Screen Time") +
  theme_minimal()

# 调整图片的宽度，使其存储时，长度大于高度

(p1 + p2 + p3 + p4 +p5 + p6 + p7 + p8 + plot_layout(ncol = 4, nrow = 2)) %>% ggsave("plots/summary_stats_boxplot.png",., width = 10, height = 8, units = "in")

#_______________________Time Series Plots_____________________________________



id_start_date <- complete_data %>%
  group_by(pseudo_ID) %>%
  summarise(start_date = min(Date)) %>% select(start_date)

# 找出最晚的日期
where_max_date = id_start_date %>% unlist() %>% which.max() 

start_date = id_start_date[where_max_date,"start_date"] %>% pull()

end_date = as.Date("2024-03-27")

common_dates = seq.Date(from = start_date[1], to = end_date, by = "day")


Total_ST_min_common =summarize_and_pivot(complete_data, "Total.ST.min", common_dates)
Social_ST_min_common=summarize_and_pivot(complete_data, "Social.ST.min", common_dates) 
Pickups_common=summarize_and_pivot(complete_data, "Pickups", common_dates)
Proportion_ST_common= summarize_and_pivot(complete_data, "Proportion.ST", common_dates) 


p1 = plot_time_series_with_means_highlight("Total_ST_min_common", "complete_data", "Total Screen Time", "sex",
                       highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

p2 = plot_time_series_with_means_highlight("Social_ST_min_common", "complete_data", "Social Screen", "sex",
                       highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

p3 = plot_time_series_with_means_highlight("Pickups_common", "complete_data", "Pickups", "sex",
                       highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

p4 = plot_time_series_with_means_highlight("Proportion_ST_common", "complete_data", "Proportion of Screen time", "sex",
                       highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

(p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)) %>% ggsave("plots/time_series_sex.png",.)


#_______________________Average Curve for Pickups for in Treatment period_____________________________________




plot_time_series_with_means_highlight <- function(data_set, category_info, metric_name, category_variable, 
                                                  highlight_period = FALSE, highlight_dates = NULL) {
  # 获取相应的数据集，这里假设数据集已经是全局环境中的一个对象
  data <- get(data_set, envir = globalenv())
  
  # 转换数据为长格式
  data <- pivot_longer(data, cols = -Date, names_to = "pseudo_ID", values_to = "value")
  
  # 获取分类信息，假设这也是一个全局数据框
  category_data <- get(category_info, envir = globalenv()) %>%
    select(pseudo_ID, !!sym(category_variable)) %>%
    distinct(pseudo_ID, .keep_all = TRUE)
  
  # 将分类信息合并到数据中
  data <- left_join(data, category_data, by = "pseudo_ID")
  
  # 计算分类特定均值
  mean_data <- data %>%
    group_by(Date, !!sym(category_variable)) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # 开始构建图形对象
  plot <- ggplot(data, aes(x = Date, y = value)) +
    geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5) +  # 所有人的线条作为背景
    geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2)  # 分类均值线条
  
  # 根据 highlight_period 决定是否高亮显示特定日期
  if (highlight_period && !is.null(highlight_dates)) {
    
    highlight_start <- as.Date(highlight_dates[1])
    highlight_end <- as.Date(highlight_dates[2])
    
    plot <- ggplot(data, aes(x = Date, y = value)) +
      geom_rect(xmin = highlight_start, xmax = highlight_end,  # 先绘制高亮区域
                ymin = -Inf, ymax = Inf,
                fill = "green", alpha = 0.5) +
      geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5) +  # 然后绘制所有人的线条
      geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2)  # 最后绘制分类均值线条
    
  }
  
  # 添加图形的其他元素和样式
  # plot <- plot +
  #   scale_color_manual(values = c("1" = "blue", "0" = "red"), labels = c("1" = "Male", "0" = "Female")) +
  #   labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
  #   theme_bw() +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, size = 14),  # 标题居中并设定大小
  #     legend.position = "right",
  #     legend.title = element_text(size = 12),  # 图例标题放大
  #     legend.text = element_text(size = 12)    # 图例文本放大
  #   )
  

  plot <- plot +
    scale_color_manual(values = c("A" = "blue", "B" = "red"), labels = c("A" = "Trt A", "B" = "Trt B")) +
    labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20),  # 标题居中并设定大小
      legend.position = "right",
    axis.title.x = element_text(size = 20),  # x轴标题放大
    axis.title.y = element_text(size = 20),  # y轴标题放大
      legend.title = element_text(size = 20),  # 图例标题放大
      legend.text = element_text(size = 20)# 图例文本放大
    )



  return(plot)
}


id_end_date <- complete_data %>%
  group_by(pseudo_ID) %>%
  summarise(end_date = max(Date)) %>% select(end_date)

where_min_date = id_end_date %>% unlist() %>% which.min() 

end_common_date = id_end_date[where_min_date,"end_date"] %>% pull()

start_date = as.Date("2024-03-20")

common_dates = seq.Date(from = start_date, to = end_common_date, by = "day")


Pickups_Phase_common=summarize_and_pivot(complete_data, "Pickups", common_dates)

pickup_treatment = plot_time_series_with_means_highlight("Pickups_Phase_common", "complete_data", "Pickups", "Treatment",
                                      highlight_period = TRUE, highlight_dates = c("2024-03-27", "2024-04-02"))
  

#___select data in Treatment period  grouped by sex

id_end_date <- complete_data %>%
  group_by(pseudo_ID) %>%
  summarise(end_date = max(Date)) %>% select(end_date)

where_min_date = id_end_date %>% unlist() %>% which.min() 

end_common_date = id_end_date[where_min_date,"end_date"] %>% pull()

start_date = as.Date("2024-03-20")

common_dates = seq.Date(from = start_date[1], to = end_common_date, by = "day")

Pickups_common=summarize_and_pivot(complete_data, "Pickups", common_dates)



plot_time_series_with_means_highlight <- function(data_set, category_info, metric_name, category_variable, 
                                                  highlight_period = FALSE, highlight_dates = NULL) {
  # 获取相应的数据集，这里假设数据集已经是全局环境中的一个对象
  data <- get(data_set, envir = globalenv())
  
  # 转换数据为长格式
  data <- pivot_longer(data, cols = -Date, names_to = "pseudo_ID", values_to = "value")
  
  # 获取分类信息，假设这也是一个全局数据框
  category_data <- get(category_info, envir = globalenv()) %>%
    select(pseudo_ID, !!sym(category_variable)) %>%
    distinct(pseudo_ID, .keep_all = TRUE)
  
  # 将分类信息合并到数据中
  data <- left_join(data, category_data, by = "pseudo_ID")
  
  # 计算分类特定均值
  mean_data <- data %>%
    group_by(Date, !!sym(category_variable)) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # 开始构建图形对象
  plot <- ggplot(data, aes(x = Date, y = value)) +
    geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5) +  # 所有人的线条作为背景
    geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2)  # 分类均值线条
  
  # 根据 highlight_period 决定是否高亮显示特定日期
  if (highlight_period && !is.null(highlight_dates)) {
    
    highlight_start <- as.Date(highlight_dates[1])
    highlight_end <- as.Date(highlight_dates[2])
    
    plot <- ggplot(data, aes(x = Date, y = value)) +
      geom_rect(xmin = highlight_start, xmax = highlight_end,  # 先绘制高亮区域
                ymin = -Inf, ymax = Inf,
                fill = "green", alpha = 0.5) +
      geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5) +  # 然后绘制所有人的线条
      geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2)  # 最后绘制分类均值线条
    
  }
  
  # 添加图形的其他元素和样式
  plot <- plot +
    scale_color_manual(values = c("1" = "blue", "0" = "red"), labels = c("1" = "Male", "0" = "Female")) +
    labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
    theme_bw() +
    theme(
          plot.title = element_text(hjust = 0.5, size = 20),  # 标题居中并设定大小
          legend.position = "right",
          axis.title.x = element_text(size = 20),  # x轴标题放大
          axis.title.y = element_text(size = 20),  # y轴标题放大
          legend.title = element_text(size = 20),  # 图例标题放大
          legend.text = element_text(size = 20)# 图例文本放大
        )

  # 
  # plot <- plot +
  #   scale_color_manual(values = c("A" = "blue", "B" = "red"), labels = c("A" = "Trt A", "B" = "Trt B")) +
  #   labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
  #   theme_bw() +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, size = 20),  # 标题居中并设定大小
  #     legend.position = "right",
  #     axis.title.x = element_text(size = 20),  # x轴标题放大
  #     axis.title.y = element_text(size = 20),  # y轴标题放大
  #     legend.title = element_text(size = 20),  # 图例标题放大
  #     legend.text = element_text(size = 20)# 图例文本放大
  #   )
  
  
  
  return(plot)
}


pickup_sex = plot_time_series_with_means_highlight("Pickups_common", "complete_data", "Pickups", "sex",
                                      highlight_period = TRUE, highlight_dates = c(as.Date("2024-03-27"), end_common_date))


# Proportion_ST_common= summarize_and_pivot(complete_data, "Proportion.ST", common_dates) 

pickup_sex+pickup_treatment + plot_layout(ncol = 2) -> combined_plot

combined_plot %>% ggsave("plots/combined_treatment_series.png",., width=20,height=12)



