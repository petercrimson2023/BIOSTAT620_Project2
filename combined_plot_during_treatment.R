

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
