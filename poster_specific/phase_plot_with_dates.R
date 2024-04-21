setwd(here::here())
load("complete_data3.RData")

# Sys.setenv(languages = "en")

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




plot_time_series_with_more_highlight_trt <- function(data_set, category_info, metric_name, category_variable,
                                                 highlight_period = FALSE, highlight_dates = NULL) {
  data <- get(data_set, envir = globalenv())
  data <- pivot_longer(data, cols = -Date, names_to = "pseudo_ID", values_to = "value")
  
  category_data <- get(category_info, envir = globalenv()) %>%
    select(pseudo_ID, !!sym(category_variable)) %>%
    distinct(pseudo_ID, .keep_all = TRUE)
  
  data <- left_join(data, category_data, by = "pseudo_ID")
  
  mean_data <- data %>%
    group_by(Date, !!sym(category_variable)) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  
  saturdays <- data$Date[weekdays(data$Date) %in% c("Saturday", "Sunday")]
  
  plot <- ggplot(data, aes(x = Date, y = value)) +
    geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5)
  
  if (highlight_period && !is.null(highlight_dates)) {
    highlight_start <- as.Date(highlight_dates[1])
    highlight_end <- as.Date(highlight_dates[2])
    plot <- plot +
      geom_rect(xmin = highlight_start, xmax = highlight_end,
                ymin = -Inf, ymax = Inf,
                fill = "green", alpha = 0.2)
  }
  
  plot <- plot +
    geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2) +
    geom_vline(xintercept = as.numeric(saturdays), linetype = "dashed", color = "blue", size = 0.8) +
    scale_color_manual(values = c("A" = "blue", "B" = "red"), labels = c("A" = "Trt A", "B" = "Trt B")) +
    labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
  
  return(plot)
}



plot_time_series_with_more_highlight_sex <- function(data_set, category_info, metric_name, category_variable,
                                                     highlight_period = FALSE, highlight_dates = NULL) {
  data <- get(data_set, envir = globalenv())
  data <- pivot_longer(data, cols = -Date, names_to = "pseudo_ID", values_to = "value")
  
  category_data <- get(category_info, envir = globalenv()) %>%
    select(pseudo_ID, !!sym(category_variable)) %>%
    distinct(pseudo_ID, .keep_all = TRUE)
  
  data <- left_join(data, category_data, by = "pseudo_ID")
  
  mean_data <- data %>%
    group_by(Date, !!sym(category_variable)) %>%
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  
  saturdays <- data$Date[weekdays(data$Date) %in% c("Saturday", "Sunday")]
  
  plot <- ggplot(data, aes(x = Date, y = value)) +
    geom_line(aes(group = pseudo_ID), color = "grey", alpha = 0.5)
  
  if (highlight_period && !is.null(highlight_dates)) {
    highlight_start <- as.Date(highlight_dates[1])
    highlight_end <- as.Date(highlight_dates[2])
    plot <- plot +
      geom_rect(xmin = highlight_start, xmax = highlight_end,
                ymin = -Inf, ymax = Inf,
                fill = "green", alpha = 0.2)
  }
  
  plot <- plot +
    geom_line(data = mean_data, aes(x = Date, y = mean_value, color = as.factor(!!sym(category_variable))), size = 1.2) +
    geom_vline(xintercept = as.numeric(saturdays), linetype = "dashed", color = "blue", size = 0.8) +
    scale_color_manual(values = c("1" = "blue", "0" = "red"), labels = c("1" = "Male", "0" = "Female")) +
    labs(title = paste("Time Series of", metric_name), x = "Date", y = metric_name, color = category_variable) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
  
  return(plot)
}



#______________________________________________________________________________

id_end_date <- complete_data %>%
  group_by(pseudo_ID) %>%
  summarise(end_date = max(Date)) %>% select(end_date)

where_min_date = id_end_date %>% unlist() %>% which.min() 

end_common_date = id_end_date[where_min_date,"end_date"] %>% pull()

start_date = as.Date("2024-03-20")

common_dates = seq.Date(from = start_date, to = end_common_date, by = "day")


Pickups_Phase_common=summarize_and_pivot(complete_data, "Pickups", common_dates)
Total_ST_Phase_common=summarize_and_pivot(complete_data, "Total.ST.min", common_dates)

pickup_treatment = plot_time_series_with_more_highlight_trt("Pickups_Phase_common", "complete_data", "Pickups", "Treatment",
                                                         highlight_period = TRUE, highlight_dates = c("2024-03-27", "2024-04-02"))


pickup_sex = plot_time_series_with_more_highlight_sex("Pickups_Phase_common", "complete_data", "Pickups", "sex",
                                                   highlight_period = TRUE, highlight_dates = c(as.Date("2024-03-27"), end_common_date))


Total_treatment = plot_time_series_with_more_highlight_trt("Total_ST_Phase_common", "complete_data", "Total.ST.min", "Treatment",
                                                         highlight_period = TRUE, highlight_dates = c("2024-03-27", "2024-04-02"))

Total_sex = plot_time_series_with_more_highlight_sex("Total_ST_Phase_common", "complete_data", "Total.ST.min", "sex",
                                                   highlight_period = TRUE, highlight_dates = c("2024-03-27", "2024-04-02"))

# Proportion_ST_common= summarize_and_pivot(complete_data, "Proportion.ST", common_dates) 

combined_plot = pickup_sex+pickup_treatment + Total_sex+ Total_treatment + plot_layout(ncol = 2)
combined_plot
