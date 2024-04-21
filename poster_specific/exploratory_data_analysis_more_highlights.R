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




plot_time_series_with_more_highlight <- function(data_set, category_info, metric_name, category_variable,
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

  
  saturdays <- data$Date[weekdays(data$Date) %in% c("星期六", "星期日")]
  
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





Sys.setenv(languages = "en")
#____________________Preparing the Data__________________________________________



id_start_date <- complete_data %>%
  group_by(pseudo_ID) %>%
  summarise(start_date = min(Date)) %>% select(start_date)

where_max_date = id_start_date %>% unlist() %>% which.max() 

start_date = id_start_date[where_max_date,"start_date"] %>% pull()

end_date = as.Date("2024-03-27")

common_dates = seq.Date(from = start_date[1], to = end_date, by = "day")


Total_ST_min_common =summarize_and_pivot(complete_data, "Total.ST.min", common_dates)
Social_ST_min_common=summarize_and_pivot(complete_data, "Social.ST.min", common_dates) 
Pickups_common=summarize_and_pivot(complete_data, "Pickups", common_dates)
Proportion_ST_common= summarize_and_pivot(complete_data, "Proportion.ST", common_dates) 



p1 = plot_time_series_with_more_highlight("Total_ST_min_common", "complete_data", "Total Screen Time", "sex",
                                           highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

p2 = plot_time_series_with_more_highlight("Social_ST_min_common", "complete_data", "Social Screen", "sex",
                                           highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

p3 = plot_time_series_with_more_highlight("Pickups_common", "complete_data", "Pickups", "sex",
                                           highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

p4 = plot_time_series_with_more_highlight("Proportion_ST_common", "complete_data", "Proportion of Screen time", "sex",
                                           highlight_period = TRUE, highlight_dates = c("2024-02-24", "2024-03-04"))

(p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2)) 
# %>% ggsave("plots/time_series_sex.png",.)

