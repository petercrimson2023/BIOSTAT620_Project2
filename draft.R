#draft

setwd(here::here())
library(readxl)
library(dplyr)

# loading the sheet 1 of exce file "Fulldata_620W24_Project2.xlsx"

#data <- 

# 读入数据的时候，Pickup.1st应该是时间变量类似于11:20:00的时间变量,但是读入为dbl小数，修改这个错误

data <- readxl::read_excel("Fulldata_620W24_Project2.xlsx", sheet = 1)

# data$Pickup.1st %>% head()
# [1] "0.3298611111111111"  "0.28125"             "0.28125"             "0.31944444444444448" "0.33680555555555558"
# [6] "0.40208333333333335"

#将 Pickup.1st转换为时间

#data$Pickup.1st <- as.POSIXct(data$Pickup.1st * 24 * 60 * 60, origin = "1970-01-01")


# checking the data

head(data)

data %>% select(pseudo_ID) %>% table()

# Removing people with pseudo_ID : 1329, 2520, 2880, 6759, 8622, 9285, 9680 because of too many missing value
# using not %in% method

  removed_data <- data %>%
  filter(
    pseudo_ID != 1329 &
      pseudo_ID != 2520 &
      pseudo_ID != 2880 &
      pseudo_ID != 6759 &
      pseudo_ID != 8622 & pseudo_ID != 9285 & pseudo_ID != 9680
  )

# checking remaining numbers of people and their pesudo_id with removed_data
  
removed_data %>% select(pseudo_ID) %>% table()


# checking the compliance of two people with pesudo_ID 6419 and 2793

removed_data %>% filter(pseudo_ID == 6419 | pseudo_ID == 2793) %>% 
  select(pseudo_ID, compliance) %>% table()


pesudo_id_list = removed_data %>% select(pseudo_ID) %>% unique()

temp_check_compliance_func = function(pesudo_id){
  compliance = removed_data %>% 
    filter(pseudo_ID == pesudo_id) %>% 
    select(compliance) %>% table(., useNA = "always")
  return(compliance)
}

for (id in pesudo_id_list$pseudo_ID){
  print(id)
  print(temp_check_compliance_func(id))
}

# 在 removed_data 中 compliance变量创建映射， 如果遇见 Fail 或者 N 则 = 0. 如果遇见 Success = 1, 如果遇见 N/A责改为NA,其他状况不能变

removed_data$compliance <- ifelse(removed_data$compliance == "Fail" | removed_data$compliance == "N", 0, 
                                  ifelse(removed_data$compliance == "Success", 1, 
                                         ifelse(removed_data$compliance == "N/A", NA, removed_data$compliance)))


for (id in pesudo_id_list$pseudo_ID){
  print(id)
  print(temp_check_compliance_func(id))
}

# 新建一个文件夹，将处理后的removed_data存储到一个excel文档中，
#使用readxl中的函数，按照pseudo_ID 分别存储到不同的sheet，sheet的名称设置为 pseudo_ID
library(openxlsx)

# 创建一个新的文件夹用于存储结果
dir.create("processed_data")

# 获取所有唯一的 pseudo_ID
unique_ids <- unique(removed_data$pseudo_ID)

# 创建一个新的 Excel 文件
excel_file <- "processed_data/removed_data_by_id.xlsx"

# 创建一个新的 workbook
wb <- createWorkbook()

# 遍历每个唯一的 pseudo_ID,并将对应的数据存储到单独的 sheet 中
for (id in unique_ids) {
  # 筛选出当前 pseudo_ID 对应的数据
  data_by_id <- removed_data %>% filter(pseudo_ID == id)
  
  # 将数据写入到 workbook 的新 sheet 中
  addWorksheet(wb, sheetName = as.character(id))
  writeData(wb, sheet = as.character(id), x = data_by_id)
}

# 保存 workbook 到 Excel 文件
saveWorkbook(wb, excel_file, overwrite = TRUE)






removed_data %>% filter(pseudo_ID == 6419) %>% select(Pickup.1st) %>% table()







