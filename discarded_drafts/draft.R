#draft

setwd(here::here())
library(readxl)
library(dplyr)

# loading the sheet 1 of exce file "Fulldata_620W24_Project2.xlsx"

#data <- 


data <- readxl::read_excel("Fulldata_620W24_Project2.xlsx", sheet = 1)

# data$Pickup.1st %>% head()
# [1] "0.3298611111111111"  "0.28125"             "0.28125"             "0.31944444444444448" "0.33680555555555558"
# [6] "0.40208333333333335"


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


removed_data$compliance <- ifelse(removed_data$compliance == "Fail" | removed_data$compliance == "N", 0, 
                                  ifelse(removed_data$compliance == "Success", 1, 
                                         ifelse(removed_data$compliance == "N/A", NA, removed_data$compliance)))


for (id in pesudo_id_list$pseudo_ID){
  print(id)
  print(temp_check_compliance_func(id))
}

library(openxlsx)

dir.create("processed_data")

unique_ids <- unique(removed_data$pseudo_ID)

excel_file <- "processed_data/removed_data_by_id.xlsx"

wb <- createWorkbook()

for (id in unique_ids) {
  data_by_id <- removed_data %>% filter(pseudo_ID == id)
  
  addWorksheet(wb, sheetName = as.character(id))
  writeData(wb, sheet = as.character(id), x = data_by_id)
}

saveWorkbook(wb, excel_file, overwrite = TRUE)






removed_data %>% filter(pseudo_ID == 6419) %>% select(Pickup.1st) %>% table()







