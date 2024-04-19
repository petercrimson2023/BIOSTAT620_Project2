rm(list = ls())
#change language to English
Sys.setenv(LANGUAGE = "en")

library(dplyr)
library(tidyr)

load("complete_data3_2307_0_5094_0.RData")

complete_data$success_loose <- ifelse(complete_data$Treatment == 'A' & complete_data$Total.ST.min <= 300, 1,
                                      ifelse(complete_data$Treatment == 'A' & complete_data$Total.ST.min >300, 0,
                                             ifelse(complete_data$Treatment == 'B' & complete_data$Pickups <= 75, 1,
                                                    ifelse(complete_data$Treatment == 'B' & complete_data$Pickups > 75, 0, NA))))


complete_data  = complete_data %>% mutate(
  Pickups_lag_log = log(Pickups_lag),
  Total.ST.min.log = log(Total.ST.min),
  is_work_day = Semester * Xt
)

complete_data <- complete_data %>% 
  group_by(pseudo_ID) %>% 
  mutate(success_lag = lag(success, order_by = Date),
         Total.ST.min.lag = lag(Total.ST.min, order_by = Date),
         Proportion.ST_lag = lag(Proportion.ST, order_by = Date),
         Social.ST.min.lag = lag(Social.ST.min, order_by = Date)
  )

# filter two people because of missingness of pre intervention period

complete_data <- complete_data %>% filter(pseudo_ID != 8622 & pseudo_ID != 9680)

complete_data$weekday <- weekdays(as.Date(complete_data$Date))


complete_data$intervention = ifelse(complete_data$Date >= as.Date("2024-03-27") & complete_data$Date <= as.Date("2024-04-02"), 1, 0)

# spliting data set by weekday

complete_data_monday <- complete_data %>% filter(weekday == "星期一")
complete_data_tuesday <- complete_data %>% filter(weekday == "星期二")
complete_data_wednesday <- complete_data %>% filter(weekday == "星期三")
complete_data_thursday <- complete_data %>% filter(weekday == "星期四")
complete_data_friday <- complete_data %>% filter(weekday == "星期五")
complete_data_saturday <- complete_data %>% filter(weekday == "星期六")
complete_data_sunday <- complete_data %>% filter(weekday == "星期日")


# construction average variables before Date 2024-03-27 grouped by pesudo_ID

complete_data_monday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>% 
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_monday

complete_data_monday <- complete_data_monday %>% left_join(average_data_monday, by = "pseudo_ID")

# add the success event of success during Monday 2024-03-27 - 2024-04-02  for each pesudo_ID

complete_data_monday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>% 
  summarise(
    success_intervention = max(success)
  ) -> success_data_monday

complete_data_monday <- complete_data_monday %>% left_join(success_data_monday, by = "pseudo_ID")
# seems like this line will change the name success into success.x
# so we need to change it back to success
# names(complete_data_monday)[names(complete_data_monday) == "success.x"] <- "success"


# select mean and sex,pets,devices,procrastination_score from complete_data_monday with treatment B and do logistic regression

complete_data_monday %>% filter(Treatment == "B") %>%
  select(mean_Pickups, 
         mean_Total.ST.min, 
         mean_Social.ST.min, 
         mean_Proportion.ST, 
         mean_success_loose,
         sex, pets, devices, 
         procrastination_score, 
         success_intervention,
         Treatment,
         mean_success_loose) %>% distinct() -> logistic_data_monday


logit_monday <-
  glm(
    success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
      procrastination_score,
    data = logistic_data_monday,
    family = binomial(link = "logit")
  )



summary(logit_monday)


#_________________________________ Redo this on remeaining days of the week__________________
# Tuesday

complete_data_tuesday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>% 
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_tue

complete_data_tuesday <- complete_data_tuesday %>% left_join(average_data_tue, by = "pseudo_ID")

# add the success event of success during Monday 2024-03-27 - 2024-04-02  for each pesudo_ID

complete_data_tuesday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>% 
  summarise(
    success_intervention = max(success)
  ) -> success_data_tue

complete_data_tuesday <- complete_data_tuesday %>% left_join(success_data_tue, by = "pseudo_ID")
# seems like this line will change the name success into success.x
# so we need to change it back to success
# names(complete_data_monday)[names(complete_data_monday) == "success.x"] <- "success"


# select mean and sex,pets,devices,procrastination_score from complete_data_monday with treatment B and do logistic regression

complete_data_tuesday %>% filter(Treatment == "B") %>%
  select(mean_Pickups, 
         mean_Total.ST.min, 
         mean_Social.ST.min, 
         mean_Proportion.ST, 
         mean_success_loose,
         sex, pets, devices, 
         procrastination_score, 
         success_intervention,
         Treatment,
         mean_success_loose) %>% distinct() -> logistic_data_tue


logit_tuesday <-
  glm(
    success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
      procrastination_score,
    data = logistic_data_tue,
    family = binomial(link = "logit")
  )

summary(logit_tuesday)

# Wednesday

# Compute averages before the intervention date
complete_data_wednesday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>%
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_wed

# Join averages back to main data
complete_data_wednesday <- complete_data_wednesday %>% left_join(average_data_wed, by = "pseudo_ID")

# Calculate success during the intervention week
complete_data_wednesday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>%
  summarise(
    success_intervention = max(success)
  ) -> success_data_wed

complete_data_wednesday <- complete_data_wednesday %>% left_join(success_data_wed, by = "pseudo_ID")

# Logistic regression for treatment B
complete_data_wednesday %>% filter(Treatment == "B") %>%
  select(
    mean_Pickups,
    mean_Total.ST.min,
    mean_Social.ST.min,
    mean_Proportion.ST,
    mean_success_loose,
    sex,
    pets,
    devices,
    procrastination_score,
    success_intervention,
    Treatment,
    mean_success_loose
  ) %>%
  distinct() -> logistic_data_wed

logit_wednesday <- glm(success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
                         procrastination_score, data = logistic_data_wed, family = binomial(link = "logit"))

summary(logit_wednesday) # failed to converge

# Thursday

complete_data_thursday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>%
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_thur

complete_data_thursday <- complete_data_thursday %>% left_join(average_data_thur, by = "pseudo_ID")

complete_data_thursday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>%
  summarise(
    success_intervention = max(success)
  ) -> success_data_thur

complete_data_thursday <- complete_data_thursday %>% left_join(success_data_thur, by = "pseudo_ID")

# Logistic regression for treatment B
complete_data_thursday %>% filter(Treatment == "B") %>%
  select(
    mean_Pickups,
    mean_Total.ST.min,
    mean_Social.ST.min,
    mean_Proportion.ST,
    mean_success_loose,
    sex,
    pets,
    devices,
    procrastination_score,
    success_intervention,
    Treatment,
    mean_success_loose
  ) %>%
  distinct() -> logistic_data_thur

logit_thursday <- glm(success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
                        procrastination_score, data = logistic_data_thur, family = binomial(link = "logit"))

summary(logit_thursday)

# Friday

# Compute averages before the intervention date
complete_data_friday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>%
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_fri

# Join averages back to main data
complete_data_friday <- complete_data_friday %>% left_join(average_data_fri, by = "pseudo_ID")

# Calculate success during the intervention week
complete_data_friday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>%
  summarise(
    success_intervention = max(success)
  ) -> success_data_fri

complete_data_friday <- complete_data_friday %>% left_join(success_data_fri, by = "pseudo_ID")

# Logistic regression for treatment B
complete_data_friday %>% filter(Treatment == "B") %>%
  select(
    mean_Pickups,
    mean_Total.ST.min,
    mean_Social.ST.min,
    mean_Proportion.ST,
    mean_success_loose,
    sex,
    pets,
    devices,
    procrastination_score,
    success_intervention,
    Treatment,
    mean_success_loose
  ) %>%
  distinct() -> logistic_data_fri

logit_friday <- glm(success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
                      procrastination_score, data = logistic_data_fri, family = binomial(link = "logit"))

summary(logit_friday)


# Saturday

# Compute averages before the intervention date
complete_data_saturday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>%
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_sat

# Join averages back to main data
complete_data_saturday <- complete_data_saturday %>% left_join(average_data_sat, by = "pseudo_ID")

# Calculate success during the intervention week
complete_data_saturday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>%
  summarise(
    success_intervention = max(success)
  ) -> success_data_sat

complete_data_saturday <- complete_data_saturday %>% left_join(success_data_sat, by = "pseudo_ID")

# Logistic regression for treatment B
complete_data_saturday %>% filter(Treatment == "B") %>%
  select(
    mean_Pickups,
    mean_Total.ST.min,
    mean_Social.ST.min,
    mean_Proportion.ST,
    mean_success_loose,
    sex,
    pets,
    devices,
    procrastination_score,
    success_intervention,
    Treatment,
    mean_success_loose
  ) %>%
  distinct() -> logistic_data_sat

logit_saturday <- glm(success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
                        procrastination_score, data = logistic_data_sat, family = binomial(link = "logit"))

summary(logit_saturday)

# Sunday

complete_data_sunday %>% filter(Date < as.Date("2024-03-27") & Phase == 0) %>%
  group_by(pseudo_ID) %>%
  summarise(
    mean_Pickups = mean(Pickups),
    mean_Total.ST.min = mean(Total.ST.min),
    mean_Social.ST.min = mean(Social.ST.min),
    mean_Proportion.ST = mean(Proportion.ST),
    mean_success_loose = mean(success_loose)
  ) -> average_data_sun

# Join averages back to the main data for Sunday
complete_data_sunday <- complete_data_sunday %>% left_join(average_data_sun, by = "pseudo_ID")


complete_data_sunday %>% filter(Date >= as.Date("2024-03-27") & Date <= as.Date("2024-04-02")) %>%
  group_by(pseudo_ID) %>%
  summarise(
    success_intervention = max(success)
  ) -> success_data_sun

complete_data_sunday <- complete_data_sunday %>% left_join(success_data_sun, by = "pseudo_ID")

complete_data_sunday %>% filter(Treatment == "B") %>%
  select(
    mean_Pickups,
    mean_Total.ST.min,
    mean_Social.ST.min,
    mean_Proportion.ST,
    mean_success_loose,
    sex,
    pets,
    devices,
    procrastination_score,
    success_intervention,
    Treatment,
    mean_success_loose
  ) %>%
  distinct() -> logistic_data_sun

logit_sunday <- glm(success_intervention ~ log(mean_Pickups) + log(mean_Total.ST.min) + sex + devices + pets +
                      procrastination_score, data = logistic_data_sun, family = binomial(link = "logit"))

summary(logit_sunday)



#____________________ Model 2 construction _________________________


logit_monday_2 <-
  glm(
    success_intervention ~ sex+devices+mean_success_loose,
    data = logistic_data_monday,
    family = binomial(link = "logit")
  )

summary(logit_monday_2)

logit_tuesday_2 <-
  glm(
    success_intervention ~ sex + devices + mean_success_loose,
    data = logistic_data_tue,
    family = binomial(link = "logit")
  )

summary(logit_tuesday_2)


logit_wednesday_2 <-
  glm(
    success_intervention ~ sex + devices + mean_success_loose,
    data = logistic_data_wed,
    family = binomial(link = "logit")
  )

summary(logit_wednesday_2)

logit_thursday_2 <-
  glm(
    success_intervention ~ sex + devices + mean_success_loose,
    data = logistic_data_thur,
    family = binomial(link = "logit")
  )

summary(logit_thursday_2)


logit_friday_2 <-
  glm(
    success_intervention ~ sex + devices + mean_success_loose,
    data = logistic_data_fri,
    family = binomial(link = "logit")
  )

summary(logit_friday_2)


logit_saturday_2 <-
  glm(
    success_intervention ~ sex + devices + mean_success_loose,
    data = logistic_data_sat,
    family = binomial(link = "logit")
  )

summary(logit_saturday_2)


logit_sunday_2 <-
  glm(
    success_intervention ~ sex + devices + mean_success_loose,
    data = logistic_data_sun,
    family = binomial(link = "logit")
  )

summary(logit_sunday_2)

#____________________ Model 3 construction _________________________

logit_monday_3 <-
  glm(
    success_intervention ~ log(mean_Pickups) + sex ,
    data = logistic_data_monday,
    family = binomial(link = "logit")
  )

summary(logit_monday_3)

# continue to do this for all days of the week without changing variable selection

logit_tuesday_3 <-
  glm(
    success_intervention ~ log(mean_Pickups) + sex,
    data = logistic_data_tue,
    family = binomial(link = "logit")
  )

summary(logit_tuesday_3)


logit_wednesday_3 <-
  glm(success_intervention ~ log(mean_Pickups) + sex,
    data = logistic_data_wed,
    family = binomial(link = "logit")
  )
  
summary(logit_wednesday_3)

logit_thursday_3 <-
  glm(success_intervention ~ log(mean_Pickups) +sex,
    data = logistic_data_thur,
    family = binomial(link = "logit")
  )

summary(logit_thursday_3)

logit_friday_3 <-
  glm(success_intervention ~ log(mean_Pickups)+sex,
    data = logistic_data_fri,
    family = binomial(link = "logit")
  )
summary(logit_friday_3)


logit_saturday_3 <-
  glm(success_intervention ~ log(mean_Pickups)+sex,
    data = logistic_data_sat,
    family = binomial(link = "logit")
  )

summary(logit_saturday_3)

logit_sunday_3 <-
  glm(success_intervention ~ log(mean_Pickups)+sex,
    data = logistic_data_sun,
    family = binomial(link = "logit")
  )

summary(logit_sunday_3)



