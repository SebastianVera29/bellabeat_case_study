# Process Phase
# Importing packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggpubr")
install.packages("ggplot2")

# Loading packages

library(tidyverse)
library(dplyr)
library(here)
library(skimr)
library(lubridate)
library(janitor)
library(ggpubr)
library(ggplot2)

# Importing datasets

dailyActivity <- read_csv("C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data 4.12.16-5.12.16 (raw data)\\dailyActivity_merged.csv")
hourlyCalories <- read.csv("C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data 4.12.16-5.12.16 (raw data)\\hourlyCalories_merged.csv")
hourlySteps <- read.csv("C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data 4.12.16-5.12.16 (raw data)\\hourlySteps_merged.csv")
sleepDay <- read.csv("C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data 4.12.16-5.12.16 (raw data)\\sleepDay_merged.csv")

# Knowing our datasets

colnames(dailyActivity)     # Column names
head(dailyActivity)         # Preview of the data frame
str(dailyActivity)          # Structure of the data frame

colnames(hourlyCalories)
head(hourlyCalories)
str(hourlyCalories)

colnames(hourlySteps)
head(hourlySteps)
str(hourlySteps)

colnames(sleepDay)
head(sleepDay)
str(sleepDay)

# Determining how many users my dataframes have

count(unique(dailyActivity[1]))
count(unique(hourlyCalories[1]))
n_unique(hourlySteps[[1]])
n_unique(sleepDay$Id)

# Checking for duplicates

sum(dailyActivity %>% duplicated())
sum(hourlyCalories %>% duplicated())
sum(duplicated(hourlySteps))
sum(duplicated(sleepDay))

# Removing duplicate rows

sleepDay <- sleepDay %>% unique()

# Cleaning column names

dailyActivity <- rename_with(dailyActivity, tolower)
hourlyCalories <- rename_with(hourlyCalories, tolower)
hourlySteps <- rename_with(hourlySteps, tolower)
sleepDay <- rename_with(sleepDay,tolower)

# Ensuring the consistency of my column names

dailyActivity <- clean_names(dailyActivity)
hourlyCalories <- clean_names(hourlyCalories)
hourlySteps <- clean_names(hourlySteps)
sleepDay <- clean_names(sleepDay)

# Formatting date column

dailyActivity[[2]] <- as.Date(dailyActivity[[2]], "%m/%d/%Y")
hourlyCalories[[2]] <- strptime(hourlyCalories[[2]],
                                   format = "%m/%d/%Y %I:%M:%S %p")
hourlySteps[[2]] <- strptime(hourlySteps[[2]],
                                   format = "%m/%d/%Y %I:%M:%S %p")
sleepDay[[2]] <- as.Date(sleepDay[[2]], "%m/%d/%Y")

# Optional: Saving dataframes into csv files for further SQL analysis

write.csv(dailyActivity, "C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data (cleaned)\\dailyActivity_cleaned.csv", 
          row.names = FALSE)
write.csv(hourlyCalories, "C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data (cleaned)\\hourlyCalories_cleaned.csv", 
          row.names = FALSE)
write.csv(hourlySteps, "C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data (cleaned)\\hourlySteps_cleaned.csv", 
          row.names = FALSE)
write.csv(sleepDay, "C:\\Users\\YAKU\\Documents\\Documentos de Sebastian\\Google DA Certificate\\00. Capstone Project\\01. Bellabeat Case Study\\Fitabase Data (cleaned)\\sleepDay_cleaned.csv", 
          row.names = FALSE)

# Merging dailyActivity and sleepDay tables

dailyActivitySleep <- merge(dailyActivity, sleepDay, 
                            by.x = c("id", "activitydate"),
                            by.y = c("id", "sleepday"))
hourlyCaloriesSteps <- merge(hourlyCalories, hourlySteps,
                             by.x = c("id", "activityhour"),
                             by.y = c("id", "activityhour"))

# Analysis Phase

# Here is going to be Table 1

# Analyzing the correlation between Very active distance and very active minutes

minutes_calories_cor <- cor(dailyActivity$veryactiveminutes,
                            dailyActivity$calories)

# According to Table 1, there is a strong correlation between Very active
# minutes and Calories burned. This correlation will be presented by the 
# following scatter plot

# Figure 1
ggplot(dailyActivity, aes(x = veryactiveminutes, y = calories)) + 
  geom_point() + 
  geom_smooth(color = "red") +
  labs(title = "Comparison between Very Active Minutes\nand Calories",
       subtitle = paste0("There is a strong correlation (r = ", 
                         round(minutes_calories_cor, digits = 3), ")"))

# From Figure 1, we can conclude that as long as very active minutes users
# have, the more calories they burn. So, Fitbit can promote challenges to
# increase these minutes during the day. 
# In addition, we can see that most users tend to have 50 or less very active 
# minutes. So let's analyze if there is a relationship between these minutes
# and the very active distances.

minutes_distance_cor <- cor(dailyActivity$veryactiveminutes,
                            dailyActivity$veryactivedistance)

# Figure 2
ggplot(dailyActivity, aes(x = veryactivedistance, y = veryactiveminutes)) + 
  geom_jitter() + 
  geom_smooth(color = "purple")+
  labs(title = "Comparison between Very Active Minutes\nand distance",
       subtitle = paste0("There is a very strong correlation (r = ", 
                         round(minutes_distance_cor, digits = 3),")"))

# As we can see, there is a strong correlation between the very active
# distance and minutes columns. Also, we can observe that most users have 5 km
# or less very active distance values.
# This insight is represented in following histogram

# Figure 3
ggplot(dailyActivity) + 
  geom_histogram(mapping = aes(x = veryactivedistance, fill = "#f49072"), 
                                 bins = 10) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Preferences of Very active distance periods") +
  theme(legend.position = "none")

# From Figure 3, we can conclude that most users tend to have a very active
# distance from 2 km or less. That insight could be related with working out
# in gyms or activities that don't require to transport very far.

# Analyzing if there is a correlation between steps and calories burned hourly

calories_steps_cor <- cor(hourlyCaloriesSteps$calories,
                          hourlyCaloriesSteps$steptotal)
# Figure 4
ggplot(hourlyCaloriesSteps, aes(x = steptotal, y = calories)) + 
  geom_jitter() +
  geom_smooth(color = "purple") + 
  labs(title = "Correlation between steps and calories burned hourly",
       subtitle = paste0("There is a very strong correlation (r = ", 
                         round(calories_steps_cor, digits = 3),")"))

# There is a strong correlation between steps and calories burned. So, as much
# an user walk, more calories burn.

hourlyCaloriesSteps <- hourlyCaloriesSteps %>% 
  separate(activityhour, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date))

# Calories during the day

# Figure 5
ggarrange(                                                                           
hourlyCaloriesSteps %>% group_by(time) %>% 
  summarize(average_calories = mean(calories)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = time, y = average_calories,
                         fill = average_calories)) +
  labs(title = "Average calories burned during the day") +
  scale_fill_gradient(low = "#72f4e9", high = "#f49072") +
  theme(axis.text.x = element_text(angle = 90)),

# Steps during the day

hourlyCaloriesSteps %>% group_by(time) %>% 
  summarize(average_steps = mean(steptotal)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = time, y = average_steps,
                         fill = average_steps)) +
  labs(title = "Average steps made during the day") +
  scale_fill_gradient(low = "#72f4e9", high = "#f49072") +
  theme(axis.text.x = element_text(angle = 90))
)

#Adding the day of the week

hourlyCaloriesSteps_v2 <-
hourlyCaloriesSteps %>% mutate(weekday = weekdays(date)) %>% 
  select(id, date, weekday, calories, steptotal) %>%
  mutate(weekday = case_when(
    weekday == "lunes" ~ "monday", #due my local system, I'll change day names
    weekday == "martes" ~ "tuesday",
    weekday == "miércoles" ~ "wednesday",
    weekday == "jueves" ~ "thursday",
    weekday == "viernes" ~ "friday",
    weekday == "sábado" ~ "saturday",
    weekday == "domingo" ~ "sunday"
  ))

# Reordering factor levels by day of the week

hourlyCaloriesSteps_v2$weekday <- ordered(
  hourlyCaloriesSteps_v2$weekday, levels = c(
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
  )
)

# Calculating average steps and calories burned by day of the week

# Figure 6
ggarrange(
hourlyCaloriesSteps_v2 %>% group_by(id, date, weekday) %>% 
  summarise(average_steps = sum(steptotal)) %>% group_by(weekday) %>% 
  summarise(average_steps = mean(average_steps)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = weekday, y = average_steps, fill = "#f49072")) +
  labs(title = "Average steps by day of the week") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45)),

hourlyCaloriesSteps_v2 %>% group_by(id, date, weekday) %>% 
  summarise(average_calories = sum(calories)) %>% group_by(weekday) %>% 
  summarise(average_calories = mean(average_calories)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = weekday, y = average_calories, fill = "#f49072")) +
  labs(title = "Average calories burned by day of the week") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45))
)

# Determining the day of the week when users sleep the best

dailyActivitySleep_v2 <- 
dailyActivitySleep %>% mutate(weekday = weekdays(activitydate)) %>% 
  select(id, activitydate, weekday, totalminutesasleep, totaltimeinbed) %>%
  mutate(weekday = case_when(
    weekday == "lunes" ~ "monday", #due my local system, I'll change day names
    weekday == "martes" ~ "tuesday",
    weekday == "miércoles" ~ "wednesday",
    weekday == "jueves" ~ "thursday",
    weekday == "viernes" ~ "friday",
    weekday == "sábado" ~ "saturday",
    weekday == "domingo" ~ "sunday"
    )
)

# Reordering factor levels by day of the week

dailyActivitySleep_v2$weekday <- ordered(
  dailyActivitySleep_v2$weekday, levels = c(
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"
  )
)

dailyActivitySleep_v2 %>% group_by(weekday) %>% 
  summarise(average_sleep_time = mean(totalminutesasleep)/60) %>% 
  ggplot() +
  geom_col(mapping = aes(x = weekday, y = average_sleep_time, 
                         fill = "#f49072", label = average_sleep_time)) + 
  geom_text(mapping = aes(x = weekday, y = average_sleep_time, 
                          label = round(average_sleep_time,2)), vjust = -0.2) +
    labs(title = "Average time asleep per day") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45)) +
  xlab("Week of the day") +
  ylab("Average sleep time (in hours)") + 
  ylim(0,8)