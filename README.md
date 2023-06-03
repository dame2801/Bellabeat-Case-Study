# Bellabeat-Case-Study
Google Cap Stone
# Loading required packages
install.packages('tidyverse')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('janitor')

library(tidyverse)
library(lubridate)
library(ggplot2)
library(janitor)
library(tidyr)
library(dplyr)

activity <- read.csv("/cloud/project/dailyActivity_merged.csv")
calories <- read.csv("/cloud/project/dailyCalories_merged.csv")
intensities <- read.csv("/cloud/project/dailyIntensities_merged.csv")
sleep <- read.csv("/cloud/project/sleepDay_merged.csv")
weight <- read.csv("/cloud/project/weightLogInfo_merged.csv")

head(activity)
head(calories)
head(intensities)
head(sleep)
head(weight)

# Converting the Data type

# intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")
# calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")
# activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
# sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
# weight
weight$Date=as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$date <- format(sleep$Date, format = "%m/%d/%y")

n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# We will only summarize the data that we want to look at and exclude the rest namely date, time, Id, etc

#activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# explore number of active minutes per category
activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# calories
calories %>%
  select(Calories) %>%
  summary()
# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()

merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)

ggplot(data =activity) +
  geom_point(mapping = aes(x = TotalSteps, y = Calories,alpha= TotalSteps), color = 'purple') +
  geom_smooth(mapping = aes(x = TotalSteps, y = Calories), color = 'black') +
  labs(title="Total Daily Steps Vs Calories Burned") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

ggplot(data =sleep) +
  geom_point(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed), color = 'purple') +
  geom_smooth(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed), color = 'black') +
  labs(title="Total time in bed Vs Total minutes asleep") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

sleep_new <- sleep %>%
  group_by(date) %>%
  drop_na() %>%
  summarise(Average_Total_Sleep = mean(TotalMinutesAsleep))

ggplot(data=sleep_new, aes(x=date, y=Average_Total_Sleep)) + geom_histogram(stat = "identity", fill='#85e0e0') +
  geom_hline(yintercept = 480) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Sleep")+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new) +
  geom_col(mapping = aes(x=time, y =mean_total_int, fill = mean_total_int)) + 
  labs(title = "Most Active Hours", x="", y="") + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
