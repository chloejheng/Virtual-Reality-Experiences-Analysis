#create document
# Pei-Yu Jheng, 11/12/23, ALY6010 Week 2

#the environment reset code 
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

#Load any standard packages might need
library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(pacman)

#Import the file into R using read.csv( ) function
#Virtual Reality Experiences
data <- read.csv("C:\\Users\\user\\Desktop\\2023 Fall\\ALY6010\\Module 2\\Project 2\\VR.data.csv")
class(data) #The class() function is used to show the object's category
glimpse(data) 

#Prepare data.frame for analysis
library("janitor")
data <- clean_names(data) #turn variables name to lower case
glimpse(data) #this function is used to see every column in a data frame.

#Remove any rows that contain NAs
df <- data %>% drop_na()

#Correcting data types
str(df)
#The data types are all correct, so no need to change

#Removing columns
df = subset(df, select = -c(user_id))

#Reorganizing the data
#make the duration column more percise
df$duration <- floor(df$duration * 10) / 10
colnames(df)[4] ="duration_mins"

#Determine descriptive statistics for interesting variables
#max, min, mean, median, mode. standard deviation, variance, and range.
summary(df) #returns the minimum, maximum, mean, median, and 1st and 3rd quartiles for a numerical vector.
#motion_sickness's mean is higher than 5, immersion level's mean is lower than 3

#count most frequent age in the dataset
freq_table <- table(df$age)
as.numeric(names(freq_table)[which.max(freq_table)])

range(df$age)

gender_character <- unique(df$gender)
gender_vector <- paste(gender_character, collapse = ", ")
cat(gender_vector)

headset_character <- unique(df$vr_headset)
headset_vector <- paste(headset_character, collapse = ", ")
cat(headset_vector)

by(df, df$vr_headset, summary) #print summary by group
#we can compare three device in vr_headset column

range(df$duration_mins)
sd(df$duration_mins)

range(df$motion_sickness)
sd(df$motion_sickness)

range(df$immersion_level)
sd(df$immersion_level)

#Part I – Produce descriptive statistics and three-line tables
#Load summarytools package
library(summarytools)

#Generate descriptive statistics table
descr <- descr(df)

descr

# Use describe() to obtain descriptive statistics
library(psych)

description <- describe(df)

# Print the result
print(description)

# Generate descriptive statistics tables by group
grouped_stats <- df %>%
  group_by(df$vr_headset) %>%
  summarise_all(list(mean = mean, sd = sd, min = min, max = max))

#Alternatively, use summarytools package for more detailed tables
grouped_stats_summarytools <- dfSummary(df)
print(grouped_stats_summarytools)

#three-line table
# Load packages
library(knitr)
library(kableExtra)
vr_ratings <- data.frame(
  grouped_stats$`df$vr_headset`,
  grouped_stats$motion_sickness_mean,
  grouped_stats$immersion_level_mean
)
formatted_table <- vr_ratings %>%
  kable("html") %>%
  kable_styling()

# Print the table
print(formatted_table)

#Part II – Data Analysis and Visualization

#Produce visualizations from the raw data
library(ggplot2) 

#age distribution
ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")+
  theme_minimal()

#gender distribution
gender_table <- table(df$gender)
gender_data <- as.data.frame(gender_table)
names(gender_data) <- c("Gender", "Count")

ggplot(gender_data, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Bar Chart",
       x = "Gender",
       y = "Number of Experiencers")

#jitter chart (1) of the relation between age and motion sickness rating
average_ratings <- df %>%
  group_by(age) %>%
  summarise(motion_sickness = mean(motion_sickness))

ggplot(average_ratings, aes(x = age, y = motion_sickness)) +
  geom_jitter() +
  labs(title = "Relation between Age & Motion Sickness Rating",
       x = "Age",
       y = "Motion Sickness Rating")

#jitter chart (2) of the relation between VR headset device and motion sickness rating
headset_count <- df %>% group_by(vr_headset) %>% tally(motion_sickness)
headset_df <- as.data.frame(headset_count)
names(headset_df) <- c("vr_headset", "Count")

ggplot(headset_df, aes(x = vr_headset, y = Count)) +
  geom_jitter() +
  labs(title = "Relation Between VR Headset & Motion Sickness", x = "Headset", y = "Count")+
  theme(plot.title = element_text(hjust = 3))
  
#jitter chart (3) of the relation between VR headset device and immersion level rating
imm_count <- df %>% group_by(vr_headset) %>% tally(immersion_level)
imm_df <- as.data.frame(imm_count)
names(imm_df) <- c("vr_headset", "Count")

ggplot(imm_df, aes(x = vr_headset, y = Count)) +
  geom_jitter() +
  labs(title = "Relation of VR Headset & Immersion Level Rating",
       x = "Category",
       y = "Value")+
  theme(plot.title = element_text(hjust = 3))

#box plot of device and Motion Sickness
ggplot(df, aes(x = vr_headset, y = motion_sickness, fill = vr_headset)) +
  geom_boxplot() +
  labs(title = "VR Motion Sickness Boxplot",
       x = "VR Headset Device",
       y = "Motion Sickness") +
  theme_minimal()
#There are no outliers in three device

#box plot of device and Immersion Level
ggplot(df, aes(x = vr_headset, y = immersion_level, fill = vr_headset)) +
  geom_boxplot() +
  labs(title = "VR Immersion Level Boxplot",
       x = "VR Headset Device",
       y = "Immersion Level") +
  theme_minimal()
#Overall, no difference between device.

#Scatter Plot: whether longer VR sessions lead to increased satisfaction
#or if there's an optimal duration after which satisfaction levels plateau or decrease
ggplot(df, aes(x = duration_mins, y = immersion_level)) +
  geom_point(size = 3) +
  labs(title = "VR Experience Scatter Plot",
       x = "Experience Time (minutes)",
       y = "User Rating") +
  theme_minimal()
 

