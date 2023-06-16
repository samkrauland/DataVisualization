library(tidyverse)
library(ggplot2)
library(lubridate)
library(viridis)
library(ggridges)
library(readxl)
setwd("/Users/samkrauland/R Scripts")

#################
# This project examines the relationship between cardiovascular deaths and
# different environmental factors such as air pollution, temperature,
# humidity, and heat wave days using publicly available data collected 
# between 2001 and 2007 in Valencia, Spain
#################

# First, I'll pull in the dataset
DataValencia <- read.csv("valencia0107.csv")

# store date as a date
DataValencia$date <- as.Date(DataValencia$date)

#removing NAs from pm10, o3, and no2 columns
DataValencia <- DataValencia[!is.na(DataValencia$pm10), ]
DataValencia <- DataValencia[!is.na(DataValencia$o3), ]
DataValencia <- DataValencia[!is.na(DataValencia$no2), ]

# month year and day as factors
DataValencia$month <- as.factor(DataValencia$month)
DataValencia$year <- year(DataValencia$date)
DataValencia$year <- as.factor(DataValencia$year)
DataValencia$day <- as.factor(DataValencia$day)

# check histograms
hist(DataValencia$cv)
hist(DataValencia$pm10)
hist(DataValencia$no2)
hist(DataValencia$o3)

# these all look pretty normally distributed

# a summary of the total number of cardiovascular deaths per year
AnnualCvDeaths <- DataValencia %>%
  group_by(year) %>%
  summarize(
    cv = sum(cv)
  )
AnnualCvDeaths

# a summary of the total number of cardiovascular deaths per month (2001-2007)
MonthlyCvDeaths <- DataValencia %>%
  group_by(month) %>%
  summarize(
    cv = sum(cv),
  )
MonthlyCvDeaths

# a summary of the total number of respiratory deaths per year
AnnualResDeaths <- DataValencia %>%
  group_by(year) %>%
  summarize(
    res = sum(res)
  )
AnnualResDeaths

# a summary of the total number of respiratory deaths per month (2001-2007)
MonthlyResDeaths <- DataValencia %>%
  group_by(month) %>%
  summarize(
    res = sum(res),
  )
MonthlyResDeaths

# boxplot of average cardiovascular deaths/day for each year in data set
ggplot() +
  geom_boxplot(aes(x = year, y = cv), data = DataValencia, outlier.shape = NA) +
  theme_bw() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.text = element_text(family = "Times New Roman",size  = 12), text = element_text(family = "Times New Roman", size  = 12), axis.text.x = element_text(angle = 45, vjust =1, hjust = 1)) +
  labs(x = "Year", y = "Average Cardiovascular Deaths per day") +
  ylim(0, 15)

# boxplot of average cardiovascular deaths/day for each month in data set
ggplot() +
  geom_boxplot(aes(x = month, y = cv), data = DataValencia, outlier.shape = NA) +
  theme_bw() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.text = element_text(family = "Times New Roman",size  = 12), text = element_text(family = "Times New Roman", size  = 12), axis.text.x = element_text(angle = 45, vjust =1, hjust = 1)) +
  labs(x = "Month", y = "Average Cardiovascular Deaths per day") +
  ylim(0, 15)

# lets try to make a model that looks at the relationship between cv deaths and different air pollution variables in our data set (no2, pm10, and o3)

Model <- lm(cv ~ pm10 + no2 + o3, data = DataValencia); summary(Model)
drop1(Model)

# lets predict how many cv deaths we would have per day with under current average o3, no2, and pm10 levels, and then with increasing average concentration. First make the fake data

# control (how many cv deaths should we expect on a day with average pm10, no2, and o3 levels)
df1 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3)
)

# first, increasing pm10
df2 <- data.frame(
  pm10 = mean(DataValencia$pm10) * 1.1,
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3)
)
df3 <- data.frame(
  pm10 = mean(DataValencia$pm10) * 1.2,
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3)
)

df4 <- data.frame(
  pm10 = mean(DataValencia$pm10) * 1.3,
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3)
)

# now, no2 
df5 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2) * 1.1,
  o3 = mean(DataValencia$o3)
)
df6 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2) * 1.2,
  o3 = mean(DataValencia$o3)
)
df7 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2) * 1.3,
  o3 = mean(DataValencia$o3)
)

# finally o3
df8 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3) * 1.1
)
df9 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3) * 1.2
)
df10 <- data.frame(
  pm10 = mean(DataValencia$pm10),
  no2 = mean(DataValencia$no2),
  o3 = mean(DataValencia$o3) * 1.3
)

# merge the fake data
df1 <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# prediction
Response <- predict(Model, newdata = df1, type = "response")
df1 <- cbind(df1, Response)

# lets reorganize our data frame so we can graph it better

df1 <- mutate(df1, Pollutant = "pm10")
df1 <- within(df1, Pollutant[df1$no2 > mean(DataValencia$no2)] <- "no2")
df1 <- within(df1, Pollutant[df1$o3 > mean(DataValencia$o3)] <- "o3")
df1[1,5] <- "Control"
df1 <- mutate(df1, Increase = "10%")
df1 <- within(df1, Increase[df1$no2 == mean(DataValencia$no2)*1.2 | df1$pm10 == mean(DataValencia$pm10)*1.2 | df1$o3 == mean(DataValencia$o3)*1.2] <- "20%")
df1 <- within(df1, Increase[df1$no2 == mean(DataValencia$no2)*1.3 | df1$pm10 == mean(DataValencia$pm10)*1.3 | df1$o3 == mean(DataValencia$o3)*1.3] <- "30%")
df1[1,6] <- "0"


# graph - shows how increasing concentrations of different pollutants affect cv deaths. While one is being increased the others are held constant. For example, with no2, the bottom dot is expected cv deaths with average o3, pm10, and 10% above average no2 level. Then the middle dot is expected cv deaths with average o3, pm10, and 20% above average no2 level. The top would be expected cv deaths with average o3, pm10, and 30% above average no2 level. Dot closest to control is 10% increase, so on.
ggplot() +
  geom_point(aes(x = Pollutant, y = Response), data = df1) +
  theme_bw() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
  theme(axis.text = element_text(family = "Times New Roman",size  = 12), text = element_text(family = "Times New Roman", size  = 12), axis.text.x = element_text(angle = 45, vjust =1, hjust = 1)) +
  labs(x = "Pollutant (10%, 20%, and 30% increases)", y = "Predicted Cardiovascular Deaths", fill = "Frequency", color = "Color Variable Label", linetype = "Linetype Label")

# so increasing no2 increases the predicted cv deaths, increasing o3 actually decreases the predicted cv deaths, and increasing pm10 increases the predicted cv deaths the most

# lets make another model that looks at the relationship between temperature and cardiovascular deaths

Model2 <- lm(cv ~ tmax + tmin + hw, data = DataValencia); summary(Model2)
drop1(Model2)


