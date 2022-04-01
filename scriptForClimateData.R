library(tidyverse)
library(lme4)
library(plyr)
theme_set(theme_classic())

#Don't forget to set your working directory! Tell R where the data are on
#your computer. Session -> Set working directory -> Choose directory...

#Read in NOAA Temperature Data 2006-2022
df <- read.csv("noaaData_2007-2022.csv",h=T)

#Create column combining year and month for summary stats
df$yearMonth <- paste(df$year, df$month, sep = "_")

#Create an observation (obs) column that is a numerical sequence the length of
#the data frame
df$obs<-1:nrow(df)

#Variables in the data set:
#t2m = temperature in C
#dpt2m = dew point
#ws = wind speed
#wd = wind direction in 360 degrees (N = 0 degrees)
#slp = sea level pressure

#Data frame including only March, April and May
marAprMay.df <- df %>% filter(between(month, 3, 5) & between(year, 2006, 2021))
summary(marAprMay.df)


#Box plots of temperatures for March, April and May of each year. *Note here I'm
#using 'obs' on the x-axis because R doesn't like the way the 'date' variable
#is formatted. I'm still working on this.
marAprMay.df %>%
  ggplot(aes(x=obs, y=t2m, color=as.factor(year), fill=as.factor(month)))+
  geom_boxplot()

#Data frame with daily mean, maximum, minimum and standard deviation
dailyTemp.df <- marAprMay.df %>%
  group_by(yearMonth, day) %>%
  dplyr::summarise(Mean = mean(t2m, na.rm = TRUE),
                   Max = max(t2m, na.rm = TRUE),
                   Min = min(t2m, na.rm = TRUE),
                   Stdev = sd(t2m, na.rm = TRUE))

#Create column calculating temperature range
dailyTemp.df$Range <- dailyTemp.df$Max - dailyTemp.df$Min

#Add back Year column
dailyTemp.df$Year <- rep(c(2006:2021),each=92)

#Add back Month column
dailyTemp.df <- dailyTemp.df %>%
  mutate(Month = case_when(
    endsWith(yearMonth, "3") ~ "3",
    endsWith(yearMonth, "4") ~ "4",
    endsWith(yearMonth, "5") ~ "5"))

#Add back obs column
dailyTemp.df$obs<-1:nrow(dailyTemp.df)

dailyTemp.df %>%
  ggplot(aes(x=yearMonth, y=Mean, fill=Month))+
  geom_boxplot()+
   theme(axis.text.x = element_blank())+
   ylab("Average Temperature (C)")+
   xlab("2006-2021")

#March-only data frame
marchDaily.df <- subset(dailyTemp.df, dailyTemp.df$Month == 3)

marchDaily.df %>%
  ggplot(aes(x=as.factor(Year), y=Mean))+
  geom_boxplot()

#April-only data frame
aprilDaily.df <- subset(dailyTemp.df, dailyTemp.df$Month == 4)

aprilDaily.df %>%
  ggplot(aes(x=as.factor(Year), y=Mean))+
  geom_boxplot()

#May-only data frame
mayDaily.df <- subset(dailyTemp.df, dailyTemp.df$Month == 5)

mayDaily.df %>%
  ggplot(aes(x=as.factor(Year), y=Mean))+
  geom_boxplot()

