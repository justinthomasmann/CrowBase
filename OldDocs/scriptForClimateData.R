library(tidyverse)
library(lme4)
library(plyr)
theme_set(theme_classic())

#Don't forget to set your working directory! Tell R where the data are on
#your computer. Session -> Set working directory -> Choose directory...

#Read in NOAA Temperature Data 2007-2022
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

#Create a new subset data frame including only March, April and May
marAprMay.df <- df %>% filter(between(month, 3, 5))





#Box plots of temperatures for March, April and May of each year. *Note here I'm
#using 'obs' on the x-axis because R doesn't like the way the 'date' variable
#is formatted. I'm still working on this.
marAprMay.df %>%
  ggplot(aes(x=obs, y=t2m, color=as.factor(year), fill=as.factor(month)))+
  geom_boxplot()

dailyMeanTemp <- marAprMay.df %>%
  group_by(yearMonth, day) %>%
  dplyr::summarise(mean = mean(t2m, na.rm = TRUE))

dailyMeanTemp %>%
  ggplot(aes(x=))





