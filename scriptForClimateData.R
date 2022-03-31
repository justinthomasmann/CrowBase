library(tidyverse)
library(lme4)
library(plyr)
theme_set(theme_classic())

#Don't forget to set your working directory! Tell R where the data are on
#your computer. Session -> Set working directory -> Choose directory...

#Read in NOAA Temperature Data 2007-2022
df <- read.csv("noaaData_2007-2022.csv",h=T)

#Variables in the dataset:
#t2m = temperature in C
#dpt2m = dew point
#ws = wind speed
#wd = wind direction in 360 degrees (N = 0 degrees)
#slp = sea level pressure

#Create a new subset dataframe including only March, April and May
marAprMay.df <- df %>% filter(between(month, 3, 5))

#Create an observation (obs) column that is a numerical sequence the length of
#the dataframe
marAprMay.df$obs<-1:nrow(marAprMay.df)

#Boxplots of temperatures for March, April and May of each year. *Note here I'm
#using 'obs' on the x-axis because R doesn't like the way the 'date' variable
#is formatted. I'm still working on this.
marAprMay.df %>%
  ggplot(aes(x=obs, y=t2m, color=as.factor(year), fill=as.factor(month)))+
  geom_boxplot()







