library(tidyverse)
library(lme4)
library(plyr)
library(plotly)
library(wesanderson)
library(climate)
theme_set(theme_classic())

df <- data.frame(read.csv("NestlingData_2002-2006.csv", h=T)) #Get our data into R

ageFilter.df <- df %>% filter(between(Age, 23, 30)) #Create a new dataframe with an age filter (only including ages 23-30)

#Plot Tarsus by Weight
tarsusByWeight <- ageFilter.df %>%
  ggplot(aes(x=Weight, y=Tarsus, color=Sex))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_color_manual(values=c("Green", "Red"))

tarsusByWeight

#Plot Tarsus by Age
tarsusByAge <- ageFilter.df %>%
  ggplot(aes(x=Age, y=Tarsus, color=Sex))+
  geom_point()

tarsusByAge

#Run a t test to see if the mean tarsus length differs between sexes
t.test(ageFilter.df$Tarsus[ageFilter.df$Sex=="M"], ageFilter.df$Tarsus[ageFilter.df$Sex=="F"])


#Plot Tarsus by Weight
headByWeight <- ageFilter.df %>%
  ggplot(aes(x=Weight, y=Head, color=Sex))+
  geom_boxplot()

headByWeight
