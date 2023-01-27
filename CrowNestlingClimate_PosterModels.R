#SCRIPT FOR POSTER MODELS

#Follow this link if you need directions for installing packages: https://derekogle.com/IFAR/supplements/installations/InstallPackagesRStudio.html
library(tidyverse) #package required for making plots



#####FUNCTIONS#####

#Function for making forest plots
forest.plot <- function(mdl){
  mdl.df <- tidy(mdl, quick = FALSE)
  ci <- as_tibble(confint(mdl))
  mdl.df$ci.lower <- ci$`2.5 %`
  mdl.df$ci.upper <- ci$`97.5 %`
  mdl.df <- mdl.df[-1,]

  ggplot(data = mdl.df)+
    geom_pointrange(aes(x=term, y=estimate, ymin=ci.lower, ymax=ci.upper), size=1.3, shape=20)+
    coord_flip()+
    geom_hline(yintercept = 0, color="red",size=1)+
    ggtitle("Parameter estimates")+
    theme(plot.title = element_text(size = 22),
          axis.text.x = element_text(size = 18),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 18),
          axis.title.y = element_blank(),
          plot.margin = margin(15,15,15,15))

}

#Function to make tables of model outputs
mdl.tbl <- function(mdl){
  mdl.df <- tidy(mdl, quick = FALSE)
  ci <- as_tibble(confint(mdl))
  mdl.df$ci.lower <- ci$`2.5 %`
  mdl.df$ci.upper <- ci$`97.5 %`
  mdl.df <- mdl.df[-1,]
  print(mdl.df)
}



#####DATAFRAMES#####

#Read in the data for both sexes

#This line of code will open a file explorer window.
#Use that window to select the data file (CrowNestlingClimate_BothSexesClean.csv) that you downloaded.
df <- read.csv(file.choose(), header=TRUE)

#Make AllSex a factor
df$AllSex <- as.factor(df$AllSex)

#summarize the dataframe to make sure everything looks good
summary(df)

#Subset df to create a female dataframe
F.df <- subset(df, AllSex=="F")

#Subset df to create a male dataframe
M.df <- subset(df, AllSex=="M")


#####PLOTTING METRICS#####

#You can use this code to make a scatterplot of the relationship between two metrics.

df %>% #Change df to F.df or M.df to look at one sex at a time
  ggplot(aes(x=GDDSum12_22, y=BillDepth, color=AllSex))+ #You can change the metrics following "x=" and "y=" to view different relationships
  geom_point(shape = 16, size = 2)+
  scale_color_manual(name = "Sex", values = c("#009E73","#E69F00"), labels = c("Female","Male"))+
  geom_smooth()



#####FEMALE MODELS#####

# Upper Bill Model
F.UB2.mdl <- lm(UB ~ GDDSum12_22*PrecipSum12_22 + Weight, data = F.df)
summary(F.UB2.mdl)
# Precipitation has a negative effect, mainly when it's cold


# Bill Nares to Tip Model
F.BNT2.mdl <- lm(BillNT ~ GDDSum12_22*PrecipSum12_22 + Weight, data = F.df)
summary(F.BNT2.mdl)
# Precipitation has a negative effect, mainly when it's cold


# Upper Bill Surface Area Model
F.UBS2.mdl <- lm(UBS ~ GDDSum12_22*PrecipSum12_22 + Weight, data = F.df)
summary(F.UBS2.mdl)
# Precipitation has a negative effect, mainly when it's cold


# Bill Depth Model
F.BD2.mdl <- lm(BillDepth ~ GDDSum12_22*PrecipSum12_22 + Weight, data = F.df)
summary(F.BD2.mdl)
# Temperature is the only important climate variable


# Tarsus Model
F.Tarsus2.mdl <- lm(Tarsus ~ GDDSum12_22*PrecipSum12_22 + Weight, data = F.df)
summary(F.Tarsus2.mdl)
# Climate variables have no effect



#####MALE MODELS#####

# Upper Bill Model
M.UB2.mdl <- lm(UB ~ GDDSum12_22*PrecipSum12_22 + Weight, data = M.df)
summary(M.UB2.mdl)
# Precipitation has a negative effect, mainly when it's cold


# Bill Nares to Tip Model
M.BNT2.mdl <- lm(BillNT ~ GDDSum12_22*PrecipSum12_22 + Weight, data = M.df)
summary(M.BNT2.mdl)
# Precipitation has a negative effect, mainly when it's cold


# Upper Bill Surface Area Model
M.UBS2.mdl <- lm(UBS ~ GDDSum12_22*PrecipSum12_22 + Weight, data = M.df)
summary(M.UBS2.mdl)
# Precipitation has a negative effect, mainly when it's cold


# Bill Depth Model
M.BD2.mdl <- lm(BillDepth ~ GDDSum12_22*PrecipSum12_22 + Weight, data = M.df)
summary(M.BD2.mdl)
# Temperature is the only important climate variable


# Tarsus Model
M.Tarsus2.mdl <- lm(Tarsus ~ GDDSum12_22*PrecipSum12_22 + Weight, data = M.df)
summary(M.Tarsus2.mdl)
# Climate variables have no effect



