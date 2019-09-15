# Andrew Lee
# Milestone 2
# GT 4801 Fall 2019

# install.packages("tidyverse") # a set of data science tools including dplyr, tidyr and stringr
# install.packages("skimr") # a package to facilitate data summaries
# install.packages("Hmisc")
# install.packages("GGally")

library(tidyverse)
library(skimr)
library(Hmisc)
library(ggplot2)
library(GGally)

library(readxl)
jobData <- read_excel("residence-data-national-level.xlsx", sheet = "percentages")
# View(jobData)

# Remove any NA rows
jobDataClean <- filter(jobData, !is.na(Sex))
View(jobDataClean)

# Create tables based on sex
# jobDataTotal <- filter(jobDataClean, Sex == "Total")
# jobDataMale <- filter(jobDataClean, Sex == "Male")
# jobDataFemale <- filter(jobDataClean, Sex == "Female")

#jobDataTotal %>%
#ggplot(aes('White non-Hispanic')) + geom_histogram()

# ggparcoord(jobDataTotal, columns=5:14, groupColumn=4)

jobDataTotalFormatted <- jobDataClean %>% 
  filter(Sex == "Total") %>%
  gather('White non-Hispanic', 'Hispanic', 'Black non-Hispanic', 'AIAN non-Hispanic', 'Asian non-Hispanic', 'NHOPI non-Hispanic', 'Black & White non-Hispanic', 'AIAN & White non-Hispanic', 'AIAN & Black non-Hispanic', 'Asian & White non-Hispanic', 'Balance 2+ Races, non-Hispanic', key = "Race", value = "Proportion")%>%
  select (Occupation, Race, Proportion)

ggplot(jobDataTotalFormatted, aes(Race, Proportion)) +  
  geom_line(aes(group = Race), color = "grey50") +  
  geom_point(aes(color = Race))

jobDataTotalFormatted %>%
  filter(Occupation =="Chief Executives (001)")%>%
  ggplot(aes(x = Race, y = Proportion, fill = Race)) + geom_bar(stat="identity") + ggtitle("Chief Executive by Race") + theme(axis.title.x=element_blank(),
                                                                                      axis.text.x=element_blank(),
                                                                                      axis.ticks.x=element_blank())
jobDataTotalFormatted %>%
  filter(Occupation =="Unemployed, No Civilian Work Experience Since 1995 (980-983, 992)")%>%
  ggplot(aes(x = Race, y = Proportion, fill = Race)) + geom_bar(stat="identity")+ +ggtitle("Unemployment by Race") + theme(axis.title.x=element_blank(),
                                                                                        axis.text.x=element_blank(),
                                                                                        axis.ticks.x=element_blank())

jobDataTotalFormatted %>%
  filter(Occupation =="Total Civilian Labor Force")%>%
  ggplot(aes(x = Race, y = Proportion, fill = Race)) + geom_bar(stat="identity") + ggtitle("Overall Makeup of Total Civilian Force") + theme(axis.title.x=element_blank(),
                                                                                        axis.text.x=element_blank(),
                                                                                        axis.ticks.x=element_blank())

model <- lm(Proportion ~ factor(Race), jobDataTotalFormatted, method="qr", model = FALSE, x=FALSE, y=FALSE)
summary(model)
