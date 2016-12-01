aud1 <- read.csv(file = "/Users/johnlloyd/GitHub/Mansfield.Phenology/Data/Vermont_256.csv", header = T, sep = ";")
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(splines)
library(MASS)
#Data manipulation and summary
##First need to summarize instances of BITH/Other detections, 
##while adding zeros for missing levels of each factor. That is,
##if no BITH was detected on a day, add a row with a "0" for
##BITH.
summary1<- aud1 %>%
  group_by(Month,Day, Year,Class) %>%
  summarize(instances = n()) %>%
  complete(Month,Day,Class, fill = list(instances = 0))
View(summary1)

summary1 <- mutate(summary1, Date = paste(Month,Day,Year, sep = "/"))
summary1 <- mutate(summary1, Date = as.Date(Date, format = "%m/%d/%Y"))

##Make figures showing number of instances of BITH vocalizations
summary1 %>%
  filter(Class == "BITH") %>%
  ggplot(., aes(x = Date, y = instances)) + geom_line() + ylab("No. of Bicknell's Thrush vocalizations") + 
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 305,
                                   vjust = 0.5),
        axis.title=element_text(size=18,face="bold"))

summary1 %>%
  filter(Class == "OTHER") %>%
  ggplot(., aes(x = Date, y = instances)) + geom_line() + ylab("No. of vocalizations by other bird species")

cp.bp <- read.csv(file = "/Users/johnlloyd/GitHub/Mansfield.Phenology/Data/CB_BP.csv", header = T, sep = ",")

##Create a figure of CP development among BITH
cp.bp %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  group_by(Species, Date, CP) %>%
  mutate(Condition = ifelse(CP == 0, "NB",ifelse(CP == 1, "NB","Breeding")))%>%
  group_by(Species, Date, Condition)%>%
  summarize(N = n()) %>%
  mutate(Percentage = N/sum(N)) %>%
  complete(Species, Date, Condition, fill = list(N = 0,Percentage = 0)) %>%
  filter(Species == "BITH", Condition == "Breeding") %>%
  ggplot(., aes(x = Date, y = Percentage)) + geom_point() + stat_smooth(method="glm",
                                                                        method.args = list(family="binomial"), 
                                                                        formula = y ~ ns(x, 3), level = 0.75) + 
  ylab("Percentage of captured male Bicknell's\nThrush in breeding condition") + 
  scale_x_date(breaks = date_breaks("4 days")) + theme(axis.text.y = element_text(size = 12),
                                                       axis.text.x = element_text(size = 12, angle = 305,
                                                                                  vjust = 0.5),
                                                       axis.title=element_text(size=18,face="bold"))

##Create a figure of BP development among BITH
cp.bp %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  group_by(Species, Date, BP) %>%
  mutate(Condition = ifelse(BP == 0, "NB","Breeding"))%>%
  group_by(Species, Date, Condition)%>%
  summarize(N = n()) %>%
  mutate(Percentage = N/sum(N)) %>%
  complete(Species, Date, Condition, fill = list(N = 0,Percentage = 0)) %>%
  filter(Species == "BITH", Condition == "Breeding") %>%
  ggplot(., aes(x = Date, y = Percentage)) + geom_point() + stat_smooth(method="glm",
                                                                        method.args = list(family="binomial"), 
                                                                        formula = y ~ ns(x, 3), level = 0.75) + 
  ylab("Percentage of captured female Bicknell's\nThrush in breeding condition") + 
  scale_x_date(breaks = date_breaks("4 days")) + theme(axis.text.y = element_text(size = 12),
                                                       axis.text.x = element_text(size = 12, angle = 305,
                                                                                  vjust = 0.5),
                                                       axis.title=element_text(size=18,face="bold"))

##Create a figure of CP development among WTSP
cp.bp %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  group_by(Species, Date, CP) %>%
  mutate(Condition = ifelse(CP == 0, "NB",ifelse(CP == 1, "NB","Breeding")))%>%
  group_by(Species, Date, Condition)%>%
  summarize(N = n()) %>%
  mutate(Percentage = N/sum(N)) %>%
  complete(Species, Date, Condition, fill = list(N = 0,Percentage = 0)) %>%
  filter(Species == "WTSP", Condition == "Breeding") %>%
  ggplot(., aes(x = Date, y = Percentage)) + geom_point() + stat_smooth(method="glm",
                                                                        method.args = list(family="binomial"), 
                                                                        formula = y ~ ns(x, 3), level = 0.75) + 
  ylab("Percentage of captured male White-throated\nSparrows in breeding condition") + 
  scale_x_date(breaks = date_breaks("4 days")) + theme(axis.text.y = element_text(size = 12),
                                                       axis.text.x = element_text(size = 12, angle = 305,
                                                                                  vjust = 0.5),
                                                       axis.title=element_text(size=18,face="bold"))

##Create a figure of BP development among WTSP
cp.bp %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))%>%
  group_by(Species, Date, BP) %>%
  mutate(Condition = ifelse(BP == 0, "NB","Breeding"))%>%
  group_by(Species, Date, Condition)%>%
  summarize(N = n()) %>%
  mutate(Percentage = N/sum(N)) %>%
  complete(Species, Date, Condition, fill = list(N = 0,Percentage = 0)) %>%
  filter(Species == "WTSP", Condition == "Breeding") %>%
  ggplot(., aes(x = Date, y = Percentage)) + geom_point() + stat_smooth(method="glm",
                                                                        method.args = list(family="binomial"), 
                                                                        formula = y ~ ns(x, 3), level = 0.75) + 
  ylab("Percentage of captured female White-throated\nSparrows in breeding condition") + 
  scale_x_date(breaks = date_breaks("4 days")) + theme(axis.text.y = element_text(size = 12),
                                                       axis.text.x = element_text(size = 12, angle = 305,
                                                                                  vjust = 0.5),
                                                       axis.title=element_text(size=18,face="bold"))

arthro_points <- read.csv(file = "/Users/johnlloyd/GitHub/Mansfield.Phenology/Data/Arthropod_survey_data_point_data.csv",
                          header = T, sep = ",")
arthro_data <- read.csv(file = "/Users/johnlloyd/GitHub/Mansfield.Phenology/Data/Arthropod_survey_data_point_data.csv",
                        header = T, sep = ",")




