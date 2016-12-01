aud1 <- read.csv(file = "/Users/johnlloyd/GitHub/Mansfield.Phenology/Data/Vermont_256.csv", header = T, sep = ";")
library(dplyr)
library(ggplot2)
library(tidyr)
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
  ggplot(., aes(x = Date, y = instances)) + geom_line() + ylab("No. of Bicknell's Thrush vocalizations")

summary1 %>%
  filter(Class == "OTHER") %>%
  ggplot(., aes(x = Date, y = instances)) + geom_line() + ylab("No. of vocalizations by other bird species")

cp.bp <- read.csv(file = "/Users/johnlloyd/GitHub/Mansfield.Phenology/Data/CB_BP.csv", header = T, sep = ",")

cp.bp %>%
  group_by(Species, Date, CP) %>%
  summarize(N = n()) %>%
  mutate(Percentage = N/sum(N)) %>%
  complete(Species, Date, CP, fill = list(N = 0,Percentage = 0)) %>%
  filter(Species == "BITH") %>%
  ggplot(., aes(x = CP, y = Percentage)) + geom_bar(stat_bin())

