## Load Library and Data =================================

library(dplyr)
library("readxl")
library("car")
library(tidyverse)
library(gapminder)
library(ggpubr)
library(rstatix)

#Load the data into R. Let's hope Xlsx haven't fucked up the data
rawData <- read_excel("TestData.xlsx")

#Make the data into a .rda file that works better with r-studio
save(rawData, file='AllRawData.rda', compress=TRUE)

# The inventory, ParticipantID, taskID and were saved as a character/num and we need them as a factor to plot it
rawData$Inventory <- as.factor(rawData$Inventory)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$taskID <- as.factor(rawData$taskID)
rawData$testIDIndividual <- as.factor(rawData$testIDIndividual)

#check that the inventory is now a factor
class(rawData$Inventory)  

## Making the main Data Frame =================================

#Making the main data frame 
finalTimes <- rawData %>%
  #filter(TimeSinceStart<30) %>% #This is to remove the outliers they were just annoying, but I'm not sure you can justify removing them
  group_by(ParticipantID, taskID, testIDIndividual, Inventory) %>% 
         summarise(maxActions = max(NumberOfActions),
                   correctItemID = max(CorrectItem),
                   totalTime = max(TimeSinceStart),
                   totalTimeMain = max(TimeInMain),
                   totalTimeFood = max(TimeInCategoryFood),
                   totalTimeTool = max(TimeInCategoryTools),
                   totalTimeMaterial = max(TimeInCategoryMaterial),
                   totalTimeDrink = max(TimeInCategoryDrink),
                   totalWrongCategory = max(CountOfIrrelevantCategory),
                   totalWrongItem = max(CounterWrongItemPicked))

#This was a nice mutation, but what happens if they entered the correct category multiple times?
finalTimes <- finalTimes %>% 
  mutate(correctCategoryTimer = case_when(correctItemID == 0 ~ totalTimeTool, 
                                          correctItemID == 1 ~ totalTimeMaterial, 
                                          correctItemID == 2 ~ totalTimeFood, 
                                          correctItemID == 4 ~ totalTimeFood, 
                                          correctItemID == 3 ~ totalTimeDrink, 
                                          TRUE ~ NA_real_))

#overview of the data frame
summary(finalTimes)

## Analyzing the time used =================================

### Checking for Normality =================================

#shapiro test to look for nomality
shapiro.test(finalTimes$totalTime)

shapiro.test(finalTimes$correctCategoryTimer)

#Looking at the qqplot there are some clear outliers
qqPlot(finalTimes$totalTime)

qqPlot(finalTimes$correctCategoryTimer)

#Looking at the histogram there is again some very clear outliers and the median line is not at the peak as we had hoped
ggplot(finalTimes, aes(x = totalTime)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = median(totalTime)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()
#correct category
ggplot(finalTimes, aes(x = correctCategoryTimer)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = median(correctCategoryTimer)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

#based on the shapiro test, qqplot and histogram it would be difficult to justify that the data is parametric 
#time spent in the correct cateogry also doesnt seem to be parametric
### Overview of the data =================================

#Having them next to each other, it is hard to see a difference I wonder if we will find any statically difference. Again Clear outlire
ggplot(finalTimes, aes(x = Inventory, y = totalTime )) +
  geom_boxplot(aes(alpha = .1)) +
  theme_bw()

#correct category
ggplot(finalTimes, aes(x = Inventory, y = correctCategoryTimer )) +
  geom_boxplot(aes(alpha = .1)) +
  theme_bw()

#making a new dataframe only containing the median total time. 
MedianTime <- finalTimes %>%
  group_by(Inventory) %>%
  summarise(medianTotal = median(totalTime))
#same for time spent inside the category
medianCorrectCategoryTime <- finalTimes %>%
  group_by(Inventory) %>%
  summarise(medianTotal = median(correctCategoryTimer))

medianCorrectAndFinal <- finalTimes %>%
  group_by(Inventory) %>%
  summarise(medianFinal = median(totalTime), medianCategory = median(correctCategoryTimer))

#Bar chart visulizing the different median times. As we can see the median is quite small only a bit over 6 - 7 sec.
ggplot(MedianTime, aes(x=Inventory, y=medianTotal)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=medianTotal), vjust=-0.3, size=3.5)+
  theme_minimal()

#time spent in correct category
ggplot(medianCorrectCategoryTime, aes(x=Inventory, y=medianTotal)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=medianTotal), vjust=-0.3, size=3.5)+
  theme_minimal()

#This shows the total time spend in each category. The plot is misleading since it does not look at each test individual most of the time they did not go into more categories.   
finalTimes %>% 
  group_by(Inventory) %>%
  summarise(n=n(),
            totalTimeMain,
            totalTimeFood,
            totalTimeTool,
            totalTimeMaterial,
            totalTimeDrink) %>% 
  gather("key", "value", - c(Inventory, n)) %>%
  ggplot(aes(x = Inventory, y = value, group = key, fill = key)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal()

### Statistical Analysis =================================

# Getting an overview of the Total time statistics
finalTimes %>%
  group_by(Inventory) %>%
  get_summary_stats(totalTime, type = "common")

#this is for some reason getting me an error and I'm not sure why, if you can get it to work is is an easy way of writing the friedman_test. Try to see here: https://www.datanovia.com/en/lessons/friedman-test-in-r/
finalTimes %>% friedman_test(totalTime ~ Inventory | testIDIndividual)
  
#I'm not sure why you take both participant and task ID - isn't the point to test the effect of inventory on the time and not the individual participant?
participantTaskMedian <- finalTimes %>% 
  group_by(ParticipantID, taskID) %>% 
  summarise(medianFinalTime=median(totalTime))

#I'm not sure why you are testing the effect of the individual participant?
friedman.test(y=participantTaskMedian$medianFinalTime, 
              groups=participantTaskMedian$taskID, 
              blocks=participantTaskMedian$ParticipantID)

#I tried to do it with Inventory
FridemanInventory <- finalTimes %>% 
  group_by(Inventory, testIDIndividual) %>% 
  summarise(medianFinalTime=median(totalTime))

#It does not look like there are much significant difference
friedman.test(y=FridemanInventory$medianFinalTime, 
              groups=FridemanInventory$Inventory, 
              blocks=FridemanInventory$testIDIndividual)

#the following analysis gives a bit more information
library(agricolae)
friedman(FridemanInventory$testIDIndividual,
         FridemanInventory$Inventory,
         FridemanInventory$medianFinalTime,
         console = TRUE)

# I could not get you code here to run
timesForCorrectCategory <- finalTimes %>%  group_by(taskID, ParticipantID, correctCategoryTimer) %>%
  summarise()

friedman.test(y = timesForCorrectCategory$correctCategoryTimer, 
              groups = timesForCorrectCategory$taskID, 
              blocks = timesForCorrectCategory$ParticipantID)



# Using regression it shows that unrestricted is significant faster, not sure if you want to use regression. you have to look it up I just did it for fun
summary(glm(totalTime ~ Inventory,  family="poisson", data = finalTimes))



## Analyzing the Errors Made =================================

# I have not made any analysis of errors I just made the structure

### Checking for Normality =================================

#Test Nomality here

shapiro.test(finalTimes$totalWrongCategory)
shapiro.test(finalTimes$totalWrongItem)

#qq plot 

qqPlot(finalTimes$totalWrongCategory)
qqPlot(finalTimes$totalWrongItem)

#histogram 

ggplot(finalTimes, aes(x = totalWrongCategory)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = median(totalWrongCategory)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

ggplot(finalTimes, aes(x = totalWrongItem)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = median(totalWrongItem)), color = "blue", linetype = "dashed", size = 1) +
  theme_bw()

### Overview of the data =================================

#making new data set, with only inventories and errors

errorData <- finalTimes %>% group_by(Inventory) %>%
                                  summarise(meanItemError = mean(totalWrongItem),
                                            meanCategoryError = mean(totalWrongCategory),
                                            totalItemError = sum(totalWrongItem),
                                            totalCategoryError = sum(totalWrongCategory))

#plotting the total number of category errors

#either use the total number of errors, or the mean, but not both, the graphs look the same

#total errors

ggplot(errorData, aes(x=Inventory, y=totalCategoryError)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=totalCategoryError), vjust=-0.3, size=3.5)+
  theme_minimal()

ggplot(errorData, aes(x=Inventory, y=totalItemError)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=totalItemError), vjust=-0.3, size=3.5)+
  theme_minimal()

#mean errors

ggplot(errorData, aes(x=Inventory, y=meanCategoryError)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=meanCategoryError), vjust=-0.3, size=3.5)+
  theme_minimal()

ggplot(errorData, aes(x=Inventory, y=meanItemError)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=meanItemError), vjust=-0.3, size=3.5)+
  theme_minimal()


# Getting an overview of the Total time 
finalTimes %>%
  group_by(Inventory) %>%
  get_summary_stats(totalWrongItem, type = "common")

finalTimes %>%
  group_by(Inventory) %>%
  get_summary_stats(totalWrongCategory, type = "common")


### Statistical Analysis =================================

#category error data, made for the friedman test
friedmanCategoryErrors <- finalTimes %>% 
  group_by(Inventory, testIDIndividual) %>% 
         summarise(categoryErrors=max(totalWrongCategory))


friedman(friedmanCategoryErrors$testIDIndividual,
         friedmanCategoryErrors$Inventory,
         friedmanCategoryErrors$categoryErrors,
         console = TRUE)

#not significant

friedmanItemErrors <- finalTimes %>% 
  group_by(Inventory, testIDIndividual) %>% 
  summarise(itemErrors=max(totalWrongItem))


friedman(friedmanItemErrors$testIDIndividual,
         friedmanItemErrors$Inventory,
         friedmanItemErrors$itemErrors,
         console = TRUE)

#not significant 