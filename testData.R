## Load Library and Data =================================

library(dplyr)
library("readxl")
library("car")
library(tidyverse)
library(gapminder)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)
library("writexl")

#Load the data into R. Let's hope Xlsx haven't fucked up the data
rawData <- read_excel("TestData.xlsx")

#Make the data into a .rda file that works better with r-studio
save(rawData, file='AllRawData.rda', compress=TRUE)




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

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
finalTimes <- finalTimes %>% 
  mutate(timeInWrongCategories = specify_decimal((totalTimeFood+totalTimeTool+totalTimeMaterial+totalTimeDrink-correctCategoryTimer),3))


#Testing for learning curve
medianTimeForTestOrder <- group_by(finalTimes, Inventory, testIDIndividual,correctCategoryTimer) %>%
  summarise()

medianTimeForTestOrder <- medianTimeForTestOrder %>% 
  mutate(newTestIDIndividual = testIDIndividual%%5)

medianTimeForTestOrder <- medianTimeForTestOrder %>% 
  mutate(actualTestOrder = case_when(newTestIDIndividual == 0 ~ 5, 
                                                       TRUE ~ newTestIDIndividual))

#this says N/A. but we will fill it out in Excel because there is only a few cases

learningCurveCheckWithMedians <- group_by(medianTimeForTestOrder, Inventory, actualTestOrder)%>%
                            summarise(timeInCorrectCategory = median(timeInCorrectCategory))

learningCurveCheck <- group_by(medianTimeForTestOrder,Inventory,actualTestOrder,correctCategoryTimer)%>%summarise()

write_xlsx(learningCurveCheck,"learningCurve.xlsx")

#friedman(learningCurveCheck$Inventory,
#         learningCurveCheck$actualTestOrder,
#         learningCurveCheck$timeInCorrectCategory,
#         console = TRUE)

LinearCurve$testIDIndividual <- as.factor(LinearCurve$testIDIndividual)
LinearCurve$medianTime <- as.factor(LinearCurve$medianTime)
GridCurve$testIDIndividual <- as.factor(GridCurve$testIDIndividual)
GridCurve$medianTime <- as.factor(GridCurve$medianTime)
UnrestrictedCurve$testIDIndividual <- as.factor(UnrestrictedCurve$testIDIndividual)
UnrestrictedCurve$medianTime <- as.factor(UnrestrictedCurve$medianTime)
CircularCurve$testIDIndividual <- as.factor(CircularCurve$testIDIndividual)
CircularCurve$medianTime <- as.factor(CircularCurve$medianTime)

cor(LinearCurve$actualTestOrder, LinearCurve$correctCategoryTimer)

cor(CircularCurve$actualTestOrder, CircularCurve$correctCategoryTimer)

cor(GridCurve$actualTestOrder, GridCurve$correctCategoryTimer)

cor(UnrestrictedCurve$actualTestOrder, UnrestrictedCurve$correctCategoryTimer)

summary(glm(correctCategoryTimer~actualTestOrder, data = LinearCurve))

summary(glm(correctCategoryTimer~actualTestOrder, data = CircularCurve))

summary(glm(correctCategoryTimer~actualTestOrder, data = GridCurve))

summary(glm(correctCategoryTimer~actualTestOrder, data = UnrestrictedCurve))

circular <- ggplot(CircularCurve, aes(x=actualTestOrder,y=correctCategoryTimer))+geom_smooth(method = lm)+geom_point()

grid <- ggplot(GridCurve, aes(x=actualTestOrder,y=correctCategoryTimer))+geom_point()+geom_smooth(method = lm)

linear <- ggplot(LinearCurve, aes(x=actualTestOrder,y=correctCategoryTimer))+geom_point()+geom_smooth(method = lm)

unrestricted <- ggplot(UnrestrictedCurve, aes(x=actualTestOrder,y=correctCategoryTimer))+geom_point()+geom_smooth(method = lm)

circular

grid

linear

unrestricted

ggplot(learningCurveCheck, aes(x=actualTestOrder,y=correctCategoryTimer,fill=Inventory))+geom_smooth(method = lm, se=FALSE,aes(color=Inventory,linetype=Inventory))+coord_cartesian(ylim=c(2.5,5))+xlab("Test NR.")+ylab("Time in correct cateogry(s)")
#overview of the data frame

# The inventory, ParticipantID, taskID and were saved as a character/num and we need them as a factor to plot it
rawData$Inventory <- as.factor(rawData$Inventory)
rawData$ParticipantID <- as.factor(rawData$ParticipantID)
rawData$taskID <- as.factor(rawData$taskID)
rawData$testIDIndividual <- as.factor(rawData$testIDIndividual)

#check that the inventory is now a factor
class(rawData$Inventory)  

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

#correct category times
friedmanInventoryInCorrectCategory <- finalTimes %>% 
  group_by(Inventory, testIDIndividual) %>% 
  summarise(medianFinalTime=median(correctCategoryTimer))

#It does not look like there are much significant difference
friedman.test(y=FridemanInventory$medianFinalTime, 
              groups=FridemanInventory$Inventory, 
              blocks=FridemanInventory$testIDIndividual)

#the following analysis gives a bit more information

friedman(FridemanInventory$testIDIndividual,
         FridemanInventory$Inventory,
         FridemanInventory$medianFinalTime,
         console = TRUE)

friedman(friedmanInventoryInCorrectCategory$testIDIndividual,
         friedmanInventoryInCorrectCategory$Inventory,
         friedmanInventoryInCorrectCategory$medianFinalTime,
         console = TRUE)

#none are significant


# Using regression it shows that unrestricted is significant faster, not sure if you want to use regression. you have to look it up I just did it for fun
summary(glm(totalTime ~ Inventory,  family="poisson", data = finalTimes))
summary(glm(correctCategoryTimer ~ Inventory,  family="poisson", data = finalTimes))










## Analyzing the Errors Made =================================

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






## Analyzing if the correct items affect the results

### The timers and errors have been previously checked for normality

### Data overview

itemsVsTimeAndError <- finalTimes %>% group_by(correctItemID) %>%
  summarise(medianTimeInCorrectCategory = median(correctCategoryTimer),
            medianFinalTime = median(totalTime),
            totalItemError = sum(totalWrongItem),
            totalCategoryError = sum(totalWrongCategory))

#this is supposed to check if the items affect the correct category time, the result is insignificant, but there's an error that also gets produced, so i dont know how reliable it is

friedmanItem <- finalTimes %>% 
  group_by(correctItemID, testIDIndividual) %>% 
  summarise(medianTimeInCorrectCategory=median(correctCategoryTimer))

friedman(friedmanItem$testIDIndividual,
         friedmanItem$correctItemID,
         friedmanItem$medianTimeInCorrectCategory,
         console = TRUE)

friedmanItemFinalTime <- finalTimes %>% 
  group_by(correctItemID, testIDIndividual) %>% 
    summarise(medianFinalTime=median(totalTime))

friedman(friedmanItemFinalTime$testIDIndividual,
         friedmanItemFinalTime$correctItemID,
         friedmanItemFinalTime$medianFinalTime,
         console = TRUE)
#this if the items affected the errors

friedmanErrorIPerItem <- finalTimes %>% 
  group_by(correctItemID, testIDIndividual) %>% 
  summarise(ErrorsType_I = max(totalWrongItem))

friedmanErrorIIPerItem <- finalTimes %>% 
  group_by(correctItemID, testIDIndividual) %>% 
  summarise(ErrorsType_II = max(totalWrongCategory))

friedman(friedmanErrorIPerItem$testIDIndividual,
         friedmanErrorIPerItem$correctItemID,
         friedmanErrorIPerItem$ErrorsType_I,
         console = TRUE)

friedman(friedmanErrorIIPerItem$testIDIndividual,
         friedmanErrorIIPerItem$correctItemID,
         friedmanErrorIIPerItem$ErrorsType_II,
         console = TRUE)



medianTimeForTestOrder <- group_by(finalTimes, Inventory, testIDIndividual) %>%
                            summarise(timeInCorrectCategory=median(correctCategoryTimer),timeInOtherCategories=median(timeInWrongCategories))

medianTimeForTestOrder <- medianTimeForTestOrder %>% 
  mutate(newTestIDIndividual = testIDIndividual%%5)



#Looking for learning curve

