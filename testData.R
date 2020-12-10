library(dplyr)


finalTimes <- group_by(Test_Data, taskID, ParticipantID , testIDIndividual) %>% 
         summarise(maxActions=max(NumberOfActions), correctItemID = max(CorrectItem),finalTime=max(TimeSinceStart))

participantTaskMedian <- group_by(finalTimes, ParticipantID, taskID) %>%
                           summarise(medianFinalTime=median(finalTime))

finalMedians <- group_by(participantTaskMedian, taskID) %>%
                    summarise(medianPerTaskID=median(medianFinalTime))

friedman.test(y=participantTaskMedian$medianFinalTime, groups=participantTaskMedian$taskID , blocks=participantTaskMedian$ParticipantID)






finalTimesPerCategory <- group_by(Test_Data, taskID, ParticipantID , testIDIndividual) %>% 
        summarise(maxActions=max(NumberOfActions), correctItemID = max(CorrectItem),finalTime=max(TimeSinceStart),finalTimeInCategoryFood = max(TimeInCategoryFood),finalTimeInCategoryTools = max(TimeInCategoryTools),finalTimeInCategoryMaterial = max(TimeInCategoryMaterial),finalTimeInCategoryDrink = max(TimeInCategoryDrink),finalTimeInMain = max(TimeInMain))

finalTimesPerCategory <- finalTimesPerCategory %>% mutate(correctCategoryTimer = case_when(correctItemID == 0 ~ finalTimeInCategoryTools, correctItemID == 1 ~ finalTimeInCategoryMaterial, correctItemID == 2 ~ finalTimeInCategoryFood, correctItemID == 4 ~ finalTimeInCategoryFood, correctItemID == 3 ~  finalTimeInCategoryDrink, TRUE ~ NA_real_))

timesForCorrectCategory <- group_by(finalTimesPerCategory, taskID, ParticipantID, correctCategoryTimer) %>%
              summarise()


friedman.test(y=timesForCorrectCategory$correctCategoryTimer, groups=timesForCorrectCategory$taskID , blocks=timesForCorrectCategory$ParticipantID)




