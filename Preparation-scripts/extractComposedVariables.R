#This script calculates the following metrics: accumulated training hours, product familiarity (accumulated productive hours), team familiarity
library(xlsx)
library(dplyr)
library(lubridate)
library(gtools)
library("prodlim")

#to remove any undesired blank space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#reading file and changing columns' names
egiPersonel <- tbl_df(read.xlsx("./EGI developers detail.xlsx", sheetIndex=1, stringsAsFactors = FALSE))
egiPersonel <- egiPersonel[egiPersonel$Hours > 0, ] #removing rows with zero hours
names(egiPersonel) <- c("ID", "Name", "Date", "Hours", "HourType", "Activity")

#selecting only training hours
productiveData <- egiPersonel[grep("Productive", egiPersonel$HourType), ]
trainingData <- egiPersonel[grep("Training", egiPersonel$HourType), ]

#reading required info from PCs and selecting only data from India
pcInfo <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=5, colIndex = c(2,3,4), stringsAsFactors = FALSE))
leadtime <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=9, colIndex = c(3,4,6), stringsAsFactors = FALSE))
pcInfo <- cbind(pcInfo, leadtime)
pcInfoEGI <- filter(pcInfo, pcInfo$location == 'India')

#reading Indian time reports and removing any undesired blank spaces regarding taskID and employeeID
egiTimeReport <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=11, stringsAsFactors = FALSE))
egiTimeReport$employeeID <- trim(egiTimeReport$employeeID)
egiTimeReport$taskID <- trim(egiTimeReport$taskID)
egiTimeReport <- filter(egiTimeReport, egiTimeReport$employeeID != "1") #remove transfer cost

#Calculating the training hours that has to be considered as productive hours
realTrainingData <- trainingData
for (i in 1:length(pcInfoEGI$ID)) { 
  
  idList <- egiTimeReport$employeeID[egiTimeReport$taskID == pcInfoEGI$ID[i]] #getting the ID of developers involved in a particular PC/task
  idList <- paste(idList, collapse = "|") #creating an unique string by collapsing the developers' IDs
  localData <- trainingData[(trainingData$Date >= pcInfoEGI$start[i]) & (trainingData$Date < pcInfoEGI$end[i]),] #selecting the data according to the passed date
  localData <- localData[grep(idList, localData$ID),] #selecting the involved developers in the task
  
  realTrainingData <- realTrainingData[!((realTrainingData$Date >= pcInfoEGI$start[i]) & (realTrainingData$Date < pcInfoEGI$end[i])),] #removing training hours related to PC periods
  
  #calculating the accumulated training hours per developer
  trainingHours <- localData %>% 
    group_by(Name) %>%
    summarise(total = sum(Hours))  
  
  #Writing the result in a spreadsheet
  if((i==1) & (length(localData$ID) > 0)){
    write.xlsx(trainingHours, file = paste("./Training Productive Hours EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = pcInfoEGI$ID[i])
  } else if((i > 1) & (length(localData$ID) > 0)){
    write.xlsx(trainingHours, file = paste("./Training Productive Hours EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = pcInfoEGI$ID[i], append = TRUE)
  }
} 

#Calculating the accumulated training hours per developer in each task
for (i in 1:length(pcInfoEGI$ID)) { 
  
  idList <- egiTimeReport$employeeID[egiTimeReport$taskID == pcInfoEGI$ID[i]] #getting the ID of developers involved in a particular PC/task
  idList <- paste(idList, collapse = "|") #creating an unique string by collapsing the developers' IDs
  localData <- realTrainingData[(realTrainingData$Date < pcInfoEGI$start[i]),] #selecting the data according to the passed date
  localData <- localData[grep(idList, localData$ID),] #selecting the involved developers in the task
  
  #calculating the accumulated training hours per developer
  trainingHours <- localData %>% 
    group_by(Name) %>%
    summarise(total = sum(Hours))  
  
  #Writing the result in a spreadsheet
  if((i==1) & (length(localData$ID) > 0)){
    write.xlsx(trainingHours, file = paste("./Training Hours EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = pcInfoEGI$ID[i])
  } else if((i>1) & (length(localData$ID) > 0)){
    write.xlsx(trainingHours, file = paste("./Training Hours EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = pcInfoEGI$ID[i], append = TRUE)
  }
}  

#Calculating the productive hours per developer in each task (product familiarity)
for (i in 1:length(pcInfoEGI$ID)) { 
  
  idList <- egiTimeReport$employeeID[egiTimeReport$taskID == pcInfoEGI$ID[i]] #getting the ID of developers involved in a particular PC/task
  idList <- paste(idList, collapse = "|") #creating an unique string by collapsing the developers' IDs
  localData <- productiveData[(productiveData$Date < pcInfoEGI$start[i]),] #selecting the data according to the passed date
  localData <- localData[grep(idList, localData$ID),] #selecting the involved developers in the task
  localData2 <- trainingData[(trainingData$Date >= pcInfoEGI$start[i]) & (trainingData$Date < pcInfoEGI$end[i]),] #selecting the data according to the passed date
  localData2 <- localData2[grep(idList, localData2$ID),] #selecting the involved developers in the task
  
  if(length(localData2$ID) > 0){
    localData <- rbind(localData, localData2)
  }
  
  #calculating the accumulated training hours per developer
  productiveHours <- localData %>% 
    group_by(Name) %>%
    summarise(total = sum(Hours))  
  
  #Writing the result in a spreadsheet
  if((i==1) & (length(localData$ID) > 0)){
    write.xlsx(productiveHours, file = paste("./Productive Hours EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = pcInfoEGI$ID[i])
  } else if((i>1) & (length(localData$ID) > 0)){
    write.xlsx(productiveHours, file = paste("./Productive Hours EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = pcInfoEGI$ID[i], append = TRUE)
  }
}

#calculating team familiarity
egiDevCombinations <- list()
for (i in 1:length(pcInfoEGI$ID)) {  
  
  idList <- egiTimeReport$employeeID[egiTimeReport$taskID == pcInfoEGI$ID[i]] #getting the ID of developers involved in a particular PC/task
  
  if (length(idList) > 2){
    localCombinations <- combinations(length(idList), 2, idList) #creating an unique string by collapsing the developers' IDs
  } else if (length(idList) == 2) {
          localCombinations <- matrix(idList, 1, 2)
          } else {
            localCombinations <- matrix(c(as.character(i),as.character(i)), 1, 2)
            }  
  
  egiDevCombinations[[i]] <- localCombinations
}

#identifying how frequent each possible combination of developers is
egiDevCombinationsDays <- rep(0, length(pcInfoEGI$ID))
for (i in 2:length(egiDevCombinations)) { 
  for (j in 1:NROW(egiDevCombinations[[i]])) { 
    for (h in 1:(i-1)) {
      pairMatched <- row.match(egiDevCombinations[[i]][j,], egiDevCombinations[[h]])
      if (!is.na(pairMatched)){
        egiDevCombinationsDays[i] <- egiDevCombinationsDays[i] + pcInfoEGI$genLeadTime[h]
      }
      
    }  
  }
  egiDevCombinationsDays[i] <- egiDevCombinationsDays[i]/NROW(egiDevCombinations[[i]])
}

egiDevCombinationsDays <- cbind(pcInfoEGI$ID, egiDevCombinationsDays)
write.xlsx(egiDevCombinationsDays, file = paste("./Team Familiarity EGI developers_", Sys.Date(), ".xlsx", sep = ""), sheetName = "teamFamiliarity")
