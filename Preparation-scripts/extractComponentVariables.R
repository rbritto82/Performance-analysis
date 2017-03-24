library(xlsx)
library(dplyr)

#Remove whtite spaces in the strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Getting list of PCs
pc <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.3.xlsx", sheetIndex=5, colIndex = 2, stringsAsFactors = FALSE))
#pc <- pc[-c(18,23),] #remove lines that we do not have the data at this point (ZAINK-069 and CIN-300)
pc$ID <- trim(pc$ID)

#Getting and formating the list of components
components <-  tbl_df(read.xlsx("./Critical_components.xlsx", sheetIndex = 1, stringsAsFactors = FALSE))
components[is.na(components)] <- "" #Removing Not A Number (NA) 
components <- mutate(components, fullFilePath = paste(tolower(Component), Files.Directories, "/", sep = "")) #combining the first and second columns to have the full path
components <- components[-c(1),] #removing first line

#Getting the files changed in each PC and putting in independent data frames
pcList <- list()
for (i in 1:nrow(pc)) {
  pcList[[i]] <- tbl_df(read.xlsx("./PC_LOC_per_person_1.xlsx", sheetIndex=i, stringsAsFactors = FALSE))
  pcList[[i]][is.na(pcList[[i]])] <- 0 #Removing Not A Number (NA)
}
names(pcList) <- pc$ID #Setting the names of each PC in the list

#Creating data frame with the data about critical components
pcListComponentData <- data.frame(ID = vector(), numberComplexComponents = vector(), numberComplexFiles = vector(), 
                                            numberTotalFiles = vector(), numberComplexLOC = vector(), numberTotalLOC = vector(), 
                                            ratioComplexFiles = vector(), ratioComplexLOC = vector())
for (i in 1:nrow(pc)){   
  ID <- pc$ID[i]
  numberTotalFiles <- as.numeric(pcList[[i]]$Files[nrow(pcList[[i]])])
  numberTotalLOC <- rowSums(pcList[[i]][nrow(pcList[[i]]), -c(1,2,3)])
  numberComplexComponents <- 0
  numberComplexFiles <- 0
  numberComplexLOC <- 0
  numberttcn3LOC <- 0
  
  ttcn3LOC <- grep("ttcn3/", pcList[[i]]$Files)
  numberttcn3LOC <- numberttcn3LOC + sum(rowSums(pcList[[i]][ttcn3LOC, -c(1,2,3)]))
  
  for (j in 1:nrow(components)) {
    criticalComponents <- grep(components$fullFilePath[j], pcList[[i]]$Files)
    if (length(criticalComponents) > 0){
      numberComplexComponents <- numberComplexComponents + 1
      numberComplexFiles <- numberComplexFiles + length(criticalComponents)
      numberComplexLOC <- numberComplexLOC + sum(rowSums(pcList[[i]][criticalComponents, -c(1,2,3)]))
    }  
  }
  
  ratioComplexFiles <- (numberComplexFiles/numberTotalFiles)
  ratioComplexLOC <- (numberComplexLOC/numberTotalLOC)
  
  pcComponentData <- data.frame(ID = ID, numberComplexComponents = numberComplexComponents, 
                                numberComplexFiles = numberComplexFiles, numberTotalFiles = numberTotalFiles, 
                                numberComplexLOC = numberComplexLOC, numberTotalLOC = numberTotalLOC, 
                                ratioComplexFiles = ratioComplexFiles, ratioComplexLOC = ratioComplexLOC, 
                                numberttcn3LOC = numberttcn3LOC)
  
  pcListComponentData <- rbind(pcListComponentData, pcComponentData)
}
#Writing summary in a spreadsheet
write.xlsx(pcListComponentData, file="Components details per Task.xlsx", sheetName="PC details")
