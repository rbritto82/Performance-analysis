library(xlsx)
library(dplyr)
#library(PMCMR)
library(ggplot2)
library(lmtest)
library(Hmisc)
library(car)
#library(psych)

source("./Analysis scripts/effectSize.R")
source("./Data preparation scripts/plotFunctions.R")

normalization <- function(x){
  (x - min(x))/(max(x) - min(x))  
}
  

#reading master file
teams <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=5))
complexity <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=6))
productivity <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=7))
effortSAT <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=8))
leadTime <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=9))
code <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v4.1.xlsx", sheetIndex=10, colIndex = 3))

complexity <- complexity[-c(1,2)]
productivity <- productivity[-c(1,2)]
effortSAT <- effortSAT[-c(1,2)]
leadTime <- leadTime[-c(1,2)]

masterSDP <- cbind(teams, complexity, productivity, effortSAT, leadTime, code)
masterSDP$taskPriority <- as.factor(masterSDP$taskPriority)
rm(teams, complexity, productivity, effortSAT, leadTime, code)

#removed v-454, v-555, v-498, t-347, v-392, v-573, v-392, t-372, v-407, v-392, v-419, v-562, all bsucs and all conttributions by architects
egiProductivityM <- masterSDP[-c(9, 18, 27, 31, 37, 38, 39, 40, 41, 44, 45:55), with(masterSDP, c("ID","refinedTeamMaturity","productivity"))]

egiThreshold <- masterSDP[-c(9, 18, 27, 31, 37, 38, 39, 40, 41, 44, 45:55), with(masterSDP, c("ID","refinedTeamMaturity","productivity", "complexityPoints", "numDevelopers"))] 
egiThreshold <-mutate(egiThreshold, threshold = numDevelopers/complexityPoints)

egiThreshold2 <- egiThreshold[egiThreshold$refinedTeamMaturity == "Mature teams",]
egiThreshold2 <- mutate(egiThreshold2, planID = seq(1, nrow(egiThreshold2), by = 1))
egiThreshold2$planID <- paste("Task",egiThreshold2$planID, sep = "-")

egiSupportM <- masterSDP[-c(9, 18, 27, 31, 37, 38, 39, 40, 41, 44, 45:55), with(masterSDP, c("ID","refinedTeamMaturity","normalizedTeamAutonomy"))]

egiRegression <- masterSDP[-c( 9, 18, 27:55),]
egiRegression <- egiRegression[order(egiRegression$end),]

#egiRegression$productivity <- as.vector(scale(egiRegression$productivity)) 
#egiRegression$normalizedEffortSAT <- as.vector(scale(egiRegression$normalizedEffortSAT))
#egiRegression$normalizedTeamAutonomy <- as.vector(scale(egiRegression$normalizedTeamAutonomy))
#egiRegression$teamFamiliarity <- as.vector(scale(egiRegression$teamFamiliarity))
#egiRegression$productFamiliarity <- as.vector(scale(egiRegression$productFamiliarity))
#egiRegression$daysExperience <- as.vector(scale(egiRegression$daysExperience))
#egiRegression$complexityPoints <- as.vector(scale(egiRegression$complexityPoints))
#egiRegression$taskGlobaDistance <- as.vector(scale(egiRegression$taskGlobaDistance))
#egiRegression$taskScaling <- as.vector(scale(egiRegression$taskScaling))
#egiRegression$submittedLOC <- as.vector(scale(egiRegression$submittedLOC))
#egiRegression$numDevelopers <- as.vector(scale(egiRegression$numDevelopers))


egiRegression <- mutate(egiRegression, threshold = numDevelopers/complexityPoints)
egiRegression <- mutate(egiRegression, planID = seq(1, nrow(egiRegression), by = 1))
egiRegression$planID <- paste("Task",egiRegression$planID, sep = "-")
egiRegression <- mutate(egiRegression, expProductivity = productivity/daysExperience)
egiRegression <- mutate(egiRegression, expAutonomy = normalizedTeamAutonomy/daysExperience)
egiRegression <- egiRegression[order(egiRegression$threshold), ]
egiRegression$typeScaling <- c("1":"24")
egiRegression$typeScaling[egiRegression$threshold > 0.2] <- "High"
egiRegression$typeScaling[egiRegression$threshold <= 0.2] <- "Low"

#other analysis
egiRegression <- mutate(egiRegression, locRatio = submittedLOC/complexityPoints)
egiRegression <- mutate(egiRegression, autoProd = productivity/normalizedTeamAutonomy)
egiRegression <- mutate(egiRegression, teamFamLOC = teamFamiliarity*locRatio)
egiRegression <- mutate(egiRegression, teamFamCom = teamFamiliarity*complexityPoints)
egiRegression <- mutate(egiRegression, teamFamProdFam = teamFamiliarity*productFamiliarity)
egiRegression <- mutate(egiRegression, teamFamExp = teamFamiliarity*daysExperience)


plotList <- list()
for (i in 1:length(egiRegression)) {
  plotList[[i]] <- data.frame(ID = c(egiRegression$ID[i], egiRegression$ID[i]), y = c(egiRegression$productivity[i], egiRegression$normalizedEffortSAT[i]), x = c(egiRegression$daysExperience[i], egiRegression$daysExperience[i]))
}

source("./Analysis scripts/RQ1.R")
source("./Analysis scripts/RQ2.R")
