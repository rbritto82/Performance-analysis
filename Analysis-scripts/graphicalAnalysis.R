#Loading required liberies
library(xlsx)
library(dplyr)

#Loading customized functions to plot the charts
source("./Data preparation scripts/plotFunctions.R")

#Loading the desired dataset
#reading master file
teams <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.6.xlsx", sheetIndex=5))
complexity <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.6.xlsx", sheetIndex=6))
productivity <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.6.xlsx", sheetIndex=7))
leadtime <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.6.xlsx", sheetIndex=8))
effortSAT <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.6.xlsx", sheetIndex=9))
code <- tbl_df(read.xlsx("./TEDD Ericsson Karlskrona learning data_v3.6.xlsx", sheetIndex=10))

complexity <- complexity[-c(1,2)]
productivity <- productivity[-c(1,2)]
leadtime <- leadtime[-c(1,2)]
effortSAT <- effortSAT[-c(1,2)]
code <- code[-c(1,2)]

masterSDP <- cbind(teams, complexity, productivity, leadtime, effortSAT, code)
masterSDP <- masterSDP[-c(18,23),]#removing PCs with missing data
masterSDP$End <- as.Date(masterSDP$end)
# componentSDP <- tbl_df(read.xlsx("./Components details per Task.xlsx", sheetIndex=1)) #reading file with components analysis
# componentSDP <- componentSDP[-c(18,23),]#removing PCs with missing data
# componentSDP <- componentSDP[-c(1,2)]#removing id columns
# masterSDP <- cbind(masterSDP, componentSDP)

# masterSDP$testPer <- masterSDP$testPer/100
# masterSDP$implementationPer <- masterSDP$implementationPer/100
# masterSDP <- mutate(masterSDP, wtComplexityPoints = complexityPoints*testPer)
# masterSDP <- mutate(masterSDP, wiComplexityPoints = complexityPoints*implementationPer)
# masterSDP <- mutate(masterSDP, numberActualLOC = numberTotalLOC-numberttcn3LOC)

 #egiData <- filter(masterSDP, location2 == "I")
 lowcomplexity <- filter(masterSDP, complexityPoints < 150)
 highcomplexity <- filter(masterSDP, complexityPoints >= 150)

#Min and max dates for the plots
minDate <- as.Date("2015-1-1")
maxDate <- as.Date("2016-12-31")
label = TRUE

#LOC vs LOC
lowappTTCN <- createPlotContinuos(lowcomplexity, "submitedTTCNLOC", "submittedApplicationLOC", "ID", 1.2, "Submitted LOC x TTCN LOC", "deepskyblue", 
                                  "TTCN LOC", "App LOC", 10, 1000, 
                                  10, 1000, label, FALSE)
ggsave("./Figures/lowappLOCxTTCNLOC.pdf", lowappTTCN)

highappTTCN <- createPlotContinuos(highcomplexity, "submitedTTCNLOC", "submittedApplicationLOC", "ID", 1.2, "Submitted LOC x TTCN LOC", "deepskyblue", 
                               "TTCN LOC", "App LOC", 10, 1500, 
                               10, 2000, label, FALSE)
ggsave("./Figures/highappLOCxTTCNLOC.pdf", highappTTCN)

#Complexity vs. Files#######################################################################################
lowFILEPlot <- createPlotContinuos(lowcomplexity, "changedFiles", "complexityPoints", "ID", 1.2, "Low Complexity points x Total Edited Files", "deepskyblue", 
                                  "Total Edited Files", "Complexity Points", 10, 5, 
                                  10, 100, label, FALSE)
ggsave("./Figures/LowComplexityFiles.pdf", lowFILEPlot)

#LOWESS curve
lowlowessFILEPlot <- ggplot(lowcomplexity, aes(x = complexityPoints, y = changedFiles)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/LowLowessComplexityFiles.pdf", lowlowessFILEPlot)

#-----------------------------------------------------------------------------------------------------------
highFILEPlot <- createPlotContinuos(highcomplexity, "changedFiles", "complexityPoints", "ID", 1.2, "High Complexity points x Total Edited Files", "deepskyblue", 
                                   "Total Edited Files", "Complexity Points", 10, 40, 
                                   10, 150, label, FALSE)
ggsave("./Figures/highComplexityFiles.pdf", highFILEPlot)

#LOWESS curve
highlowessFILEPlot <- ggplot(highcomplexity, aes(x = complexityPoints, y = changedFiles)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/highLowessComplexityFiles.pdf", highlowessFILEPlot)

#Complexity vs. LOC#######################################################################################
lowLOCPlot <- createPlotContinuos(lowcomplexity, "submittedLOC", "complexityPoints", "ID", 1.2, "Complexity points x Submitted LOC", "deepskyblue", 
                                        "Submitted LOC", "Complexity Points", 10, 5, 
                                        10, 2000, label, FALSE)
ggsave("./Figures/LowComplexityLOC.pdf", lowLOCPlot)

#LOWESS curve
lowlowessLOCPlot <- ggplot(lowcomplexity, aes(x = complexityPoints, y = submittedLOC)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/LowLowessComplexityLOC.pdf", lowlowessLOCPlot)
#-----------------------------------------------------------------------------------------------------------
highLOCPlot <- createPlotContinuos(highcomplexity, "submittedLOC", "complexityPoints", "ID", 1.2, "Complexity points x Submitted LOC", "deepskyblue", 
                                         "Submitted LOC", "Complexity Points", 10, 40, 
                                         10, 5000, label, FALSE)
ggsave("./Figures/highComplexityLOC.pdf", highLOCPlot)

#LOWESS curve
highlowessLOCPlot <- ggplot(highcomplexity, aes(x = complexityPoints, y = submittedLOC)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/highLowessComplexityLOC.pdf", highlowessLOCPlot)
#-----------------------------------------------------------------------------------------------------------
lowttcnLOCPlot <- createPlotContinuos(lowcomplexity, "submitedTTCNLOC", "complexityPoints", "ID", 1.2, "Complexity points x TTCN LOC", "deepskyblue", 
                                        "TTCN LOC", "Complexity Points", 10, 5, 
                                        10, 2000, label, FALSE)
ggsave("./Figures/lowTTCNComplexityLOC.pdf", lowttcnLOCPlot)

#LOWESS curve
lowttcnlowessLOCPlot <- ggplot(lowcomplexity, aes(x = complexityPoints, y = submitedTTCNLOC)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/lowTTCNLowessComplexityLOC.pdf", lowttcnlowessLOCPlot)



#-----------------------------------------------------------------------------------------------------------
highttcnLOCPlot <- createPlotContinuos(highcomplexity, "submitedTTCNLOC", "complexityPoints", "ID", 1.2, "Complexity points x TTCN LOC", "deepskyblue", 
                                   "TTCN LOC", "Complexity Points", 10, 10, 
                                   10, 2500, label, FALSE)
ggsave("./Figures/highTTCNComplexityLOC.pdf", highttcnLOCPlot)

#LOWESS curve
highttcnlowessLOCPlot <- ggplot(highcomplexity, aes(x = complexityPoints, y = submitedTTCNLOC)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/highTTCNLowessComplexityLOC.pdf", highttcnlowessLOCPlot)

#-----------------------------------------------------------------------------------------------------------
lowActualLOCPlot <- createPlotContinuos(lowcomplexity, "submittedApplicationLOC", "complexityPoints", "ID", 1.2, "Complexity points x Application LOC", "deepskyblue", 
                                       "Application LOC", "Complexity Points", 10, 5, 
                                       10, 2000, label, FALSE)
ggsave("./Figures/lowAppComplexityLOC.pdf", lowActualLOCPlot)

#LOWESS curve
lowActuallowessLOCPlot <- ggplot(lowcomplexity, aes(x = complexityPoints, y = submittedApplicationLOC)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/lowAppLowessComplexityLOC.pdf", lowActuallowessLOCPlot)



#-----------------------------------------------------------------------------------------------------------
highActualLOCPlot <- createPlotContinuos(highcomplexity, "submittedApplicationLOC", "complexityPoints", "ID", 1.2, "Complexity points x Application LOC", "deepskyblue", 
                                        "Application LOC", "Complexity Points", 10, 10, 
                                        10, 2500, label, FALSE)
ggsave("./Figures/highAppComplexityLOC.pdf", highActualLOCPlot)

#LOWESS curve
highActuallowessLOCPlot <- ggplot(highcomplexity, aes(x = complexityPoints, y = submittedApplicationLOC)) + geom_point() + stat_smooth(method = "loess")
ggsave("./Figures/highAppLowessComplexityLOC.pdf", highActuallowessLOCPlot)

#Complexity plots ########################################################################################
complexityCriticalComponents <- createPlotContinuos(egiData, "numberComplexComponents", "complexityPoints", "ID", 1.2, "Complexity x Number Critical Components", "deepskyblue", 
                                        "Critical Components", "Complexity points", 0, 20, 
                                        1, 1, label, FALSE)

complexityCriticalFiles <- createPlotContinuos(egiData, "ratioComplexFiles", "complexityPoints", "ID", 1.2, "Complexity x Ratio Critical Files", "deepskyblue", 
                                               "Ratio Critical Files", "Complexity points", 0, 20, 
                                               1, 1, label, FALSE)

complexityCriticalLOC <- createPlotContinuos(egiData, "ratioComplexLOC", "complexityPoints", "ID", 1.2, "Complexity x Ratio Critical LOC", "deepskyblue", 
                                             "Ratio Critical LOC", "Complexity points", 0, 20, 
                                             1, 1, label, FALSE)

combinedPlot <- arrangeGrob(complexityCriticalComponents, complexityCriticalFiles, complexityCriticalLOC,
                            ncol=3, nrow =1)

ggsave("./Figures/ComplexityCritical.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

#Combined plots for End Date ######################################################################################################

productivityPlot <- createPlotDate(egiData, "productivity", "end", "ID", 1.2, "End Date X Productivity","deepskyblue", 
                                   "Productivity", "Date", "%y/%m", "1 month", minDate, maxDate, 10, 10, label)
ggsave("./Figures/Productivity.pdf")

SATPlot <- createPlotDate(egiData, "normalizedEffortSAT", "end", "ID", 1.2, "End Date X Normalized SAT Support Effort","deepskyblue", 
                          "Normalized SAT Support Effort", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 0.5, label)
ggsave("./Figures/SAT Support.pdf") 

leadTimePlot <- createPlotDate(egiData, "normalizedGenLeadTime", "end", "ID", 1.2, "End Date X Normalized Lead Time","deepskyblue", 
                               "Normalized Lead Time", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 0.5, label)
ggsave("./Figures/Lead Time.pdf")

#combinedPlot <- arrangeGrob(productivityPlot, SATPlot, leadTimePlot,ncol=3, nrow =1)

#ggsave("./Figures/GeneralEnd.pdf", combinedPlot, width = 22, height = 11, dpi = 120)
  
positiveVerdictsPlot <- createPlotDate(egiData, "normalizedPositiveVerdicts", "end", "ID", 1.2, "End Date X Normalized Number of Positive Verdicts","deepskyblue", 
                                       "Normalized Number of Positive Verdicts", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 1, label)
  
negativeVerdictsPlot <- createPlotDate(egiData, "normalizedNegativeVerdicts", "end", "ID", 1.2, "End Date X Normalized Number of Negative Verdicts","deepskyblue", 
                                       "Normalized Number of Negative Verdicts", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 5, label)
  
ratioLOCPlot <- createPlotDate(egiData, "normalizedRatioLOC", "end", "ID", 1.2, "End Date X Normalized Ratio Submitted/Abandoned LOC","deepskyblue", 
                               "Normalized Ratio Submitted/Abandoned LOC", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 1, label)

technicalDebtPlot <- createPlotDate(egiData, "normalizedTechnicalDebt", "end", "ID", 1.2, "End Date X Normalized Technical Debt","deepskyblue", 
                                    "Normalized Technical Debt", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 10, label)

duplicatedCodePlot <- createPlotDate(egiData, "normalizedDuplicatedCode", "end", "ID", 1.2, "End Date X Normalized Duplicated Code","deepskyblue", 
                 "Normalized Number of Negative Verdicts", "Date", "%y/%m", "1 month", minDate, maxDate, 1, 20, label)

combinedPlot <- arrangeGrob( positiveVerdictsPlot, negativeVerdictsPlot,
                             technicalDebtPlot, ratioLOCPlot, duplicatedCodePlot, 
                             ncol=3, nrow =2)

ggsave("./Figures/QualityEnd.pdf", combinedPlot, width = 22, height = 11, dpi = 120)


#Combined plots for Accumulated Experience #########################################################################

productivityPlot <- createPlotContinuos(egiData, "productivity", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Productivity", "deepskyblue", 
                                 "Productivity", "Accumulated Complexity Points", 10, 40, 
                                 10, 10, label)

SATPlot <- createPlotContinuos(egiData, "normalizedEffortSAT", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized SAT Support Effort", "deepskyblue", 
                    "Normalized SAT Support Effort", "Accumulated Complexity Points", 10, 40, 
                    1, 0.5, label)

leadTimePlot <- createPlotContinuos(egiData, "normalizedGenLeadTime", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized Lead Time", "deepskyblue", 
                    "Normalized Lead Time", "Accumulated Complexity Points", 10, 40,
                    1, 0.5, label)

combinedPlot <- arrangeGrob(productivityPlot, SATPlot, leadTimePlot,
                            ncol=3, nrow =1)

ggsave("./Figures/GeneralAccumulated.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

positiveVerdictsPlot <- createPlotContinuos(egiData, "normalizedPositiveVerdicts", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized Positive Verdicts", "deepskyblue", 
                    "Normalized Number of Positive Verdicts", "Accumulated Complexity Points", 10, 40,
                    1, 1, label)

negativeVerdictsPlot <- createPlotContinuos(egiData, "normalizedNegativeVerdicts", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized Negative Verdicts", "deepskyblue", 
                    "Normalized Number of Negative Verdicts", "Accumulated Complexity Points", 10, 40, 
                    1, 5, label)

ratioLOCPlot <- createPlotContinuos(egiData, "normalizedRatioLOC", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized Ratio Submitted/Abandoned LOC", "deepskyblue", 
                                    "Normalized Ratio Submitted/Abandoned LOC", "Accumulated Complexity Points", 10, 40, 
                                    1, 1, label)

technicalDebtPlot <- createPlotContinuos(egiData, "normalizedTechnicalDebt", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized Technical Debt", "deepskyblue", 
                    "Normalized Technical Debt", "Accumulated Complexity Points", 10, 40,
                    1, 10, label)


duplicatedCodePlot <- createPlotContinuos(egiData, "normalizedDuplicatedCode", "accumulatedComplexity", "ID", 1.2, "Accumulated complexity points x Normalized Duplicated Code", "deepskyblue", 
                    "Normalized Duplicated Code", "Accumulated Complexity Points", 10, 40,
                    1, 20, label)

combinedPlot <- arrangeGrob(positiveVerdictsPlot, negativeVerdictsPlot,
                             technicalDebtPlot, ratioLOCPlot, duplicatedCodePlot, 
                             ncol=3, nrow =2)

ggsave("./Figures/QualityAccumulated.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

#Combined plots for Critical components #########################################################################
productivityPlot <- createPlotContinuos(egiData, "productivity", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Productivity", "deepskyblue", 
                                        "Productivity", "Number Critical Components", 1, 1, 
                                        10, 10, label, FALSE)

SATPlot <- createPlotContinuos(egiData, "normalizedEffortSAT", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized SAT Support Effort", "deepskyblue", 
                               "Normalized SAT Support Effort", "Number Critical Components", 1, 1, 
                               1, 0.5, label, FALSE)

leadTimePlot <- createPlotContinuos(egiData, "normalizedGenLeadTime", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized Lead Time", "deepskyblue", 
                                    "Normalized Lead Time", "Number Critical Components", 1, 1,
                                    1, 0.5, label, FALSE)

combinedPlot <- arrangeGrob(productivityPlot, SATPlot, leadTimePlot,
                            ncol=3, nrow =1)

ggsave("./Figures/GeneralCriticalComponents.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

positiveVerdictsPlot <- createPlotContinuos(egiData, "normalizedPositiveVerdicts", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized Positive Verdicts", "deepskyblue", 
                                            "Normalized Number of Positive Verdicts", "Number Critical Components", 1, 1,
                                            1, 1, label, FALSE)

negativeVerdictsPlot <- createPlotContinuos(egiData, "normalizedNegativeVerdicts", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized Negative Verdicts", "deepskyblue", 
                                            "Normalized Number of Negative Verdicts", "Number Critical Components", 1, 1, 
                                            1, 5, label, FALSE)

ratioLOCPlot <- createPlotContinuos(egiData, "normalizedRatioLOC", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized Ratio Submitted/Abandoned LOC", "deepskyblue", 
                                    "Normalized Ratio Submitted/Abandoned LOC", "Number Critical Components", 1, 1, 
                                    1, 1, label, FALSE)

technicalDebtPlot <- createPlotContinuos(egiData, "normalizedTechnicalDebt", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized Technical Debt", "deepskyblue", 
                                         "Normalized Technical Debt", "Number Critical Components", 1, 1,
                                         1, 10, label, FALSE)


duplicatedCodePlot <- createPlotContinuos(egiData, "normalizedDuplicatedCode", "numberComplexComponents", "ID", 1.2, "Number Critical Components x Normalized Duplicated Code", "deepskyblue", 
                                          "Normalized Duplicated Code", "Number Critical Components", 1, 1,
                                          1, 20, label, FALSE)

combinedPlot <- arrangeGrob(positiveVerdictsPlot, negativeVerdictsPlot,
                            technicalDebtPlot, ratioLOCPlot, duplicatedCodePlot, 
                            ncol=3, nrow =2)

ggsave("./Figures/QualityCriticalComponents.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

#Combined plots for Critical files Ratio #########################################################################
productivityPlot <- createPlotContinuos(egiData, "productivity", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Productivity", "deepskyblue", 
                                        "Productivity", "Ratio Critical Files", 1, 1, 
                                        10, 10, label, FALSE)

SATPlot <- createPlotContinuos(egiData, "normalizedEffortSAT", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized SAT Support Effort", "deepskyblue", 
                               "Normalized SAT Support Effort", "Ratio Critical Files", 1, 1, 
                               1, 0.5, label, FALSE)

leadTimePlot <- createPlotContinuos(egiData, "normalizedGenLeadTime", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized Lead Time", "deepskyblue", 
                                    "Normalized Lead Time", "Ratio Critical Files", 1, 1,
                                    1, 0.5, label, FALSE)

combinedPlot <- arrangeGrob(productivityPlot, SATPlot, leadTimePlot,
                            ncol=3, nrow =1)

ggsave("./Figures/GeneralCriticalFiles.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

positiveVerdictsPlot <- createPlotContinuos(egiData, "normalizedPositiveVerdicts", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized Positive Verdicts", "deepskyblue", 
                                            "Normalized Number of Positive Verdicts", "Ratio Critical Files", 1, 1,
                                            1, 1, label, FALSE)

negativeVerdictsPlot <- createPlotContinuos(egiData, "normalizedNegativeVerdicts", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized Negative Verdicts", "deepskyblue", 
                                            "Normalized Number of Negative Verdicts", "Ratio Critical Files", 1, 1, 
                                            1, 5, label, FALSE)

ratioLOCPlot <- createPlotContinuos(egiData, "normalizedRatioLOC", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized Ratio Submitted/Abandoned LOC", "deepskyblue", 
                                    "Normalized Ratio Submitted/Abandoned LOC", "Ratio Critical Files", 1, 1, 
                                    1, 1, label, FALSE)

technicalDebtPlot <- createPlotContinuos(egiData, "normalizedTechnicalDebt", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized Technical Debt", "deepskyblue", 
                                         "Normalized Technical Debt", "Number Critical Components", 1, 1,
                                         1, 10, label, FALSE)


duplicatedCodePlot <- createPlotContinuos(egiData, "normalizedDuplicatedCode", "ratioComplexFiles", "ID", 1.2, "Ratio Critical Files x Normalized Duplicated Code", "deepskyblue", 
                                          "Normalized Duplicated Code", "Ratio Critical Files", 1, 1,
                                          1, 20, label, FALSE)

combinedPlot <- arrangeGrob(positiveVerdictsPlot, negativeVerdictsPlot,
                            technicalDebtPlot, ratioLOCPlot, duplicatedCodePlot, 
                            ncol=3, nrow =2)

ggsave("./Figures/QualityCriticalFiles.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

#Combined plots for Critical LOC Ratio#########################################################################
productivityPlot <- createPlotContinuos(egiData, "productivity", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Productivity", "deepskyblue", 
                                        "Productivity", "Ratio Critical LOC", 1, 1, 
                                        10, 10, label, FALSE)

SATPlot <- createPlotContinuos(egiData, "normalizedEffortSAT", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized SAT Support Effort", "deepskyblue", 
                               "Normalized SAT Support Effort", "Ratio Critical LOC", 1, 1, 
                               1, 0.5, label, FALSE)

leadTimePlot <- createPlotContinuos(egiData, "normalizedGenLeadTime", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized Lead Time", "deepskyblue", 
                                    "Normalized Lead Time", "Ratio Critical LOC", 1, 1,
                                    1, 0.5, label, FALSE)

combinedPlot <- arrangeGrob(productivityPlot, SATPlot, leadTimePlot,
                            ncol=3, nrow =1)

ggsave("./Figures/GeneralCriticalLOC.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

positiveVerdictsPlot <- createPlotContinuos(egiData, "normalizedPositiveVerdicts", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized Positive Verdicts", "deepskyblue", 
                                            "Normalized Number of Positive Verdicts", "Ratio Critical LOC", 1, 1,
                                            1, 1, label, FALSE)

negativeVerdictsPlot <- createPlotContinuos(egiData, "normalizedNegativeVerdicts", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized Negative Verdicts", "deepskyblue", 
                                            "Normalized Number of Negative Verdicts", "Ratio Critical LOC", 1, 1, 
                                            1, 5, label, FALSE)

ratioLOCPlot <- createPlotContinuos(egiData, "normalizedRatioLOC", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized Ratio Submitted/Abandoned LOC", "deepskyblue", 
                                    "Normalized Ratio Submitted/Abandoned LOC", "Ratio Critical LOC", 1, 1, 
                                    1, 1, label, FALSE)

technicalDebtPlot <- createPlotContinuos(egiData, "normalizedTechnicalDebt", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized Technical Debt", "deepskyblue", 
                                         "Normalized Technical Debt", "Number Critical LOC", 1, 1,
                                         1, 10, label, FALSE)


duplicatedCodePlot <- createPlotContinuos(egiData, "normalizedDuplicatedCode", "ratioComplexLOC", "ID", 1.2, "Ratio Critical LOC x Normalized Duplicated Code", "deepskyblue", 
                                          "Normalized Duplicated Code", "Ratio Critical LOC", 1, 1,
                                          1, 20, label, FALSE)

combinedPlot <- arrangeGrob(positiveVerdictsPlot, negativeVerdictsPlot,
                            technicalDebtPlot, ratioLOCPlot, duplicatedCodePlot, 
                            ncol=3, nrow =2)

ggsave("./Figures/QualityCriticalLOC.pdf", combinedPlot, width = 22, height = 11, dpi = 120)

