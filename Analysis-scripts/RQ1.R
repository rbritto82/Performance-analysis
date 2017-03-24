#Productivity AND Maturity-----------------------------------------------------------------------------------------
egiMaturityProductivityBoxplot <- ggplot(egiProductivityM, aes(x = refinedTeamMaturity, y = productivity)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Productivity in complexity \npoints per 1000 hours",
                     breaks = seq(0, max(egiProductivityM$productivity), 5), limits=c(0, max(egiProductivityM$productivity))) + 
  scale_x_discrete(name = "Maturity") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Figures/RQ1/Maturity_Productivity_Boxplot.pdf", egiMaturityProductivityBoxplot)

#testing differences between different team maturity groups for productivity
wilcox <- wilcox.test(productivity ~ refinedTeamMaturity, data = egiProductivityM, alternative = "two.sided")

effectSize <- AMeasure(egiProductivityM$productivity[egiProductivityM$refinedTeamMaturity == "Immature teams"], 
                       egiProductivityM$productivity[egiProductivityM$refinedTeamMaturity == "Mature teams"])
#kruskal <- kruskal.test(productivity ~ refinedTeamMaturity, data = egiProductivityM)
#posthoc <- posthoc.kruskal.dunn.test(productivity ~ refinedTeamMaturity, data = egiProductivity, p.adjust.method="bonferroni")

#Calculating descriptive statistics groped by maturity
egiMaturityProductivity <- group_by(egiProductivityM, refinedTeamMaturity)

egiMaturityProductivitySummary <- summarise(egiMaturityProductivity, observations = length(refinedTeamMaturity), 
                                    mean = mean(productivity,  na.rm = TRUE),
                                    median = median(productivity,  na.rm = TRUE),
                                    min = min(productivity,  na.rm = TRUE),
                                    max = max(productivity,  na.rm = TRUE),
                                    standardDeviation = sd(productivity,  na.rm = TRUE))
#Exporting results to an excel spreadsheet
write.xlsx(egiMaturityProductivitySummary, file = paste("./Results/RQ1/Maturity_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Maturity summary")
write.xlsx(wilcox$p.value, file = paste("./Results/RQ1/Maturity_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcoxon", append = TRUE)
write.xlsx(effectSize, file = paste("./Results/RQ1/Maturity_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Effect Size", append = TRUE)


#Support hours AND Maturity-----------------------------------------------------------------------------------------
egiMaturitySupportBoxplot <- ggplot(egiSupportM, aes(x = refinedTeamMaturity, y = normalizedTeamAutonomy)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Team autonomy",
                     breaks = seq(0, max(egiSupportM$normalizedTeamAutonomy), 0.5), limits=c(0, max(egiSupportM$normalizedTeamAutonomy))) + 
  scale_x_discrete(name = "Maturity") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Figures/RQ1/Maturity_Support_Boxplot.pdf", egiMaturitySupportBoxplot)

#testing differences between different team maturity groups for productivity
wilcox <- wilcox.test(normalizedTeamAutonomy ~ refinedTeamMaturity, data = egiSupportM, alternative = "two.sided")
effectSize <- AMeasure(egiSupportM$normalizedTeamAutonomy[egiSupportM$refinedTeamMaturity == "Immature teams"], 
                       egiSupportM$normalizedTeamAutonomy[egiSupportM$refinedTeamMaturity == "Mature teams"])

#Calculating descriptive statistics groped by maturity
egiMaturitySupport <- group_by(egiSupportM, refinedTeamMaturity)

egiMaturitySupportSummary <- summarise(egiMaturitySupport, observations = length(refinedTeamMaturity), 
                                    mean = mean(normalizedTeamAutonomy,  na.rm = TRUE),
                                    median = median(normalizedTeamAutonomy,  na.rm = TRUE),
                                    min = min(normalizedTeamAutonomy,  na.rm = TRUE),
                                    max = max(normalizedTeamAutonomy,  na.rm = TRUE),
                                    standardDeviation = sd(normalizedTeamAutonomy,  na.rm = TRUE))

#Exporting results to an excel spreadsheet
write.xlsx(egiMaturitySupportSummary, file = paste("./Results/RQ1/Maturity_Support_", Sys.Date(), ".xlsx", sep = ""), sheetName="Maturity summary")
write.xlsx(wilcox$p.value, file = paste("./Results/RQ1/Maturity_Support_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcox", append = TRUE)
write.xlsx(effectSize, file = paste("./Results/RQ1/Maturity_Support_", Sys.Date(), ".xlsx", sep = ""), sheetName="Effect size", append = TRUE)