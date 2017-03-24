#LOWESS Plots
ggplot(egiRegression, aes(x = end, y = productivity, label = planID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="End date", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$end), max(egiRegression$end))) +geom_label_repel()
 ggsave("./Figures/RQ2/LOWESS_productivity.pdf")
 
ggplot(egiRegression, aes(x = end, y = normalizedTeamAutonomy, label = planID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="End Date", y="Team autonomy") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$end), max(egiRegression$end))) +geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy.pdf")

ggplot(egiRegression, aes(x = start, y = daysExperience, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Start Date", y="Days of experience") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$start), max(egiRegression$start))) +geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_exp_start.pdf")

ggplot(egiRegression, aes(x = start, y = teamFamiliarity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Start Date", y="Team familiarity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$start), max(egiRegression$start))) +geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_familiarity_start.pdf")

ggplot(egiRegression, aes(x = daysExperience, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Days of experience", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$daysExperience), 30))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_exp.pdf")

ggplot(egiRegression, aes(x = productFamiliarity, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Product familiarity", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$productFamiliarity), 50))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_prodfamiliarity.pdf")

ggplot(egiRegression, aes(x = productFamiliarity, y = normalizedTeamAutonomy, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Product familiarity", y="Team autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$productFamiliarity), 50))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy_prodfamiliarity.pdf")

ggplot(egiRegression, aes(x = numDevelopers, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Number of developers", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$numDevelopers), 1))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_numdev.pdf")

ggplot(egiRegression, aes(x = numDevelopers, y = normalizedTeamAutonomy, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Number of developers", y="Team autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$numDevelopers), 1))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy_numdev.pdf")

ggplot(egiRegression, aes(x = taskGlobaDistance, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Global distances", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$taskGlobaDistance), 0.1))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_globaldist.pdf")

ggplot(egiRegression, aes(x = taskGlobaDistance, y = normalizedTeamAutonomy, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Global distance", y="Team autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$taskGlobaDistance), 0.1))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy_globaldist.pdf")

ggplot(egiRegression, aes(x = teamFamiliarity, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Team familiarity", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$teamFamiliarity), 30))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_familiarity.pdf")

ggplot(egiRegression, aes(x = teamFamiliarity, y = normalizedTeamAutonomy, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Team familiarity", y="Team autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$teamFamiliarity), 30))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy_familiarity.pdf")

ggplot(egiRegression, aes(x = locRatio, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="LOC/Complexity", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$locRatio), 30))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_ratio.pdf")

ggplot(egiRegression, aes(x = locRatio, y = normalizedTeamAutonomy, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="LOC/Complexity", y="Team autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$locRatio), 30))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy_ratio.pdf")

ggplot(egiRegression, aes(x = end, y = locRatio, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="End Date", y="LOC/Complexity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$end), max(egiRegression$end)))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_locRatio_end.pdf")

ggplot(egiRegression, aes(x = end, y = complexityPoints, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="End Date", y="Complexity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$end), max(egiRegression$end)))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_Complexity_end.pdf")

ggplot(egiRegression, aes(x = complexityPoints, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Complexity", y="Team productivity") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$complexityPoints), 5))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_complexity.pdf")

ggplot(egiRegression, aes(x = complexityPoints, y = autoProd, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Complexity", y="Productivity/Autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$autoProd), 30))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autoProd_complexity.pdf")

ggplot(egiRegression, aes(x = threshold, y = productivity, label = planID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Number of developers/Complexity", y="Team productivity") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$threshold), 0.02))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_threshold.pdf")

ggplot(egiRegression, aes(x = threshold, y = productivity, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Number of developers/Complexity", y="Team productivity") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$threshold), 0.02))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_productivity_threshold.pdf")

ggplot(egiRegression, aes(x = threshold, y = normalizedTeamAutonomy, label = ID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="Number of developers/Complexity", y="Team autonomy") + 
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_continuous(breaks=seq(0, max(egiRegression$threshold), 0.02))+
  geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_autonomy_threshold.pdf")

ggplot(egiRegression, aes(x = end, y = expProductivity, label = planID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="End date", y="Productivity/Days of experience") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$end), max(egiRegression$end))) +geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_expProductivity.pdf")

ggplot(egiRegression, aes(x = end, y = expAutonomy, label = planID)) + 
  geom_point() + 
  stat_smooth(method = "loess")+
  labs(x="End date", y="Autonomy/Days of experience") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_date(labels = date_format("%y/%m"), date_breaks = "1 month", limits = c(min(egiRegression$end), max(egiRegression$end))) +geom_label_repel()
ggsave("./Figures/RQ2/LOWESS_expAutonomy.pdf")

#-----------------------------------------------------------------------------------------------------------------------------------

#descriptive for the scaling effect
summary(egiThreshold[egiThreshold$refinedTeamMaturity == "Mature teams",])
summary(egiThreshold[egiThreshold$refinedTeamMaturity == "Immature teams",])

#Productivity---------
egiScalingProductivityBoxplot <- ggplot(egiRegression, aes(x = typeScaling, y = productivity)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Productivity in complexity \npoints per 1000 hours") + 
  scale_x_discrete(name = "Number of developers/complexity points (High > 0.2)") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Figures/RQ2/Scaling_Productivity_Boxplot.pdf", egiScalingProductivityBoxplot)

#testing differences between different team maturity groups for productivity
wilcox <- wilcox.test(productivity ~ typeScaling, data = egiRegression, alternative = "two.sided")

effectSize <- AMeasure(egiRegression$productivity[egiRegression$typeScaling == "Low"], 
                       egiRegression$productivity[egiRegression$typeScaling == "High"])
#kruskal <- kruskal.test(productivity ~ refinedTeamMaturity, data = egiProductivityM)
#posthoc <- posthoc.kruskal.dunn.test(productivity ~ refinedTeamMaturity, data = egiProductivity, p.adjust.method="bonferroni")

#Calculating descriptive statistics groped by maturity
egiScalingProductivity <- group_by(egiRegression, typeScaling)

egiScalingProductivitySummary <- summarise(egiScalingProductivity, observations = length(typeScaling), 
                                            mean = mean(productivity,  na.rm = TRUE),
                                            median = median(productivity,  na.rm = TRUE),
                                            min = min(productivity,  na.rm = TRUE),
                                            max = max(productivity,  na.rm = TRUE),
                                            standardDeviation = sd(productivity,  na.rm = TRUE))
#Exporting results to an excel spreadsheet
write.xlsx(egiMaturityProductivitySummary, file = paste("./Results/RQ2/Scaling_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Scaling summary")
write.xlsx(wilcox$p.value, file = paste("./Results/RQ2/Scaling_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcoxon", append = TRUE)
write.xlsx(effectSize, file = paste("./Results/RQ2/Scaling_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Effect Size", append = TRUE)

#Productivity---------
egiScalingProductivityBoxplot <- ggplot(egiRegression, aes(x = typeScaling, y = productivity)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Productivity in complexity \npoints per 1000 hours") + 
  scale_x_discrete(name = "Number of developers/complexity points (High > 0.2)") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Figures/RQ2/Scaling_Productivity_Boxplot.pdf", egiScalingProductivityBoxplot)

#testing differences between different team maturity groups for productivity
wilcox <- wilcox.test(productivity ~ typeScaling, data = egiRegression, alternative = "two.sided")

effectSize <- AMeasure(egiRegression$productivity[egiRegression$typeScaling == "Low"], 
                       egiRegression$productivity[egiRegression$typeScaling == "High"])
#kruskal <- kruskal.test(productivity ~ refinedTeamMaturity, data = egiProductivityM)
#posthoc <- posthoc.kruskal.dunn.test(productivity ~ refinedTeamMaturity, data = egiProductivity, p.adjust.method="bonferroni")

#Calculating descriptive statistics groped by maturity
egiScalingProductivity <- group_by(egiRegression, typeScaling)

egiScalingProductivitySummary <- summarise(egiScalingProductivity, observations = length(typeScaling), 
                                           mean = mean(productivity,  na.rm = TRUE),
                                           median = median(productivity,  na.rm = TRUE),
                                           min = min(productivity,  na.rm = TRUE),
                                           max = max(productivity,  na.rm = TRUE),
                                           standardDeviation = sd(productivity,  na.rm = TRUE))
#Exporting results to an excel spreadsheet
write.xlsx(egiScalingProductivitySummary, file = paste("./Results/RQ2/Scaling_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Scaling summary")
write.xlsx(wilcox$p.value, file = paste("./Results/RQ2/Scaling_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcoxon", append = TRUE)
write.xlsx(effectSize, file = paste("./Results/RQ2/Scaling_Productivity_", Sys.Date(), ".xlsx", sep = ""), sheetName="Effect Size", append = TRUE)

#Autonomy---------
egiScalingAutonomyBoxplot <- ggplot(egiRegression, aes(x = typeScaling, y = normalizedTeamAutonomy)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Autonomy") + 
  scale_x_discrete(name = "Number of developers/complexity points (High > 0.2)") +
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + 
  theme(axis.text.x = element_text())
ggsave("./Figures/RQ2/Scaling_Autonomy_Boxplot.pdf", egiScalingAutonomyBoxplot)

#testing differences between different team maturity groups for productivity
wilcox <- wilcox.test(normalizedTeamAutonomy ~ typeScaling, data = egiRegression, alternative = "two.sided")

effectSize <- AMeasure(egiRegression$normalizedTeamAutonomy[egiRegression$typeScaling == "Low"], 
                       egiRegression$normalizedTeamAutonomy[egiRegression$typeScaling == "High"])
#kruskal <- kruskal.test(productivity ~ refinedTeamMaturity, data = egiProductivityM)
#posthoc <- posthoc.kruskal.dunn.test(productivity ~ refinedTeamMaturity, data = egiProductivity, p.adjust.method="bonferroni")

#Calculating descriptive statistics groped by maturity
egiScalingAutonomy <- group_by(egiRegression, typeScaling)

egiScalingAutonomySummary <- summarise(egiScalingAutonomy, observations = length(typeScaling), 
                                           mean = mean(normalizedTeamAutonomy,  na.rm = TRUE),
                                           median = median(normalizedTeamAutonomy,  na.rm = TRUE),
                                           min = min(normalizedTeamAutonomy,  na.rm = TRUE),
                                           max = max(normalizedTeamAutonomy,  na.rm = TRUE),
                                           standardDeviation = sd(normalizedTeamAutonomy,  na.rm = TRUE))
#Exporting results to an excel spreadsheet
write.xlsx(egiScalingAutonomySummary, file = paste("./Results/RQ2/Scaling_Autonomy_", Sys.Date(), ".xlsx", sep = ""), sheetName="Scaling summary")
write.xlsx(wilcox$p.value, file = paste("./Results/RQ2/Scaling_Autonomy_", Sys.Date(), ".xlsx", sep = ""), sheetName="Wilcoxon", append = TRUE)
write.xlsx(effectSize, file = paste("./Results/RQ2/Scaling_Autonomy_", Sys.Date(), ".xlsx", sep = ""), sheetName="Effect Size", append = TRUE)


#Calculating the correlation among the numerical variables
cors <- rcorr(as.matrix(egiRegression[with(egiRegression, c("productivity", "normalizedTeamAutonomy", "autoProd", "daysExperience", 
                                                            "teamFamiliarity", "complexityPoints", "locRatio", "productFamiliarity",
                                                            "numDevelopers", "taskGlobaDistance"))]), type = "spearman")

#Regression

productivity <- lm(productivity~numDevelopers*complexityPoints+taskGlobaDistance, data = egiRegression)
summary(productivity)
bgtest(productivity) #Breusch-Godfrey for serial correlation
bptest(productivity) #Breusch-Pagan for heteroskedasticity
dwtest(productivity) #Durbin-Watson for auto correlation
vif(productivity)

autonomy <- lm(normalizedTeamAutonomy~daysExperience, data = egiRegression)
summary(autonomy)
bgtest(autonomy) #Breusch-Godfrey for serial correlation
bptest(autonomy) #Breusch-Pagan for heteroskedasticity

autonomy <- lm(normalizedTeamAutonomy~daysExperience, data = egiRegression)
summary(autonomy)
bgtest(autonomy) #Breusch-Godfrey for serial correlation
bptest(autonomy) #Breusch-Pagan for heteroskedasticity

#-----------------------------------------------------------------------------------------------------------------------
#PCA

factorAnalysis <- prcomp(egiRegression[with(egiRegression, c("daysExperience", "teamFamiliarity", "complexityPoints", "locRatio", 
                                                           "productFamiliarity", "numDevelopers", "taskScaling", "numSites", "taskGlobaDistance"))],
                      factors = 4, rotation = "varimax")

print(factorAnalysis, digits = 2, cutoff = .2, sort = TRUE)
plot(factorAnalysis, type="l")
summary(factorAnalysis)
#colnames(fit.3$loadings)<-c("Endurance","Strength","Hand-Eye")
 #----------------------------------------------------------------------------------------------------------------------- 
#productivity line
outputPlot <- ggplot(aes(y = productivity, x = daysExperience, label = ID), data = egiRegression, stat="identity") +
  geom_point(size=2, color = "deepskyblue") + #add points for each observation
  #geom_line(size=1.5, color = "deepskyblue")+
  labs(x="Experience in days", y="Productivity (blue points) and Mentoring (red points)") + #change the labels
  theme(legend.position="bottom", legend.direction="horizontal",legend.title = element_blank()) + #set legend theme
  scale_y_continuous(breaks=seq(0, 1, 0.05)) +
  scale_x_continuous(breaks=seq(0, 600, 20)) +
  
  #SAT support points
  geom_point(size=2, color = "red", aes(y = normalizedEffortSAT, x = daysExperience), data = egiRegression)
  
#Line connecting productivity and SAT hours
  for (i in 1:length(plotList)) {
    outputPlot <- outputPlot + geom_line(aes(y = y, x = x), data = plotList[[i]])
  }
  
  outputPlot <- outputPlot + geom_label_repel()

ggsave("./Figures/RQ2/Productivity.pdf")








