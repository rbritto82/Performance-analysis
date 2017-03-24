library(xlsx)
library(dplyr)

#reading xlxs, removing noise from the original file and changing the name of the column lead time
dataSDP <- tbl_df(read.xlsx("./master_v3.0.xlsx", sheetIndex=5))
#dataSDP <- filter(dataSDP, complete.cases(dataSDP$Location))
#names(dataSDP)[5] <- "LeadTime"

# Multiple Linear Regression Example 
fit <- lm(Accumulated ~ Productivity, data=dataSDP)
summary(fit) # show results

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)

# K-fold cross-validation
library(DAAG)
cv.lm(df=mydata, fit, m=3) # 3 fold cross-validation