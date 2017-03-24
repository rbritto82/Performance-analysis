library(xlsx)
library(dplyr)

#reading file and adding a colum with submited lines of code
egiPersonel <- tbl_df(read.xlsx("./master_2.7.xlsx", sheetIndex=5, colIndex = c(3, 4, 7, 8)))

#changing some names of the spreadsheet
names(egiPersonel) <- c("idNumber", "name", "team", "role")

#removing empty rows
egiPersonel <- filter(egiPersonel, complete.cases(egiPersonel$team))

#removing duplications
egiPersonel <- egiPersonel[!duplicated(egiPersonel$name), ]

#selecting the staff in India
egiPersonel <- egiPersonel[grep("EGI.", egiPersonel$team), ]

#Selecting only developers and sp-writers
egiPersonel <- egiPersonel[grep("Architect|Technical|Configuration", egiPersonel$role, invert = T), ]

#ordering the final result by team and role
egiPersonel <- egiPersonel[with(egiPersonel, order(team, role)),]

#Writing the result in a spreadsheet
write.xlsx(egiPersonel, "list_developers.xlsx")
