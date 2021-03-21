#install.packages("tidyverse")
#install.packages("readxl")
library("tidyverse")
library(readxl)

App_2020_2021 <- read_excel("./datasets/2020-2021-app-data-by-school-q4.xls", skip=5)
App_2020_2021['Year'] = 2020

App_2019_2020 <- read_excel("./datasets/2019-2020-app-data-by-school-q4.xls", skip=5)
App_2019_2020['Year'] = 2019

App_2018_2019 <- read_excel("./datasets/2018-2019-app-data-by-school-q4.xls", skip=5)
App_2018_2019['Year'] = 2018

App_2017_2018 <- read_excel("./datasets/2017_2018App_Data_by_School_Q4.xls", skip=5)
App_2017_2018['Year'] = 2017

App_2016_2017 <- read_excel("./datasets/2016_2017App_Data_by_School_Q4.xls", skip=5)
App_2016_2017['Year'] = 2016

App_2015_2016 <- read_excel("./datasets/2015_2016App_Data_by_School_Q4.xls", skip=5)
App_2015_2016['Year'] = 2015

App_2014_2015 <- read_excel("./datasets/AppDatabySchool20142015Q4.xls", skip=5)
App_2014_2015['Year'] = 2014

App_2013_2014 <- read_excel("./datasets/2013_14App_Data_by_SchoolQ4.xls", skip=5)
App_2013_2014 <- subset(App_2013_2014, select = -c(...10))
names(App_2013_2014)[10] <- "Independent Students...10"
App_2013_2014['Year'] = 2013

App_2012_2013 <- read_excel("./datasets/2012_13App_Data_by_SchoolQ4.xls", skip=5)
App_2012_2013['Year'] = 2012

App_2011_2012 <- read_excel("./datasets/AppDatabySchool20112012Qtr4.xls", skip=5)
App_2011_2012['Year'] = 2011

App_2010_2011 <- read_excel("./datasets/AppDataBySchool20102011Q4.xls", skip=5)
App_2010_2011['Year'] = 2010

App_2009_2010 <- read_excel("./datasets/AppDatabySchool20092010Qtr4.xls", skip=5)
App_2009_2010['Year'] = 2009

App_2008_2009 <- read_excel("./datasets/AppDatabySchool20082009Qtr4.xls", skip=5)
App_2008_2009['Year'] = 2008

App_2007_2008 <- read_excel("./datasets/AppDatabySchool20072008Qtr4.xls", skip=5)
App_2007_2008['Year'] = 2007

App_2006_2007 <- read_excel("./datasets/AppDatabySchool20062007Qtr4.xls", skip=5)
App_2006_2007['Year'] = 2006

App_Total <- rbind(App_2006_2007, App_2007_2008, App_2008_2009, App_2009_2010, App_2010_2011, App_2011_2012, App_2012_2013, App_2013_2014, App_2014_2015, App_2015_2016, App_2016_2017, App_2017_2018, App_2018_2019, App_2019_2020, App_2020_2021)
App_Total_Non_Covid <- rbind(App_2006_2007, App_2007_2008, App_2008_2009, App_2009_2010, App_2010_2011, App_2011_2012, App_2012_2013, App_2013_2014, App_2014_2015, App_2015_2016, App_2016_2017, App_2017_2018, App_2018_2019, App_2019_2020)
App_By_State_NonCovid <- aggregate(as.numeric(App_Total$`Award Year To Date Total`), by=list(State=App_Total$State), FUN=sum)
App_By_State_NonCovid$x <- App_By_State_NonCovid$x / 14
App_By_State_Covid <- aggregate(as.numeric(App_2020_2021$`Award Year To Date Total`), by=list(State=App_2020_2021$State), FUN=sum)

App_By_State_Diff_List <- (App_By_State_Covid$x - App_By_State_NonCovid$x)/(App_By_State_NonCovid$x)
App_By_State_Diff <- do.call(rbind, Map(data.frame, State=App_By_State_Covid$State, Diff=App_By_State_Diff_List))

Covid_Per_Capita <- read_excel("./datasets/Covid Cases per 100k.xlsx")

Covid_Vs_Diff <- merge(Covid_Per_Capita, App_By_State_Diff, by=c("State"))

plot(Covid_Vs_Diff$Count, Covid_Vs_Diff$Diff, main="FAFSA vs. Covid Cases per Capita", xlab="Covid Cases per 100k", ylab="Percent Change in Total Apps", col = "red", pch = 15)
abline(lm(Covid_Vs_Diff$Diff ~ Covid_Vs_Diff$Count), col = "blue")

# #1
# rm(list=ls())
# 
# n = 14
# preCovAvg = 26968264.214
# preCovSd = 0
# se = preCovSD / sqrt(n)
# postCovAvg = 0
# 
# t = (postCovAvg - preCovAvg) / se
# alpha = 0.05
# df = n - 1
# tCrit = qt(0.05, df, lower.tail = FALSE);
# 
# reject = tCrit < t

#2
#rm(list=ls())


# dataE <- read_excel("./datasets/e.xlsx", sheet = "Sheet1")
# linReg = lm(Count~Year, data = dataE)
# summary(linReg)
# 
# years <- 6:19
# 
# vec <- c()
# 
# for (val in years) {
#   vec <- c(vec, 838922 * val + 16481736)
# }
# 
# 
# vec_resid <- dataE$Count - vec
# sd_resid <- sd(vec_resid)
# sd_resid = sd_resid / sqrt(length(years))
# mean_resid = mean(vec_resid)
# 
# CI95LB = mean_resid - 1.96 * sd_resid
# CI95UB = mean_resid + 1.96 * sd_resid
#   
# covYear = 20
# covAppObs = 27548632
# covAppExp = 838922 * covYear + 16481736
# covResid = covAppObs - covAppExp
# 
# trend = (covResid > CI95LB) && (covResid < CI95UB)

Debt_2020 <- read_excel("./datasets/Portfolio-by-Location.xls", skip=5)
Debt_2020$Average_Debt <- Debt_2020$`Balance (in billions)` / Debt_2020$`Borrowers (in thousands)` * 10^6
Debt_2020 <- head(Debt_2020, -4)
Debt_2020 <- Debt_2020[-c(40),]
Debt_2020 <- Debt_2020[-c(9),]

Debt_2019 <- read_excel("./datasets/TICAS.org-State-Data-2019.xlsx")
Debt_2019 <- Debt_2019[order(Debt_2019$State),]
Debt_2019 <- Debt_2019[-c(9),]

Debt_Pct_Change <- (Debt_2020$Average_Debt - Debt_2019$`Average Debt of Graduates (2018-19)`) / Debt_2019$`Average Debt of Graduates (2018-19)`

Debt_2020_2019 <- do.call(rbind, Map(data.frame, State=Debt_2020$Location, Avg2019=Debt_2019$`Average Debt of Graduates (2018-19)`, Avg2020=Debt_2020$Average_Debt, PctChange=Debt_Pct_Change))

plot(Covid_Vs_Diff$Count, Debt_Pct_Change, main = "Student Debt vs. Covid Cases per Capita" ,xlab="Covid Cases per 100k", ylab="Percent Change in Average Student Debt", col = "red", pch = 15)
abline(lm(Debt_Pct_Change ~ Covid_Vs_Diff$Count), col = "blue")

