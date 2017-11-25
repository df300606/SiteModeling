# Load Libraries
library(data.table)
library(dplyr)
library(forcats)
library(qdapTools)
library(zoo)
library(lubridate)

# Import Raw Data
QuickDesignTimes <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/QuickDesignTimesData.csv")
QuickDesignRequest <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/QDRequests.csv")
SiteDesignPriority <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/SiteDesignPriority.csv")

# Delete information from Oppty ID = 0063200001nG0yx
QuickDesignTimes <- QuickDesignTimes[QuickDesignTimes$Opportunity.ID!="0063200001nG0yx",]

# Create order and relate to data
OldValue_order = data.frame(Old.Value = c("","Open","In Progress","System Declined","Complete"), y = c(4,0,1,2,3))
QuickDesignTimes$OV_order = lookup(QuickDesignTimes$Old.Value, OldValue_order)

# Arrange the data.frame in desired order
QuickDesignTimes = QuickDesignTimes[with(QuickDesignTimes, order(QuickDesignTimes$Opportunity.ID, as.POSIXct(QuickDesignTimes$Edit.Date, format = "%m/%d/%Y %H:%M"), OV_order)),]

# Apply logic to calculate DesignRequested, StartTime & EndTime                                                                                                                                            
QuickDesignTimes$TimeRequested <- ifelse(QuickDesignTimes$Old.Value=="Open",as.character(QuickDesignTimes$Site.Design.Requested, format = '%Y/%m/%d %H:%M'),NA)
QuickDesignTimes$EndTime <- ifelse(((QuickDesignTimes$New.Value=="Complete"|QuickDesignTimes$New.Value=="System Declined"|QuickDesignTimes$New.Value=="Software Error")&(QuickDesignTimes$Old.Value=="In Progress"|QuickDesignTimes$Old.Value=="Open")),as.character(QuickDesignTimes$Edit.Date, format = '%Y/%m/%d %H:%M'),NA)
QuickDesignTimes$StartTime <- ifelse(((QuickDesignTimes$Old.Value=="Open"|QuickDesignTimes$Old.Value==""|QuickDesignTimes$Old.Value=="Complete"|QuickDesignTimes$Old.Value=="System Declined"|QuickDesignTimes$Old.Value=="Software Error")&QuickDesignTimes$New.Value=="In Progress"),as.character(QuickDesignTimes$Edit.Date, format = '%Y/%m/%d %H:%M'),
                                     ifelse((QuickDesignTimes$TimeRequested>0&QuickDesignTimes$EndTime>0),as.character(QuickDesignTimes$EndTime, format = '%Y/%m/%d %H:%M'),NA))

QuickDesignTimes <- QuickDesignTimes[,c(1:11,13,12)]

# Identify Breaks in Process
QuickDesignTimes$BreaksInProcess <- ifelse((QuickDesignTimes$Old.Value=="Open"&(QuickDesignTimes$New.Value=="System Declined"|QuickDesignTimes$New.Value=="Software Error"|QuickDesignTimes$New.Value=="Complete")),1,0)

# Determine if project was "System Declined" or "Completed"
QuickDesignTimes$SiteDesignStatus <- ifelse(QuickDesignTimes$EndTime>0,as.character(QuickDesignTimes$New.Value),"")
QuickDesignTimes$SiteDesignStatus <- zoo::na.locf.default(QuickDesignTimes$SiteDesignStatus, fromLast = TRUE)

# Merge QuickDesignTimes into respective row
QuickDesignTimes <- QuickDesignTimes %>%
  mutate(check = as.numeric(
    as.character(fct_collapse(New.Value, 
                              `1` = "In Progress",
                              `2` = c("Complete", "System Declined", "Software Error"))))) %>%
  group_by(Opportunity.ID) %>%
  mutate(group = cumsum(c(TRUE, diff(check) != 1))) %>%
  group_by(Opportunity.ID, group) %>%
  mutate_at(vars(StartTime:EndTime),
            funs(if(identical(check[1:2], as.numeric(1:2))){
              sort(., na.last = TRUE)} else{.}
            )) %>%
  select(-check, -group)

# Find Projects that have timed out
QuickDesignTimes$TimedOut <- ifelse((QuickDesignTimes$Old.Value=="In Progress"&QuickDesignTimes$New.Value=="Open"),1,0)
QuickDesignTimes$TimedOut <- with(QuickDesignTimes, ifelse((QuickDesignTimes$Edited.By %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling","Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson")&QuickDesignTimes$TimedOut==1),1,
                                                           ifelse(((grepl("Dimension", QuickDesignTimes$Edited.By))&QuickDesignTimes$TimedOut==1),1,0)))
QuickDesignTimes$TimedOut <- ifelse((QuickDesignTimes$Opportunity.ID %in% QuickDesignTimes$Opportunity.ID[QuickDesignTimes$TimedOut>0]),1,0)

# Create corrected EditedBy column
QuickDesignTimes$EditedBy <- QuickDesignTimes$Edited.By

# Export Timed Out Report
TimedOutReport <- QuickDesignTimes[QuickDesignTimes$TimedOut==1,]

TimedOutNewValue_order = data.frame(New.Value = c("Open","In Progress","Complete","System Declined",""), y = c(0,1,2,3,4))
TimedOutReport$TimedOutNewValue_order = lookup(TimedOutReport$New.Value, TimedOutNewValue_order)

TimedOutReport = TimedOutReport[with(TimedOutReport, order(TimedOutReport$Opportunity.ID, as.POSIXct(TimedOutReport$Edit.Date, format = "%m/%d/%Y %H:%M"), TimedOutNewValue_order)),]

TimedOutReport$TimedOutModeler <- ifelse((TimedOutReport$Old.Value=="In Progress"&TimedOutReport$New.Value=="Open"),as.character(TimedOutReport$EditedBy),"")
TimedOutReport$TimedOutModeler <- with(TimedOutReport, ifelse(TimedOutReport$TimedOutModeler %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling","Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson"),TimedOutReport$TimedOutModeler,
                                                              ifelse((grepl("Dimension", TimedOutReport$TimedOutModeler)),TimedOutReport$TimedOutModeler,"")))

TimedOutReport$ProjectStarted <- ifelse(TimedOutReport$New.Value=="In Progress",as.character(TimedOutReport$Edit.Date),NA)
TimedOutReport$ProjectEntersQueue <- ifelse((TimedOutReport$Old.Value=="In Progress"&TimedOutReport$New.Value=="Open"),as.character(TimedOutReport$Edit.Date),NA)

TimedOutReport <- TimedOutReport %>%
  mutate(check = as.numeric(
    as.character(fct_collapse(New.Value, 
                              `1` = "In Progress",
                              `2` = "Open")))) %>%
  group_by(Opportunity.ID) %>%
  mutate(group = cumsum(c(TRUE, diff(check) != 1))) %>%
  group_by(Opportunity.ID, group) %>%
  mutate_at(vars(ProjectStarted:ProjectEntersQueue),
            funs(if(identical(check[1:2], as.numeric(1:2))){
              sort(., na.last = TRUE)} else{.}
            )) %>%
  select(-check, -group)

TimedOutReport$ProjectStarted <- as.POSIXct(TimedOutReport$ProjectStarted, format = '%m/%d/%Y %H:%M')
TimedOutReport$ProjectEntersQueue <- as.POSIXct(TimedOutReport$ProjectEntersQueue, format = '%m/%d/%Y %H:%M')
TimedOutReport$Difference <- TimedOutReport$ProjectEntersQueue - TimedOutReport$ProjectStarted
TimedOutReport$TimedOut <- ifelse(TimedOutReport$Difference>=15,TimedOutReport$TimedOut==1,0)

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
TimedOutReport$TimedOutModeler <- shift(TimedOutReport$TimedOutModeler, 1)

TimedOutReport$CompletedModeler <- ifelse((TimedOutReport$New.Value=="Complete"|TimedOutReport$New.Value=="System Declined"|TimedOutReport$New.Value=="Software Error"),as.character(TimedOutReport$EditedBy),NA)
TimedOutReport <- TimedOutReport[,-c(1,6,8,11,19)]
TimedOutReport <- TimedOutReport[,c(1:7,14,8:13,15:19)]
TimedOutReport$Team <- with(TimedOutReport, ifelse(TimedOutReport$TimedOutModeler %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling","Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson","Kevin Dematteo"),"Denver",
                                                   ifelse((grepl("Dimension", TimedOutReport$TimedOutModeler)),"DI","")))

TimedOutReport$Year <- ifelse(!is.na(TimedOutReport$Team),as.numeric(format(TimedOutReport$ProjectStarted, '%Y')),NA)

write.csv(TimedOutReport, "C:/Users/david.feldman/Desktop/DataWarehouse/TimedOutReport.csv", row.names = FALSE)

# Removal of rows with no time data
QuickDesignTimes$CompleteSet <- ifelse((is.na(QuickDesignTimes$TimeRequested)&is.na(QuickDesignTimes$StartTime)&is.na(QuickDesignTimes$EndTime)),1,0)
QuickDesignTimes <- QuickDesignTimes[QuickDesignTimes$CompleteSet==0,]

# Remove rows with Complete or System Declined in Old.Value
QuickDesignTimes <- QuickDesignTimes[(QuickDesignTimes$Old.Value=="Open"|QuickDesignTimes$Old.Value=="In Progress"),]

# Redefine the CompleteSet variable
QuickDesignTimes$CompleteSet <- ifelse((!is.na(QuickDesignTimes$TimeRequested)&!is.na(QuickDesignTimes$StartTime)&!is.na(QuickDesignTimes$EndTime)&QuickDesignTimes$group==1),1,0)
     
# split QuickDesignData frame
QuickDesignTimes2 <- QuickDesignTimes[(QuickDesignTimes$CompleteSet==0|is.na(QuickDesignTimes$CompleteSet)),]
QuickDesignTimes <- QuickDesignTimes[QuickDesignTimes$CompleteSet==1,]
          
QuickDesignTimes2 <- QuickDesignTimes2[!is.na(QuickDesignTimes2$Opportunity.ID),]

setDT(QuickDesignTimes2)[,RunningCount:=1:.N, by = c("Opportunity.ID", "New.Value")]
QuickDesignTimes2 <- QuickDesignTimes2[!(QuickDesignTimes2$New.Value=="In Progress"&QuickDesignTimes2$RunningCount>1),]

# Merge QuickDesignTimes into respective row
QuickDesignTimes2 <- QuickDesignTimes2 %>%
  mutate(check = as.numeric(
    as.character(fct_collapse(New.Value, 
                              `1` = "In Progress",
                              `2` = c("Complete", "System Declined", "Software Error"))))) %>%
  group_by(Opportunity.ID) %>%
  mutate(group = cumsum(c(TRUE, diff(check) != 1))) %>%
  group_by(Opportunity.ID, group) %>%
  mutate_at(vars(StartTime:EndTime),
            funs(if(identical(check[1:2], as.numeric(1:2))){
              sort(., na.last = TRUE)} else{.}
            )) %>%
  select(-check, -group)

# Clean QuickDesignTimes datasets and rbind together
QuickDesignTimes <- QuickDesignTimes[,-c(1,6:11,19)]
QuickDesignTimes2 <- QuickDesignTimes2[,-c(1,6:11,19:20)]
QuickDesignTimes <- rbind(QuickDesignTimes, QuickDesignTimes2)

# Filter data for complete cases
QuickDesignTimes <- QuickDesignTimes[complete.cases(QuickDesignTimes),]
QuickDesignTimes <- QuickDesignTimes[QuickDesignTimes$TimeRequested!="",]

# Adjust SiteDesignPriority dataset and match with QuickDesignTimes
QuickDesignTimes$SiteDesignPriority <- QuickDesignRequest[match(QuickDesignTimes$Opportunity.ID, QuickDesignRequest$Opportunity.ID),9]
QuickDesignTimes$Site.Design.Priority <- QuickDesignTimes$SiteDesignPriority
QuickDesignTimes <- QuickDesignTimes[,-c(12)]

# Convert DesignRequested, StartTime & EndTime to POSIXct 
QuickDesignTimes$TimeRequested <- as.POSIXct(QuickDesignTimes$TimeRequested, format = '%m/%d/%Y %H:%M')
QuickDesignTimes$StartTime <- as.POSIXct(QuickDesignTimes$StartTime, format = '%m/%d/%Y %H:%M')
QuickDesignTimes$EndTime <- as.POSIXct(QuickDesignTimes$EndTime, format = '%m/%d/%Y %H:%M')

# Convert QueueTime, DesignTime and TotalTime to time series
Fmt.default <- function(x) {
  y <- abs(x)
  sprintf("%s%02d:%02d:%02d", 
          ifelse(x < 0, "-", ""), # sign
          y %% 86400 %/% 3600,  # hours 
          y %% 3600 %/% 60,  # minutes
          y %% 60 %/% 1)} # seconds

# Calculate the QueueTime, DesignTime & TotalTime
QuickDesignTimes$QueueTime <- difftime(QuickDesignTimes$StartTime, QuickDesignTimes$TimeRequested, units = "secs")
QuickDesignTimes$DesignTime <- difftime(QuickDesignTimes$EndTime, QuickDesignTimes$StartTime, units = "secs")
QuickDesignTimes$TotalTime <- as.numeric(QuickDesignTimes$QueueTime + QuickDesignTimes$DesignTime)

QuickDesignTimes$QueueTime <- Fmt.default(as.numeric(QuickDesignTimes$QueueTime))
QuickDesignTimes$DesignTime <- Fmt.default(as.numeric(QuickDesignTimes$DesignTime))
QuickDesignTimes$TotalTime <- Fmt.default(QuickDesignTimes$TotalTime)

# Include a running count of Completed Quick Designs
setDT(QuickDesignTimes)[,RunningCount:=1:.N,Opportunity.ID]

# Group Modelers into Teams
QuickDesignTimes$Team <- with(QuickDesignTimes, ifelse(QuickDesignTimes$EditedBy %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling","Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson","Kevin Dematteo"),"Denver",
                                                       ifelse((grepl("Dimension", QuickDesignTimes$EditedBy)),"DI","Other")))

# Calculate Time related information
QuickDesignTimes$mmmyy <- format(QuickDesignTimes$TimeRequested, "%m/%Y")
QuickDesignTimes$Year <- as.numeric(format(QuickDesignTimes$TimeRequested, '%Y'))
QuickDesignTimes$Month <- as.numeric(format(QuickDesignTimes$TimeRequested, '%m'))
QuickDesignTimes$Day <- as.numeric(format(QuickDesignTimes$TimeRequested, '%d'))
QuickDesignTimes$Hour <- as.numeric(format(QuickDesignTimes$TimeRequested, '%H'))
QuickDesignTimes$Weekday <- weekdays(QuickDesignTimes$TimeRequested)
QuickDesignTimes$Weeknumber <- lubridate::week(ymd_hms(QuickDesignTimes$TimeRequested))

# Calculate Counts of High and Normal Priority completed within SLA times
QuickDesignTimes$HighPriorityInSLA <- ifelse((QuickDesignTimes$Site.Design.Priority=="High"&QuickDesignTimes$TotalTime<="00:15:00"),1,0)
QuickDesignTimes$NormalPriorityInSLA <- ifelse((QuickDesignTimes$Site.Design.Priority=="Normal"&QuickDesignTimes$TotalTime<="00:30:00"),1,0)

# Calculate count of Quick Design High/Normal Priority projects with Queue Time over 5:00
QuickDesignTimes$HighQueueOver5Min <- ifelse((QuickDesignTimes$Site.Design.Priority=="High"&QuickDesignTimes$QueueTime>"00:05:00"),1,0)
QuickDesignTimes$NormalQueueOver5Min <- ifelse((QuickDesignTimes$Site.Design.Priority=="Normal"&QuickDesignTimes$QueueTime>"00:05:00"),1,0)

# Bucket Quick Design completion times
QuickDesignTimes$UnderThree <- ifelse(QuickDesignTimes$TotalTime<"00:03:00",1,0)
QuickDesignTimes$ThreeToSix <- ifelse((QuickDesignTimes$TotalTime>="00:03:00"&QuickDesignTimes$TotalTime<"00:06:00"),1,0)
QuickDesignTimes$SixtoNine <- ifelse((QuickDesignTimes$TotalTime>="00:06:00"&QuickDesignTimes$TotalTime<"00:09:00"),1,0)
QuickDesignTimes$NinetoTwelve <- ifelse((QuickDesignTimes$TotalTime>="00:09:00"&QuickDesignTimes$TotalTime<"00:12:00"),1,0)
QuickDesignTimes$TwelvetoFifteen <- ifelse((QuickDesignTimes$TotalTime>="00:12:00"&QuickDesignTimes$TotalTime<"00:15:00"),1,0)
QuickDesignTimes$OverFifteen <- ifelse(QuickDesignTimes$TotalTime>="00:15:00",1,0)

# *** Outlier MD projects submitted outside Denver Hours ***
QuickDesignTimes$State <- QuickDesignRequest[match(QuickDesignTimes$Opportunity.ID, QuickDesignRequest$Opportunity.ID), 4]
QuickDesignTimes$Sales.Partner <- QuickDesignRequest[match(QuickDesignTimes$Opportunity.ID, QuickDesignRequest$Opportunity.ID), 1]

QuickDesignTimes$Outliers <- ifelse((QuickDesignTimes$EndTime-QuickDesignTimes$TimeRequested)>=10800,1,0)

# Add Sales Rep
QuickDesignTimes$Sales.Rep <- QuickDesignRequest[match(QuickDesignTimes$Opportunity.ID, QuickDesignRequest$Opportunity.ID), 12]

# Format data sets
QuickDesignTimes <- QuickDesignTimes[,c(1:4,9,34:35,37,11,5:7,12:14,8,10,36,15:33)]

# Adjust TimedOut logic
TimedOutReport <- TimedOutReport[TimedOutReport$Difference>0, ]
QuickDesignTimes$TimedOut <- TimedOutReport[match(QuickDesignTimes$Opportunity.ID, TimedOutReport$Opportunity.ID), 14]
QuickDesignTimes[is.na(QuickDesignTimes)] <- ""

# Remove records before 12/1/2016
QuickDesignTimes <- QuickDesignTimes[QuickDesignTimes$TimeRequested > "2016-12-01 00:00:00"]
QuickDesignTimes <- QuickDesignTimes[QuickDesignTimes$TotalTime>0,]

# Write data to csv and xlsx
write.csv(QuickDesignTimes, "C:/Users/david.feldman/Desktop/DataWarehouse/QuickDesignTimes.csv", row.names = FALSE)





# Determine if Site Design Priority was changed prior to design being started
df <- merge(x = QuickDesignTimes, y = SiteDesignPriority, by = "Opportunity.ID")
df$Edit.Date <- as.POSIXct(df$Edit.Date, format = '%m/%d/%Y %H:%M')
df <- df[df$Edit.Date < df$StartTime, ]
QuickDesignTimes$ChangeInPriority <- ifelse((QuickDesignTimes$Opportunity.ID %in% df$Opportunity.ID)&(QuickDesignTimes$StartTime %in% df$StartTime),1,"")

df$delta <- df$Edit.Date - df$StartTime
df <- df[df$RunningCount==1]
df$delta <- Fmt.default(as.numeric(df$delta))
df$delta <- abs(df$delta)


# Analysis of DI "System Declined" Projects
DIAnalysis <- QuickDesignTimes[QuickDesignTimes$RunningCount==1,]
DIAnalysis <- DIAnalysis[DIAnalysis$Team=="DI",]

Opportunity <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/Opportunity.csv")
RedesignRequest <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/RedesignRequests.csv")
DQReasons <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/DQ - QuickDesigns.csv")

DIAnalysis$QDStatusCurrent <- Opportunity[match(DIAnalysis$Opportunity.ID, Opportunity$Opportunity.ID),22]
DIAnalysis$RedesignStatusCurrent <- Opportunity[match(DIAnalysis$Opportunity.ID, Opportunity$Opportunity.ID),23]

DIAnalysis$FailureReason <- DQReasons[match(DIAnalysis$Opportunity.ID, DQReasons$Opportunity.ID),8]
DIAnalysis$RedesignReason <- RedesignRequest[match(DIAnalysis$Opportunity.ID, RedesignRequest$Opportunity.ID),12]

DIAnalysis <- DIAnalysis[,-c(22:36)]

write.csv(DIAnalysis, "C:/Users/david.feldman/Desktop/DIAnalysis.csv", row.names = FALSE)




## FOR WEEKLY REPORT ##
QDM <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/QDM - QD.csv")
Region <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/Regions.csv")

# Add Region to QDM dataset
QDM$Region <- Region[match(QDM$Install.Branch, Region$SFDC.Name),3]

# Merge QDM and RedesignTimes datasets
QDData <- merge(QDM, QuickDesignTimes, all.x = TRUE, by = "Opportunity.ID")

# Adjust class of Site.Design.Requested.x
QDData$Site.Design.Requested.x <- as.character(QDData$Site.Design.Requested.x)
QDData$Site.Design.Requested.x <- as.POSIXct(QDData$Site.Design.Requested.x, format = "%m/%d/%Y %H:%M")

# Clean RedesignData 
QDData <- QDData[,-c(8,14:15,17:20,31:39)]

# Calculate Project count for High and Normal priorities
QDData$ProjectCountHigh <- ifelse(QDData$Site.Design.Priority.y=="High",1,0)
QDData$ProjectCountNormal <- ifelse(QDData$Site.Design.Priority.y=="Normal",1,0)

# Group Modelers into Teams
QDData$Team <- with(QDData, ifelse(QDData$EditedBy %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling", "Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson","Kevin Dematteo"),"Denver",
                                   ifelse((grepl("Dimension", QDData$EditedBy)),"DI","Other")))

# Calculate Time related information
QDData$mmyy <- format(QDData$Site.Design.Requested.x, "%m/%Y")
QDData$Year <- as.numeric(format(QDData$Site.Design.Requested.x, '%Y'))
QDData$Month <- as.numeric(format(QDData$Site.Design.Requested.x, '%m'))
QDData$Day <- as.numeric(format(QDData$Site.Design.Requested.x, '%d'))
QDData$Hour <- as.numeric(format(QDData$Site.Design.Requested.x, '%H'))
QDData$Weekday <- weekdays(QDData$Site.Design.Requested.x)

# Order QDData by Site.Design.Requested.x
QDData <- QDData[order(QDData$Site.Design.Requested.x),]

# Include a running count of Completed Redesigns
setDT(QDData)[,RunningCount:=1:.N,Opportunity.ID]

# subset data for WeeklyReport (Update)
QDData <- QDData[QDData$Site.Design.Requested.x>"2017-10-01 00:00:00",]

# Write to desktop (csv)
write.csv(QDData, "C:/Users/david.feldman/Desktop/QDData.csv", row.names = FALSE)
