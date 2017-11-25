# Load Libraries
library(data.table)
library(dplyr)
library(forcats)
library(qdapTools)
library(zoo)
library(lubridate)

# Import Raw Data
RedesignTimes <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/RedesignTimesData.csv")
RedesignRequest <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/RedesignRequests.csv")
SiteDesignPriority <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/SiteDesignPriority.csv")

# Create order and relate to data
OldValue_order = data.frame(Old.Value = c("","Open","In Progress","System Declined","Complete"), y = c(0,1,2,4,3))
RedesignTimes$OV_order = lookup(RedesignTimes$Old.Value, OldValue_order)

# Arrange the data.frame in desired order
RedesignTimes = RedesignTimes[with(RedesignTimes, order(RedesignTimes$Opportunity.ID, as.POSIXct(RedesignTimes$Edit.Date, format = "%m/%d/%Y %H:%M"), OV_order)),]

# Apply logic to calculate RedesignRequested, StartTime & EndTime                                                                                                                                            
RedesignTimes$RedesignRequested <- ifelse(((RedesignTimes$Old.Value==""|RedesignTimes$Old.Value=="Complete"|RedesignTimes$Old.Value=="System Declined"|RedesignTimes$Old.Value=="In Progress")&RedesignTimes$New.Value=="Open"),as.character(RedesignTimes$Edit.Date, format = '%Y/%m/%d %H:%M'),NA)
RedesignTimes$StartTime <- ifelse((RedesignTimes$Old.Value=="Open"&RedesignTimes$New.Value=="In Progress"),as.character(RedesignTimes$Edit.Date, format = '%Y/%m/%d %H:%M'),NA)
RedesignTimes$EndTime <- ifelse(((RedesignTimes$New.Value=="Complete"|RedesignTimes$New.Value=="System Declined"|RedesignTimes$New.Value=="")&(RedesignTimes$Old.Value=="In Progress"|RedesignTimes$Old.Value=="Open")),as.character(RedesignTimes$Edit.Date, format = '%Y/%m/%d %H:%M'),NA)

# Identify Breaks in Process
RedesignTimes$BreaksInProcess <- ifelse((RedesignTimes$Old.Value=="Open"&(RedesignTimes$New.Value=="System Declined"|RedesignTimes$New.Value=="Complete")),1,0)
RedesignTimes$BreaksInProcess <- shift(RedesignTimes$BreaksInProcess, 1)

# Determine if project was "System Declined" or "Completed"
RedesignTimes$RedesignStatus <- ifelse(RedesignTimes$EndTime>0,as.character(RedesignTimes$New.Value),"")
RedesignTimes$RedesignStatus <- zoo::na.locf.default(RedesignTimes$RedesignStatus, fromLast = TRUE)

# Merge RedesignTimes into respective row
RedesignTimes <- RedesignTimes %>%
  mutate(check = as.numeric(
    as.character(fct_collapse(New.Value, 
                              `1` = c("Open",""),
                              `2` = "In Progress",
                              `3` = c("Complete", "System Declined"))))) %>%
  group_by(Opportunity.ID) %>%
  mutate(group = cumsum(c(TRUE, diff(check) < 1))) %>%
  group_by(Opportunity.ID, group) %>%
  mutate_at(vars(RedesignRequested:EndTime),
            funs(if(identical(check[1:3], as.numeric(1:3))){
              sort(., na.last = TRUE)} else{.}
            ))

RedesignTimes <- RedesignTimes %>%
  mutate(check = as.numeric(
    as.character(fct_collapse(New.Value, 
                              `1` = "Open",
                              `2` = c("Complete", "System Declined"),
                              `3` = "In Progress")))) %>%
  group_by(Opportunity.ID, group) %>%
  mutate_at(vars(RedesignRequested,EndTime),
            funs(if(identical(check[1:2], as.numeric(1:2))){
              sort(., na.last = TRUE)} else{.}
            )) %>%
  select(-check, -group)

# For Breaks in Process calculate StartTime
RedesignTimes$StartTime <- ifelse(((RedesignTimes$RedesignRequested>0&RedesignTimes$EndTime>0&is.na(RedesignTimes$StartTime))==TRUE),as.character(RedesignTimes$RedesignRequested, format = '%Y/%m/%d %H:%M'), as.character(RedesignTimes$StartTime, format = '%Y/%m/%d %H:%M'))

# Convert DesignRequested, StartTime & EndTime to POSIXct 
RedesignTimes$RedesignRequested <- as.POSIXct(RedesignTimes$RedesignRequested, format = '%m/%d/%Y %H:%M')
RedesignTimes$StartTime <- as.POSIXct(RedesignTimes$StartTime, format = '%m/%d/%Y %H:%M')
RedesignTimes$EndTime <- as.POSIXct(RedesignTimes$EndTime, format = '%m/%d/%Y %H:%M')

# Create corrected EditedBy column
setDT(RedesignTimes)[, EditedBy := Edited.By[2L] ,.(Opportunity.ID, grp=cumsum(New.Value == "Open"))]

# Choose method of filtering data
RedesignTimes <- RedesignTimes[complete.cases(RedesignTimes),] # Option 1 - Remove any row with NA

# Convert QueueTime, DesignTime and TotalTime to time series
Fmt.default <- function(x) {
  y <- abs(x)
  sprintf("%s%02d:%02d:%02d", 
          ifelse(x < 0, "-", ""), # sign
          y %% 86400 %/% 3600,  # hours 
          y %% 3600 %/% 60,  # minutes
          y %% 60 %/% 1)} # seconds

# Calculate the QueueTime, DesignTime & TotalTime
RedesignTimes$QueueTime <- difftime(RedesignTimes$StartTime, RedesignTimes$RedesignRequested, units = "secs")
RedesignTimes$DesignTime <- difftime(RedesignTimes$EndTime, RedesignTimes$StartTime, units = "secs")
RedesignTimes$TotalTime <- as.numeric(RedesignTimes$QueueTime + RedesignTimes$DesignTime)

# Convert format of times
RedesignTimes$QueueTime <- Fmt.default(as.numeric(RedesignTimes$QueueTime))
RedesignTimes$DesignTime <- Fmt.default(as.numeric(RedesignTimes$DesignTime))
RedesignTimes$TotalTime <- Fmt.default(RedesignTimes$TotalTime)

# Include a running count of Completed Redesigns
setDT(RedesignTimes)[,RunningCount:=1:.N,Opportunity.ID]

# Determine Preventables
# Ready for
df3 <- merge(x = RedesignTimes, y = RedesignRequest, by = "Opportunity.ID")
df3$Edit.Date.y <- as.POSIXct(df3$Edit.Date.y, format = '%m/%d/%Y %H:%M')
df3 <- df3[df3$RedesignRequested < df3$Edit.Date.y & df3$Edit.Date.y <= df3$StartTime, ]
RedesignTimes$Preventable2 <- ifelse((RedesignTimes$Opportunity.ID %in% df3$Opportunity.ID)&(RedesignTimes$RedesignRequested %in% df3$RedesignRequested),"Ready for","") 

# Site Design Priority
df4 <- merge(x = RedesignTimes, y = SiteDesignPriority, by = "Opportunity.ID")
df4$Edit.Date.y <- as.POSIXct(df4$Edit.Date.y, format = '%m/%d/%Y %H:%M')
df4 <- df4[df4$Old.Value.y=="",]
df4 <- df4[df4$RedesignRequested < df4$Edit.Date.y, ]
RedesignTimes$Preventable3 <- ifelse((RedesignTimes$Opportunity.ID %in% df4$Opportunity.ID)&(RedesignTimes$RedesignRequested %in% df4$RedesignRequested),"Site Design Priority","")

# Merge Preventable Reasons
RedesignTimes$Preventable <- paste(RedesignTimes$Preventable2, RedesignTimes$Preventable3)

# Group Modelers into Teams
RedesignTimes$Team <- with(RedesignTimes, ifelse(RedesignTimes$EditedBy %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling", "Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson","Kevin Dematteo"),"Denver",
                                                 ifelse((grepl("Dimension", RedesignTimes$EditedBy)),"DI","Other")))

# Calculate Time related information
RedesignTimes$mmyy <- format(RedesignTimes$RedesignRequested, "%m/%Y")
RedesignTimes$Year <- as.numeric(format(RedesignTimes$RedesignRequested, '%Y'))
RedesignTimes$Month <- as.numeric(format(RedesignTimes$RedesignRequested, '%m'))
RedesignTimes$Day <- as.numeric(format(RedesignTimes$RedesignRequested, '%d'))
RedesignTimes$Hour <- as.numeric(format(RedesignTimes$RedesignRequested, '%H'))
RedesignTimes$Weekday <- weekdays(RedesignTimes$RedesignRequested)
RedesignTimes$Weeknumber <- lubridate::week(ymd_hms(RedesignTimes$RedesignRequested))

# Determine Outliers by Weekday, Hour of Submission, and Total Time (needed when cleaning up old "open" and "in progress" projects)
RedesignTimes$Outliers <- ifelse(((RedesignTimes$Weekday=="Monday"|RedesignTimes$Weekday=="Tuesday"|RedesignTimes$Weekday=="Wednesday"|RedesignTimes$Weekday=="Thursday"|RedesignTimes$Weekday=="Friday")&(RedesignTimes$Hour>=22|RedesignTimes$Hour<=5)),1,
                                 ifelse((RedesignTimes$Weekday=="Saturday"&(RedesignTimes$Hour>=21|RedesignTimes$Hour<=6)),1,
                                        ifelse((RedesignTimes$Weekday=="Sunday"&(RedesignTimes$Hour>=17|RedesignTimes$Hour<=7)),1,
                                               ifelse(RedesignTimes$EndTime - RedesignTimes$RedesignRequested>=10800,1,0))))

# Calculate Counts of Redesign Projects completed within SLA
RedesignTimes$RedesignInSLA <- ifelse(RedesignTimes$TotalTime<="00:15:00",1,0)

# Calculate count of Redesign projects with Queue Time over 5:00
RedesignTimes$QueueOver5Min <- ifelse(RedesignTimes$QueueTime>"00:05:00",1,0)

# Bucket Redesign Times
RedesignTimes$UnderThree <- ifelse(RedesignTimes$TotalTime<"00:03:00",1,0)
RedesignTimes$ThreeToSix <- ifelse((RedesignTimes$TotalTime>="00:03:00"&RedesignTimes$TotalTime<"00:06:00"),1,0)
RedesignTimes$SixtoNine <- ifelse((RedesignTimes$TotalTime>="00:06:00"&RedesignTimes$TotalTime<"00:09:00"),1,0)
RedesignTimes$NinetoTwelve <- ifelse((RedesignTimes$TotalTime>="00:09:00"&RedesignTimes$TotalTime<"00:12:00"),1,0)
RedesignTimes$TwelvetoFifteen <- ifelse((RedesignTimes$TotalTime>="00:12:00"&RedesignTimes$TotalTime<"00:15:00"),1,0)
RedesignTimes$OverFifteen <- ifelse(RedesignTimes$TotalTime>="00:15:00",1,0)

# Add Redesign Request to times
RedesignRequest2 <- RedesignRequest
setDT(RedesignRequest2)[,RunningCount:=1:.N,Opportunity.ID]
RedesignRequest2 <- RedesignRequest2[,-c(1,3:11,14:19)]
RedesignTimes <- merge(RedesignTimes, RedesignRequest2, by=c("Opportunity.ID", "RunningCount"), all.x = TRUE)

# Remove Redesigns completed by Other team
RedesignTimes <- RedesignTimes[RedesignTimes$Team!="Other",]
RedesignTimes <- RedesignTimes[RedesignTimes$Opportunity.ID!="0063200001nG0yx",]

# Format dataframe
RedesignTimes <- RedesignTimes[,-c(3,5:10,20:21)]
RedesignTimes <- RedesignTimes[,c(3,1,8,31:32,9,4:6,10:12,7,2,13:30)]

# Write as csv file
write.csv(RedesignTimes, "C:/Users/david.feldman/Desktop/DataWarehouse/RedesignTimes.csv", row.names = FALSE)

# Print object size
print(object.size(RedesignTimes),units="Mb")







# FOR WEEKLY REPORT ##
QDM <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/QDM - Redesigns.csv")
Region <- read.csv("C:/Users/david.feldman/Desktop/DataWarehouse/Regions.csv")

# Add Region to QDM dataset
QDM$Region <- Region[match(QDM$Install.Branch, Region$SFDC.Name),3]

# Merge QDM and RedesignTimes datasets
RedesignData <- merge(QDM, RedesignTimes, all.x = TRUE, by = "Opportunity.ID")

# Calculate true RedesignRequestedDate
RedesignData$Redesign.Requested <- as.POSIXct(as.character(RedesignData$Redesign.Requested), format = "%m/%d/%Y %H:%M")

RedesignData$RedesignRequestedDate <- ifelse(is.na(RedesignData$RedesignRequested),as.character(RedesignData$Redesign.Requested),as.character(RedesignData$RedesignRequested))
RedesignData$RedesignRequestedDate <- as.POSIXct(RedesignData$RedesignRequestedDate, format = "%Y-%m-%d %H:%M:%S")

# Clean RedesignData 
RedesignData <- RedesignData[,-c(12,16:19,21,28,30:37)]
RedesignData <- RedesignData[,c(1:15,32,16:31)]

# Group Modelers into Teams
RedesignData$Team <- with(RedesignData, ifelse(RedesignData$EditedBy %in% c("Evan Healey","Adam Duncan","Anthony Medina","Emmanuel Rodriguez","Gabby Castro Diaz","Benjamin Leatherwood","Martin Miller","Jonathan Weisiger","Virginia Demaio","Alex Weisiger","Benjamin Albers","Ben Albers","Dennis Olguin","Derek Miller","Eric Kvamme","Ginny DeMaio","Karl Johanson","Marielle Martin","Michael Behling","Mike Behling", "Nickolas Brown","Richard Bohannon","Shane Mitchell","Stefan Crumley","Nora Hendrickson","Kevin Dematteo"),"Denver",
                                               ifelse((grepl("Dimension", RedesignData$EditedBy)),"DI","Other")))

# Calculate Time related information
RedesignData$mmyy <- format(RedesignData$RedesignRequestedDate, "%m/%Y")
RedesignData$Year <- as.numeric(format(RedesignData$RedesignRequestedDate, '%Y'))
RedesignData$Month <- as.numeric(format(RedesignData$RedesignRequestedDate, '%m'))
RedesignData$Day <- as.numeric(format(RedesignData$RedesignRequestedDate, '%d'))
RedesignData$Hour <- as.numeric(format(RedesignData$RedesignRequestedDate, '%H'))
RedesignData$Weekday <- weekdays(RedesignData$RedesignRequestedDate)

# Order RedesignData by RedesignRequestedDate
RedesignData <- RedesignData[order(RedesignData$RedesignRequestedDate),]

# Include a running count of Completed Redesigns
setDT(RedesignData)[,RunningCount:=1:.N,Opportunity.ID]

# subset data for WeeklyReport (Update)
RedesignData <- RedesignData[RedesignData$RedesignRequestedDate>"2017-10-01 00:00:00",]

# Write to desktop (csv)
write.csv(RedesignData, "C:/Users/david.feldman/Desktop/RedesignData.csv", row.names = FALSE)

