
# Combine dataset from 8 provinces Callreport dataset into single CallReport dataset
BC_CallReports<-read.csv("BC_211_CallReports.csv",fileEncoding = "latin1")
Calgary_CallReports<-read.csv("Calgary_211_CallReports.csv",fileEncoding = "latin1")
Central_CallReports<-read.csv("Central_211_CallReports.csv",fileEncoding = "latin1")
Edmonton_CallReports<-read.csv("Edmonton_211_CallReports.csv",fileEncoding = "latin1")
NovaScotia_CallReports<-read.csv("NovaScotia_211_CallReports.csv",fileEncoding = "latin1")
Ontario_CallReports<-read.csv("Ontario_211_CallReports.csv",fileEncoding = "latin1")
PEI_CallReports<-read.csv("PEI_211_CallReports.csv",fileEncoding = "latin1")
Saskatchewan_CallReports<-read.csv("Saskatchewan_211_CallReports.csv",fileEncoding = "latin1")
common_columns<-Reduce(intersect,list(colnames(BC_CallReports),colnames(Calgary_CallReports),colnames(Central_CallReports),colnames(Edmonton_CallReports),
                                      colnames(NovaScotia_CallReports),colnames(Ontario_CallReports),colnames(PEI_CallReports),colnames(Saskatchewan_CallReports)
                                      ))
CallReports<-rbind(subset(BC_CallReports,select=common_columns),subset(Calgary_CallReports,select = common_columns),subset(Central_CallReports,select = common_columns),
                   subset(Edmonton_CallReports,select = common_columns),subset(NovaScotia_CallReports,select = common_columns),
                   subset(Ontario_CallReports,select =common_columns),subset(PEI_CallReports,select = common_columns),subset(Saskatchewan_CallReports,select = common_columns))

#NEW CODE
CallReports <- CallReports %>% mutate(newcount=ifelse(Age =="19-54",sum(count[Age %in% c("35-54","19-54","35-44","25-34","45-54","19-34","40-49","30-39","25-29",
                                                                                         "20-24","54","40","33","38","28","39","30","29","50","32","42","35","36","22","21","34","31","23","26","44","37","41","20","53","27","51","43","25","45","49","19","52","46","47","48","24","12-24")]),
                                                      ifelse(Age == "65+", sum(count[Age %in% c("59","65+","58","60","64","63","62","67","66","69")]),
                                                             ifelse(Age =="55-64",sum(count[Age %in% c("55","55-64","56","61","67")]),
                                                                    ifelse(Age=="60-69",sum(count[Age %in% c("60-69","65","68")]),
                                                                           ifelse(Age=="70-79",sum(count[Age %in% c("70","70-79","74","75","71","72","76","77","79","73","78")]),
                                                                                  ifelse(Age =="80+",sum(count[Age %in% c("80+","80","81","90","85","86","83","84","88","82","92",
                                                                                                                          "87","89","93","94","95","91","96","100","98","103","99")]),
                                                                                         ifelse(Age=="18-24",sum(count[Age %in% c("18-24","18")]),
                                                                                                ifelse(Age=="13-18",sum(count[Age %in% c("13-15","16","17","13","15","14","13-18")]),
                                                                                                       ifelse(Age=="0-12",sum(count[Age %in% c("0-12","0-4","5-9","12","0","1","11","6","9","3","5","8","10","2","4","7","0-6")]),
                                                                                                              ifelse(Age=="15-19",sum(count[Age %in% c("15-19","16-17")]),
                                                                                                                     ifelse(Age=="45-65",sum(count[Age %in% c("45-65")]),
                                                                                                                            ifelse(Age=="25-65",sum(count[Age %in% c("25-65")]),
                                                                                                                                   ifelse(Age=="25-64",sum(count[Age %in% c("25-64")]),
                                                                                                                                          ifelse(Age=="Senior",sum(count[Age %in% c("Senior")]),
                                                                                                                                                 ifelse(Age=="Adult",sum(count[Age %in% c("Adult")]),
                                                                                                                                                        ifelse(Age =="",sum(count[Age %in% c("")]),
                                                                                                                                                               0)))))))))))))))))

#View(CallReports)
#Check for rows with NA
colSums(is.na(CallReports))
# Check for rows with Space
colSums(CallReports == "")
#Check for unique columns
length(unique(CallReports$CallReportNum))
#Remove duplicate rows
CallReports[duplicated(CallReports), ]
CallReports_clean <- CallReports[!duplicated(CallReports), ]
#View((CallReports_clean))

#write.csv(CallReports_clean, "CallReport.csv", row.names = FALSE)

# Combine dataset from 8 provinces CallNeeds dataset into single Needs dataset
BC_Needs<-read.csv("BC_211_Needs.csv",fileEncoding = "latin1")
Calgary_Needs<-read.csv("Calgary_211_Needs.csv",fileEncoding = "latin1")
Central_Needs<-read.csv("Central_211_Needs.csv",fileEncoding = "latin1")
Edmonton_Needs<-read.csv("Edmonton_211_Needs.csv",fileEncoding = "latin1")
NovaScotia_Needs<-read.csv("NovaScotia_211_Needs.csv",fileEncoding = "latin1")
Ontario_Needs<-read.csv("Ontario_211_Needs.csv",fileEncoding = "latin1")
PEI_Needs<-read.csv("PEI_211_Needs.csv",fileEncoding = "latin1")
Saskatchewan_Needs<-read.csv("Saskatchewan_211_Needs.csv",fileEncoding = "latin1")
common_columns1<-Reduce(intersect,list(colnames(BC_Needs),colnames(Calgary_Needs),colnames(Central_Needs),colnames(Edmonton_Needs),
                                      colnames(NovaScotia_Needs),colnames(Ontario_Needs),colnames(PEI_Needs),colnames(Saskatchewan_Needs)))
Needs<-rbind(subset(BC_Needs,select=common_columns1),subset(Calgary_Needs,select = common_columns1),subset(Central_Needs,select = common_columns1),
                   subset(Edmonton_Needs,select = common_columns1),subset(NovaScotia_Needs,select = common_columns1),
                   subset(Ontario_Needs,select =common_columns1),subset(PEI_Needs,select = common_columns1),subset(Saskatchewan_Needs,select = common_columns1))
#View(Needs)
# check for space
colSums(Needs == "")
#Check for NA
colSums(is.na(Needs))
#Check for unique value
Needs[duplicated(Needs), ]
#Remove duplicate rows
Needs_clean <- Needs[!duplicated(Needs), ]
#View(Needs_clean)

#write.csv(Needs_clean, "Needs.csv", row.names = FALSE)

# Combine dataset from 8 provinces AgencyRefferals dataset into single Refferals dataset
BC_Referrals<-read.csv("BC_211_Referrals.csv",fileEncoding = "latin1")
Calgary_Referrals<-read.csv("Calgary_211_Referrals.csv",fileEncoding = "latin1")
Central_Referrals<-read.csv("Central_211_Referrals.csv",fileEncoding = "latin1")
Edmonton_Referrals<-read.csv("Edmonton_211_Referrals.csv",fileEncoding = "latin1")
NovaScotia_Referrals<-read.csv("NovaScotia_211_Referrals.csv",fileEncoding = "latin1")
Ontario_Referrals<-read.csv("Ontario_211_Referrals.csv",fileEncoding = "latin1")
PEI_Referrals<-read.csv("PEI_211_Referrals.csv",fileEncoding = "latin1")
Saskatchewan_Referrals<-read.csv("Saskatchewan_211_Referrals.csv",fileEncoding = "latin1")

common_columns2<-Reduce(intersect,list(colnames(BC_Referrals),colnames(Calgary_Referrals),colnames(Central_Referrals),colnames(Edmonton_Referrals),
                                       colnames(NovaScotia_Referrals),colnames(Ontario_Referrals),colnames(PEI_Referrals),colnames(Saskatchewan_Referrals)))
Refferals<-rbind(subset(BC_Referrals,select=common_columns2),subset(Calgary_Referrals,select = common_columns2),subset(Central_Referrals,select = common_columns2),
             subset(Edmonton_Referrals,select = common_columns2),subset(NovaScotia_Referrals,select = common_columns2),
             subset(Ontario_Referrals,select =common_columns2),subset(PEI_Referrals,select = common_columns2),subset(Saskatchewan_Referrals,select = common_columns2))
#View(Refferals)
#check for rows with Na
colSums(is.na(Refferals))
# check for rows with space
colSums(Refferals=="")
#remove duplicates
Refferals_clean <- Refferals[!duplicated(Refferals), ]
#View(Refferals_clean)
#write.csv(Refferals_clean, "Refferals.csv", row.names = FALSE)

Resources_211<-read.csv("211_Resources_Dataset.csv",fileEncoding = "latin1")
# We could not see any observation where whole row is null.
library(dplyr)
#here we can see like even Calltype is space,but needs are met, so treated as valid observation
joined_df_space<-CallReports %>% left_join(Needs,by=c("CallReportNum"="ReportNeedNum")) %>% mutate(
  checking_val1=ifelse(CallType == "",Needs$NeedMet,0)
)
View(joined_df_space)


#which gender makes more enquiry
Gender_Person<- CallReports %>% group_by(Gender) %>% summarise(count=n())
Sorted_val2<-arrange(Gender_Person,desc(count))
print(Sorted_val2)
View(Gender_Person)
# which age range more call received
Age_range_Person<- CallReports %>% group_by(Age) %>% summarise(count=n())
Sorted_val3<-arrange(Age_range_Person,desc(count))
View(Sorted_val3)

#NEW CODE
Sorted_val3 <- Sorted_val3 %>% mutate(newcount=ifelse(Age =="19-54",sum(count[Age %in% c("35-54","19-54","35-44","25-34","45-54","19-34","40-49","30-39","25-29",
                                                    "20-24","54","40","33","38","28","39","30","29","50","32","42","35","36","22","21","34","31","23","26","44","37","41","20","53","27","51","43","25","45","49","19","52","46","47","48","24","12-24")]),
                                      ifelse(Age == "65+", sum(count[Age %in% c("59","65+","58","60","64","63","62","67","66","69")]),
                                       ifelse(Age =="55-64",sum(count[Age %in% c("55","55-64","56","61","67")]),
                                         ifelse(Age=="60-69",sum(count[Age %in% c("60-69","65","68")]),
                                          ifelse(Age=="70-79",sum(count[Age %in% c("70","70-79","74","75","71","72","76","77","79","73","78")]),
                                          ifelse(Age =="80+",sum(count[Age %in% c("80+","80","81","90","85","86","83","84","88","82","92",
                                                                                  "87","89","93","94","95","91","96","100","98","103","99")]),
                                            ifelse(Age=="18-24",sum(count[Age %in% c("18-24","18")]),
                                              ifelse(Age=="13-18",sum(count[Age %in% c("13-15","16","17","13","15","14","13-18")]),
                                                ifelse(Age=="0-12",sum(count[Age %in% c("0-12","0-4","5-9","12","0","1","11","6","9","3","5","8","10","2","4","7","0-6")]),
                                                    ifelse(Age=="15-19",sum(count[Age %in% c("15-19","16-17")]),
                                                      ifelse(Age=="45-65",sum(count[Age %in% c("45-65")]),
                                                          ifelse(Age=="25-65",sum(count[Age %in% c("25-65")]),
                                                                 ifelse(Age=="25-64",sum(count[Age %in% c("25-64")]),
                                                                        ifelse(Age=="Senior",sum(count[Age %in% c("Senior")]),
                                                                               ifelse(Age=="Adult",sum(count[Age %in% c("Adult")]),
                                                                                ifelse(Age =="",sum(count[Age %in% c("")]),
                                                                                   0)))))))))))))))))
View(Sorted_val3)
Sorted_age_val<- filter(Sorted_val3,Sorted_val3$newcount != 0)
print(Sorted_age_val)
#which call type we receive more enquiry
Service_call_count<-CallReports %>% group_by(CallType) %>% summarise(count=n())
View(Service_call_count)


#NEW CODE
transfer_sum <- sum(Service_call_count$count[grepl("Transfer", Service_call_count$CallType, ignore.case = TRUE)])
Service_call_count<- Service_call_count %>% mutate(call_details=ifelse(grepl("Transfer",Service_call_count$CallType,ignore.case = TRUE) == TRUE,transfer_sum,count))
print(Service_call_count)
new_row<-filter(Service_call_count,grepl("^Cold Transfer$",Service_call_count$CallType)==TRUE)
pattern="Cold transfer provided$"
Service_call_count<-filter(Service_call_count,grepl(pattern,Service_call_count$CallType)!= TRUE & grepl("Cold Transfer$",Service_call_count$CallType)!=TRUE)
Service_call_count<-rbind(Service_call_count,new_row)

sorted_value<-arrange(Service_call_count,desc(count))
print(sorted_value[1,])
#which calls have more count based on Age, call type and gender.
Service_call_ByGender_Age_calltype<-CallReports %>% group_by(CallType,Gender,Age) %>% summarise(count=n())
View(Service_call_ByGender_Age_calltype)
Sorted_val1<-arrange(Service_call_ByGender_Age_calltype,desc(count))

#Generating MHRC dataset
install.packages("jsonlite")
library(jsonlite)
lines1 <- readLines("nominal_df.json")
json_text <- paste0("[", paste(lines1, collapse = ","), "]")
MHRC_dataset <- fromJSON(json_text)
View(MHRC_dataset)

write.csv(MHRC_dataset, "MHRC.csv", row.names = FALSE)
#shows Needs and count 
Calls_related_Mentalissues <- Needs_clean %>% group_by(Level1Name) %>% summarise(count_level1= n())
print(Calls_related_Mentalissues)


#getting data from 211 dataset related to Mental health and support related
Mental_health_data_211<- Needs_clean %>% filter(Needs_clean$Level1Name %in% c("Criminal Justice and Legal Services"
,"Mental Health and Substance Use Disorder Services","Target Populations") | (Needs_clean$Level2Name=="Mutual Support"))
print(Mental_health_data_211)


#Get the needed column
Mental_health_211<-select(Mental_health_data_211,-Level4Name,-Level5Name,-ParentResourceNum,-ParentAgencyName)
print(Mental_health_211)
write.csv(Mental_health_211, "Mental_health_211.csv", row.names = FALSE)
summary(Mental_health_211)

#joining CallReport_clean and Mental_health_211(so can work on provinces, dates and related mental health)
Combined_CallReport_mentalhealth<-inner_join(CallReports_clean,Mental_health_211,by="CallReportNum")
View(Combined_CallReport_mentalhealth)


#generating a new column year and month from datetime column based on call start time
sum(is.na(Combined_CallReport_mentalhealth$CallDateAndTimeStart))
library(lubridate)
Combined_CallReport_mentalhealth$CallDateAndTimeStart <- as.POSIXct(Combined_CallReport_mentalhealth$CallDateAndTimeStart,tz="UTC")
Combined_CallReport_mentalhealth <- Combined_CallReport_mentalhealth %>% mutate(Year = format(Combined_CallReport_mentalhealth$CallDateAndTimeStart,"%Y"),
                                                                                )
print(Combined_CallReport_mentalhealth$Year)
Combined_CallReport_mentalhealth<-Combined_CallReport_mentalhealth %>% mutate(Month_name = format(Combined_CallReport_mentalhealth$CallDateAndTimeStart, "%b"))
print(Combined_CallReport_mentalhealth$Month_name)


# Created 2 daatset from 211 based on call received,Province,year and also MHRC poll date,month and province[ Can plot and tried to compare]
print("211_dataset")
Combined_CallReport_mentalhealth %>% group_by(StateProvince,Year,Month_name) %>% summarise(count=n())
print("MHRC Dataset")
MHRC_dataset %>% group_by(PROV,WAVE) %>% summarise(count=n())

# generated a dataset with needs not met and summarise the reason
Needs_not_met <- Combined_CallReport_mentalhealth %>% filter(NeedMet == "False") 
print(Needs_not_met)
Needs_not_met_summarise_reason <- Needs_not_met %>% group_by(ReasonUnmet) %>% summarise(count=n())
print(Needs_not_met_summarise_reason)
# summarize based on Year[ Trend showing 2022 most unmet needs happens]
Needs_not_met_summarise_reason_Year <- Needs_not_met %>% group_by(Year) %>% summarise(count=n())
print(Needs_not_met_summarise_reason_Year)

#for 2022 reason for unmet needs is higher for Caller refused the refferal
filter(Needs_not_met,Needs_not_met$Year == "2022") %>% group_by(ReasonUnmet) %>% summarise(count=n())

