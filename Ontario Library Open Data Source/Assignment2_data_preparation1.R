#---------------------------------------------------------------------------------------------------------------------------#
#                                                                                                                           #
#   CODE FOR LISTING NUMBER OF ACTIVE CARD HOLDERS CORRESPONDING TO EACH YEAR AND LIBRARY FULL NAME                         #                                                                                               
#                                                                                                                           #
#                                                                                                                           #
#---------------------------------------------------------------------------------------------------------------------------#

#Reading Ontario Library data in CSV files to data objects[data_2019, data_2020, data_2021,data_2022,data_2023].
    data_2019 <- read.csv("2019_ontario_public_library_statistics_open_data.csv",fileEncoding = "latin1")
    data_2020 <- read.csv("2020_ontario_public_library_statistics_open_data.csv",fileEncoding = "latin1")
    data_2021 <- read.csv("2021_ontario_public_library_statistics_open_data.csv",fileEncoding = "latin1")
    data_2022 <- read.csv("ontario_public_library_statistics_2022_open_data.csv",fileEncoding = "latin1")
    data_2023 <- read.csv("ontario_public_library_statistics_open_data_2023.csv",fileEncoding = "latin1")


#Checking weather the required field names and data types for the program are same.
    attributes(data_2019)
    attributes(data_2020)
    attributes(data_2021)
    attributes(data_2022)
    attributes(data_2023)

# For 2019 data field name is different.So for combining 2019 data, change the column name. 
    colnames(data_2019)[colnames(data_2019) == "Library.Full.Name"] <- "ï..Library.Full.Name"
#Code for getting similar columns and appending them vertically
    common_columns <- Reduce(intersect, list(colnames(data_2019),colnames(data_2020),colnames(data_2021),colnames(data_2022),colnames(data_2023)))
    print(common_columns)
    data_combined <- rbind(subset(data_2019, select = common_columns),subset(data_2020, select = common_columns),
                       subset(data_2021,select=common_columns),subset(data_2022,select=common_columns),
                       subset(data_2023,select=common_columns))

#Changing field names and removing NA values for particular columns.
    colnames(data_combined)[colnames(data_combined)=="A1.14..No..of.Active.Library.Cardholders"]<-"Active Card Holders"
    colnames(data_combined)[colnames(data_combined) == "ï..Library.Full.Name"] <- "Library Name"
    colnames(data_combined)[colnames(data_combined) == "Survey.Year.From"] <- "Year"
    data_cleaned0<-data_combined %>% filter(!is.na(`Library Name`))
    data_cleaned <- data_cleaned0 %>% filter(!is.na(Year))
    data_cleaned1<- data_cleaned %>% filter(!is.na(`Active Card Holders`))
   #print(unique(data_cleaned0$`Library Name`))
   
   
#Checking for missing space, only 1 detected so leave it untreated   
    b<-sum(trimws(data_cleaned0$`Year`) == "")
    b
    #install.packages("tidyr",dependencies = TRUE)

## Summarizing and creating tables for listing total active card holders for a particular library on the corresponding year.
    library(tidyr)
    library_table <- data_cleaned1 %>%
    group_by(Year, `Library Name`) %>%
    summarise(`Total_Active_CardHolders` = `Active Card Holders`)
    str(library_table)
    library_table1 <- library_table %>% pivot_wider(names_from = Year, values_from = Total_Active_CardHolders, values_fill = list(Total_Active_CardHolders= NA))
    library_table1

#Generating data in table format        
    Library_table2<-head(library_table1)
    #install.packages("pander")
    library(pander)
    pander(Library_table2)
