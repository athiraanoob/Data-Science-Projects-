######################################################################################################################
# PROGRAM FOR LISTING NUMBER OF LIBRARIES IN EACH CITY FOR CORRESPONDING YEARS 2019,2020,2021,2022,2023 IN A TABLE   #                                                                                                         
#                                                                                                                    #
######################################################################################################################


##Reading CSV file of library data for years 2019,2020,2021,2022,2023 
##into 5 different objects[data_2019,data_2020,data_2021,data_2022,data_2023]
    data_2019 <- read.csv("2019_ontario_public_library_statistics_open_data.csv",fileEncoding = "latin1")
    data_2020 <- read.csv("2020_ontario_public_library_statistics_open_data.csv",fileEncoding = "latin1")
    data_2021 <- read.csv("2021_ontario_public_library_statistics_open_data.csv",fileEncoding = "latin1")
    data_2022 <- read.csv("ontario_public_library_statistics_2022_open_data.csv",fileEncoding = "latin1")
    data_2023 <- read.csv("ontario_public_library_statistics_open_data_2023.csv",fileEncoding = "latin1")


##This block is for combining 5 different objects into one object data_combined
#Getting the common columns from all the 5 objects and append the common columns vertically/raw wise
    common_columns <- Reduce(intersect, list(colnames(data_2019),colnames(data_2020),
                                         colnames(data_2021),colnames(data_2022),colnames(data_2023)))
    data_combined <- rbind(subset(data_2019, select = common_columns),subset(data_2020, select = common_columns),
                       subset(data_2021,select=common_columns),subset(data_2022,select=common_columns),
                       subset(data_2023,select=common_columns))


##checking for all common column names are present in data_combined object
    #attributes(data_combined)


##Renaming the columns A1.10.City.Town to City and Survey.Year.From to Year
    #install.packages("dplyr",dependencies = TRUE)
    library(dplyr)
    #install.packages("magrittr",dependencies = TRUE)  # Install magrittr if needed
    library(magrittr) 
    colnames(data_combined)[colnames(data_combined) == "A1.10.City.Town"] <- "City"
    colnames(data_combined)[colnames(data_combined) == "Survey.Year.From"] <- "Year"



##removing NA from city and Year columns
    data_cleaned1<- data_combined %>% filter(!is.na(City))
    data_cleaned <- data_cleaned1 %>% filter(!is.na(Year))
    #print(unique(data_cleaned$City))


#Doing aggregation function for getting number of libraries in a city and for corresponding year
    library_counts <- data_cleaned %>%
        group_by(City, Year) %>%
        summarise(Num_Libraries = n())
    #print(library_counts, n=150)
    #install.packages("tidyr",dependencies = TRUE)

##Generating the output into a table
    library(tidyr)
    library_table <- library_counts %>% pivot_wider(names_from = Year, values_from = Num_Libraries, 
                                                    values_fill = list(Num_Libraries = 0))
    #library_table

#Generating output in table format  
    library_table1 <- head(library_table)
    library(pander)
    pander(library_table1)





#---------------------------------------------------------------------------------------------------------------------------#
#                                                                                                                           #
#   CODE FOR LISTING NUMBER OF ACTIVE CARD HOLDERS CORRESPONDING TO EACH YEAR AND LIBRARY FULL NAME                         #                                                                                               
#                                                                                                                           #
#                                                                                                                           #
#---------------------------------------------------------------------------------------------------------------------------#

#Data Objects[data_2019, data_2020, data_2021,data_2022,data_2023] are accessed from above code block.

#Checking weather the required field names and data types for the program are same.
    #attributes(data_2019)
    #attributes(data_2020)
    #attributes(data_2021)
    #attributes(data_2022)
    #attributes(data_2023)

# For 2019 data field name is different.So for combining 2019 data, change the column name. 
    colnames(data_2019)[colnames(data_2019) == "Library.Full.Name"] <- "ï..Library.Full.Name"
#Code for getting similar columns and appending them vertically
    common_columns <- Reduce(intersect, list(colnames(data_2019),colnames(data_2020),
                                             colnames(data_2021),colnames(data_2022),colnames(data_2023)))
#print(common_columns)
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
    #b<-sum(trimws(data_cleaned0$`Year`) == "")
    #install.packages("tidyr",dependencies = TRUE)

## Summarizing and creating tables for listing total active card holders for a particular library on the corresponding year.
library(tidyr)
    library_table <- data_cleaned1 %>%
      group_by(Year, `Library Name`) %>%
      summarise(`Total_Active_CardHolders` = `Active Card Holders`)
    #str(library_table)
    library_table1 <- library_table %>% pivot_wider(names_from = Year, values_from = Total_Active_CardHolders, values_fill = list(Total_Active_CardHolders= NA))
    library_table1

#Generating data in table format        
    Library_table2<-head(library_table1)
    #install.packages("pander")
    library(pander)
    pander(Library_table2)



#-------------------------------------------------------------------------------------------------------------------------#
#                                                                                                                         #
#                                                                                                                         #
#              PROGRAM FOR LISTING TOP 5 LIBRARIES WITH HIGHEST AVERAGE TOTAL OPERATING REVENUE                           #
#                                                                                                                         #
#                                                                                                                         #
#-------------------------------------------------------------------------------------------------------------------------#


#DATASETS USED[2012,2013,2014,2015,2016,2017]
#Reading and writing CSV data files into 6 different objects[data_2012,data_2013,data_2014,data_2015,data_2016,data_2017]
    data_2012 <- read.csv("ontario_public_library_statistics_2012_open_data_may_22_2015_csv_final_9.csv",fileEncoding = "latin1")
    data_2013 <- read.csv("ontario_public_library_statistics_2013_open_data_may_22_2015_csv_final.csv",fileEncoding = "latin1")
    data_2014 <- read.csv("ontario_public_library_statistics_2014_open_data_csv_february_17_2016.csv",fileEncoding = "latin1")
    data_2015 <- read.csv("2015_ontario_public_library_statistics_open_data_dec_2017rev.csv",fileEncoding = "latin1")
    data_2016 <- read.csv("ontario_public_library_statistics_open_data_2016.csv",fileEncoding = "latin1")
    data_2017 <- read.csv("ontario_public_library_statistics_open_data_july_2019_rev1.csv",fileEncoding = "latin1")

#Checking the filed names for ensuring similarity.As the name is different change into common column name for 2012 and 2013 data set  
    #attributes(data_2012)
    #attributes(data_2013)
    #attributes(data_2014)
    #attributes(data_2015)
    #attributes(data_2016)
    #attributes(data_2017)
    colnames(data_2012)[colnames(data_2012) == "B2.9...Total.Operating.Revenues"] <- "B2.9..Total.Operating.Revenues"
    colnames(data_2013)[colnames(data_2013) == "B2.9...Total.Operating.Revenues"] <- "B2.9..Total.Operating.Revenues"

#Finding the common columns and append the objects vertically into single object.
    common_columns <- Reduce(intersect, list(colnames(data_2012),colnames(data_2013),colnames(data_2014),colnames(data_2015),colnames(data_2016),colnames(data_2017)))
    #print(common_columns)
    data_combined <- rbind(subset(data_2012, select = common_columns),subset(data_2013, select = common_columns),
                       subset(data_2014,select=common_columns),subset(data_2015,select=common_columns),
                       subset(data_2016,select=common_columns),subset(data_2017, select=common_columns))


##Getting average of total operating Revenue 
    #str(data_combined)# looking for the datatypes of the column name
    #print(data_combined$B2.9..Total.Operating.Revenues)
    data_combined$B2.9..Total.Operating.Revenues <- 
      gsub("[^0-9.-]", "", data_combined$B2.9..Total.Operating.Revenues)#checking for any character value present
    data_combined$B2.9..Total.Operating.Revenues <-
        as.numeric(data_combined$B2.9..Total.Operating.Revenues)# Converting character data type into numerical data type
    #print(data_combined$B2.9..Total.Operating.Revenues)

#Finding the Average and assign to object Aggregate_result
    aggregate_result<- aggregate(B2.9..Total.Operating.Revenues ~ Library.Full.Name,data=data_combined,FUN = mean)
    #print(aggregate_result)


# Sort the object to list top5 libraries having high Total Operating Revenue
    top_5 <- aggregate_result[order(-aggregate_result$B2.9..Total.Operating.Revenues),][1:5,]
    #print(top_5)


#Generating data in table format
    #install.packages("pander")
    library(pander)
    pander(top_5)

