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
    str(data_combined)# looking for the datatypes of the column name
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
    print(top_5)

    
#Generating data in table format
    #install.packages("pander")
    library(pander)
    pander(top_5)
