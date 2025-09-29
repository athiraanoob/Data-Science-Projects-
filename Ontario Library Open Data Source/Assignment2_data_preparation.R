######################################################################################################################
# PROGRAM FOR LISTING NUMBER OF LIBRARIES IN EACH CITY FOR CORRESPONDING YEARS 2019,2020,2021,2022,2023 IN A TABLE    #                                                                                                         
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
  library_table <- library_counts %>% pivot_wider(names_from = Year, values_from = Num_Libraries, values_fill = list(Num_Libraries = 0))
  library_table
  
#Generating output in table format  
  library_table1 <- head(library_table)
  library(pander)
  pander(library_table1)
