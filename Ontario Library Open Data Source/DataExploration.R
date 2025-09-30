###############################################################################################################
#                                                                                                             #                   
# CODE FOR FINDING TOTAL OPERATING REVENUE PER CARDHOLDER AND RELATED, INFERRED INSIGHTS BASED ON DATA        #                 
#                                                                                                             #                   
#                                                                                                             #                   
###############################################################################################################

##CODE
  #Reading CSV file into object
    library_data<-read.csv(file = "library_data.csv",encoding = "latin")
    library(dplyr)
    library("rapportools")
    library("plyr")
  #Using mutate, created new column Total_operating_Rvnue_pr_ActivCardHolder after checking for duplicates and cleaning the corresponding input columns
    num<-sum(duplicated(library_data))
    library_data1 <- library_data %>%
     mutate(
      B2.9..Total.Operating.Revenues = as.numeric(gsub("[^0-9.]", "",
                                        B2.9..Total.Operating.Revenues)),
      A1.14..No..of.Active.Library.Cardholders = as.numeric(gsub("[^0-9.]", "",
                                        A1.14..No..of.Active.Library.Cardholders)),
      Total_operating_Rvnue_pr_ActivCardHolder =ifelse(A1.14..No..of.Active.Library.Cardholders==0, NA,
                                B2.9..Total.Operating.Revenues / A1.14..No..of.Active.Library.Cardholders
          ))
    View(library_data1)
    #checking for correlation between total operating revenue and number of active card holders, 
                #we are getting cor value as 0.9828333,means positive correlation.
    # as based on pi value shows statistically significant.
    s<-cor.test(library_data1$B2.9..Total.Operating.Revenues,library_data1$A1.14..No..of.Active.Library.Cardholders)
    print(s)
  #Plot the histogram with Total_operating_Rvnue_pr_ActivCardHolder and noticed deviations in each range[0-225,(225-500), above 500] 
    library(ggplot2)
    ggplot(data = library_data1) +
    geom_histogram(mapping = aes(x=Total_operating_Rvnue_pr_ActivCardHolder),binwidth = 10)+
                             coord_cartesian(ylim=c(0,75))


##INSIGHT 1
  #Create a data set with library data greater than 500 for analysis.
    outliner<-library_data1 %>% 
            filter(Total_operating_Rvnue_pr_ActivCardHolder>500)
    View(outliner)
    Outliner_data<-select(outliner,Library.Full.Name,A1.4.Type.of.Library.Service..English.,
                        A1.14..No..of.Active.Library.Cardholders,B2.9..Total.Operating.Revenues,Total_operating_Rvnue_pr_ActivCardHolder)
    View(Outliner_data)
    install.packages("kableExtra")
    library(kableExtra)
    knitr::kable(Outliner_data, caption = "Outliner Data") %>%
      kable_styling(bootstrap_options = c("striped", "hover"))
  #Generating data in table format
  #install.packages("pander")
    #library(pander)
    #pander(Outliner_data)
    #tinytex::install_tinytex()
    #tinytex::tlmgr_install("multirow")
##EXPLANATION 1
  #We are having 39 out-liner libraries
  #In the above code we are looking for the Total_operating_Rvnue_pr_ActivCardHolder,whose value is greater than 500,which is considered as an out-liner.
  #Higher value for Total_operating_Rvnue_pr_ActivCardHolder means, 
         #lower number of active card holders compared to the Operating revenue provided for them.
  #When analyzing the data set we can see that most of the libraries are First Nation Libraries.
##SUGGESSIONS FOR TAKING ACTIONS
  #Tried to identified the needs and generate programs for supporting First Nation Libraries and people.
  #Can use surveys and social media for identifying their needs and plan accordingly.
  #Tried to create facilities, resources, services and programs which increases user interactions,and in that way utilize the Total Operating Revenue to its maximum benefit.
  

  
##INSIGHT 2
  #Create a data set with Total_operating_Rvnue_pr_ActivCardHolder column with value NA for analysis
    library(dplyr)
    zero_card_hlders<-library_data1 %>% filter(is.na(Total_operating_Rvnue_pr_ActivCardHolder) & 
                                               A1.14..No..of.Active.Library.Cardholders == 0)
    count1<-nrow(zero_card_hlders)
    print(count1)
    Library_details_zero_cardholder<-select(zero_card_hlders,
                                      Library.Full.Name,A1.4.Type.of.Library.Service..English.,
                                      A1.14..No..of.Active.Library.Cardholders,
                                      B2.9..Total.Operating.Revenues,Total_operating_Rvnue_pr_ActivCardHolder)
    Library_details_zero_cardholder1= head(Library_details_zero_cardholder,15)
  #Generating data in table format
    library(pander)
    pander(Library_details_zero_cardholder1)
  # Getting the count and listing the libraries with number of active card holders is 0
    count<-sum(library_data1$A1.14..No..of.Active.Library.Cardholders == 0, na.rm = TRUE)
##EXPLANATION 2
  #Wherever Total_operating_Rvnue_pr_ActivCardHolder is NA,the corresponding library's number of active card holders is also 0.
  #That indicates for the corresponding libraries, number of library users is 0,but total operating revenue is generated.
  #Most of the library type is contacting municipality or contracting LSB type. 
##SUGGESSIONS FOR TAKING ACTIONS  
  #So need to check the efficiently of contracting firms and their contracting terms of working.
  #Initially we need to check weather those libraries are existing or still contract is valid.
  #There will be less possibility that libraries with 0 active card holders exist. 
  #If existing generate programs, resources , services which improves user interactions with public. 
  #Also social media advertisements can also be helpful.
  #This helps in increasing number of active card holders, and maximum utilization of the Total operating revenue.
  #If library is not existing or contract is not existing, tried to utilize the libraries total operating revenue for supporting other libraries.


  
##INSIGHT 3
  # Creating a data set where majority of Total_operating_Rvnue_pr_ActivCardHolder resides, which is less than 500
    majority=library_data1 %>% 
    filter(Total_operating_Rvnue_pr_ActivCardHolder<500)
    View(majority)
    # when Analyze the majority values,i can see A1.4.Type.of.Library.Service..English. with value County, County co-operative or Regional Municipality Library
    #is listed bellow
    vv<-majority %>% filter(A1.4.Type.of.Library.Service..English.=="County, County co-operative or Regional Municipality Library")
    View(vv)
    #trying to find relation between above 2 variables using bar chart
    majority$B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..<-as.numeric(gsub("[^0-9.]","",majority$B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..))
    print(majority$B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..)
    cor.test(majority$B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..,majority$B2.9..Total.Operating.Revenues)
    #There is an extremely strong, statistically significant positive correlation between val and Total Operating Revenues. These two variables are almost linearly dependent.
    ggplot(majority, 
           aes(x =B2.9..Total.Operating.Revenues,
                       y=B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..)) +
                       geom_point(stat = "identity") +coord_cartesian(ylim=c(0,30000),xlim = c(0,1000000))+
                       theme(axis.text.x= element_text(size = 10, angle = 0, hjust = 1)) +
                       theme_minimal() +
                       labs(title = "Comparison of Library Revenues",
                       x = "Operating revenue",
                       y = "Self generating revenue") +
                       theme(legend.position = "none")
  #General trend looks like self generating revenue is proportionately increases with operating revenue.
  # From plot we could find like most of values between 0-250000 range on X-axis.
  # We need to concentrate here on out-liners with less Total operating revenue, but corresponding one have more Self generating revenue.
  #From the plot I could infer like points between 15,0000-500000in X-axis with corresponding points between 15000-35000 in Y-axis
    nn<-majority %>% filter((between(B2.9..Total.Operating.Revenues,150000,600000)) & (between(B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..,15000,50000)))
    View(nn)
    
    core_analysis<-majority %>% filter(Total_operating_Rvnue_pr_ActivCardHolder>250)
  #Sorting data based on self generated revenue,Total_operating_Rvnue_pr_ActivCardHolder and Net balance brought from previous year 
       View(core_analysis)                                                                                  #after cleaning the corresponding columns.
    core_analysis$B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc.. <- as.numeric(
                gsub("[^0-9.]", "", 
                core_analysis$B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..))
    model_data<-arrange(core_analysis,B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..,
                      Total_operating_Rvnue_pr_ActivCardHolder,B1.1..Net.Balance.brought.forward.from.previous.year)
  #print(model_data)
  #Select the last 5 observations from bottom of data set, which can be model to other libraries
    options(max.print = 10000)  # Set a higher limit
    Model_Libraries <- arrange(tail(model_data),
                          desc(B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..))
    Model_Libraries1<-select(Model_Libraries,Library.Number,Library.Full.Name,
                       Survey.Year.From,A1.3.Ontario.Library.Service..OLS..Region..English.,
                       A1.10.City.Town,A1.14..No..of.Active.Library.Cardholders,
                       B2.6..Self.Generated.Revenue..e.g..fines..fees..sales.fundraising..room.rentals..cafe.revenue..etc..,
                      B2.9..Total.Operating.Revenues,Total_operating_Rvnue_pr_ActivCardHolder)
    View(Model_Libraries1)
    head(Model_Libraries1)
##EXPLANATION 3
  #Using the above code we are trying to find first 6 libraries and their details, where their interactions, programs and services that can be model to other libraries.
  # Library names specified above can be analyzed to identify programs that can be model to other libraries.
  # Libraries with medium values on Total_operating_Rvnue_pr_ActivCardHolder will be doing greatly as,
          #those have optimized or medium desirable value, as they have great number of active card holders and total operating revenue utilization.
  #Here we are trying to list libraries based on highest self generated revenue, 
           #with corresponding Total_operating_Rvnue_pr_ActivCardHolder and revenue get back from previous year.
##SUGGESSIONS FOR TAKING ACTIONS  
  # The listed libraries have lots of interesting programs and services created for public.
  # I had selected top5 model libraries based on their data.
  # Needs a through analysis on the data of corresponding libraries.
  #They will gives us an idea on how we can implement same on other libraries.
  #If we could able to implement same in other libraries will improves the user interactions and services libraries can offer.


