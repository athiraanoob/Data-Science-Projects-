install.packages("readxl",dependencies = True)
Sys.which("make")
install.packages("pkgbuild")
library(pkgbuild)
has_rtools()
install.packages("readxl")
demo("tripadvisor")
Return
library(nycflights13)
library(tidyverse)
nycflights13::flights
view(flights)
filter(flights,month==1, day==1)
(dec25 <- filter(flights, month == 12, day == 25))
near(sqrt(2) ^ 2,  2)
filter(flights, month == 11 | month == 12)
nov_dec<- filter(flights,month %in% c(11,12))
nov_dec
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)
is.na(x)
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
library(tibble)
df<- tibble(x=1:5,y= letters[1:5])
df
filter(df, is.na(x) | x > 1)
ar_delay_2hr<- filter(flights,flights$arr_delay==2)
view(ar_delay_2hr)
library(dplyr)
destination <- length(filter(flights,flights$dest %in% c("IAH","HOU")))
destination
oprt_by<- filter(flights, flights$carrier %in% c("UA","US","DL"))
oprt_by
departed_summer <- filter(flights,flights$month %in% c(7,8,9))
departed_summer
addnd<- filter(flights, flights$arr_delay>2,flights$dep_delay<=0)
addnd
Departed between midnight and 6am (inclusive)
colSums(is.na(flights))
arrange(flights,year,day,month)
arrange(flights, desc(dep_delay))
df<-tibble(x=c(1,2,3,NA))
df
arrange(df,desc(x))
library(nycflights13)
library(dplyr)

# Sort rows so missing values in dep_time appear first
sorted_flights <- flights %>%
  arrange(is.na(dep_time), dep_time)

# View result
head(sorted_flights)
sorted_flights <- flights %>%
  arrange(is.na(dep_time), is.na(arr_delay), dep_time, arr_delay)
sorted_flights
arrange(flights,desc(arr_delay),dep_time)
arrange(flights,desc(arr_delay))
arrange(flights,dep_time)
speed <- flights$distance/(flights$air_time/60)
m<-desc(speed)
m
fatest_flights <- flights %>% mutate(speed=distance/(air_time/60)) %>% arrange(desc(speed));
head(fatest_flights)
farthest<-arrange(flights, desc(distance))
farthest
print("farthest")
head(farthest)
print("nearest") 
tail(farthest)
select(flights,year,month, day)
select(flights,(year:day))
select(flights,-(year:day))
num_range("x",1:3)
rename(flights,tail_num=tailnum)
rename(flights,destination=dest)
select(flights,origin,destination,everything())

library(nycflights13)
library(dplyr)

# Define a vector of column names
vars <- c("year", "month", "day", "dep_delay", "arr_delay", "missing_col")

# Use any_of() inside select()
flights_selected <- flights %>%
  select(any_of(vars))

# View column names
colnames(flights_selected)

select(flights, contains("TIME")) # Select the field name that contain time

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)
#you only want to keep the new variables, use transmute()
transmute(flights_sml,
          gain=dep_delay-arr_delay,
          speed= distance/ air_time *60)

x<-1:10
lead(x)
lag(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
#> [1]  1  2  2 NA  4  5
min_rank(desc(y))
#> [1]  5  3  3 NA  2  1
ss<-arrange(flights,desc(arr_delay))
mm=head(ss$arr_delay,10)
tibble(f=(ss$arr_delay))
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day<- group_by(flights,year,month,day)
summarise(by_day,delay=mean(dep_delay, na.rm=TRUE))


by_dest<- group_by(flights,dest)
delay <- summarise(by_dest,
                  dist=mean(distance, na.rm=TRUE),
                  delay=mean(arr_delay,na.rm=TRUE))
delay
delay <- filter(delay, n() > 20, dest != "HNL")
delay
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count(), alpha = 1/3) +
  geom_smooth(se = FALSE)
  
  
  delays <- flights %>% 
    group_by(dest) %>% 
    summarise(
      count = n(),
      dist = mean(distance, na.rm = TRUE),
      delay = mean(arr_delay, na.rm = TRUE)
    ) %>% 
    filter(count > 20, dest != "HNL")
delay
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count, alpha = 1/3) +
               geom_smooth(se = FALSE)
             
             
             delays <- not_cancelled %>% 
               group_by(tailnum) %>% 
               summarise(
                 delay = mean(arr_delay)
               )
             
             ggplot(data = delays, mapping = aes(x = delay)) + 
               geom_freqpoly(binwidth = 10)
not_cancelled <- flights %>% 
               filter(!is.na(dep_delay), !is.na(arr_delay))
             
             not_cancelled %>% 
               group_by(year, month, day) %>% 
               summarise(mean = mean(dep_delay))
             delays <- not_cancelled %>% 
               group_by(tailnum) %>% 
               summarise(
                 delay = mean(arr_delay)
               )
             
             ggplot(data = delays, mapping = aes(x = delay)) + 
               geom_freqpoly(binwidth = 10)

             delays <- not_cancelled %>% 
               group_by(tailnum) %>% 
               summarise(
                 delay = mean(arr_delay, na.rm = TRUE),
                 n = n()
               )
             
             ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
               geom_point(alpha = 1/3)
             
             batting <- as_tibble(Lahman::Batting)
batting             
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
batters %>% arrange(desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )


daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
per_year <- summarise(per_month, flights= sum(flights))
per_year

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

library(tidyr)
library(dplyr)
library(tibble)

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half   = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks

library(tidyr)
library(tibble)

df1 <- tibble(x = c("a,b,c", "d,e,f,g", "h,i,j"))
df1
df1 %>% separate(x, c("one", "two", "three"))

view(who)
who1<-who %>% pivot_longer(new_sp_m014:newrel_f65, 
                           names_to = "key",
                           values_to = "cases",
                           values_drop_na = TRUE)
who1
who1 %>% count(key)