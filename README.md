# Bike_Share
In this project I have analyzed the bike riding characteristics of casual and subscribers(members) of a bike share company located in Chicago. I have used the public data availbe to everyone of the year 2021.By analysing the different patterns of users I was able to provide data driven solutions to convert casual riders to members of the company.

## This project is a part of Google data analytics professional certificaate

# (i) Ask Phase
## 1.Guided questions
In this step of the analysis, I prepared a list of SMART(Specific, Measurable,Action Oriented,Reliable and Time bound) questions which I would be answering by the end of this project.
1. How do annual members and casual riders use Cyclistic bikes differently?
2. Why would casual riders buy Cyclistic annual memberships?
3. How can Cyclistic use digital media to influence casual riders to become members?

## 2.Key tasks
### Business Task:understand how casual riders and annual members use Cyclistic bikes differently
### Stakeholders
●  **Lily Moreno:** The director of marketing and your manager. Moreno is responsible for the development of campaigns
and initiatives to promote the bike-share program. These may include email, social media, and other channels.

● Cyclistic marketing analytics team: A team of data analysts who are responsible for collecting, analyzing, and
reporting data that helps guide Cyclistic marketing strategy. You joined this team six months ago and have been busy
learning about Cyclistic’s mission and business goals — as well as how you, as a junior data analyst, can help Cyclistic
achieve them.

● Cyclistic executive team: The notoriously detail-oriented executive team will decide whether to approve the
recommended marketing program.

# (ii) Prepare Phase
## Data Source: 
The datasets have a different name because Cyclistic is a fictional company. For the purposes of this case study,
the datasets are appropriate and will enable you to answer the business questions. The data has been made available by
Motivate International Inc. under this [license](https://www.divvybikes.com/data-license-agreement).

## Data integrity:
The data organized in all the datasets has coherence and data integrity is maintained.

## Does the data ROCCC
**Yes!!**

**Reliable:** The data comes from a highly reliable source

**Original:** The data is obtained from first party organistaion

**Comprehensive:** The data contains all the necessary parameters required for the analysis

**Current:** The data used is of the year 2021 which is fairly new

**Cited:** The data collected from first party [source](https://divvy-tripdata.s3.amazonaws.com/index.html)

# (iii) Process Phase
## Deciding the tool for analysis:
After downloading and viewing the data, I reliazed that the amount of data is very huge and when combined together for the analysis, the total number of observations will be over 5.5 Million. Microsoft excel can only process data upto 1 million rows so, I decided that excel will not be an appropriate tool for the analysis. 

And I have decided to use **Rstudio** for the analysis which is fast and can process tons of data.

## 1.Loading required Libraries:

Loading all the necessary packages required for thr project
```{r}
library(tidyverse) # metapackage of all tidyverse packages

library(readr) # to read csv files

library(dplyr) # for functions like bind_rows

library(ggplot2) # for visualisations

library(tidyr) # for performing necessary cleaning operations
```
## 2.Uploading the datasets to Rstudio:
```{r}
jan_2021 <- read_csv("/kaggle/input/bike-s/202101-divvy-tripdata/202101-divvy-tripdata.csv")

feb_2021 <- read_csv("/kaggle/input/bike-s/202102-divvy-tripdata/202102-divvy-tripdata.csv")

mar_2021 <- read_csv("/kaggle/input/bike-s/202103-divvy-tripdata/202103-divvy-tripdata.csv")

apr_2021 <- read_csv("/kaggle/input/bike-s/202104-divvy-tripdata/202104-divvy-tripdata.csv")

may_2021 <- read_csv("/kaggle/input/bike-s/202105-divvy-tripdata/202105-divvy-tripdata.csv")

jun_2021 <- read_csv("/kaggle/input/bike-s/202106-divvy-tripdata/202106-divvy-tripdata.csv")

jul_2021 <- read_csv("/kaggle/input/bike-s/202107-divvy-tripdata/202107-divvy-tripdata.csv")

aug_2021 <- read_csv("/kaggle/input/bike-s/202108-divvy-tripdata/202108-divvy-tripdata.csv")

sep_2021 <- read_csv("/kaggle/input/bike-s/202109-divvy-tripdata/202109-divvy-tripdata.csv")

oct_2021 <- read_csv("/kaggle/input/bike-s/202110-divvy-tripdata/202110-divvy-tripdata.csv")

nov_2021 <- read_csv("/kaggle/input/bike-s/202111-divvy-tripdata/202111-divvy-tripdata.csv")

dec_2021 <- read_csv("/kaggle/input/bike-s/202112-divvy-tripdata/202112-divvy-tripdata.csv")
```
## 3.creating a single large dataset & Cleaning :
I have verfied that all the data from 12 months have same number of columns and data in each column is of same type.

Now using bindrows function, we combine the rows of all twelve months.

```{r}
all_trips <- bind_rows(jan_2021,feb_2021,mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,sep_2021,oct_2021,nov_2021,dec_2021)
# There are a total of 5,595,063 obeservations in the combined data set all_trips
```
## 4.adding new columns and performing necessary actions
```{r}
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd

all_trips$month <- format(as.Date(all_trips$date), "%m")# Creates a month column

all_trips$day <- format(as.Date(all_trips$date), "%d") # creates a day Column

all_trips$year <- format(as.Date(all_trips$date), "%Y") # Creates a year column

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") # Creates a Day column

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at) # Calculates ride_length in time

# ride_length is store in seconds for accuracy
```
Storing ride_length as numeric data and verifying wether ride_length is numeric or not
```{r}
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

is.numeric(all_trips$ride_length)
```
> TRUE

dropping rows with no values with start_station_name as their column 

```{r}
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "" | all_trips$ride_length<0),]
count(all_trips_v2)
```
>A tibble: 1 × 1
n
> <int>
> 5594916


and we see a reduction in the rows to 5,594,916 rows

creating a version 3 of all trips by removing all the rows with NA values in them using drop_na() function 
```{r}
all_trips_v3 <- all_trips_v2 %>% drop_na()
count(all_trips_v3)
```
>A tibble: 1 × 1
n
> <int>
> 4588186

we see the rows got reduced to 4,588,186 rows from the 
initial number 5,595,063

## The data is now ready for analysis

# (iv) Analyse Phase:

## Firstly, we will look at a summary of the data:
```{r}
summary(all_trips_v3$ride_length)
```
>Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
> 
> 0     417     732    1309    1327  3356649 

## We can also summarise manually 

```{r}
mean(all_trips_v3$ride_length) #straight average (total ride length / rides)

median(all_trips_v3$ride_length) #midpoint number in the ascending array of ride lengths

max(all_trips_v3$ride_length) #longest ride

min(all_trips_v3$ride_length) #shortest ride

```
>1308.69209770484 is average ride length in seconds
>
>732 median ride length in seconds
>
>3356649 longest ride in seconds
>
>0 shortest ride

## Comparing members and casual users
```{r}
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)

aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)
```
> [output1]()

## See the average ride time by each day for members vs casual users

```{r}
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
```
> [output2]()

## Notice that the days of the week are out of order. Let's fix that.
```{r}
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
```
> [output3]()

## analyze ridership data by type and weekday
```{r}
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()                           #calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>%        # calculates the average duration
  arrange(member_casual, weekday)
```
> [output4]()

## Let's visualize the number of rides by rider type by weekday grouping
```{r}
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```
> [output5]()

## Let's create a visualization for average duration by weekday grouping
```{r}
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```
> [output6]()


## I have created the rest of the visualizations using Tableau 
 Here is the tableau viz [link](https://public.tableau.com/app/profile/phaneendra.pittala/viz/bike_share_16897164389530)
[Tableau visualizations pdf]()
 # (v) Share Phase:

 In share phase I have presented this work using many platforms like 
 [Kaggle](https://www.kaggle.com/code/phaneendrapittala/bike-share-project) , Tableau as mentioned above and github.

 # (vi) Act Phase

## These are the conclusions I made from analysing the data : 
1. Top three most popular stations are (1.Streeter Dr & GrandAve),
(2.Michigan Ave & OakSt) and 
(3.Millennium Park)

2. Number of rides took by members is 2,539,851 and by casual users is 2,048,335. However, average ride length of members is 13.18 minutes and by casual users is 32.51 minutes.
3. Most used bike type is Classic bike. 3,241,904 rides out of 4,588,186  rides are by classic bikes.
4. Casual users ride more during the weekend compared members who ride more during the weekdays. This happens because most members use bikes to commute for work whereas casual users ride for other purposes like exercise, leisure etc.,
5.  Bike rides peaked in the evening most of them occuring at 5PM.Members also ride in the morning usually at 8AM.
6.  Most popular month is July.

## Suggestions for Stakeholders:

1. Promote the advantages of using bikes at the most popular stations using billboard ads as this would attract more people.
2. Use social media to push ads concentrated in most popular station location.
3. Explain people how they can save money by taking membership compared to riding casually.
4. Explain how taking a membership motivates people to ride more often which results in good health.
5. Organize special events for members which builds connections betweem members and also attracts casual users to take subscriptions.
6. Promote how people can reduce their carbon footprint by using bikes.
