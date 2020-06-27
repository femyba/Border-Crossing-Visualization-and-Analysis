# MSDS692_40B_Data SCience Practicum-I
# Date: 6/24/2020
# Title: Border Crossing Visualization and Analysis
# Name: Olufemi Babalola
# Dataset: US Border Crossings entry data downloaded from kaggle

##########################################################################

# The dataset is provided by the Bureau of Transportation Statistics (BTS) and covers the
# Incoming vehicle, container, passenger, and pedestrian counts at U.S.-Mexico
# and U.S.-Canada land border ports.

# Dataset description

# The data reflect the number of vehicles, containers, passengers or 
# pedestrians entering the United States.
# Border: Identifies which border was crossed
# Date: Timestamp indicating month and year of crossing
# Measure: Indicates the mode of transportation in border crossing being measured 
# Value: indicates the total number of crossings

# Install "tidyverse" package
install.packages("tidyverse")

# load the tidyverse package into library
library(tidyverse)

# load the package ggplot2 into library
library(ggplot2)

# import dataset into r
bc <- read.csv("~/Practicum I/Border_Crossing_Entry_Data.csv")

####exploring the content and structure of our dataset

# check the dimension of the dataset
dim(bc)

# check the class of the dataset
class(bc)

# check column names of the dataset
colnames(bc)

# check the structure of the dataset
str(bc)

# check if there are any missing values
any(is.na(bc))

# check the top 5 rows of the dataset
head(bc)

# check the summary statistics
summary(bc)

# change the colmuns headers to lowercase for uniformity
names(bc) <- tolower(names(bc))

# reformat the date column to exclude the time
date <- format(as.POSIXct(strptime(bc$date,"%m/%d/%Y %H:%M",tz="")) ,format = "%m/%d/%Y")
bc$date <- date

# Extract the year, month and day from the date column
bc$year <- format(as.Date(bc$date, format="%m/%d/%Y"), "%Y")
bc$month<-format(as.Date(bc$date, format="%m/%d/%Y"), "%m")
View(bc)
head(bc)


### Create visualizations from the dataset

################################################################
library(dplyr)
library(ggplot2)
library(data.table)


################################################################
# checking inbound traffic at US Borders
summarized.border = bc[, list(total=sum(value)), by="border"]
summarized.border

#################################################################

# number of inbound_crossings by US border
incoming_crossing_border = bc %>% 
  group_by(border) %>% 
  summarise(incoming_crossing = sum(value))-> border_crossings
border_crossings <- as.data.frame(border_crossings)
border_crossings

# plot of inbound crossings at US borders

ggplot(data = border_crossings,
       mapping = aes(x = border,
                     y = incoming_crossing)) + 
   geom_bar(stat = "identity", fill = "blue")

##################################################################
# checking inbound traffic at US Ports
summarized.port = bc[, list(total=sum(value)), by="port.name"]
summarized.port

###################################################################

# number of inbound_crossings by Port name
incoming_crossing_port = bc %>% 
  group_by(port.name) %>% 
  summarise(inbound_crossing = sum(value))-> Port_crossings
Port_crossings <- as.data.frame(Port_crossings)
Port_crossings

# plot of inbound crossings at US Ports

ggplot(data = Port_crossings,
       mapping = aes(x = port.name,
                     y = inbound_crossing)) +
  geom_bar(stat = "identity", fill = "green") +
  ggtitle("Inbound crossing at US Ports")

# sort Port_crossings data in descending order
df_Ports <- Port_crossings[order(-Port_crossings$inbound_crossing),]

# check the inbound crossings for top 10 Ports
# and assigned to new data frame "Top10_Ports"
Top10_Ports <- head(df_Ports, 10)
Top10_Ports

# plot of inbound crossings for top 10 ports
ggplot(data = Top10_Ports,
       mapping = aes(x = port.name,
                     y = inbound_crossing)) +
  geom_col(stat="identity", fill="green") + coord_flip() +
  ggtitle("Inbound crossing of 10 Top Ports")

##################################################################
#checking inbound traffic at US Border States
summarized.state = bc[, list(total=sum(value)), by="state"]
summarized.state
##################################################################

# number of inbound_crossings by State
incoming_crossing_States = bc %>% 
  group_by(state) %>% 
  summarise(inbound_crossing = sum(value))-> States_crossings
States_crossings <- as.data.frame(States_crossings)
States_crossings

# plot of inbound crossings at US Border States
library(ggplot2)
ggplot(data = States_crossings,
       mapping = aes(x = state,
                     y = inbound_crossing)) +
  geom_bar(stat = "identity", fill = "green") +
  ggtitle("Inbound crossing at US Border states")

# sort States_crossings data in descending order
#Border_States <- States_crossings[order(-States_crossings$inbound_crossing),]

# check the inbound crossings for top 10 Border States
# and assigned to new data frame "Top10_States"
#Top10_States <- head(Border_States, 10)
#Top10_States

# plot of inbound crossings for top 10 US Border States
#ggplot(data = Top10_States,
#       mapping = aes(x = state,
#                     y = inbound_crossing)) +
#  geom_bar(stat = "identity", fill = "purple") +
#  ggtitle("Inbound crossing of 10 Top states")

###################################################################
#checking inbound Traffic by means of Transportation
summarized.measure = bc[, list(total=sum(value)), by="measure"]
summarized.measure
####################################################################

# number of inbound_crossings by Means of Tansportation
incoming_crossing_measure = bc %>% 
  group_by(measure) %>% 
  summarise(inbound_crossing = sum(value))-> Transportation_Method
Transportation_Method <- as.data.frame(Transportation_Method)
Transportation_Method

# plot of inbound crossings by transportation methods
library(ggplot2)
ggplot(data = Transportation_Method,
       mapping = aes(x = measure,
                     y = inbound_crossing)) +
  geom_bar(stat = "identity", fill = "brown") +
  ggtitle("Inbound crossing by Means of transportation")

# sort Measure in descending order
Traffic <- summarized.measure[order(-summarized.measure$total),]

# check the inbound crossings for top 10 measures
# and assigned to new data frame "Top_10_Measure"
Top_10_Measure <- head(Traffic, 10)
Top_10_Measure

# plot of inbound crossings for top 10 Transportation Methods
ggplot(data = Top_10_Measure,
       mapping = aes(x = measure,
                     y = total)) +
  geom_col(stat = "identity", fill = "purple") + coord_flip() +
  ggtitle("Inbound crossing by Means of Transportation")

###################################################################
# checking inbound traffic by year
summarized.year = bc[, list(total_crossing=sum(value)), by="year"]
summarized.year
###################################################################

# plot Inbound traffic at US Borders by year
ggplot(bc, aes(x=year, y=value)) + stat_summary(fun="mean", geom="line", aes(group=border, color=border)) + 
  ggtitle("Inbound Crossings at US Borders by Year") + ylab("Count") + xlab("Year") +
  theme(axis.text.x = element_text(angle = 60)) + 
  theme(legend.position="right") +
  scale_color_manual(values= c("blue", "brown"))

####################################################################
# Inbound traffic at US Borders by year and by measure
summarized.measure.year = bc[, list(total=sum(value)), by=c("year","measure")]
measure_by_year <- summarized.measure.year
measure_by_year

###################################################################
# Inbound traffic at US Borders by month and by measure
summarized.measure.year = bc[, list(total=sum(value)), by=c("month","measure")]
measure_by_month <- summarized.measure.month
measure_by_month

###################################################################
# Inbound traffic at US Borders by States, Borders and Ports
summarized.state.port.border = bc[, list(total=sum(value)), by=c("state","border","port.name")]
summarized.state.port.border


  

