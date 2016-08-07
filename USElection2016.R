#Get data from the website
library(XML)

#Data From Polls published on Huffington Post

rawHuff <- readHTMLTable('http://elections.huffingtonpost.com/pollster/2016-general-election-trump-vs-clinton')
Huff <- data.frame(rawHuff[[1]])
Huff <- Huff %>% filter(!grepl("2015",Poll))
Huff_copy <- Huff

# Get Structure of Data

View(Huff)
dim(Huff)
str(Huff)

#Clean Data

library(stringr)
library(dplyr)

# Replace Poll Field to end with "Voters"
Huff$Poll <- gsub("Adults","Adult Voters",Huff$Poll)


# Extract num_voters from Poll Column
num_voters <- str_extract(Huff$Poll,word(Huff$Poll,-3))
num_voters <- gsub(",","",num_voters)
as.numeric(num_voters)

# Extract Voter Type from Poll Column

voter_type <- str_extract(Huff$Poll,word(Huff$Poll,-2))
voter_type<- as.vector(voter_type)


# Extract Poll name and clean it.
poll_name <- str_extract(Huff$Poll,word(Huff$Poll,1))

poll_name <- gsub("\n","",poll_name)

# Extract poll week
patt <- '(\\w+)\\s*(\\w+)\\s*\u2013\\s*(\\w+)\\s*(\\w+)'


poll_week <- str_extract(Huff_copy$Poll,patt)
poll_week

start_date <- word(poll_week,1,2)
end_date <- word(poll_week,-2,-1)

poll_start_date <- as.Date(start_date,"%b %d")
poll_end_date <- as.Date(end_date,"%b %d")

str(poll_start_date)

install.packages("lubridate")
library(lubridate)

ifelse(month(poll_start_date) == 01,"Jan","false")

# Add 5 new columns to Huff Dataframe
Huff_copy <- mutate(Huff_copy, num_of_voters = num_voters,type_of_voter = voter_type,
                    poll_name = poll_name,poll_week = poll_week,
                    poll_start_date = poll_start_date,poll_end_date = poll_end_date)

Huff_copy$num_of_voters <- as.numeric(Huff_copy$num_of_voters)
View(Huff_copy)

# Transpose data
library(tidyr)

Huff_gathered <- Huff_copy %>% gather(Candidate,percent_votes,Clinton:Other)

View(Huff_gathered)

Huff_gathered$percent_votes <- as.numeric(Huff_gathered$percent_votes)

Huff_gathered$Poll <- NULL

Huff_gathered <- Huff_gathered[c(4,5,8,9,6,7,2,3,1)]

str(Huff_gathered)

Huff_gathered$month <- ifelse(month(Huff_gathered$poll_start_date)==1,1,
                                   ifelse(month(Huff_gathered$poll_start_date)==2,2,
                                          ifelse(month(Huff_gathered$poll_start_date)==3,3,
                                                 ifelse(month(Huff_gathered$poll_start_date)==4,4,
                                                        ifelse(month(Huff_gathered$poll_start_date)==5,5,
                                                               ifelse(month(Huff_gathered$poll_start_date)==6,6,
                                                                      ifelse(month(Huff_gathered$poll_start_date)==7,7,
                                                                             ifelse(month(Huff_gathered$poll_start_date)==8,8,8))))))))


View(Huff_gathered)

# Basic Plot

library(ggplot2)

ggplot() +
  geom_point(data = Huff_gathered,aes(x = poll_name,y = poll_week,color = Candidate))

# grouped by polls

Huff_grouped <- aggregate(percent_votes ~ poll_name + Candidate+month,Huff_gathered,mean)
ggplot(Huff_grouped,aes(percent_votes,poll_name)) + geom_point(data = Huff_grouped,aes(color = Candidate))

# grouped by polls and showing only Clinton Vs Trump

Huff_CT<- Huff_grouped %>% filter(Candidate == "Clinton" |Candidate == "Trump")
ggplot(Huff_CT,aes(percent_votes,poll_name)) + geom_line(data = Huff_CT,aes(color = Candidate))


# Trend from January to Present 

Huff_CT_trend <- aggregate(percent_votes ~ Candidate + month,Huff_gathered,mean)

ggplot(Huff_CT_trend,aes(month,percent_votes)) + geom_line(data = Huff_CT_trend,aes(color = Candidate))



# Trend for each poll Clinton
Huff_Clinton_bymonth <- Huff_grouped %>% filter(Candidate == "Clinton")

Huff_Clinton_bymonth <- aggregate(percent_votes ~ poll_name + month,Huff_Clinton_bymonth,mean)

ggplot(data=Huff_Clinton_bymonth, aes(x=month, y=percent_votes , group=poll_name, colour=poll_name)) + geom_line() 

# Trend for each poll Trump


Huff_Trump_bymonth <- Huff_grouped %>% filter(Candidate == "Trump")

Huff_Trump_bymonth <- aggregate(percent_votes ~ poll_name + month,Huff_Trump_bymonth,mean)

ggplot(data=Huff_Trump_bymonth, aes(x=month, y=percent_votes , group=poll_name, colour=poll_name)) + geom_line() 

# Trend for each poll Other

Huff_Other_bymonth <- Huff_grouped %>% filter(Candidate == "Other")

Huff_Other_bymonth <- aggregate(percent_votes ~ poll_name + month,Huff_Other_bymonth,mean)

ggplot(data=Huff_Other_bymonth, aes(x=month, y=percent_votes , group=poll_name, colour=poll_name)) + geom_line() 

# Trend for each poll Undecided

Huff_Undecided_bymonth <- Huff_grouped %>% filter(Candidate == "Undecided")

Huff_Undecided_bymonth <- aggregate(percent_votes ~ poll_name + month,Huff_Undecided_bymonth,mean)

ggplot(data=Huff_Undecided_bymonth, aes(x=month, y=percent_votes , group=poll_name, colour=poll_name)) + geom_line() 
