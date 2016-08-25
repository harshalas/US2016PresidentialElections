#Get data from the website
library(XML)

library(stringr)
library(dplyr)
library(lubridate)
library(forecast)
library(tidyr)

#Data From Polls published on Huffington Post

rawHuff <- readHTMLTable('http://elections.huffingtonpost.com/pollster/2016-general-election-trump-vs-clinton')
Huff <- data.frame(rawHuff[[1]])
Huff <- Huff %>% dplyr::filter(!grepl("2015",Poll))
Huff_copy <- Huff

# Get Structure of Data

View(Huff)
dim(Huff)
str(Huff)

#Clean Data

# Replace Poll Field to end with "Voters"
Huff$Poll <- gsub("Adults","Adult Voters",Huff$Poll)

# Extract num_voters from Poll Column
num_voters <- str_extract(Huff$Poll,word(Huff$Poll,-3))
num_voters <- as.numeric(gsub(",","",num_voters))

# Extract Voter Type from Poll Column
voter_type <- as.vector(str_extract(Huff$Poll,word(Huff$Poll,-2)))

# Extract Poll name and clean it.
poll_name <- str_extract(Huff$Poll,word(Huff$Poll,1))
poll_name <- gsub("\n","",poll_name)

# Extract poll week
patt <- '(\\w+)\\s*(\\w+)\\s*\u2013\\s*(\\w+)\\s*(\\w+)'

poll_week <- str_extract(Huff$Poll,patt)

start_date <- word(poll_week,1,2)
end_date <- word(poll_week,-2,-1)

poll_start_date <- as.Date(start_date,"%b %d")
poll_end_date <- as.Date(end_date,"%b %d")

#install.packages("lubridate")
library(lubridate)


# Add 5 new columns to Huff Dataframe
Huff <- mutate(Huff, num_of_voters = num_voters,type_of_voter = voter_type,
                    poll_name = poll_name,poll_week = poll_week,
                    poll_start_date = poll_start_date,poll_end_date = poll_end_date)


# Compute month variable from poll_start_date
Huff$month <- ifelse(month(Huff$poll_start_date)==1,"Jan",
                              ifelse(month(Huff$poll_start_date)==2,"Feb",
                                     ifelse(month(Huff$poll_start_date)==3,"Mar",
                                            ifelse(month(Huff$poll_start_date)==4,"Apr",
                                                   ifelse(month(Huff$poll_start_date)==5,"May",
                                                          ifelse(month(Huff$poll_start_date)==6,"Jun",
                                                                 ifelse(month(Huff$poll_start_date)==7,"Jul",
                                                                        ifelse(month(Huff$poll_start_date)==8,"Aug",
                                                                               ifelse(month(Huff$poll_start_date)==9,"Sep",
                                                                                      ifelse(month(Huff$poll_start_date)==10,"Oct",
                                                                        ifelse(month(Huff$poll_start_date)==11,"Nov","Nov")))))))))))


Huff$month <-as.Date(as.yearmon(Huff$month,"%b"))

# Remove Poll Column
Huff$Poll <- NULL

# Convert percent_votes from char to numeric
Huff$Trump <- as.numeric(as.character(Huff$Trump))
Huff$Clinton <- as.numeric(as.character(Huff$Clinton))
Huff$Other <- as.numeric(as.character(Huff$Other))
Huff$Undecided <- as.numeric(as.character(Huff$Undecided))

Huff <- Huff[c(8,9,1,2,3,4,12,10,11,7,6,5)]


Clinton_by_poll_month <- aggregate(Clinton~poll_name+month,data = Huff,mean)

Trump_by_poll_month <- aggregate(Trump~poll_name+month,data = Huff,mean)


ClintonVsTrump_by_poll_month <- merge(Clinton_by_poll_month,Trump_by_poll_month,  by = c("poll_name","month"))

Clinton_by_poll <- aggregate(Clinton ~ poll_name,ClintonVsTrump_by_poll_month,mean)

Trump_by_poll <- aggregate(Trump ~ poll_name,ClintonVsTrump_by_poll_month,mean)


ClintonVsTrump_by_poll <- merge(Clinton_by_poll,Trump_by_poll,  by = "poll_name")


Huff$ClintonLead <-  with(Huff, ifelse(word(Spread,1) == "Clinton",1,0))

Huff$TrumpLead <-  with(Huff, ifelse(word(Spread,1) == "Trump",1,0))

Huff$ClintonLeads <- Huff$Clinton - Huff$Trump


# Transpose data
library(tidyr)

Huff_gathered <- Huff %>% gather(Candidate,percent_votes,Trump:Undecided)
str(Huff_gathered)
Huff_gathered$Candidate <- as.factor(Huff_gathered$Candidate)

# Basic Plot

library(ggplot2)

#### IMP ####
ggplot(Huff_gathered,aes(month,percent_votes)) + 
  geom_line(data = Huff_gathered,aes(group = Candidate,color = Candidate)) +
  facet_wrap(~poll_name)+ scale_colour_manual(values=c("blue","green","red","black"))

# Trend from January to Present 

Huff_CT_trend <- aggregate(percent_votes ~ Candidate + month,Huff_gathered,mean)
ggplot(Huff_CT_trend,aes(month,percent_votes)) + 
  geom_line(data = Huff_CT_trend,aes(group = Candidate,color = Candidate)) +
  scale_colour_manual(values=c("blue","green","red","black"))


# grouped by polls and showing only Clinton Vs Trump

Huff_CT<- Huff_gathered %>% dplyr::filter(Candidate == "Clinton" |Candidate == "Trump")

ggplot(Huff_CT, aes(x=month,y=percent_votes,fill=Candidate))+
  geom_bar(stat="identity",position="dodge") + facet_wrap(~poll_name) + 
  scale_fill_manual(values=c("blue","red"))


ggplot(Huff_CT, aes(month,percent_votes, fill=Candidate)) + 
  geom_bar(stat="identity",position="dodge") + 
  scale_fill_manual(values=c("blue","red"))


Huff_CTO_Trend <- aggregate(percent_votes ~ Candidate + month,Huff_CT,mean)


## Line Plot for clinton Vs Trump 
ggplot(Huff_CT,aes(month,percent_votes)) + 
  geom_line(data = Huff_CT,aes(group = Candidate,color = Candidate)) +
  facet_wrap(~poll_name) + scale_fill_manual(values = alpha(c("blue", "red")))



# Modelling

## Time Series Model

Huff$ClintonLeads <- Huff$Clinton - Huff$Trump
Huff_clinton <- select(Huff,month,Clinton)
Huff_clintonLeads <- select(Huff,month,ClintonLeads)


library(zoo)
library(timeSeries)

Huff_agg <- aggregate(Clinton~month,data= Huff_clinton,mean)
Huff_ts_agg <- ts(Huff_agg$Clinton)

plot.ts(Huff_ts_agg,col="blue",xlab = "month", ylab = "Clinton Leads")


library(forecast)

Clintonforecasts <- forecast(Huff_ts_agg,h=8,level=c(80,95))

plot(Clintonforecasts)
plot(fitted(Clintonforecasts))

plot(Clintonforecasts, xlab="time")
lines(fitted(Clintonforecasts),col="blue")
summary(Clintonforecasts)

