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

#install.packages("lubridate")
library(lubridate)

#ifelse(month(poll_start_date) == 01,"Jan","false")

# Add 5 new columns to Huff Dataframe
Huff_copy <- mutate(Huff_copy, num_of_voters = num_voters,type_of_voter = voter_type,
                    poll_name = poll_name,poll_week = poll_week,
                    poll_start_date = poll_start_date,poll_end_date = poll_end_date)

Huff_copy$num_of_voters <- as.numeric(Huff_copy$num_of_voters)
str(Huff_copy)

Huff_copy$type_of_voter <- as.factor(Huff_copy$type_of_voter)

Huff_copy$month <- ifelse(month(Huff_copy$poll_start_date)==1,1,
                              ifelse(month(Huff_copy$poll_start_date)==2,2,
                                     ifelse(month(Huff_copy$poll_start_date)==3,3,
                                            ifelse(month(Huff_copy$poll_start_date)==4,4,
                                                   ifelse(month(Huff_copy$poll_start_date)==5,5,
                                                          ifelse(month(Huff_copy$poll_start_date)==6,6,
                                                                 ifelse(month(Huff_copy$poll_start_date)==7,7,
                                                                        ifelse(month(Huff_copy$poll_start_date)==8,8,8))))))))


Huff_copy$month <- as.factor(Huff_copy$month)

Huff_copy$Poll <- NULL
Huff_copy$Trump <- as.numeric(as.character(Huff_copy$Trump))
Huff_copy$Clinton <- as.numeric(as.character(Huff_copy$Clinton))
Huff_copy$Other <- as.numeric(as.character(Huff_copy$Other))
Huff_copy$Undecided <- as.numeric(as.character(Huff_copy$Undecided))
summary(Huff_copy)
str(Huff_copy)

Huff_copy %>% filter(Trump > Clinton) %>% select(poll_name,Clinton,Trump,month)

nrow(Huff_copy)

nrow(Huff_copy %>% filter(month == 8))

attach(Huff_copy)

ggplot(data=Huff_copy, aes(x=month, y=Trump)) +
  geom_line()+
  facet_wrap(~poll_name)

Clinton_by_poll <- aggregate(Clinton~poll_name+month,data = Huff_copy,mean)

Trump_by_poll <- aggregate(Trump~poll_name+month,data = Huff_copy,mean)

ClintonVsTrump_by_poll <- merge(Clinton_by_poll,Trump_by_poll,  by.x = "poll_name", by.y = "poll_name",by.x = "month", by.y = "month")

ClintonVsTrump_by_poll <- merge(Clinton_by_poll,Trump_by_poll,  by = c("poll_name","month"))


#summary(lm(ClintonVsTrump_by_poll$Trump>ClintonVsTrump_by_poll$Clinton~ClintonVsTrump_by_poll$poll_name))
#summary(lm(ClintonVsTrump_by_poll$Clinton>ClintonVsTrump_by_poll$Trump~ClintonVsTrump_by_poll$poll_name))


ggplot(data = Huff_copy,mapping = aes(month,Clinton)) +  geom_line() + facet_wrap(~poll_name)

# Transpose data
library(tidyr)

Huff_gathered <- Huff_copy %>% gather(Candidate,percent_votes,Trump:Undecided)

View(Huff_gathered)

str(Huff_gathered)

Huff_gathered$Candidate <- as.factor(Huff_gathered$Candidate)


Huff_gathered$Poll <- NULL

Huff_gathered <- Huff_gathered[c(4,5,9,10,8,6,7,2,3,1)]

str(Huff_gathered)

summary(Huff_gathered)

summary(Huff_gathered$type_of_voter)

summary(Huff_gathered$num_of_voters)

# Basic Plot

library(ggplot2)

ggplot(Huff_gathered,aes(month,percent_votes)) + 
  geom_line(data = Huff_gathered,aes(group = Candidate,color = Candidate)) +
  facet_wrap(~poll_name)


# grouped by polls and showing only Clinton Vs Trump

Huff_CT<- Huff_gathered %>% filter(Candidate == "Clinton" |Candidate == "Trump")
ggplot(Huff_CT,aes(month,percent_votes)) + 
  geom_line(data = Huff_CT,aes(group = Candidate,color = Candidate)) +
  facet_wrap(~poll_name)


# Trend from January to Present 

Huff_CT_trend <- aggregate(percent_votes ~ Candidate + month,Huff_gathered,mean)

ggplot(Huff_CT_trend,aes(month,percent_votes)) + geom_line(data = Huff_CT_trend,aes(group = Candidate,color = Candidate))





# Modelling
set.seed(123)

test <- sample(nrow(Huff_copy),0.3*nrow(Huff_copy))  # Take a random sample of 30% of the data as "test"

data.train <- Huff_copy[-test,]  # All non-test data are classified as part of the training set
data.test <- Huff_copy[test,]    # Testing set (out-of-sample data; holdout; data not yet seen)

nrow(data.train)
nrow(data.test)

### Linear Models
### (1) Simple Linear Regression (y vs x)

# For Clinton

lm.Clinton <- lm(Clinton>Trump~poll_name, data=data.train)
lm.Clinton
summary(lm.Clinton)
abline(lm.Clinton)
plot(lm.Clinton)

# Use the model on the training set:
lmClinton.train <- predict(lm.Clinton,data=data.train)

# Show the mean squared error (MSE): known actual - predicted
mean((lmClinton.train-data.train$Clinton)^2) # 2044.965 ~ 45.22129 people will vote for Clinton

# Similarly, use the model on the testing set and solve for the mean squared error:
lmClinton.test <- predict(lm.Clinton,data.test)
mean((lmClinton.test-data.test$Clinton)^2) # 2009.237 ~ 44.82% people will vote for Clinton

# For Trump
lm.Trump <- lm(Trump>Clinton~poll_name, data=data.train)
lm.Trump
summary(lm.Trump)

# Use the model on the training set:
lmTrump.train <- predict(lm.Trump,data=data.train)

# Show the mean squared error (MSE): known actual - predicted
mean((lmTrump.train-data.train$Trump)^2) # 1654.744 ~ 40.67% people will vote for Trump

# Similarly, use the model on the testing set and solve for the mean squared error:
lmTrump.test <- predict(lm.Trump,data.test)
mean((lmTrump.test-data.test$Trump)^2) # 1648.087 ~ 40.59% people will vote for Trump


###### Model for Clinton Vs Trump for each month(based on the issues each candidate supports)

lm.ClintonA <- lm(Clinton>Trump~poll_name+month, data=data.train)
lm.ClintonA
summary(lm.ClintonA)
plot(lm.ClintonA)


# Use the model on the training set:
lmClintonA.train <- predict(lm.ClintonA,data=data.train)

# Show the mean squared error (MSE): known actual - predicted
mean((lmClintonA.train-data.train$Clinton)^2) # 2044.847 ~ 45.219% people will vote for Clinton

# Similarly, use the model on the testing set and solve for the mean squared error:
lmClintonA.test <- predict(lm.ClintonA,data.test)
mean((lmClintonA.test-data.test$Clinton)^2) # 2008.237 ~ 44.81% people will vote for Clinton

plot((lmClintonA.train-data.train$Clinton)^2)

# For Trump
lm.TrumpB <- lm(Trump>Clinton~poll_name+month, data=data.train)
lm.TrumpB
summary(lm.TrumpB)

plot(lm.TrumpB)

# Use the model on the training set:
lmTrumpB.train <- predict(lm.TrumpB,data=data.train)

# Show the mean squared error (MSE): known actual - predicted
mean((lmTrumpB.train-data.train$Trump)^2) # 1654.588 ~ 40.67% people will vote for Trump

# Similarly, use the model on the testing set and solve for the mean squared error:
lmTrumpB.test <- predict(lm.TrumpB,data.test)
mean((lmTrumpB.test-data.test$Trump)^2) # 1648.743 ~ 40.60% people will vote for Trump

