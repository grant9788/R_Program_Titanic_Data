# Brent Grant
# 5/27/2018
# Resource - https://www.youtube.com/watch?v=32o0DnuRjfg&t=3931s


# Load Data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)


# Add a Survived variable to the test set to allow for combining data sets
# data fram is to create data frame function - need type ?data.frame in console
# Add a variable to the data frame 
# rep() is to rep the value of num in the rows and assign to survived varible
# test[,] I want to in rows and colums  if left blank you will get all the rows and colunms
test.survived <- data.frame(Survived = rep("None",nrow(test)), test[,])

# Combine data sets - ?rbind for help
data.combined <- rbind.data.frame(train, test.survived)

# A bit about R data types (e.g., factors)  - str function tell you the varibles of data
# factor difine a varbile only takes of a limit set of values - drop down like picking state 
str(data.combined)

# This is to change the varible in pclass and survived
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# Take a look at gross survival rates - take data and put at a table
table(data.combined$Survived)

# Distributtion across classes 
table(data.combined$Pclass)

# Load up ggplot2 package to use for visualizations - Package a visual graph
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate ? over 2 and 3
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + # Make graph
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examine the first few names in the training data set - 
# Head command just lets you look at the first or last part
# As.character grab name and give me first few in Characters
head(as.character(train$Name))

# How many unique names are there across both train & test ? -unique is just that from names
length(unique(as.character(data.combined$Name)))

# Two duplicat names, take a closer look
# First, get the duplicate names and store them as a vector
# grab the combind name and covert to a char string and then run the duplicate elements
# which grad the names that are duplicated
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# Next, take a look at the records in the combined data set
# pull out all those records and display them 
data.combined[which(data.combined$Name %in% dup.names),]

# What is up with the Miss and Mr thing
library(stringr)

# Any correlation with orther variables (e.g., sibsp)?
# grab ever name and detect if miss is in that srting 
# which miss is in the name
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]
# most of the women that were single and in third class survived

# Hypothesis - Name titles correlate with age - just with Mrs this time
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check out males to see if any patterns - and most men did
males <- data.combined[which(train$Sex, "male"),]
males[1:5,]

# Expand upon the realtionship between "Survived and Pclass" by adding the new "Title" variable to the
# data set and then explore a potential 3 dimensional relationship

# Create a utility function to help with title extraction
# grep is a pattern function 
extractTitle <- function(Name){
    Name <- as.character(Name)
    
    if(length(grep("Miss.", Name)) > 0){
      return("Miss.")
    } else if (length(grep("Master.", Name)) > 0){
      return("Master.")
    } else if (length(grep("Mrs.", Name)) > 0){
      return("Mrs.")
    } else if (legth(grep("Mr.", Name)) > 0){
      return("Mr.")
    }
    else{
      return("Other")
    }
}
# take advantage of the funaction - create a new varible called titles  and loop over
# all the values we know of and call the function
# then create a new varible and combined it with the loop creation
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
  
}
data.combined$title <- as.factor(titles)


# Since we only have survived labels for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived))+
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Whats the distribution of females to males across train & test
table(data.combined$Sex)






