# Setting the working directory
setwd("~/Documents/my_passion")

# Loading the Datasets
train_data <- read.csv(file.choose(),header = TRUE,sep=",")
test_data <- read.csv(file.choose(),header = TRUE,sep=",")

# Checking for the missing values
colSums(is.na(train_data))
colSums(is.na(test_data))

# Both the dataset has Missing values in "Age" Columns
ncol(train_data)
ncol(test_data)

# Adding extra column called Survived in the test set
test_data$Survived <- NA

# Merge both the dataset for Cleaning
merged_data <- rbind(train_data,test_data)
View(merged_data)

# STEP 1  CLEANING THE DATASET

min(merged_data$Age,na.rm = TRUE)
max(merged_data$Age,na.rm=TRUE)

#Finding the mean Category wise to get more precise result
# Finding separate mean for "Female" and "Male"
sex_category_mean <- aggregate(Age ~ Sex , data = merged_data , mean)
sex_category_mean$Age <- round(sex_category_mean$Age)

# Impute the missing values in the age column with the mean of females for female categry 
merged_data$Age <- ifelse(is.na(merged_data$Age & merged_data$Sex == "female"),sex_category_mean[1,2],merged_data$Age)

# Impute the missing values in the age column with the mean of males for male categry 
merged_data$Age <- ifelse(is.na(merged_data$Age & merged_data$Sex == "male"),sex_category_mean[2,2],merged_data$Age)

# Again checking for the missing entries in the dataset
colSums(is.na(merged_data))

# Since the fare column has missing values and the fare is related with the category of class so better to group the fare based on class

fare_caterogy_wise <- aggregate(Fare ~ Pclass + SibSp + Parch , data = merged_data , mean) # Mean category wise

# Since only one row is having missing value for fare so imputing directly in that row
merged_data[1044,"Fare"] <- fare_caterogy_wise[3,4]

colSums(is.na(merged_data))
#There is no more missing values left in the dataframe except the output variable


# STEP 2 FEATURE ENGINEERING
names(merged_data)
head(merged_data)
View(merged_data)

# Extracting the Title from the Name
merged_data$Title <- gsub('(.*, )|(\\..*)', '', merged_data$Name)

#show the title by Sex
table(merged_data$Sex,merged_data$Title)

# Title with very low count can be renamed as "rare" title
rare_title <- c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major","Rev","Sir","the Countess")

# Rename the Salutation which are given in French like "Mlle" , "Mme" and "Ms"
merged_data$Title[merged_data$Title == "Mlle"] <- "Miss"
merged_data$Title[merged_data$Title == "Mme"] <- "Mrs"
merged_data$Title[merged_data$Title == "Ms"] <- "Miss"

merged_data$Title[merged_data$Title %in% rare_title] <- "Rare Title"

# Bin the age into categories 
merged_data$Age_group<-cut(merged_data$Age, seq(0,80,20))

#Again show the count of Title
table(merged_data$Sex ,merged_data$Title)

# Again splitting the dataset back to train and test set
train_data_filtered <- subset(merged_data,!is.na(Survived))
test_data_filtered <- subset(merged_data,is.na(Survived))


## STEP 3 FINDING THE STRENGTH OF ASSOCIATION BETWEEN THE VARIABLES USING CRAMER V RULE 
str(train_data_filtered)

# Since Cramer v rule applies only in categorical variable so we will convert the variables into factors

# Converting all variables into factors
col_names <- c("Survived","Pclass","Sex","Embarked","Title","Age_group")
train_data_filtered[,col_names] <- lapply(train_data_filtered[,col_names] , factor)

cramers_association <- train_data_filtered[,col_names]
class(cramers_association)

#Applying Cramers V Rule here
cramer_v <- function(x) { 
  require(vcd) 
  nc <- ncol(x) 
  v <- expand.grid(1:nc, 1:nc) 
  matrix(mapply(function(i1, i2) assocstats(table(x[,i1],x[,i2]))$cramer, v[,1], v[,2]), nc, nc) 
  
} 

#Calling the cramer function for the filtered dataset
cramer_v_result <- cramer_v(cramers_association)

column_names <- names(cramers_association)
colnames(cramer_v_result) <- column_names
rownames(cramer_v_result) <- column_names

# Seeing the plot it shows that "Pclass" , "Sex" , "Title" , "Age_group" are the have some significant association

# Visilaising the features to get more insigths of the data from the cramers association result
library(ggplot2)

# Before Visualising the results lets label the levels of Survived
levels(train_data_filtered$Survived)[1] <- "Not Survived"
levels(train_data_filtered$Survived)[2] <- "Survived"

levels(train_data_filtered$Survived)

# Based on the correlation plot Visualising the relation between Survived and Pclass
survived_pclass_relation <- train_data_filtered %>% group_by(Survived,Pclass) %>% summarise(count=n()) %>% mutate(percentage = count/sum(count)*100)
View(survived_pclass_relation)
survived_pclass_relation$percentage <- round(survived_pclass_relation$percentage,1)

# visulaising the result
survived_pclass_survived_plot <- ggplot(data=survived_pclass_relation,aes(reorder(Survived,count),count,fill=Pclass)) +
  geom_bar(stat="identity") + geom_text(aes(label=paste(survived_pclass_relation$percentage,"%",sep="")),position = position_stack(vjust = 0.5))  + xlab("Survived Status") + ylab("Survived Status Vs Passenger Class wsie in %") + ggtitle("Pclass Vs Survived Status") + theme(axis.text.y= element_blank(),axis.ticks.x=element_blank(),axis.ticks.y = element_blank(),panel.background = element_blank(),axis.title.x = element_text(colour = "black"))

# Sex Vs Survived Status
survived_sex_relation <- train_data_filtered %>% group_by(Survived,Sex) %>% summarise(count=n()) %>% mutate(percentage = count/sum(count)*100) 
View(survived_sex_relation)
survived_sex_relation$percentage <- round(survived_sex_relation$percentage,1)

# Visualising the result
survived_sex_relation_plot <- ggplot(data=survived_sex_relation,aes(reorder(Survived,count),count,fill=Sex)) +
  geom_bar(stat="identity") + geom_text(aes(label=paste(survived_sex_relation$percentage,"%",sep="")),position = position_stack((vjust = 0.5))) +xlab("Survived Status") + ylab("Survived Status Vs Sex in %") + ggtitle("Sex Vs Survived Status") + theme(axis.text.y = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank(),panel.background = element_blank(),axis.title = element_text(colour = "black"))

# Title Vs Survived Status

