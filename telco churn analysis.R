##DATA PROCESSING
churn_raw <- read.csv("C:/Users/Shreya pc/Downloads/Telco-Customer-Churn.csv") 
View(churn_raw)

library(dplyr) ## data manipulation
library(corrplot) ## for correlation matrix
library(ggplot2) ##  for visulasisation
library(ggthemes)## foe graph decoration
library(caret)## for data spliting
library(party)## for decision tree
library(plyr)##support library of dplr
library(caTools)## for randomisation

## class = data.fram ( that means it can store any type of data)
class(churn_raw)

# summary of dataset
summary(churn_raw)

# to know the starting 5 columns
head(churn_raw)

# to know rows and columns of data
dim(churn_raw)

# type of data it contains(by use of str)
str(churn_raw)

## size of dataset
object.size(churn_raw)

## to impute the missing values
sapply(churn_raw, function(x) sum(is.na(x))) ## sapply functions returns a vector or matrix n also to avoid the use of loops
churn_raw <-churn_raw[complete.cases(churn_raw), ]
sapply(churn_raw, function(x) sum(is.na(x)))

##DATA WRANGLING
cols_recode1 <- c(10:15)  ## map values replace specified values wid new values
for(i in 1:ncol(churn_raw[,cols_recode1])) {
        churn_raw[,cols_recode1][,i] <- as.factor(mapvalues
                                              (churn_raw[,cols_recode1
                                                   ][,i], from =c("No internet service"),to=c("No")))
}


View(churn_raw)

churn_raw$MultipleLines <- as.factor(mapvalues(churn_raw$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))
View(churn_raw)

min(churn_raw$tenure);max(churn_raw$tenure)
# since te minimum tenure is 1 month and maximum tenure is 72 months,we can group them into five tenure groups

group_tenure <- function(tenure){
        if (tenure >= 0 & tenure <= 12){
                return('0-12 Month')
        }else if(tenure > 12 & tenure <= 24){
                return('12-24 Month')
        }else if (tenure > 24 & tenure <= 48){
                return('24-48 Month')
        }else if (tenure > 48 & tenure <=60){
                return('48-60 Month')
        }else if (tenure > 60){
                return('> 60 Month')
        }
}

churn_raw$tenure_group <- sapply(churn_raw$tenure,group_tenure)
churn_raw$tenure_group <- as.factor(churn_raw$tenure_group)

summary(churn_raw)
View(churn_raw)

churn_raw$SeniorCitizen <-as.factor(mapvalues(churn_raw$SeniorCitizen,
                                              from = c("0","1"),
                                              to = c("NO","Yes")))
View(churn_raw)
summary(churn_raw)

# we remove the colum we dont need for analysis
churn_raw$customerID <- NULL
churn_raw$tenure <- NULL

##EXPLORATORY DATA ANALYSIS
## CORRELATION BETWEEN NUMERICAL VARIABLES

numeric_var <- sapply(churn_raw, is.numeric)
corr.matrix <- cor(churn_raw[,numeric_var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")
## monthly charges and total charges are correalted so one has to be removed from the model

churn_raw$TotalCharges <- NULL
View(churn_raw)

## VISUALIZATION OF CATEGORICAL VARIABLE
ggplot(data = churn_raw)+
  ggtitle("Gender")+
  geom_bar(aes(x = gender, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Senior Citizen")+
  geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Partner")+
  geom_bar(aes(x = Partner, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Dependents")+
  geom_bar(aes(x = Dependents, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Phone Service")+
  geom_bar(aes(x = PhoneService, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Multiple lines")+
  geom_bar(aes(x = MultipleLines, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Internet services")+
  geom_bar(aes(x = InternetService, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Online Security")+
  geom_bar(aes(x = OnlineSecurity, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Online Backup")+
  geom_bar(aes(x = OnlineBackup, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Device Protection")+
  geom_bar(aes(x = DeviceProtection, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Tech Support")+
  geom_bar(aes(x = TechSupport, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Streaming TV")+
  geom_bar(aes(x = StreamingTV, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Streaming Movies")+
  geom_bar(aes(x = StreamingMovies, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Contract")+
  geom_bar(aes(x = Contract, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Paperless Billing")+
  geom_bar(aes(x = PaperlessBilling, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Payment Method")+
  geom_bar(aes(x = PaymentMethod, fill = Churn), position = "dodge")

ggplot(data = churn_raw)+
  ggtitle("Tenure Group")+
  geom_bar(aes(x = tenure_group, fill = Churn), position = "dodge")

## CORRELATION BETWEEN ALL VARIABLES
corr <- as.data.frame(lapply(churn_raw,as.numeric))
corrr <- cor(corr$Churn,corr)
corrr

## LOGISTIC REGRESSIOn
set.seed(123)
split <- sample.split(churn_raw$Churn, SplitRatio = 0.75)

training_set <- subset(churn_raw, split == TRUE)
test_set <- subset(churn_raw, split == FALSE)

classifier <- glm(formula = Churn~.,
                    family='binomial',
                    data = training_set)
classifier
summary(classifier)

## probability
test_set$Churn <- as.character(test_set$Churn)
test_set$Churn[test_set$Churn == "No"] <- "0"
test_set$Churn[test_set$Churn == "Yes"] <- "1"
prob_pred <- predict(classifier, type = 'response',newdata = test_set)
prob_pred
y_pred <- ifelse(prob_pred>0.5,1,0)
y_pred

##confusion matrix
cm <- table(test_set[,18],y_pred)
cm

##accuracy of logistic regression
error= mean(y_pred!=test_set$Churn)
print(paste('logistic regression accuracy',1-error))

##decision tree
tree<- ctree(Churn~Contract+tenure_group+PaperlessBilling,training_set)
plot(tree)

## decision the confusion matrix
pred_tree<-predict(tree,test_set) 
print("Confusion Matrix  for Decision Tree");table(Predicted = pred_tree,Actual= test_set$Churn)

## descision tree accuracy
p1 <- predict(tree,training_set)
tab1 <- table(Predicted = p1,Actual=training_set$Churn)
tab2 <- table(Predicted = pred_tree,Actual = test_set$Churn)
print(paste("Decision Tree Accuracy",sum(diag(tab2))/sum(tab2)))
