library(e1071)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(car)
library(Amelia)
library(rlist)
library(rattle)
library(rpart)
library(rpart.plot)
library(Boruta)
library(caret)
library(pROC)

data=read.csv("Attrition.csv",na.strings = c("","NA"))
str(data)
summary(data)
View(data)


#Preprocessing of data

#no identifier
#Transformation of datatypes

data$StockOptionLevel <- as.factor(data$StockOptionLevel)
data$JobLevel<-as.factor(data$JobLevel)
data$TrainingTimesLastYear<-as.factor(data$TrainingTimesLastYear)

#Numeric to ordinal variable conversion

data$Education<-as.factor(data$Education)
data$Education<-factor(data$Education,ordered = TRUE,levels = c('1','2','3','4','5'))

data$EnvironmentSatisfaction<-as.factor(data$EnvironmentSatisfaction)
data$EnvironmentSatisfaction<-factor(data$EnvironmentSatisfaction,ordered = TRUE,levels = c('1','2','3','4'))

data$JobInvolvement<-as.factor(data$JobInvolvement)
data$JobInvolvement<-factor(data$JobInvolvement,ordered = TRUE,levels = c('1','2','3','4'))


data$JobSatisfaction<-as.factor(data$JobSatisfaction)
data$JobSatisfaction<-factor(data$JobSatisfaction,ordered = TRUE,levels = c('1','2','3','4'))

data$PerformanceRating<-as.factor(data$PerformanceRating)
data$PerformanceRating<-factor(data$PerformanceRating,ordered = TRUE,levels = c('3','4'))

data$RelationshipSatisfaction<-as.factor(data$RelationshipSatisfaction)
data$RelationshipSatisfaction<-factor(data$RelationshipSatisfaction,ordered = TRUE,levels = c('1','2','3','4'))

data$WorkLifeBalance<-as.factor(data$WorkLifeBalance)
data$WorkLifeBalance<-factor(data$WorkLifeBalance,ordered = TRUE,levels = c('1','2','3','4'))

str(data)
summary(data)

#Removing same valueor highly biased variables
data<- data[!colnames(data) %in% c("Over18","EmployeeCount","StandardHours","EmployeeNumber")]
dim(data)

#check for NA
colSums(is.na(data))
missmap(data)
#no NA/missing value in the dataset

#checking for duplicate rows
which(duplicated(data))

#segregating the data in numeric & factor
data.numeric <- data[sapply(data,is.numeric)]
data.factor <- data[sapply(data,is.factor)]
colnames(data.factor)
colnames(data.numeric)

#Exploratory data Analysis(EDA)

#Univariate analysis

#Analysing histogram of each numeric values
numplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_histogram(aes(y=..density..),fill = "grey", color = "black")+
    geom_density(fill='blue', alpha=0.2)+
    xlab(column)
}

np <- lapply(colnames(data.numeric)[1:7], numplot, df=data.numeric)
do.call("grid.arrange", np)


#correlation
corrplot::corrplot(cor(data.numeric))
corrplot.mixed(cor(data.numeric), lower.col = "black", number.cex = .7)

#so with correlation plot we can see that multicollinearity exist , we will treat it later.

factplot <- function(column, df)
{
  ggplot(df, aes_string(x=column))+
    geom_bar(fill = "blue", color = "black", alpha= 0.2)+ scale_y_continuous(labels=scales::percent_format())+
    xlab(column)
}
#calling all bar plot
fp <- lapply(colnames(data.factor[1:5]), factplot, df=data.factor)
do.call("grid.arrange", fp)

colnames(data.factor)
colnames(data.numeric)

#Bivriate analysis

##Attrition VS business travel

ggplot(data,aes(BusinessTravel,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Travel Frequency",y="Count",title="Attrition Vs Business Travel")

table_travel<-table(data$BusinessTravel, data$Attrition)
chisq.test(table_travel)

#The barplot shows that employees who travel rarely do not frequently quit the job. 
#Thus, attrition is dependent on business travel, and the chi square test proves this.


#Attrition VS Distance from home

#Anova 
y=data.numeric$DistanceFromHome
x=data.factor$Attrition
p <-aov(y~x)
summary(p)

ggplot(data,aes(DistanceFromHome,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Distance from home",y="Count",title="Attrition Vs Distance from home")

#As the p-value is less than alpha(0.05), 
#the attrition of employee depends on distance from home.
#Barplot shows that employees who lives close to office don't quit the job frequently.


#Attrition VS Job Level
ggplot(data,aes(JobLevel,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Job level",y="Count",title="Attrition Vs job Level")

table_level<-table(data$JobLevel, data$Attrition)
chisq.test(table_level)

#The barplot shows that employees at job level 1 are the one who go most for attrition.
#As p-value is less than alpha, attrition depends on job level.


#Attrition VS Job Role
ggplot(data,aes(JobRole,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Job Role",y="Count",title="Attrition Vs job Role")

table_role<-table(data$JobRole, data$Attrition)
chisq.test(table_role)
#Bar plot shows that Sales executive are the one who goes most for attrition
#As p-value is less than alpha, attrition depends on job role.

#Attrition VS Job Satisfaction
ggplot(data,aes(JobSatisfaction,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Job satisfaction",y="Count",title="Attrition Vs job satisfaction")

table_job_sat<-table(data$JobSatisfaction, data$Attrition)

chisq.test(table_job_sat)

#bar plot shows that employees with high job satisfaction don't go for attrition
#As p-value is less than alpha, attrition depends on job satisfaction of employees.

#Attrition VS Marital Status
ggplot(data,aes(data$MaritalStatus,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Marital status",y="Count",title="Attrition Vs Marital Status")


table_mar<-table(data$MaritalStatus, data$Attrition)
chisq.test(table_mar)
#Bar plot shows that single people mostly go for attrition.
#As p-value is less than alpha, attrition depends on the marital status of employees.

#Attrition VS Monthly Income
ggplot(data,aes(MonthlyIncome,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="Income level",y="Count",title="Attrition Vs Monthly income")

y=data.numeric$MonthlyIncome
x=data.factor$Attrition
p <-aov(y~x)
summary(p)


#As Annova shows, attrition is highly dependent on monthly income.

#Attrition VS Number of companies worked
y=data.numeric$NumCompaniesWorked
x=data.factor$Attrition
p <-aov(y~x)
summary(p)


#As p value is more than alpha , Attrition is not dependent on number of companies worked.

#Attrition VS Overtime

ggplot(data,aes(OverTime, fill=Attrition))+geom_bar()+ggtitle("Attrition VS Overtime")

table_over<-table(data$OverTime, data$Attrition)

chisq.test(table_over)
#As the barplot shows, the less an employee works overtime ,the lower is attrition rate. 
#And the chi square test proves that.

#Attrition VS Percent Salary Hike (Yearly increase in salary)
y=data.numeric$PercentSalaryHike
x=data.factor$Attrition
p <-aov(y~x)
summary(p)


#As p-value is more than alpha, the attrition rate does not depend on yearly increase in salary.


#Attrition VS Performance Rating
table_perf<-table(data$PerformanceRating, data$Attrition)
chisq.test(table_perf)
#Performance Rating is not a good indicator of Attrition, as p-value is more than alpha.

#Attrition VS Relationship satisfaction

table_rel<-table(data$RelationshipSatisfaction, data$Attrition)
chisq.test(table_rel)
#Attrition is not dependent on Relationship satisfaction of an employee as p-value is more than alpha.


#Attrition VS Work/Life Balance
ggplot(data,aes(WorkLifeBalance,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="worklife balance",y="Count",title="Attrition Vs work/life balance")

table_balance<-table(data$WorkLifeBalance, data$Attrition)
chisq.test(table_balance)
#As per chi-square test Attrition depends on Work/Life balance as p-value is less than alpha.

#Attrition VS Training times last year
ggplot(data,aes(data$TrainingTimesLastYear,fill=Attrition))+geom_bar(position=position_dodge())+
  labs(x="TrainingTimesLastYear",y="Count",title="Attrition Vs TrainingTimesLastYear")

#Anova 
y=data.numeric$TrainingTimesLastYear
x=data.factor$Attrition
p <-aov(y~x)
summary(p)

#As p-value is less than alpha, attrition rate depends on trainings.
#We can see that When training's are more attrition is seen lowest.


#cat-num analysis
colnames(data.numeric)
ggplot(data, aes(x = Age, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = DailyRate, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = DistanceFromHome, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = HourlyRate, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = MonthlyIncome, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = MonthlyRate, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = NumCompaniesWorked, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = PercentSalaryHike, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = TotalWorkingYears, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsAtCompany, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsInCurrentRole, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsSinceLastPromotion, color = Attrition)) + geom_density() + theme_minimal()
ggplot(data, aes(x = YearsWithCurrManager, color = Attrition)) + geom_density() + theme_minimal()

#check for skewness & outliers
#skewness
(data.skewed <- apply(data.numeric, c(2), skewness))

#outlier 

out_std_check=function(x){
  m=mean(x)
  s=sd(x)
  lc=m-3*s #lower cut off
  uc=m+3*s # upper cut off
  n=sum(x>uc|x<lc)
  val=list(num=n,lower_cutoff=lc,upper_cutoff=uc)
  return(n)
}
np <- apply(data.numeric, c(2), out_std_check)
np

#outliers percentage ;if lesss than 2% we can remove

#Totalworkingyears
(16/1470)*100
#yearsatcompany
(25/1470)*100
#yearssincelastpromotion
(42/1470)*100

#Since percentage of outlier is very less we can delete them.

out_std_fix=function(x){
  m=mean(x)
  s=sd(x)
  lc=m-3*s #lower cut off
  uc=m+3*s # upper cut off
  out_value <- which(x > uc | x < lc)
  x[out_value] <-m
  return(x)
}

data.numeric <- apply(data.numeric, c(2), out_std_fix)
data.numeric <- as.data.frame(data.numeric)
#View(data.numeric)

# Check skewness again
(data.skewed <- apply(data.numeric, c(2), skewness))


#check biaseness in target variable
ggplot(data = data)+geom_bar(mapping = aes(x=Attrition,y=(..count..)/sum(..count..)))+
scale_y_continuous(labels = scales::percent)


#As per ouranalysis with chi-square below variables are not significant so need to be removed
#gender,performancerating , relationshipsatisfaction, education

#after this ,model building is not considering below variables as significant so remove these also
#Education field,worklifebalance are insignificant as per model

data.factor<-data.factor[(!colnames(data.factor)%in%c("Gender","PerformanceRating",
"RelationshipSatisfaction","EducationField","WorkLifeBalance","Education"))]

dim(data.factor)
colnames(data.factor)

#according to anova test significant variables are
#age,distance from home,MonthlyIncome","StockOptionLevel","TotalWorkingYears","YearsAtCompany"
"YearsInCurrentRole","YearsWithCurrManager"

# so removing the columns

data.numeric <- data.numeric[(!colnames(data.numeric)%in% c("DailyRate","EmployeeNumber",
"HourlyRate","MonthlyRate","PercentSalaryHike","TrainingTimesLastYear","YearsSinceLastPromotion"))]
dim(data.numeric)
colnames(data.numeric)

#Boruta check for variable selection

set.seed(111)
boruta <- Boruta(Attrition~.,data=data,doTrace=2,maxRuns=130)
print(boruta)
plot(boruta,las=2,cex.axis=0.7)
#Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)
getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)


#merge the data
data <- cbind(data.numeric,data.factor)
dim(data)

#Model1
library(plyr)
data$Attrition <- mapvalues(data$Attrition, from=c('Yes', 'No'), to = c(1,0))

#sampling
set.seed(123)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
#View(test)
#colnames(test)
test1 <- test[!colnames(test) %in% "Attrition"]


#logistic regression
model=glm(Attrition~.,data=train,family = binomial("logit"))
summary(model)

fitted.results = predict(model,newdata=test, type='response')
fitted.results1 = ifelse(fitted.results >0.5,1,0)
test$predictedatrrition <- fitted.results1

#Confusion Matrix

cm<-confusionMatrix(table(test$Attrition,test$predictedatrrition))
test$predictedatrrition <- as.factor(test$predictedatrrition)
print(cm)
print(cm$byClass)

#Validation parameter of Model1:-

#Accuracy=88.21%
#Sensitivity or Recall= 0.89
#precision = 0.96
#specificity= 0.75
#error= 1-accuracy=11.79


#AUC & ROC curve
roccurve=roc(test$Attrition,fitted.results1)
plot(roccurve)
auc(roccurve)
#auc=0.72


#Model2- Decision Tree

set.seed(111)
s=sample(1:nrow(data),0.70*nrow(data))
train=data[s,]
test=data[-s,]
#View(test)

#applying rpart algorithm
#create a model using decision tree
regressor <- rpart(formula=Attrition~.,data=train,method = "class")
regressor
rpart.plot(regressor)
printcp(regressor)
summary(regressor)

#view fancy plot
fancyRpartPlot(regressor)

#PREDICTION
pred <- predict(regressor,test,type="class")

#results & accuracy
conf_mat <- table(test$Attrition, pred)
Acc <- (conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)
conf_mat
Acc

library(pROC)
#ROC-AUC Curve
pred_test_roc=ifelse(pred=="Yes",1,0)
roccurve=roc(test$Attrition,pred_test_roc)
plot(roccurve)
auc(roccurve)
-------------------------------//--------------