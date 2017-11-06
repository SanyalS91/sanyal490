# Modeling
#==============================================================================
# Data import and sanity check
#==============================================================================
custdata <- read.table(file = "C:\\Users\\Suprit\\Desktop\\Anaken R\\Tutorial Eight\\CustomerData.txt",header = TRUE,sep = "\t",na.strings = c("",NA))
head(custdata)
tail(custdata)
dim(custdata)
str(custdata)
custdata$is.employed <- as.factor(custdata$is.employed)
custdata$recent.move <- as.factor(custdata$recent.move)
custdata$state.of.res <- as.character(custdata$state.of.res)
custdata$health.ins <- as.factor(custdata$health.ins)
names(custdata)

#Exploratory data analysis
#==================================================================================
summary(custdata)
names(custdata) <- tolower(names(custdata))

#Missing data
#==================================================================================
table(custdata$is.employed,useNA = "ifany")
prop.table(table(custdata$is.employed,useNA = "ifany"))*100

table(custdata$recent.move,useNA = "ifany")
prop.table(table(custdata$recent.move,useNA = "ifany"))*100

summary(custdata[is.na(custdata$housing.type),])
summary(custdata[is.na(custdata$housing.type),c("recent.move","num.vehicles")])

#Remove missing values
#---------------------------------------------------------------------------------
custdata <- custdata[!is.na(custdata$housing.type),]

#Renaming missing values
#---------------------------------------------------------------------------------
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"not in active workforce",
                               ifelse(custdata$is.employed==TRUE,"employed","not employed"))
custdata$is.employed <- NULL
custdata$is.employed.fix <- as.factor(custdata$is.employed.fix)


# Missing values for numeric data
#---------------------------------------------------------------------------------
custdata1 <- custdata
custdata1$income[sample(1:nrow(custdata1),328)] <- NA
summary(custdata1$income)

#Alt 1
#---------------------------------------------------------------------------------

meanIncome <- mean(custdata1$income, na.rm=T)
custdata1$Income.fix <- ifelse(is.na(custdata1$income),meanIncome,custdata1$income)
summary(custdata1)

#Alt 2
#---------------------------------------------------------------------------------
breaks <- c(0, 10000, 50000, 100000, 250000, 1000000)
Income.groups <- cut(custdata1$income,breaks=breaks, include.lowest=TRUE)
Income.groups
summary(Income.groups)
Income.groups <- as.character(Income.groups)
Income.groups <- ifelse(is.na(Income.groups),"no income", Income.groups)
Income.groups <- as.factor(Income.groups)
summary(Income.groups)

#Alt 3
#---------------------------------------------------------------------------------
missingIncome <- is.na(custdata1$income)
missingIncome <- ifelse(is.na(custdata1$income), 0, custdata1$income)
summary(missingIncome)

# Extreme Observations
#=======================================================================================
custdata.quant <- custdata[ ,sapply(custdata, function(x) is.numeric(x) || is.integer(x))]
library(pastecs)

options(scipen=100)
options(digits = 2)
stat.desc(custdata.quant)[,-1]

quantile(x = custdata$income,probs = seq(0,1,.01))
sapply(custdata.quant,function(x) quantile(x,probs=seq(0,1,.01)))

dev.off()
library(ggplot2)
ggplot(custdata) + geom_density(aes(x=income)) 
ggplot(custdata) + geom_histogram(aes(x=income)) 
boxplot(custdata$income,horizontal = TRUE)

library(scales)
custdata$income <- squish(custdata$income,quantile(custdata$income,probs = c(0.1,.99)))

#Capping and Flooring of numeric variables
#-------------------------------------------------------------------------------------------
custdata[,c("income","num.vehicles","age")] <- sapply(custdata[,c("income","num.vehicles","age")],function(x) squish(x,quantile(x,probs=c(.01,.99)))) 

Univariate <- function(x)
{
  c(Mean=mean(x),Median =median(x),Max= max(x),Min = min(x), SD= sd(x))
}

#Bivariate Analysis
aggregate(custdata$income,by=list(custdata$health.ins),Univariate)
aggregate(custdata$num.vehicles,by=list(custdata$health.ins),Univariate)
aggregate(custdata$age,by=list(custdata$health.ins),Univariate)

# Categorical Analysis
#===================================================================================
xtabs(~health.ins+marital.stat,data = custdata)
prop.table(xtabs(~health.ins+marital.stat,data = custdata),2)*100
ggplot(custdata) + geom_bar(aes(x=marital.stat,fill=health.ins),position="fill")

xtabs(~health.ins+sex,data = custdata)
prop.table(xtabs(~health.ins+sex,data = custdata),2)*100
ggplot(custdata) + geom_bar(aes(x=sex,fill=health.ins),position="fill")

xtabs(~health.ins+is.employed.fix,data = custdata)
prop.table(xtabs(~health.ins+is.employed.fix,data = custdata),2)*100
ggplot(custdata) + geom_bar(aes(x=is.employed.fix,fill=health.ins),position="fill")

xtabs(~health.ins+housing.type,data = custdata)
prop.table(xtabs(~health.ins+housing.type,data = custdata),2)*100
ggplot(custdata) + geom_bar(aes(x=housing.type,fill=health.ins),position="fill")

xtabs(~health.ins+recent.move,data = custdata)
prop.table(xtabs(~health.ins+recent.move,data = custdata),2)*100
ggplot(custdata) + geom_bar(aes(x=recent.move,fill=health.ins),position="fill")

custdata$state.of.res<-NULL

## Training and test samples
#=====================================================================================

# Splitting the dataset

train <- sample(1:dim(custdata)[1],size = 0.7*dim(custdata)[1])
data.train <- custdata[train,]
data.test <- custdata[-train,]

set.seed(500)
fit <- glm(health.ins ~ ., data=data.train, family=binomial(link="logit"))
summary(fit)

fit2 <- glm(health.ins ~ sex+income+age+I(1*(is.employed.fix =="not employed")), data=data.train, family=binomial(link="logit"))
summary(fit2)

library(car)
vif(fit2)

pred.train <- predict(fit2, newdata= data.train , type ="response")
pred.test <- predict(fit2, newdata= data.test , type ="response")

# Evaluating output

train.table <- table(Actual=data.train$health.ins,Predicted=pred.train> 0.5) 
train.accuracy <- sum(diag(train.table))/sum(train.table)
train.accuracy

test.table <- table(Actual=data.test$health.ins,Predicted=pred.test> 0.5) 
test.accuracy <- sum(diag(test.table))/sum(test.table)
test.accuracy
