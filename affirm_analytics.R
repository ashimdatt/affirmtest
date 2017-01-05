#####- Affirm Analytics Test 
#### Solutions by Ashim Datta, email- datta.ashim2@gmail.com   #####


#### q1- Anomalies in data
## 1. 629 user information missin
## 2. No merchant information available for 1 merchant in funnel data
## 3. No merchant information available for 1 merchant in loan data
#  4. No loan information for one merchant in funnel data
#  5. Funnel information is missing for 3016 loans

#### q3- Which industries to go for
#  1. Focus on Jewellery- Jewellery has least confirmation rate and hence contributes to least revenue. But every thing else remaining same positively affects revenue
#  2. Try to increase satisfaction for repeat customers- Repeat customers negatively affect revenue
#  3. Old is Gold- Older people and high fico scores contribute more to revenue



setwd("/Users/ashimdatta/Documents/Affirm Analytics Test")
library("sqldf")
library("randomForest")
library("ggplot2")
library("gridExtra")
library("lubridate")
library("Hmisc")


funnel<-read.csv("funnel.csv")
loans<-read.csv("loans.csv")
merchants<-read.csv("merchants.csv")

summary(funnel)
x<-is.na(funnel$checkout_id)
## Check if checkoutid was captured for every event
table(x)

# All checkoutid were loaded

y<-table(funnel$checkout_id)
max(y)
min(y)

## No checkoutid was present for more than 4 times

num_checkout_loaded<-length(which(funnel$action=='Checkout Loaded'))
num_loan_run<-length(which(funnel$action=='Loan Terms Run'))

num_users_missing<-length(which(funnel$user_id==0))

ideal_useers_missing<-num_checkout_loaded-num_loan_run

missing_data<-num_users_missing-ideal_useers_missing
# Number of user records missing
missing_data

merchant1<-sqldf("select distinct merchant_id from funnel",drv="SQLite")
merchant2<-sqldf("select distinct merchant_id from merchants",drv="SQLite")
merchant3<-sqldf("select distinct merchant_id from loans",drv="SQLite")

merchant1_match_merchant2<-sqldf("select a.merchant_id as merchant_funnel,b.merchant_id from merchant1 a
left join
merchant2 b on a.merchant_id=b.merchant_id ",drv='SQLite')

merchant1_match_merchant3<-sqldf("select a.merchant_id as merchant_funnel,b.merchant_id from merchant1 a
left join
merchant3 b on a.merchant_id=b.merchant_id ",drv='SQLite')

merchant2_match_merchant3<-sqldf("select a.merchant_id as merchant_funnel,b.merchant_id from merchant3 a
left join
                                 merchant2 b on a.merchant_id=b.merchant_id ",drv='SQLite')

print(table(is.na(merchant1_match_merchant2$merchant_id)))
## No merchant information available for 1 merchant in funnel data

print(table(is.na(merchant1_match_merchant3$merchant_id)))
## No loan information for one merchant in funnel data

print(table(is.na(merchant2_match_merchant3$merchant_id)))
## No merchant information available for 1 merchant in loan data

# Date for which we have data

# Minimum date for loan checkout from funnel data
min(as.Date(funnel[which(funnel$action=="Checkout Completed"),'action_date'],format = "%m/%d/%y %H:%M"))

# Maximum date for loan checkout from funnel data
max(as.Date(funnel[which(funnel$action=="Checkout Completed"),'action_date'],format = "%m/%d/%y %H:%M"))

# Minimum date for loan checkout from loans data
min(as.Date(loans$checkout_date,format = "%m/%d/%y %H:%M"))

# Maximum date for loan checkout from loans data
max(as.Date(loans$checkout_date,format = "%m/%d/%y %H:%M"))

## Number loans checked out from funnel data should ideally match with number loans in loans data

funnel_checkouts<-length(unique(funnel[which(funnel$action=="Checkout Completed"),'checkout_id']))
loans_checkouts<-length(unique(loans[,'checkout_id']))

print(loans_checkouts-funnel_checkouts)

# Funnel information is missing for 3016 loans


str(funnel)

## Converting action_date to date

funnel$action_date2<-as.Date(funnel$action_date,format = "%m/%d/%y %H:%M")

funnel_agg<-sqldf("select action_date2,
sum(case when action='Checkout Loaded' then 1 else 0 end) as num_loaded,
sum(case when action='Loan Terms Run' then 1 else 0 end) as num_applied,
sum(case when action='Loan Terms Approved' then 1 else 0 end) as num_approved,
sum(case when action='Checkout Completed' then 1 else 0 end) as num_confirmed
from funnel group by 1",drv="SQLite")

funnel_agg$application_rate<-funnel_agg$num_applied/funnel_agg$num_loaded
funnel_agg$approval_rate<-funnel_agg$num_approved/funnel_agg$num_applied
funnel_agg$confirmation_rate<-funnel_agg$num_confirmed/funnel_agg$num_approved


##2. Calculate conversion through the funnel by day such that the data structure is:

head(funnel_agg)

###3.Which merchant industry and/or user demographic would you focus business development on based on current checkout funnel and loan performance? (Assume we have roughly the same market penetration in each so that saturation isn't a concern and assume revenue to Affirm = (mdr + loan_return_percentage) * loan_amount). Please put together a 3-page PowerPoint presentation to the executive team with your recommendation (title and agenda slides don’t count in the total).


## To understand what affects funnel, we would not be able to look at user demographics as user demographics data exists only for loans confirmed
## We can look at the affect of industry on funnel

funnel_merchant<-sqldf("select a.*,b.merchant_name,b.category from
                      funnel a left join merchants b
                       on lower(a.merchant_id)=lower(b.merchant_id)", drv="SQLite")

funnel_merchant_agg<-sqldf("select category,
                  sum(case when action='Loan Terms Approved' then 1 else 0 end) as num_approved,
                  sum(case when action='Checkout Completed' then 1 else 0 end) as num_confirmed
                  from funnel_merchant group by 1",drv="SQLite")

funnel_merchant_agg$confirmation_rate<-funnel_merchant_agg$num_confirmed/funnel_merchant_agg$num_approved

ggplot(funnel_merchant_agg, aes(x=category,y=round((confirmation_rate)*100,2),fill=category)) + 
  geom_bar(stat = "identity") +
  theme(panel.grid.major = element_line(colour = "grey40"),
      panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("Category of Industry") + ylab("Confirmation Rate") +
  ggtitle("Industries vs Confirmation Rate")

### Apparel and Furniture have the highest confirmation rate

## Let us try to see if industry can be a predictor for confirmation

funnel_merchant$confirmned<-ifelse(funnel_merchant$action=='Checkout Completed',1,0)
funnel_merchantapproved<-funnel_merchant[which(funnel_merchant$action=='Loan Terms Approved'),]
funnel_merchantconfirm<-funnel_merchant[which(funnel_merchant$action=='Checkout Completed'),]

##confirmed loans for the ones which are approved
funnel_merchantapproved_conf<-sqldf("select a.category, b.confirmned 
                                    from funnel_merchantapproved a
                                    left join
                                    funnel_merchantconfirm b
                                    on 
                                    a.checkout_id=b.checkout_id", drv="SQLite")
funnel_merchantapproved_conf[is.na(funnel_merchantapproved_conf)]<-0

split <- sample(seq_len(nrow(funnel_merchantapproved_conf)), size = floor(0.75 * nrow(funnel_merchantapproved_conf)))
trainData <- funnel_merchantapproved_conf[split, ]
testData <- funnel_merchantapproved_conf[-split, ]

## Using logistic regression as the dependent variable is categorical

model <- glm(confirmned ~.,family=binomial(link='logit'),data=trainData)
summary(model)

## Amongst the industries, jewellery is possibly suffering the most and furniture has better confirmation rates 


anova(model, test="Chisq")

fitted.results <- predict(model,newdata=testData,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testData$confirmned, na.rm = TRUE)
print(paste('Accuracy',1-misClasificError))

## This is possibly a good model as it can predict confirmation with ~65% accuracy

## Now, we will look into the loan performance for industry and users who have completed checkouts

loans_merchant<-sqldf("select a.*,b.merchant_name,b.category from
                      loans a left join merchants b
                      on lower(a.merchant_id)=lower(b.merchant_id)", drv="SQLite")
str(loans_merchant)

loans_merchant$revenue_affirm<-(loans_merchant$mdr+loans_merchant$loan_return_percentage)*loans_merchant$loan_amount

loans_merchant$repeatcust<-ifelse(loans_merchant$users_first_capture==0,0,1)

loans_merchant$user_age<-year(Sys.Date()) - loans_merchant$user_dob_year

## Variables that can affect Affirm revenue are- repeatcust,user_age,apr,fico_score, merchant_name and cateogry

loans_merchant2<-loans_merchant[,c('repeatcust','user_age','apr','fico_score','category','revenue_affirm')]

loans_merchant2[is.na(loans_merchant2)]<-0

loans_merchant2_agg<-sqldf("select repeatcust,sum(revenue_affirm) as revenue_affirm
                           from loans_merchant2 group by 1 ",
                           drv="SQLite")

x<-ggplot(loans_merchant2_agg, aes(x=as.factor(repeatcust),y=round((revenue_affirm),2),fill=repeatcust)) + 
  geom_bar(stat = "identity") +
  theme(panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("Repeat(1) or non Repeat(0) customer") + ylab("Revenue") +
  ggtitle("Revenue by customertype")


y<-ggplot(loans_merchant2, aes(x=user_age,y=round((revenue_affirm),2))) + 
  geom_line() +
  theme(panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("Age") + ylab("Revenue") +
  ggtitle("Revenue by Customer Age")

z<-ggplot(loans_merchant2, aes(x=apr,y=round((revenue_affirm),2))) + 
  geom_line() +
  theme(panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("APR") + ylab("Revenue") +
  ggtitle("Revenue by Customer's APR")

  
t<-ggplot(loans_merchant2, aes(x=fico_score,y=round((revenue_affirm),2))) + 
  geom_line() +
  theme(panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("Fico score") + ylab("Revenue") +
  ggtitle("Revenue by Customer's Ficoscore")

loans_merchant2_agg2<-sqldf("select category,sum(revenue_affirm) as revenue_affirm
                           from loans_merchant2 group by 1 ",
                           drv="SQLite")

p<-ggplot(loans_merchant2_agg2, aes(x=category,y=round((revenue_affirm),2),fill=category)) + 
  geom_bar(stat = "identity") +
  theme(panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("Category of Industry") + ylab("Revenue") +
  ggtitle("Industries vs Revenue")


grid.arrange(x, y, z, t, p, ncol=3, nrow =2)

### Points to Note
## 1.Repeat customers do not contribute to a lot of revenue
## Furniture category contributes to most revenue and Jewellery contributes to least



## Let us try to apply regression to the dataset and check if we can predict affirm_revenuebased on the above variables

head(loans_merchant)

hist(loans_merchant2$revenue_affirm,xlab=" ",main="revenue ", col="skyblue")
hist(log(loans_merchant2$revenue_affirm),xlab=" ",main="revenue ", col="skyblue")

ggplot(loans_merchant2, aes(x=log(revenue_affirm))) + geom_density(alpha=.3)

## Log of dependent variable can be used to regress and hence we will apply log linear regression

set.seed(123)
split <- sample(seq_len(nrow(loans_merchant2)), size = floor(0.75 * nrow(loans_merchant2)))
trainData <- loans_merchant2[split, ]
testData <- loans_merchant2[-split, ]

## base model

best.guess <- mean(trainData$revenue_affirm) 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-testData$revenue_affirm)^2,na.rm=TRUE))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-testData$revenue_affirm),na.rm=TRUE)
MAE.baseline




predictionModel <- lm(log(revenue_affirm+1) ~ repeatcust + user_age + apr + fico_score
                        +category, data = trainData)


summary(predictionModel)

test.pred.lin <- exp(predict(predictionModel,testData))-1

RMSE.lin.reg <- sqrt(mean((test.pred.lin-testData$revenue_affirm)^2,na.rm = TRUE))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-testData$revenue_affirm),na.rm = TRUE)
MAE.lin.reg


## Root mean squared error and Mean absolute errors are reduced by this predictive model
## Points to note, every thing else remaining same
##     Repeat customers are not good for revenue
##     Older people contribute to more revenue
##     Better fico score users contribute to more revenue
##     Apparel and Jewellery is good for revenue
##     Music is bad for revenue

