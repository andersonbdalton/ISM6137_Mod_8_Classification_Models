#Dalton Anderson


library(ggplot2)
library(corrplot)
library(stargazer)
library(PerformanceAnalytics)
library(stargazer)
library(lme4)
#install.packages("dplyr")   
library(dplyr)
library(readxl)
#import data
df_master <- read_excel("TelcoChurn.xlsx")
library(dplyr)
colnames(df_master)=tolower(make.names(colnames(df_master)))

#descriptive statistics light 

ggplot(df_master, aes(x=monthlycharges, y=churn)) + geom_point()
#looks like I need to use a glm model
hist(log(df$count))

colSums(is.na(df))
#totalcharges has 11 nas



table(df$churn)
#unbalaced sample

#data wrangling
df <- df_master

#churn number
tempdf=df %>%
  mutate(
    churnnum = case_when(
      churn == "Yes" ~ 1,
      churn == "No" ~ 0
    )
  )
df=tempdf

#contract type
tempdf=df %>%
  mutate(
    contracttype = case_when(
      contract == "Month-to-month" ~ "Monthly",
      contract == "One year" ~ "Yearly",
      contract == "Two year" ~ "Yearly"
    )
  )
df=tempdf

#payment type
tempdf=df %>%
  mutate(
    paymenttype = case_when(
      paymentmethod == 'Electronic check' ~ "Manual",
      paymentmethod == 'Mailed check' ~ "Manual",
      paymentmethod == 'Bank transfer (automatic)' ~ "Automatic",
      paymentmethod == 'Credit card (automatic)' ~ "Automatic"
    )
  )
df=tempdf

#internet plus
tempdf=df %>%
  mutate(
    internetplus = case_when(
      onlinesecurity == "Yes" & onlinebackup == "Yes" & deviceprotection == "Yes" 
      & techsupport == "Yes" & streamingtv == "Yes" & streamingmovies == "Yes"  ~ "Ultimate Package",
      
      onlinesecurity == "Yes" & onlinebackup == "Yes" & deviceprotection == "Yes" 
      & techsupport == "Yes" & streamingtv == "Yes" | streamingmovies == "Yes"  ~ "Ultimate Package",
      
      onlinesecurity == "Yes" & onlinebackup == "Yes" & deviceprotection == "Yes" 
      & techsupport == "Yes" & streamingtv == "Yes" & streamingmovies == "Yes"  ~ "Ultimate Package",
      
      onlinesecurity == "Yes" & onlinebackup == "Yes" & deviceprotection == "Yes" 
      | techsupport == "Yes" | streamingtv == "Yes" | streamingmovies == "Yes"  ~ "Premium Package",
      
      onlinesecurity == "Yes" | onlinebackup == "Yes" | deviceprotection == "Yes" 
      & techsupport == "Yes" & streamingtv == "Yes" & streamingmovies == "Yes"  ~ "Premium Package",
      
      onlinesecurity == "Yes" & onlinebackup == "Yes" | deviceprotection == "Yes" 
      | techsupport == "Yes" & streamingtv == "Yes" & streamingmovies == "Yes"  ~ "Premium Package",
      
      onlinesecurity == "Yes" | onlinebackup == "Yes" & deviceprotection == "Yes" 
      & techsupport == "Yes" & streamingtv == "Yes" | streamingmovies == "Yes"  ~ "Premium Package",
      
      
      TRUE ~ "Base Internet"

    )
  )
df=tempdf
#both lines
tempdf=df %>%
  mutate(
    bothlines = case_when(
      phoneservice != 'No' & internetservice != 'No'  ~ "Yes",
      TRUE ~ "No"
    )
  )
df_both=tempdf

df=tempdf
tempdf=df
#reorder data to predictor table 
df <- tempdf %>%
  select(churn,
         churnnum,
         seniorcitizen,
         partner,
         dependents,
         tenure,
         phoneservice,
         multiplelines,
         internetservice,
         onlinesecurity,
         onlinebackup,
         deviceprotection,
         techsupport,
         streamingtv,
         streamingmovies,
         internetplus,
         contract,
         contracttype,
         monthlycharges,
         totalcharges,
         paymentmethod,
         paymenttype
  )

#descriptive statistics  

table(df$internetplus)

#Subscriber Type by Tenure
p <- ggplot(df, aes(internetplus, tenure))
p + labs(title="Subcriber Type by Tenure",x="Internet Plus", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "darkgreen", colour = "gold2")
#clear that amount of internet services that a customer has effects tenure of time someone is subsribed to any service.

#Payment Type Type by Tenure
p <- ggplot(df, aes(paymenttype, tenure))
p + labs(title="Payment Type by Tenure",x="Payment Type", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "gold3", colour = "darkblue")

#clear that payment type effect length of time someone is subscribed to any service.

#multiple lines by Tenure 
p <- ggplot(df, aes(multiplelines, tenure))
p + labs(title="Multiple Lines by Tenure",x="Multiple Lines", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "red3", colour = "black")

#people with multiple lines stay longer then customers with TV and TV customers stay longer than single line phone customers.


# Internet Service by Tenure 
p <- ggplot(df, aes(internetservice, tenure))
p + labs(title="Internet Service by Tenure",x="Internet Service", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "purple", colour = "black")
#not a huge difference

#Dependents by Tenure 
p <- ggplot(df, aes(dependents, tenure))
p + labs(title="Dependents by Tenure",x="Dependents", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "yellow2", colour = "black")
#not a huge difference

#partner lines by Tenure 
p <- ggplot(df, aes(partner, tenure))
p + labs(title="partner Type by Tenure",x="Internet Plus", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "yellow2", colour = "black")
#partner has an effect on tenure

#senior citizen lines by Tenure 
p <- ggplot(df, aes(seniorcitizens, tenure))
p + labs(title="senior citizen Type by Tenure",x="Internet Plus", y = "Tenure")
p + geom_boxplot()
p + geom_boxplot(fill = "yellow2", colour = "black")
#partner has an effect on tenure

ggplot(df, aes(x=tenure, y=monthlycharges)) + geom_point()
ggplot(df, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

as.factor(df$partner)
as.factor(df$multiplelines)
as.factor(df$seniorcitizens)
#subset data by request

df_phone <- subset(df, phoneservice == "Yes" & internetservice == "No" )

df_internet <- subset(df, internetservice != "No" & phoneservice != "Yes" )

df_both <- subset(df, phoneservice == "Yes" & internetservice != "No" )


# Classification metrics using training and test data sets


#phone - code from class
set.seed(1024)
trainIndex <- sample(1:nrow(df_phone), size=round(0.75*nrow(df)), replace=FALSE)
train_phone <- df[trainIndex,]
test_phone  <- df[-trainIndex,]
dim(train_phone); dim(test_phone)

#internet
set.seed(1024)
trainIndex <- sample(1:nrow(df_internet), size=round(0.75*nrow(df)), replace=FALSE)
train_internet <- df[trainIndex,]
test_internet  <- df[-trainIndex,]
dim(test_internet); dim(test_internet)

#both
set.seed(1024)
trainIndex <- sample(1:nrow(df), size=round(0.75*nrow(df_both)), replace=FALSE)
train_both <- df[trainIndex,]
test_both  <- df[-trainIndex,]
dim(train_both); dim(test_both)

#reduce test data to model selection
test_phone <- test %>%
  select(churnnum,
         seniorcitizen,
         partner,
         tenure,
         phoneservice,
         multiplelines,
         monthlycharges,
         contracttype,
         paymenttype
  )

test_internet <- test %>%
  select(churnnum,
         seniorcitizen,
         partner,
         tenure,
         monthlycharges,
         internetplus,
         contracttype,
         monthlycharges,
         paymenttype
  )

test_both <- test %>%
  select(churnnum,
         seniorcitizen,
         partner,
         tenure,
         multiplelines,
         internetplus,
         contracttype,
         monthlycharges,
         paymenttype
  )

# phone logit model
logit_phone  <- glm(churnnum ~ seniorcitizen + partner + tenure 
              + multiplelines + monthlycharges
              + contracttype + paymenttype, family=binomial (link="logit"), data=train_phone)

test_phone_model <-  predict(logit_phone, test_phone, type = "response")
test_phone_model_out <- ifelse(test_phone_model>0.5, 1, 0)
table(test_phone$churnnum,test_phone_model_out)

table(test_phone$churnnum, test_phone_model_out)                         # Confusion matrix
ClassificationError <- mean(test_phone_model_out != test_phone$churnnum) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accurate rate



#install.packages("ROCR")
library(ROCR)
pr_phone <- prediction(test_phone_model_out, test_phone$churnnum)
prf_phone <- performance(pr_phone, measure="tpr", x.measure="fpr")
plot(prf_phone) 



# internet logit model
logit_internet  <- glm(churnnum ~ seniorcitizen + partner + tenure 
                   + monthlycharges + internetplus
                   + contracttype + paymenttype, family=binomial (link="logit"), data=train_internet)

test_internet_model <-  predict(logit_internet, test_internet, type = "response")
internet_model_out <- ifelse(test_internet_model>0.5, 1, 0)
table(test$churnnum,internet_model_out)

table(test_internet$churnnum, internet_model_out)                         # Confusion matrix
ClassificationError <- mean(internet_model_out != test_internet$churnnum) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accurate rate

pr_internet <- prediction(internet_model_out, test_internet$churnnum)
prf_internet <- performance(pr_internet, measure="tpr", x.measure="fpr")
plot(prf_internet) 

# both logit model
logit_both  <- glm(churnnum ~ seniorcitizen + partner + tenure 
                  + multiplelines + internetplus + contracttype + monthlycharges 
                  + paymenttype, family=binomial (link="logit"), data=train_both)

test_both_model <-  predict(logit_both, test_both, type = "response")
test_both_model_out <- ifelse(test_both_model>0.5, 1, 0)
table(test_both$churnnum,test_both_model_out)

table(test_both$churnnum, test_both_model_out)                         # Confusion matrix
ClassificationError <- mean(test_both_model_out != test_both$churnnum) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accurate rate

pr_both <- prediction(test_both_model_out, test_both$churnnum)
prf_both <- performance(pr_both, measure="tpr", x.measure="fpr")
plot(prf_both) 
install.packages("margins")
#install.packages("stargazer")
library(stargazer)
stargazer(logit_both, logit_phone, logit_internet, type="text")


margins::margins(logit_both)
margins::margins(logit_phone)
margins::margins(logit_internet)

