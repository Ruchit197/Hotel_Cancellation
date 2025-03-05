
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(tinytex)

setwd("/Users/Personal/Desktop/Files/Profile Project/Hotel Cancellation") # Properly escaped for LaTeX
df<-read.csv(file="/Users/Personal/Desktop/Files/Profile Project/Hotel Cancellation/hotel_bookings.csv",header=TRUE,sep = ",",stringsAsFactors = FALSE)

names(df) <- tolower(names(df))
original<-df

#Summarising the data
str(df)
summary(df)
head(df)
dim(df)
summary(is.na(df))

############# Converting Charactor Variables into Factors ######################
df<-df%>%
  mutate(is_canceled=as.factor(is_canceled))
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
str(df)
##################

###################### deleting a variable from the data frame #####################

df = subset(df, select = -c(agent,company)) #Too many Null Values or irrelevant variable
summary(df)

###################### Missing Value Treatment #####################

#Replacing missing values in children column from the corresponding babies column
n <- length(df$children)
for (i in 1:n) {
  if (is.na(df$children[i]))
    df$children[i] <- df$babies[i]
}


#Replacing undefined as SC. Both implies no meal package
df$meal <-replace(df$meal,df$meal=='Undefined','SC')
unique(df$meal)
summary(df)

#Checking unique values in market_segment
df$market_segment <-replace(df$market_segment,df$market_segment=='Undefined','Online TA')
unique(df$market_segment)

#Checking unique values in market_segment
df$distribution_channel <-replace(df$distribution_channel,df$distribution_channel=='Undefined','TA/TO')
unique(df$distribution_channel)

#checking missing values in numeric columns
sapply(df, function(x) sum(is.na(x)))

#unique values
sapply(df, function(x) length(unique(x)))

####################### Derived Variable ##########################

df2 <- df %>% 
  mutate(stay_nights_total = stays_in_weekend_nights + stays_in_week_nights,
         stay_cost_total = adr * stay_nights_total) #Total cost = Avg. Daily Rate (adr) X Total nights stayed

summary(df2)

#Outlier Detection:

par(mfrow = c(1,1)) # Partitioning graphics window in to 2 parts
boxplot(df2$stay_cost_total, main = "Box-plot of stay_cost_total")
boxplot(df2$adr, main = "Box-plot of adr")

# Detect and treat outliers in the `adr` column using the IQR method
# Treat outliers in 'adr' using IQR
adr_Q1 <- quantile(df2$adr, 0.25, na.rm = TRUE)
adr_Q3 <- quantile(df2$adr, 0.75, na.rm = TRUE)
adr_IQR <- adr_Q3 - adr_Q1
adr_lower_bound <- adr_Q1 - 1.5 * adr_IQR
adr_upper_bound <- adr_Q3 + 1.5 * adr_IQR

# Cap outliers
df2$adr <- ifelse(df2$adr <= adr_lower_bound, adr_lower_bound, 
                         ifelse(df2$adr > adr_upper_bound, adr_upper_bound, df2$adr))

boxplot(df2$adr, main = "Box-plot of adr")


##################### Data Visualisation ##############################
#the most visited hotle
ggplot(data = df2,aes(hotel,fill=hotel))+
  geom_bar()+
  labs(title = "City Hotel VS Resort Hotel")

#Year wise hotel booking
df2%>%
  ggplot(aes(x=arrival_date_year,fill=hotel))+
  geom_bar()

#the cancellations in city hotel and resort hotel
ggplot(data = df2,aes(is_canceled,fill=is_canceled ))+
  geom_bar( ) +
  facet_wrap(~hotel)  +
  labs(title = "cancellations in city hotel vs resort hotel")

# most visited months
ggplot(data = df2,aes(arrival_date_month,fill=arrival_date_month))+
  geom_bar()+
  theme(axis.text.x=element_text(angle =  30 ))+
  labs(title = "most visited months")


# distribution channel
ggplot(data = df2,aes(x=distribution_channel,fill=distribution_channel))+
  geom_bar()+
  labs(title = "Distribution Channel")

ggplot(data=df2) +
  geom_bar(mapping=aes(x=distribution_channel, fill=market_segment)) +
  labs(title='Market Segment vs. Distribution Channel', x='Distribution Channel', y='Market Segment Proportion')

options(repr.plot.width=12, repr.plot.height=8)
ggplot(df2) + 
  geom_bar(mapping=aes(x=reservation_status, fill=hotel)) +
  labs(x = "Reservation Status", title = "Hotel Reservation Status", fill = "Reservation Status") 
theme_light(base_size = 20)

# Exploring the number of countries invloved
df2%>%
  group_by(country)%>%
  summarise(num=n())%>%
  arrange(desc(num))

#Lead time and hotel cancellations
df2%>%
  ggplot(aes(x=lead_time,fill=is_canceled))+
  geom_histogram(binwidth=10,position="stack")

###### Cancellation By Hotel Type based on Lead time
ggplot(data = df2, aes(x = hotel,y = lead_time,fill = factor(is_canceled))) +
  
  geom_boxplot(position = position_dodge()) +
  
  labs(
    title = "Cancellation By Hotel Type based on Lead time",
    x = "Hotel Type",
    y = "Lead Time (Days)") +
  
  theme(axis.text.x = element_text(face="bold", size=15),axis.text.y = element_text(face="bold", size=15))+
  
  scale_fill_manual(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Not Cancelled", "Cancelled"),
    values = c("0" = "Green", "1"="pink")
  )

#Remove NA's
colSums(is.na(df2))
df2$children[is.na(df2$children)] = round(mean(df2$children, na.rm = TRUE),0)
sum(is.na(df2$children))


####################### Model building using all Variables #############################
#Train and Test split
set.seed(100)   # set a random seed 
index <- sample(nrow(df2), nrow(df2)*0.25) 

test <- df2[index,]       #  25% as a test dataset
training <-df2[-index,]   # save the rest as a training set
head(training)


table(training$is_canceled)/nrow(training)
table(test$is_canceled)/nrow(test)

training_features <- training[c('hotel','is_canceled','lead_time','adults','children','babies','meal',
                                          'market_segment','distribution_channel','is_repeated_guest',
                                          'previous_cancellations','previous_bookings_not_canceled','reserved_room_type','booking_changes',
                                          'deposit_type','days_in_waiting_list','customer_type','adr',
                                          'required_car_parking_spaces','stay_nights_total','stay_cost_total')]

logit_model_1<-glm(is_canceled~ hotel+lead_time+adults+children+babies+meal+
                   market_segment+distribution_channel+is_repeated_guest+
                   previous_cancellations+previous_bookings_not_canceled+reserved_room_type+booking_changes+
                   deposit_type+days_in_waiting_list+customer_type+adr+
                   required_car_parking_spaces+stay_nights_total+stay_cost_total,family="binomial",data=training)
summary(logit_model_1)

library(car)
vif(logit_model_1)

####### Taking out Variables one by one where GVIF >5 indicating multicollinearity. ##########
logit_model_2<-glm(is_canceled~ hotel+lead_time+adults+children+babies+meal+
                     distribution_channel+is_repeated_guest+
                     previous_cancellations+previous_bookings_not_canceled+reserved_room_type+booking_changes+
                     deposit_type+days_in_waiting_list+customer_type+adr+
                     required_car_parking_spaces+stay_nights_total+stay_cost_total,family="binomial",data=training)
summary(logit_model_2)

vif(logit_model_2)

logit_model_3<-glm(is_canceled~ hotel+lead_time+adults+children+babies+meal+
                     distribution_channel+is_repeated_guest+
                     previous_cancellations+previous_bookings_not_canceled+reserved_room_type+booking_changes+
                     deposit_type+days_in_waiting_list+customer_type+adr+
                     stay_nights_total,family="binomial",data=training)
summary(logit_model_3)

vif(logit_model_3)

logit_model_4<-glm(is_canceled~ hotel+lead_time+adults+children+babies+meal+
                     distribution_channel+is_repeated_guest+
                     previous_cancellations+previous_bookings_not_canceled+reserved_room_type+booking_changes+
                     deposit_type+customer_type+adr+
                     stay_nights_total,family="binomial",data=training)
summary(logit_model_4)

logit_model_5<-glm(is_canceled~ hotel+lead_time+adults+children+babies+meal+
                     distribution_channel+is_repeated_guest+
                     previous_cancellations+previous_bookings_not_canceled+booking_changes+
                     deposit_type+customer_type+adr+
                     stay_nights_total,family="binomial",data=training)
summary(logit_model_5)

load("logistic_model_4.rda")

logit_pred_prob_4<-predict(logit_model_4,test,type="response")
logit_pred_class_4<-ifelse(logit_pred_prob_4>0.5,"1","0") 

table(test$is_canceled==logit_pred_class_4)


train_logit_pred_prob_4<-predict(logit_model_4,training,type="response")
train_logit_pred_class_4<-ifelse(train_logit_pred_prob_4>0.5,"1","0") 

logit_pred_prob_5<-predict(logit_model_5,test,type="response")
logit_pred_class_5<-ifelse(logit_pred_prob_5>0.5,"1","0") 

table(test$is_canceled==logit_pred_class_5)


train_logit_pred_prob_5<-predict(logit_model_5,training,type="response")
train_logit_pred_class_5<-ifelse(train_logit_pred_prob_5>0.5,"1","0") 


####### AUC PLOT ###########

# Check using other logit_models
library(ROCR)
pr_5 <- prediction(logit_pred_prob_5, test$is_canceled)
prf_5 <- performance(pr_5, measure = "tpr", x.measure = "fpr")
plot(prf_5, colorize = TRUE, text.adj = c(-0.2,1.7))
auc_5 <- performance(pr_5, measure = "auc")
auc_5 <- auc_5@y.values[[1]]
auc_5  #0.8123899

#test
library(ROCR)
pr <- prediction(logit_pred_prob_4, test$is_canceled)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  #0.8129363  -----------------------This is the best AUC


#train
library(ROCR)
train_pr <- prediction(train_logit_pred_prob_4, training$is_canceled)
train_prf <- performance(train_pr, measure = "tpr", x.measure = "fpr")
plot(train_prf, colorize = TRUE, text.adj = c(-0.2,1.7))
train_auc <- performance(train_pr, measure = "auc")
train_auc <- train_auc@y.values[[1]]
train_auc  #0.8070865

### Confusion Matrix 
library(caTools)
library(caret)
confusionMatrix(as.factor(logit_pred_class_4), test$is_canceled)


##### Saving the models #########
save(logit_model_4, file = "logistic_model_4.RData") #====== Final Model
save(logit_model_4, file = "logistic_model_4.rda")

save(logit_model_5, file = "logistic_model_5.RData")
save(logit_model_5, file = "logistic_model_5.rda")


#### PREDICTION USING LOGISTIC ########

training$train_logit_pred_prob_4<-predict(logit_model_4,training,type="response")
training$decile <- ntile(-training$train_logit_pred_prob_4, 10)  

summary(training$train_logit_pred_prob_4)
summary(training$decile)
#training$train_logit_pred_prob_4[is.na(training$logit_pred_prob_4)] <- mean(training$logit_pred_prob_4, na.rm = T)

write.csv(training ,file = "HOTEL_CANCEL_TRAIN_SCORED_DATA.CSV",na= "")


test$logit_pred_prob_4<-predict(logit_model_4,test,type="response")
test$decile <- ntile(-test$logit_pred_prob_4, 10)  

summary(test$logit_pred_prob_4)
summary(test$decile)
test$logit_pred_prob_4[is.na(test$logit_pred_prob_4)] <- mean(test$logit_pred_prob_4, na.rm = T)


write.csv(test ,file = "HOTEL_CABCEL_TEST_SCORED_DATA.CSV",na= "")



knitr::stitch('Hotel_Cancellation_prediction.r')














