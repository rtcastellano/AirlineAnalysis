library(dplyr)
library(ggplot2)
library(caret)
library(ggrepel)
library(reshape2)
library(RColorBrewer)


#Read in data -- one for each month
df_readin = read.csv('data/832175209_T_ONTIME 1.csv')

for (i in 2:12) 
{
  df_readin = rbind(df_readin, read.csv(paste('data/832175209_T_ONTIME ', as.character(i), '.csv', sep = '')))
}

#######################################
########### DATA PREPROCESSING ########
#######################################

df = df_readin
df$MONTH = factor(df$MONTH)

#Read in carriers data
carriers = read.csv('carriers.csv')
carriers = carriers[,1:2]              #Remove empty columns
carriers = carriers[2*seq(2982/2),]    #Remove empty rows

airports = read.csv('L_AIRPORT_SEQ_ID.csv')    #Names of airlines
markets = read.csv('L_CITY_MARKET_ID.csv')

df = merge(df, carriers, by.x = 'UNIQUE_CARRIER', by.y = 'Code')     #Add names of airlines
colnames(df)[22] <- "AIRLINE"
df = df[,-21]                #Remove empty column (came from downloading it)
levels(df$AIRLINE)[1403] <- "US Airways Inc."

#Join names of origin and destination airports
df = merge(df, airports, by.x = 'ORIGIN_AIRPORT_SEQ_ID', by.y = 'Code')     #Add names of airlines
colnames(df)[22] <- "ORIGIN_AIRPORT"

df = merge(df, airports, by.x = 'DEST_AIRPORT_SEQ_ID', by.y = 'Code')     #Add names of airlines
colnames(df)[23] <- "DEST_AIRPORT"

#Join names of origin and destination markets
df = merge(df, markets, by.x = 'ORIGIN_CITY_MARKET_ID', by.y = 'Code')     #Add names of airlines
colnames(df)[24] <- "ORIGIN_MARKET"

df = merge(df, markets, by.x = 'DEST_CITY_MARKET_ID', by.y = 'Code')     #Add names of airlines
colnames(df)[25] <- "DEST_MARKET"

##################################
#Convert variables to proper format and get rid of excess levels (coming from carriers and airports dataframe)
df$DEST_CITY_MARKET_ID = factor(df$DEST_CITY_MARKET_ID)
df$ORIGIN_CITY_MARKET_ID = factor(df$ORIGIN_CITY_MARKET_ID)
df$DEST_AIRPORT_SEQ_ID = factor(df$DEST_AIRPORT_SEQ_ID)
df$ORIGIN_AIRPORT_SEQ_ID = factor(df$ORIGIN_AIRPORT_SEQ_ID)
df$AIRLINE = factor(df$AIRLINE)
df$ORIGIN_AIRPORT = factor(df$ORIGIN_AIRPORT)
df$DEST_AIRPORT = factor(df$DEST_AIRPORT)
df$ORIGIN_MARKET = factor(df$ORIGIN_MARKET)
df$DEST_MARKET = factor(df$DEST_MARKET)


df$DAY_OF_WEEK = factor(df$DAY_OF_WEEK)
levels(df$DAY_OF_WEEK) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

df$DEP_DEL15 = factor(df$DEP_DEL15)
df$ARR_DEL15 = factor(df$ARR_DEL15)

#This is dataframe with all rows
df_wna = df

#####################################################################
#####################################################################

#Remove rows without delay information. Together this is ~1% of the data. However, this includes all flights
# that were cancelled. The percent of flights with delay information that is missing, but not cancelled is
# about .01%
df = df[!is.na(df$ARR_DEL15),]

nrow(df)
#There are 5.7 million flights

######################## FLIGHTS OVERVIEW ##################

###########################################################
############## FLIGHTS BY MONTH ###########################
###########################################################


by_month = group_by(df, MONTH) %>% summarise(count = n())

ggplot(by_month, aes(x = MONTH, y = count/1000)) + geom_line(aes(group = 1)) +
  xlab("Month") + ylab("Number of flights (in thousands)") + 
  ggtitle("Total flights by month (2015)") + theme_minimal() +
  scale_y_continuous(limits = c(0,550))

#############################################################
############### FLIGHTS BY DAY OF THE WEEK #################
############################################################

#by_dayofweek = group_by(df, DAY_OF_WEEK) %>% summarise(count = n())

#ggplot(by_dayofweek, aes(x = DAY_OF_WEEK, y = count)) + geom_bar(stat = 'identity') + xlab("Day of the Week")

####################################################
########### THESE ARE ALL DESTINATION #############

#############################################################
############### FLIGHTS BY AIRPORT #################
############################################################

#Here we are counting the number of flights with a given airport as its destination, but airports have almost identical number of arrivals/departures

by_dest_airport = group_by(df, DEST_AIRPORT) %>% summarise(count = n())
by_dest_airport = by_dest_airport[order(-by_dest_airport$count),]

ggplot(by_dest_airport[1:30,], aes(reorder(x = DEST_AIRPORT, -count), y = count/1000)) + 
  geom_bar(aes(fill = factor(3)), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + 
  xlab("Airport") + ylab("Number of flights (in thousands)") + guides(fill = F) +
  ggtitle("Top airports by number of arrivals") +
  scale_fill_brewer(palette = "Set2")

###########################################################
############### FLIGHTS BY MARKET #########################
###########################################################

#Same as for airport as far as arrival/departure

by_dest_market = group_by(df, DEST_MARKET) %>% summarise(count = n())
by_dest_market = by_dest_market[order(-by_dest_market$count),]

ggplot(by_dest_market[1:30,], aes(reorder(x = DEST_MARKET, -count), y = count/1000)) + 
  geom_bar(aes(fill = factor(2)), stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + 
  xlab("Market") + ylab("Number of flights (in thousands)") + 
  ggtitle("Top markets by number of arrivals") + guides(fill = F) +
  scale_fill_brewer(palette = "Set2")

#############################################################
############### ROUTES BY AIRPORT #################
############################################################

by_route = group_by(df, ORIGIN_AIRPORT, DEST_AIRPORT) %>% summarise(count = n())
by_route = by_route[order(-by_route$count),]

ggplot(by_route[2*(1:10),], aes(reorder(
  x = paste(paste('Airport 1:', ORIGIN_AIRPORT), 
            paste('Airport 2:', DEST_AIRPORT, '\n'), sep = '\n'),
  -count))) + 
  geom_bar(stat = 'identity', aes(y = count, fill = factor(1))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + 
  xlab("Airport route") + ylab("Number of flights") + 
  ggtitle("Top routes by airport (one way)") + guides(fill = F) +
  scale_fill_brewer(palette = "Set2")

#############################################################
############### ROUTES BY MARKET ###########################
############################################################

by_route_markets = group_by(df, ORIGIN_MARKET, DEST_MARKET) %>% summarise(count = n())
by_route_markets = by_route_markets[order(-by_route_markets$count),]

ggplot(by_route_markets[2*(1:10),], aes(reorder(
  x = paste(paste('Origin:', ORIGIN_MARKET), 
            paste('Dest:', DEST_MARKET, '\n'), sep = '\n'),
  -count))) + 
  geom_bar(stat = 'identity', aes(y = count, fill = factor(2))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + 
  xlab("Market route") + ylab("Number of flights") + 
  ggtitle("Top routes by market (one way)") + guides(fill = F) +
  scale_fill_brewer(palette = "Set2")

#############################################################
############### TOTAL NUMBER OF FLIGHTS BY AIRLINE ##########
#############################################################
airline_summary = group_by(df, AIRLINE) %>% summarise(pct_delayed = sum(ARR_DEL15 == 1)/n(),
                                                      avg_delay = mean(ARR_DELAY_NEW),
                                                      count = n(), 
                                                      avg_distance = mean(DISTANCE))

#ggplot(airline_summary, aes(reorder(x = AIRLINE, -count), y = count)) + 
#  geom_bar(stat = 'identity') + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())

ggplot(airline_summary, aes(x = count/1000, y = avg_distance)) + geom_point() + 
  geom_text_repel(aes(label=AIRLINE)) + xlab('Number of flights (in thousands)') + 
  ylab('Avergage distance of flight (miles)') + 
  ggtitle('Total flights and average distance by carriers') + 
  theme_minimal()

#########Distance bar graph############
# ggplot(airline_summary, aes(reorder(x = AIRLINE, -count), y = distance)) + 
#   geom_bar(stat = 'identity') + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())

###############################################################################
############################ DELAYS ########################################
###############################################################################

###################### OVERVIEW OF DELAYS ############################

sum(df$ARR_DEL15 == 1)/nrow(df)
# 18.6% of flights are delayed (defined as being delayed)

mean(df$ARR_DELAY_NEW)
#Flights are delayed 12.1 minutes on average

mean(df[df$ARR_DEL15 == 1, ]$ARR_DELAY_NEW)
median(df[df$ARR_DEL15 == 1, ]$ARR_DELAY_NEW)

#The average delayed flight is delayed 58.9 minutes
#The median delay is 37

#########################
### DELAY BY AIRPORT#####
########################

delayed_by_airport = df %>% group_by(DEST_AIRPORT) %>%
  summarise(pct = sum(ARR_DEL15 == 1)/n(),
            count = n())

#ggplot(delayed_by_airport, aes(x = count, y = pct)) + geom_point()

delayed_by_airport = delayed_by_airport[order(-delayed_by_airport$pct),]

ggplot(delayed_by_airport, aes(x = pct*100)) + 
  geom_histogram(aes(fill = factor(2)), color = "black") +
  xlab("Percent of flights delayed") + ylab("Number of airports") +
  ggtitle("Distribution of average delay time by airport") + guides(fill = F) +
  scale_fill_brewer(palette = "Set2") + theme_minimal()

ggplot(delayed_by_airport[1:30,], aes(reorder(x = DEST_AIRPORT, -pct))) + 
  geom_bar(stat = 'identity', aes(y = pct*100, fill = factor(2))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + 
  xlab("Airport") + ylab("Percent of flights delayed") + 
  ggtitle('Airports with the greatest percent of flights delayed') + guides(fill = F) +
  scale_fill_brewer(palette = "Set2")

################################################
########### DELAYS AND MILES ###################
################################################

flights_distance = group_by(df, distance_group = cut(df$DISTANCE, breaks = c(0,500,1000,1500,2000,2500, 3000, 3500, 4000, 4500, 5000))) %>% 
  summarise(num = n())

ggplot(flights_distance, aes(x = distance_group, y = num/1000)) + 
  geom_bar(stat = 'identity', aes(fill = factor(2))) + 
             scale_x_discrete(labels = c("0-500", "500-1000", "1000-1500", "1500-2000",
                                         "2000-2500", "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500-5000")) + 
             xlab("Distance (miles)") + ylab("Number of flights (in thousands)") + 
             ggtitle("Distribution of flights by distance") + guides(fill = F) +
             scale_fill_brewer(palette = "Set2") + theme_minimal()
           
           
flights_delayed_distance = group_by(df, distance_group = cut(DISTANCE, breaks = c(0,500,1000,1500,2000,2500, 3000, 3500, 4000, 4500, 5000))) %>% 
   summarise(pct_delayed = sum(ARR_DEL15 == 1, na.rm = T)/n(), 
             avg_delay = mean(ARR_DELAY_NEW))
           
ggplot(flights_delayed_distance, aes(x = distance_group, y = pct_delayed*100)) + 
 geom_bar(stat = 'identity', aes(fill = factor(2))) +  
 scale_x_discrete(labels = c("0-500", "500-1000", "1000-1500", "1500-2000",
                           "2000-2500", "2500-3000", "3000-3500", "3500-4000", "4000-4500", "4500-5000")) + 
 xlab("Distance of flight (miles)") + ylab("Percent of flights delayed") + 
 ggtitle("Percent of flights delayed by distance") + guides(fill = F) +
 scale_fill_brewer(palette = "Set2") + theme_minimal()
           
#ggplot(flights_delayed_distance, aes(x = distance_group, y = avg_delay)) +
#  geom_bar(stat = 'identity')  
       
###############################
########TYPE OF DELAY#########
##############################
#TOTAL MINUTES DELAYED BY REASON
           
df_delayed = filter(df, ARR_DEL15 == 1)
           
reason_for_delay = summarise(df_delayed, Carrier = sum(CARRIER_DELAY > 0, na.rm = T)/n(),
                                        Weather = sum(WEATHER_DELAY > 0, na.rm = T)/n(),
                                        NAS = sum(NAS_DELAY > 0, na.rm = T)/n(),
                                        Security = sum(SECURITY_DELAY > 0, na.rm = T)/n(),
                                        Aircraft = sum(LATE_AIRCRAFT_DELAY > 0, na.rm = T)/n())
           # mean(CARRIER_DELAY, na.rm = T)/n(),
           # mean(WEATHER_DELAY, na.rm = T)/n(),
           # mean(NAS_DELAY, na.rm = T)/n(),
           # mean(SECURITY_DELAY, na.rm = T)/n(),
           # mean(LATE_AIRCRAFT_DELAY, na.rm = T)/n()
           
reason_for_delay = data.frame(pct = t(reason_for_delay))
reason_for_delay$reason = rownames(reason_for_delay)
           
ggplot(reason_for_delay, aes(x = reason)) + 
 geom_bar(stat = 'identity', aes(y = pct*100, fill = reason)) + xlab("Reason for delay") + 
 ylab("Percent of delayed flights") + 
 ggtitle("Reason for delay among delayed flights\n(Flights may be delayed for multiple reasons)") + guides(fill = F) +
 scale_fill_brewer(palette = "Set2") + theme_minimal() +
 scale_x_discrete(labels = c("Late aircraft", "Carrier", "NAS", "Security", "Weather"))
           
           
#Not sure this one is useful
#avg_reason_time = summarise(df, Carrier = mean(CARRIER_DELAY, na.rm = T),
#                                Weather = mean(WEATHER_DELAY, na.rm = T),
#                                NAS = mean(NAS_DELAY, na.rm = T),
#                                Security = mean(SECURITY_DELAY, na.rm = T),
#                                Aircraft = mean(LATE_AIRCRAFT_DELAY, na.rm = T))
#avg_reason_time = data.frame(avg = t(avg_reason_time))
#avg_reason_time$reason = rownames(avg_reason_time)
           
#ggplot(avg_reason_time, aes(x = reason)) + geom_bar(stat = 'identity', aes(y = avg))
           
# #Avg reason for delay among delayed flights
#Non delayed flights are marked with NA delays
# avg_reason_time_amongdelayed = summarise(df_delayed, Carrier = mean(CARRIER_DELAY, na.rm = T),
#                             Weather = mean(WEATHER_DELAY, na.rm = T),
#                             NAS = mean(NAS_DELAY, na.rm = T),
#                             Security = mean(SECURITY_DELAY, na.rm = T),
#                             Aircraft = mean(LATE_AIRCRAFT_DELAY, na.rm = T))
# avg_reason_time_amongdelayed = data.frame(avg = t(avg_reason_time_amongdelayed))
# avg_reason_time_amongdelayed$reason = rownames(avg_reason_time_amongdelayed)
# 
# ggplot(avg_reason_time_amongdelayed, aes(x = reason)) + geom_bar(stat = 'identity', aes(y = avg))
           
           
           
           
#Averge delay by reason (when delayed for that reason)
reasons = c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY")
         
avg_delay = data.frame(reasons)
avg_delay['avg']= 0
for (reason in reasons)
{
   avg_delay[which(reasons == reason), 'avg'] = mean(df[df[reason] > 0, reason], na.rm = T)
}

for (reason in reasons)
{
  avg_delay[which(reasons == reason), 'avg'] = mean(df[df[reason] > 0, reason], na.rm = T)
}

CARRIER = df[!is.na(df['CARRIER_DELAY']) & df['CARRIER_DELAY'] > 0, 'CARRIER_DELAY']
WEATHER = df[!is.na(df['WEATHER_DELAY']) & df['WEATHER_DELAY'] > 0, 'WEATHER_DELAY']
NAS = df[!is.na(df['NAS_DELAY']) & df['NAS_DELAY'] > 0, 'NAS_DELAY']
SECURITY = df[!is.na(df['SECURITY_DELAY']) & df['SECURITY_DELAY'] > 0, 'SECURITY_DELAY']
LATE_AIRCRAFT = df[!is.na(df['LATE_AIRCRAFT_DELAY']) & df['LATE_AIRCRAFT_DELAY'] > 0, 'LATE_AIRCRAFT_DELAY']

delay_types <- rbind(data.frame(group = 'Late aircraft', delay = LATE_AIRCRAFT),
                     data.frame(group = 'Carrier', delay = CARRIER),
                     data.frame(group = 'NAS', delay = NAS),
                     data.frame(group = 'Security', delay = SECURITY), 
                     data.frame(group = 'Weather', delay = WEATHER))

ggplot(delay_types, aes(x = group)) + 
  geom_boxplot(aes(y = delay, fill = group), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,85)) + scale_fill_brewer(palette = "Set2") + theme_minimal() +
  xlab("Reason") + ylab("Delay (minutes)") + ggtitle("Length of delay by reason") + guides(fill = F)
           
# ggplot(avg_delay, aes(x = reasons)) + 
#    geom_bar(stat = 'identity', aes(y = avg, fill = reasons)) + 
#    xlab("Reason for delay") + ylab("Average delay (minutes)") + 
#    ggtitle("Average delay by reason") +
#    scale_x_discrete(labels = c("Late aircraft", "Carrier", "NAS", "Security", "Weather")) + guides(fill = F) +
#    scale_fill_brewer(palette = "Set2") + theme_minimal()
           
##############################################################
################### DELAYS BY CARRIER ########################
##############################################################
           
#Useless 
# ggplot(df[df$ARR_DEL15 == 1,], aes(x = AIRLINE)) + 
#   geom_violin(aes(y = ARR_DELAY_NEW)) + scale_y_continuous(limits = c(0, 100)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())
           
delay_by_airline = df %>% group_by(AIRLINE) %>% summarise(pct_delayed = sum(ARR_DEL15 == 1, na.rm = T)/n(), 
                                                                     avg_delay = mean(ARR_DELAY_NEW))
           
ggplot(delay_by_airline, aes(reorder(x = AIRLINE, -pct_delayed))) + 
 geom_bar(aes(y = pct_delayed*100, 
              fill = colorRampPalette(brewer.pal(8, "Set2"))(length(levels(delay_by_airline$AIRLINE)))), 
              stat = 'identity') + 
 theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + xlab("Airline") + ylab("Percent of flights delayed") +
 ggtitle("Percent of flights delayed by airline") + guides(fill = F)
           
#ggplot(delay_by_airline, aes(reorder(x = AIRLINE, -avg_delay))) + 
#  geom_bar(aes(y = avg_delay), stat = 'identity') + 
#  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())
           
#ggplot(delay_by_airline, aes(x = pct_delayed, y = avg_delay)) + geom_point() +
#  geom_text_repel(aes(label=AIRLINE))
           
# airline_delay_numflights = df_delayed_notna %>% group_by(Airline) %>% summarise(pct_delayed = sum(ARR_DELAY_NEW > DELAY_TIME_FIXED, na.rm = T)/n(), 
#                                                                                 avg_delay = mean(ARR_DELAY_NEW),
#                                                                                 count = n(), 
#                                                                                 distance = sum(DISTANCE))
# 
# ggplot(airline_delay_numflights, aes(x = distance, y = pct_delayed)) + geom_point() + 
#   geom_text_repel(aes(label=Airline))
# 
# ggplot(airline_delay_numflights, aes(x = count, y = pct_delayed)) + geom_point() + 
#   geom_text_repel(aes(label=Airline))
# 
# ggplot(airline_delay_numflights, aes(x = distance, y = pct_delayed)) + geom_point() + 
#   geom_text_repel(aes(label=Airline))
           
#Reason for delay among delayed flights by carrier
reason_for_delay_carriers = group_by(df_delayed, AIRLINE) %>%
             summarise(Carrier = sum(CARRIER_DELAY > 0, na.rm = T)/n(),
                       Weather = sum(WEATHER_DELAY > 0, na.rm = T)/n(),
                       NAS = sum(NAS_DELAY > 0, na.rm = T)/n(),
                       Security = sum(SECURITY_DELAY > 0, na.rm = T)/n(),
                       Aircraft = sum(LATE_AIRCRAFT_DELAY > 0, na.rm = T)/n())
           
reason_for_delay_carriers_long = melt(id.vars = 'AIRLINE', reason_for_delay_carriers) 
           
#ggplot(reason_for_delay_carriers_long, aes(x = AIRLINE, y = value)) + 
           #  geom_bar(stat = 'identity', aes(fill = variable), position = 'dodge') +
           #  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())
           
           
#ggplot(reason_for_delay_carriers_long, aes(x = AIRLINE, y = value)) + 
#  geom_bar(stat = 'identity', aes(fill = variable), position = 'stack') +   
#  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())
           
brewerpal(n=length(levels(delay_by_airport$AIRLINE)), palette=Accent)

getPalette = colorRampPalette(brewer.pal(8, "Set2"))

getPalette(1)

ggplot(reason_for_delay_carriers_long, aes(x = AIRLINE, y = value*100)) + 
 geom_bar(stat = 'identity', 
            aes(fill = AIRLINE), position = 'dodge') + 
 scale_fill_manual(values = getPalette(14), name = "Airline") +
 facet_wrap(~ variable) +
 theme(axis.text.x=element_blank(), axis.ticks = element_blank()) +
 xlab("Airline") + ylab("Percent of delayed flights") + 
 ggtitle("Reason for delayed flights by airline\n(Flights may be delayed for multiple reasons)")
           
           
#ggplot(reason_for_delay_carriers_long, aes(x = variable, y = value)) + 
#  geom_bar(stat = 'identity', aes(fill = AIRLINE), position = 'fill') +   
#  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())
           
######
#Didn't look at this, but is a possibility
#mean(df[df$AIRLINE == "Delta Air Lines Inc." & df$CARRIER_DELAY > 0,'CARRIER_DELAY'], na.rm = T)
######

#######################################
######################################
###### LOGISTIC REGRESSION ###########
#####################################
pchisq(logit_no_market$deviance, logit_no_market$df.residual, lower.tail = FALSE)

anova(logit_no_market, logit_with_market, test = "Chisq")

#Top airports
by_dest_airport = group_by(predictors_response, DEST_AIRPORT) %>% summarise(count = n())
by_dest_airport = by_dest_airport[order(-by_dest_airport$count),]
top_airports = by_dest_airport$DEST_AIRPORT[1:30]

predictors_response_topdest = predictors_response[predictors_response$DEST_AIRPORT %in% top_airports & 
                                                    predictors_response$ORIGIN_AIRPORT %in% top_airports,]
predictors_response_topdest$ORIGIN_AIRPORT = factor(predictors_response_topdest$ORIGIN_AIRPORT)
predictors_response_topdest$DEST_AIRPORT = factor(predictors_response_topdest$DEST_AIRPORT)
predictors_response_topdest$ORIGIN_MARKET = factor(predictors_response_topdest$ORIGIN_MARKET)
predictors_response_topdest$DEST_MARKET = factor(predictors_response_topdest$DEST_MARKET)

sample = sample(1:nrow(predictors_response_topdest), 200000)
train = sample(sample, .8*length(sample))
test = sample[!(sample %in% train)]

time_bucket <- function(x) 
{
  if (x %in% c("0600-0659")) {
    return("Early morning")
  } else if (x %in% c("0700-0759", "0800-0859", "0900-0959", "1000-1059")) {
    return("Morning")
  } else if (x %in% c("1100-1159", "1200-1259", "1300-1359", "1400-1459", "1500-1559",
                      "1600-1659", "1700-1759")) {
    return("Afternoon")
  } else {
    return("Night")
  }
}

month_bucket <- function(x) 
{
  if (x %in% c(12, 1, 2, 3)) {
    return("Winter")
  } else if (x %in% c(4, 5)) {
    return("Spring")
  } else if (x %in% c(6, 7, 8, 9)) {
    return("Summer")
  } else {
    return("Fall")
  }
}

predictors_response_topdest$DEP_BUCKET = factor(sapply(predictors_response_topdest$DEP_TIME_BLK, time_bucket))
predictors_response_topdest$MONTH_BUCKET = factor(sapply(predictors_response_topdest$MONTH, month_bucket))

#####################################


#Preliminary analysis
#Significant p-value (<2.2e-16)
chisq.test(table(predictors_response_topdest$DEST_AIRPORT, predictors_response_topdest$response))

######################################
##### FINAL LOGIT MODEL ANALYSIS #####
######################################
logit = glm(response ~ . - response - ORIGIN_MARKET - DEST_MARKET - 
              ARR_TIME_BLK - MONTH - DEP_BUCKET - DAY_OF_WEEK, 
            family = binomial, 
            data = predictors_response_topdest[train,])



mean(predictors_response_topdest$response[train] == 0)
#80%

roc_logit = roc(predictors_response_topdest$response[train], logit$fitted.values)
roc_logit$auc
#AUC of .65

roc_logit_coords = coords(roc_logit, x = 'best')
threshold = roc_logit_coords['threshold']
table(logit$fitted.values > threshold, predictors_response_topdest$response[train])
#(74247 + 20169)/160000= 59%
#specificity sensitivity 
#0.5774112   0.6420386 


table(round(logit$fitted.values), predictors_response_topdest$response[train])
#(128492 + 91)/160000
#80%

logit.test.predicted = predict(logit, predictors_response_topdest[test,], type = 'response')
table(logit.test.predicted > threshold, predictors_response_topdest$response[test])
#(18564 + 5017)/40000
#59%
#Specificity = 18564/(18564 + 13506) = 58%
#Sensitivity = 5017/(5017 + 2913) = 63%


#####################################
##### RANDOM FOREST ################
#####################################
library(randomForest)

rf_original = randomForest(response ~ . - response - ORIGIN_MARKET - DEST_MARKET - 
                             - MONTH_BUCKET - DEP_BUCKET, 
                           data = predictors_response_topdest[train,])

rf = randomForest(response ~ . - response - ORIGIN_MARKET - DEST_MARKET - 
                    ARR_TIME_BLK - MONTH - DEP_BUCKET, 
                  family = binomial, 
                  data = predictors_response_topdest[train,], mtry = 1, ntree = 300)

rf

predicted.rf = predict(rf, predictors_response_topdest[train,], type = 'prob')
roc.rf = roc(predictors_response_topdest$response[train], predicted.rf[,2])
coords(roc.rf, x = 'best')

table(predicted.rf[,2] > .191, predictors_response_topdest$response[train])

#Test
predicted.rf.test = predict(rf, predictors_response_topdest[test,], type = 'prob')

table(predicted.rf.test[,2] > .191, predictors_response_topdest$response[test])

