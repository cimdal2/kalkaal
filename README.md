# kalkaal
R scripts for kalkaal project
setwd("~/Google Drive/..../12.1 Data for visualisation_analysis/S03")
rm(list = ls())

install.packages("stringr")
install.packages("xlsx")
install.packages("epitools")
install.packages("forestplot")
library(stringr) 
library(xlsx)
library(epitools)
library(forestplot)

###################################################################################################################
##################################################################MEDIAINK#########################################
###################################################################################################################
mediaink0 <- read.csv("mediaink_s03_9weeks_1002.csv")

length(unique(mediaink0$phone))

names(mediaink0)

#Mediaink parse date
mediaink0$date2 <- as.Date(mediaink0$date, format="%m/%d/%Y")
prop.table(table(is.na(mediaink0$date)))

mediaink0$date <- NULL
mediaink0$date <- mediaink0$date2
mediaink0$date2 <- NULL
class(mediaink0$date)

summary(mediaink0$date)

mediaink0$week <- as.character(mediaink0$week)

table(mediaink0$week)
class(mediaink0$week)

mediaink0$week <- as.numeric(mediaink0$week)
mediaink <- subset(mediaink0, mediaink0$week<9)
length(unique(mediaink$phone))

table(mediaink$week)

mediaink1 <- subset(mediaink, mediaink$week=="1")
mediaink2 <- subset(mediaink, mediaink$week=="2")
mediaink3 <- subset(mediaink, mediaink$week=="3")
mediaink4 <- subset(mediaink, mediaink$week=="4")
mediaink5 <- subset(mediaink, mediaink$week=="5")
mediaink6 <- subset(mediaink, mediaink$week=="6")
mediaink7 <- subset(mediaink, mediaink$week=="7")
mediaink8 <- subset(mediaink, mediaink$week=="8")

length(unique(mediaink1$phone))
length(unique(mediaink2$phone))
length(unique(mediaink3$phone))
length(unique(mediaink4$phone))
length(unique(mediaink5$phone))
length(unique(mediaink6$phone))
length(unique(mediaink7$phone))
length(unique(mediaink8$phone))

mediaink$ID <- mediaink$phone
mediaink$phone <- NULL

mediaink <- mediaink[,c("time", "week", 'text', "ID")]

rm(mediaink0, mediaink1, mediaink2, mediaink3, mediaink4, mediaink5, mediaink6, mediaink7, mediaink8)
############################################################################################################################
##################################################################UNICEF####################################################
############################################################################################################################
unicef0 <- read.csv('tableau_unicef_s03_9weeks_0902.csv')
length(unique(unicef0$ID))

#UNICEF de-duplicate
unicef <- unicef0[!duplicated(unicef0[c("ID")]),]
length(unique(unicef$ID))

#duplicates <- unicef0[duplicated(unicef0[c("ID")]),]
#write.csv(duplicates, "duplicates.csv")

final0 <- merge(mediaink, unicef, by="ID", all.x=TRUE)
names(final0)

final <- final0[!duplicated(final0[c("ID")]),]
length(unique(final$ID))

###################################################################################################################################
#########################################################################Participation study#######################################
###################################################################################################################################
#Get participation

part <- final0[c("week", "ID")]

part$week0 <- 1

#Get participation weeks
part2 <- reshape(part, timevar = "week", idvar=c("ID"), direction = "wide")

names(part2)

part2$week0.1[is.na(part2$week0.1)] <- "0"
part2$week0.2[is.na(part2$week0.2)] <- "0"
part2$week0.3[is.na(part2$week0.3)] <- "0"
part2$week0.4[is.na(part2$week0.4)] <- "0"
part2$week0.5[is.na(part2$week0.5)] <- "0"
part2$week0.6[is.na(part2$week0.6)] <- "0"
part2$week0.7[is.na(part2$week0.7)] <- "0"
part2$week0.8[is.na(part2$week0.8)] <- "0"

part2[,2:9] <- apply(part2[,2:9], 2, as.numeric)

#Get participation patterns
pattern <- aggregate(part2, by=part2[,2:9], FUN=length)

pattern2 <- pattern[c(1:8)]
pattern2$part <- rowSums(pattern2[,1:8])

length(part2$ID)
part2 <- part2[c("ID", "week0.1", "week0.2", "week0.3", "week0.4", "week0.5", "week0.6", "week0.7", "week0.8")]

apply(subset(part2, part2$week0.7==1)[2:9], 2, function(x) table(x))


#Link participation patterns

final1 <- merge(final, part2, by='ID', all.x=TRUE)

names(final1)

final1$join_week <- ifelse(final1$week0.1==1,1,NA)
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.2==1,2,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.3==1,3,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.4==1,4,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.5==1,5,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.6==1,6,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.7==1,7,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.8==1,8,NA))
final1$join_week <- ifelse(!is.na(final1$join_week),final1$join_week,ifelse(final1$week0.9==1,9,NA))

table(final1$join_week)

final1$drop_week <- ifelse(final1$week0.8==1,8,NA)
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.7==1,7,NA))
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.6==1,6,NA))
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.5==1,5,NA))
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.4==1,4,NA))
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.3==1,3,NA))
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.2==1,2,NA))
final1$drop_week <- ifelse(!is.na(final1$drop_week),final1$drop_week,ifelse(final1$week0.1==1,1,NA))

table(final1$drop_week)

############################################################################################################################################################
###########################################################################Preparation#####################################################################
############################################################################################################################################################
final1$week <- NULL
final1$text <- NULL
final1$time <- NULL

final1[] <- lapply(final1, as.character)

class(final1$join_week)
final1$join_week <- as.numeric(final1$join_week)

names(final1)

#season 02
final1$diarrhea_treatment[final1$join_week>1] <- "NASK"
final1$hand_wash[final1$join_week>2] <- "NASK"
final1$complication[final1$join_week>3] <- "NASK"
final1$complication_decide[final1$join_week>3] <- "NASK"
final1$delivery_practice[final1$join_week>4] <- "NASK"
final1$delivery_decide[final1$join_week>4] <- "NASK"
final1$feeding_practice[final1$join_week>5] <- "NASK"
final1$malaria_practice[final1$join_week>6] <- "NASK"
final1$mosquito_net_absence[final1$join_week>6] <- "NASK"
final1$fever_practice[final1$join_week>7] <- "NASK"
final1$next_season_pref[final1$join_week>8] <- "NASK"
final1$health_issue_attention[final1$join_week>8] <- "NASK"

#season 03
final1$immunisation_practice[final1$join_week>1] <- "NASK"
final1$immunisation_decide[final1$join_week>2] <- "NASK"
final1$health_advice_practice[final1$join_week>4] <- "NASK"
final1$delivery_practice_q2[final1$join_week>5] <- "NASK"
final1$cough_practice[final1$join_week>6] <- "NASK"
final1$early_initiation_practice[final1$join_week>7] <- "NASK"
final1$test_practice[final1$join_week>8] <- "NASK"

#season 02 and 03
table(final1$nomad, final1$area_type)
final1$nomad[final1$area_type=="urban"] <- "NL"

table(final1$idp)
table(final1$idp, final1$nomad)
final1$idp[final1$nomad=="yes"] <- "NL"

#season 02
final1$diarrhea_treatment[final1$parent=="no"] <- "NL"
final1$complication[final1$parent=="no"] <- "NL"
final1$complication_decide[final1$parent=="no"] <- "NL"
final1$delivery_practice[final1$parent=="no"] <- "NL"
final1$malaria_practice[final1$parent!="no"] <- "NL"
final1$feeding_practice[final1$parent!="no"] <- "NL"
final1$mosquito_net_absence[final1$malaria_practice!="no"] <- "NL"
final1$fever_practice[final1$parent!="no"] <- "NL"

#season 03
final1$immunisation_practice[final1$parent=="no"] <- "NL"
final1$delivery_practice_q2[final1$parent=="no"] <- "NL"
final1$cough_practice[final1$parent=="no"] <- "NL"
final1$early_initiation_practice[final1$parent=="no"] <- "NL"
final1$test_practice[final1$parent!="no"] <- "NL"

final1[] <- lapply(final1, as.character)
names(final1)

#season 02
as.matrix(table(final1$immunisation_practice))
table(is.na(final1$immunisation_practice))
table(is.na(final1$immunisation_practice))
table(final1$immunisation_practice,final1$immunisation_decide)

names(final1)

#season 02 (multiple answers)
final1[,24:33]<-apply(final1[,24:33], 2, function(x) {ifelse(is.na(final1$feeding_practice),NA,x)})
final1[,34:41]<-apply(final1[,34:41], 2, function(x) {ifelse(is.na(final1$diarrhea_treatment),NA,x)})
final1[,42:53]<-apply(final1[,42:53], 2, function(x) {ifelse(is.na(final1$hand_wash),NA,x)})
final1[,54:61]<-apply(final1[,54:61], 2, function(x) {ifelse(is.na(final1$complication),NA,x)})
final1[,62:70]<-apply(final1[,62:70], 2, function(x) {ifelse(is.na(final1$complication_decide),NA,x)})
final1[,71:79]<-apply(final1[,71:79], 2, function(x) {ifelse(is.na(final1$delivery_practice),NA,x)})
final1[,80:92]<-apply(final1[,80:92], 2, function(x) {ifelse(is.na(final1$delivery_decide),NA,x)})
final1[,93:102]<-apply(final1[,93:102], 2, function(x) {ifelse(is.na(final1$mosquito_net_absence),NA,x)})
final1[,103:111]<-apply(final1[,103:111], 2, function(x) {ifelse(is.na(final1$fever_practice),NA,x)})
final1[,112:124]<-apply(final1[,112:124], 2, function(x) {ifelse(is.na(final1$next_season_pref),NA,x)})
final1[,125:143]<-apply(final1[,125:143], 2, function(x) {ifelse(is.na(final1$health_issue_attention),NA,x)})

final1[,24:33]<-apply(final1[,24:33], 2, function(x) {ifelse(final1$feeding_practice=="NC","NC",x)})
final1[,34:41]<-apply(final1[,34:41], 2, function(x) {ifelse(final1$diarrhea_treatment=="NC","NC",x)})
final1[,42:53]<-apply(final1[,42:53], 2, function(x) {ifelse(final1$hand_wash=="NC","NC",x)})
final1[,54:61]<-apply(final1[,54:61], 2, function(x) {ifelse(final1$complication=="NC","NC",x)})
final1[,62:70]<-apply(final1[,62:70], 2, function(x) {ifelse(final1$complication_decide=="NC","NC",x)})
final1[,71:79]<-apply(final1[,71:79], 2, function(x) {ifelse(final1$delivery_practice=="NC","NC",x)})
final1[,80:92]<-apply(final1[,80:92], 2, function(x) {ifelse(final1$delivery_decide=="NC","NC",x)})
final1[,93:102]<-apply(final1[,93:102], 2, function(x) {ifelse(final1$mosquito_net_absence=="NC","NC",x)})
final1[,103:111]<-apply(final1[,103:111], 2, function(x) {ifelse(final1$fever_practice=="NC","NC",x)})
final1[,112:124]<-apply(final1[,112:124], 2, function(x) {ifelse(final1$next_season_pref=="NC","NC",x)})
final1[,125:143]<-apply(final1[,125:143], 2, function(x) {ifelse(final1$health_issue_attention=="NC","NC",x)})

final1[,24:33]<-apply(final1[,24:33], 2, function(x) {ifelse(final1$feeding_practice=="NASK","NASK",x)})
final1[,34:41]<-apply(final1[,34:41], 2, function(x) {ifelse(final1$diarrhea_treatment=="NASK","NASK",x)})
final1[,42:53]<-apply(final1[,42:53], 2, function(x) {ifelse(final1$hand_wash=="NASK", "NASK",x)})
final1[,54:61]<-apply(final1[,54:61], 2, function(x) {ifelse(final1$complication=="NASK","NASK",x)})
final1[,62:70]<-apply(final1[,62:70], 2, function(x) {ifelse(final1$complication_decide=="NASK","NASK",x)})
final1[,71:79]<-apply(final1[,71:79], 2, function(x) {ifelse(final1$delivery_practice=="NASK","NASK",x)})
final1[,80:96]<-apply(final1[,80:96], 2, function(x) {ifelse(final1$delivery_decide=="NASK","NASK",x)})
final1[,93:102]<-apply(final1[,93:102], 2, function(x) {ifelse(final1$mosquito_net_absence=="NASK","NASK",x)})
final1[,103:111]<-apply(final1[,103:111], 2, function(x) {ifelse(final1$fever_practice=="NASK","NASK",x)})
final1[,112:124]<-apply(final1[,112:124], 2, function(x) {ifelse(final1$next_season_pref=="NASK","NASK",x)})
final1[,125:143]<-apply(final1[,125:143], 2, function(x) {ifelse(final1$health_issue_attention=="NASK","NASK",x)})

final1[,24:33]<-apply(final1[,24:33], 2, function(x) {ifelse(final1$feeding_practice=="NL","NL",x)})
final1[,71:79]<-apply(final1[,71:79], 2, function(x) {ifelse(final1$delivery_practice=="NL","NL",x)})
final1[,80:96]<-apply(final1[,80:96], 2, function(x) {ifelse(final1$delivery_decide=="NL","NL",x)})
final1[,93:102]<-apply(final1[,93:102], 2, function(x) {ifelse(final1$mosquito_net_absence=="NL","NL",x)})
final1[,103:111]<-apply(final1[,103:111], 2, function(x) {ifelse(final1$fever_practice=="NL","NL",x)})


#season 02 (NA)
final1$diarrhea_treatment[is.na(final1$diarrhea_treatment)] <- "NA"
final1$hand_wash[is.na(final1$hand_wash)] <- "NA"
final1$complication[is.na(final1$complication)] <- "NA"
final1$complication_decide[is.na(final1$complication_decide)] <- "NA"
final1$delivery_practice[is.na(final1$delivery_practice)] <- "NA"
final1$delivery_decide[is.na(final1$delivery_decide)] <- "NA"
final1$feeding_practice[is.na(final1$feeding_practice)] <- "NA"
final1$malaria_practice[is.na(final1$malaria_practice)] <- "NA"
final1$mosquito_net_absence[is.na(final1$mosquito_net_absence)] <- "NA"
final1$fever_practice[is.na(final1$fever_practice)] <- "NA"
final1$gender[is.na(final1$gender)] <- "NA"
final1$age[is.na(final1$age)] <- "NA"
final1$district[is.na(final1$district)] <- "NA"
final1$area_type[is.na(final1$area_type)] <- "NA"
final1$idp[is.na(final1$idp)] <- "NA"
final1$nomad[is.na(final1$nomad)] <- "NA"
final1$parent[is.na(final1$parent)] <- "NA"
final1$last_station[is.na(final1$last_station)] <- "NA"

#season 03 (NA)
final1$immunisation_practice[is.na(final1$immunisation_practice)] <- "NA"
final1$immunisation_decide[is.na(final1$immunisation_decide)] <- "NA"
final1$health_advice_practice[is.na(final1$health_advice_practice)] <- "NA"
final1$delivery_practice_q2[is.na(final1$delivery_practice_q2)] <- "NA"
final1$cough_practice[is.na(final1$cough_practice)] <- "NA"
final1$early_initiation_practice[is.na(final1$early_initiation_practice)] <- "NA"
final1$test_practice[is.na(final1$test_practice)] <- "NA"
final1$gender[is.na(final1$gender)] <- "NA"
final1$age[is.na(final1$age)] <- "NA"
final1$district[is.na(final1$district)] <- "NA"
final1$area_type[is.na(final1$area_type)] <- "NA"
final1$idp[is.na(final1$idp)] <- "NA"
final1$nomad[is.na(final1$nomad)] <- "NA"
final1$parent[is.na(final1$parent)] <- "NA"
final1$last_station[is.na(final1$last_station)] <- "NA"

names(final1)

write.csv(final1, "ForAlexandClaudiaS03.csv")

################################################################################################################################
####################################################################Response rates##############################################
################################################################################################################################

table.it <- function(x) {
  as.matrix(prop.table(table(x)))*100
}

table.it(final1$last_station)

#S02
#Response rate within radio show
final1$diarrhea_treatment[final1$diarrhea_treatment!='NA'&final1$diarrhea_treatment!='NC'&final1$diarrhea_treatment!='NL'&final1$diarrhea_treatment!='NASK'] <- "coded"
final1$hand_wash[final1$hand_wash!='NA'&final1$hand_wash!='NC'&final1$hand_wash!='NL'&final1$hand_wash!='NASK'] <- "coded"
final1$complication[final1$complication!='NA'&final1$complication!='NC'&final1$complication!='NL'&final1$complication!='NASK'] <- "coded"
final1$complication_decide[final1$complication_decide!='NA'&final1$complication_decide!='NC'&final1$complication_decide!='NL'&final1$complication_decide!='NASK'] <- "coded"
final1$delivery_practice[final1$delivery_practice!='NA'&final1$delivery_practice!='NC'&final1$delivery_practice!='NL'&final1$delivery_practice!='NASK'] <- "coded"
final1$delivery_decide[final1$delivery_decide!='NA'&final1$delivery_decide!='NC'&final1$delivery_decide!='NL'&final1$delivery_decide!='NASK'] <- "coded"
final1$feeding_practice[final1$feeding_practice!='NA'&final1$feeding_practice!='NC'&final1$feeding_practice!='NL'&final1$feeding_practice!='NASK'] <- "coded"
final1$malaria_practice[final1$malaria_practice!='NA'&final1$malaria_practice!='NC'&final1$malaria_practice!='NL'&final1$malaria_practice!='NASK'] <- "coded"
final1$mosquito_net_absence[final1$mosquito_net_absence!='NA'&final1$mosquito_net_absence!='NC'&final1$mosquito_net_absence!='NL'&final1$mosquito_net_absence!='NASK'] <- "coded"
final1$fever_practice[final1$fever_practice!='NA'&final1$fever_practice!='NC'&final1$fever_practice!='NL'&final1$fever_practice!='NASK'] <- "coded"

prop.table(table(final1$diarrhea_treatment))*100
prop.table(table(final1$hand_wash))*100
prop.table(table(final1$complication))*100
prop.table(table(final1$complication_decide))*100
prop.table(table(final1$delivery_practice))*100
prop.table(table(final1$delivery_decide))*100
prop.table(table(final1$feeding_practice))*100
prop.table(table(final1$malaria_practice))*100
prop.table(table(final1$mosquito_net_absence))*100
prop.table(table(final1$fever_practice))*100

prop.table(table(final1$week0.1, final1$diarrhea_treatment),2)*100
prop.table(table(final1$week0.2, final1$hand_wash),2)*100
prop.table(table(final1$week0.3, final1$complication),2)*100
prop.table(table(final1$week0.3, final1$complication_decide),2)*100
prop.table(table(final1$week0.4, final1$delivery_practice),2)*100
prop.table(table(final1$week0.4, final1$delivery_decide),2)*100
prop.table(table(final1$week0.5, final1$feeding_practice),2)*100
prop.table(table(final1$week0.6, final1$malaria_practice),2)*100
prop.table(table(final1$week0.6, final1$mosquito_net_absence),2)*100
prop.table(table(final1$week0.7, final1$fever_practice),2)*100

#S03
final1$immunisation_practice[final1$immunisation_practice!='NA'&final1$immunisation_practice!='NC'&final1$immunisation_practice!='NL'&final1$immunisation_practice!='NASK'] <- "coded"
final1$immunisation_decide[final1$immunisation_decide!='NA'&final1$immunisation_decide!='NC'&final1$immunisation_decide!='NL'&final1$immunisation_decide!='NASK'] <- "coded"
final1$health_advice_practice[final1$health_advice_practice!='NA'&final1$health_advice_practice!='NC'&final1$health_advice_practice!='NL'&final1$health_advice_practice!='NASK'] <- "coded"
final1$delivery_practice_q2[final1$delivery_practice_q2!='NA'&final1$delivery_practice_q2!='NC'&final1$delivery_practice_q2!='NL'&final1$delivery_practice_q2!='NASK'] <- "coded"
final1$cough_practice[final1$cough_practice!='NA'&final1$cough_practice!='NC'&final1$cough_practice!='NL'&final1$cough_practice!='NASK'] <- "coded"
final1$early_initiation_practice[final1$early_initiation_practice!='NA'&final1$early_initiation_practice!='NC'&final1$early_initiation_practice!='NL'&final1$early_initiation_practice!='NASK'] <- "coded"
final1$test_practice[final1$test_practice!='NA'&final1$test_practice!='NC'&final1$test_practice!='NL'&final1$test_practice!='NASK'] <- "coded"

prop.table(table(final1$immunisation_practice))*100
prop.table(table(final1$immunisation_decide))*100
prop.table(table(final1$health_advice_practice))*100
prop.table(table(final1$delivery_practice_q2))*100
prop.table(table(final1$cough_practice))*100
prop.table(table(final1$early_initiation_practice))*100
prop.table(table(final1$test_practice))*100

prop.table(table(final1$week0.1, final1$immunisation_practice),2)*100
prop.table(table(final1$week0.2, final1$immunisation_decide),2)*100
prop.table(table(final1$week0.4, final1$health_advice_practice),2)*100
prop.table(table(final1$week0.5, final1$delivery_practice_q2),2)*100
prop.table(table(final1$week0.6, final1$cough_practice),2)*100
prop.table(table(final1$week0.7, final1$early_initiation_practice),2)*100
prop.table(table(final1$week0.8, final1$test_practice),2)*100

#S03
#Response rate relevant week
table.it((subset(final1, final1$week0.1==1))$immunisation_practice)
table.it((subset(final1, final1$week0.2==1))$immunisation_decide)
table.it((subset(final1, final1$week0.4==1))$health_advice_practice)
table.it((subset(final1, final1$week0.5==1))$cough_practice)
table.it((subset(final1, final1$week0.6==1))$feeding_practice)
table.it((subset(final1, final1$week0.7==1))$early_initiation_practice)
table.it((subset(final1, final1$week0.8==1))$test_practice)

#S02
#Response rate relevant theme
table.it((subset(final1, final1$week0.1==1|final1$week0.2==1))$immunisation_practice)
table.it((subset(final1, final1$week0.1==1|final1$week0.2==1))$immunisation_decide)
table.it((subset(final1, final1$week0.4==1))$health_advice_practice)
table.it((subset(final1, final1$week0.5==1|final1$week0.6==1||final1$week0.7==1))$cough_practice)
table.it((subset(final1, final1$week0.5==1|final1$week0.6==1||final1$week0.7==1))$feeding_practice)
table.it((subset(final1, final1$week0.5==1|final1$week0.6==1||final1$week0.7==1))$early_initiation_practice)
table.it((subset(final1, final1$week0.8==1))$test_practice)

#S03
#Response rate relevant theme
table.it((subset(final1, final1$week0.1==1|final1$week0.2==1))$immunisation_practice)
table.it((subset(final1, final1$week0.1==1|final1$week0.2==1))$immunisation_decide)
table.it((subset(final1, final1$week0.4==1))$health_advice_practice)
table.it((subset(final1, final1$week0.5==1|final1$week0.6==1||final1$week0.7==1))$cough_practice)
table.it((subset(final1, final1$week0.5==1|final1$week0.6==1||final1$week0.7==1))$feeding_practice)
table.it((subset(final1, final1$week0.5==1|final1$week0.6==1||final1$week0.7==1))$early_initiation_practice)
table.it((subset(final1, final1$week0.8==1))$test_practice)

names(final1)

#Repeated participation
final1[,18:25] <- lapply(final1[,18:25], as.numeric)
final1$count <- rowSums(final1[,18:25])

table <- as.data.frame.matrix(table.it(final1$count))

final1 <- read.csv("ForAlexandClaudia.csv")

final1[] <- lapply(final1, as.character)

names(final1)

table(final1$hand_wash, final1$hand_wash_after.waking.up)

#Missings
final1[]<-apply(final1[], 2, function(x) {ifelse(is.na(x)|x=="NC","Missing",x)})
final1[]<-apply(final1[], 2, function(x) {ifelse(x=="NL"|x=="NASK",NA,x)})
