# Libraries
library(foreign)

rm(list = ls())

setwd('C:/Users/jsanc/Downloads/UGA_2009_UNPS_v01_M_STATA')

# TBE weights
# write a read me and a file describing the results


# WEALTH ############################################################################
# housing and durable good wealth
data1 <- read.dta('Uganda_2009/GSEC14.dta')
data1[data1$h14q2=="Land",5] <- NA # exclude land here to get the values from the 
# agricultural survey
wealth <- aggregate(data1$h14q5, by=list(Category=data1$HHID), FUN=sum, na.rm=TRUE)
colnames(wealth) <- c('HHID', 'h14q5')

# land wealth
data2 <- read.dta('Uganda_2009/AGSEC2A.dta')
data2 <- data2[, c(1, 9)]
data2 <- aggregate(data2$a2aq10, by=list(Category=data2$HHID), FUN=sum, na.rm=TRUE)
colnames(data2) <- c('HHID', 'a2aq10')
wealth <- merge(wealth, data2, by='HHID', all = TRUE)

# fishery equipment
data3 <- read.dta('Uganda_2009/AGSEC9C.dta')
data3 <- data.frame(data3$HHID, data3$a9q10c*data3$a9q10d)
colnames(data3) <- c("HHID", "fish_val")
data3 <- aggregate(data3$fish_val, by=list(Category=data3$HHID), FUN=sum, na.rm=TRUE)
colnames(data3) <- c("HHID", "fish_val")
wealth <- merge(wealth, data3, by='HHID', all = TRUE)

# cattle
data4 <- read.dta('Uganda_2009/AGSEC6A.dta')
data4 <- data.frame(data4$HHID, data4$a6aq5*data4$a6aq6)
colnames(data4) <- c("HHID", "cattle_val")
data4 <- aggregate(data4$cattle_val, by=list(Category=data4$HHID), FUN=sum, na.rm=TRUE)
colnames(data4) <- c("HHID", "cattle_val")
wealth <- merge(wealth, data4, by='HHID', all = TRUE)

# small animals
data5 <- read.dta('Uganda_2009/AGSEC6B.dta')
data5 <- data.frame(data5$HHID, data5$a6bq5*data5$a6bq6)
colnames(data5) <- c("HHID", "small_val")
data5 <- aggregate(data5$small_val, by=list(Category=data5$HHID), FUN=sum, na.rm=TRUE)
colnames(data5) <- c("HHID", "small_val")
wealth <- merge(wealth, data5, by='HHID', all = TRUE)

# poultry
data6 <- read.dta('Uganda_2009/AGSEC6C.dta')
data6 <- data.frame(data6$HHID, data6$a6cq5*data6$a6cq6)
colnames(data6) <- c("HHID", "poultry_val")
data6 <- aggregate(data6$poultry_val, by=list(Category=data6$HHID), FUN=sum, na.rm=TRUE)
colnames(data6) <- c("HHID", "poultry_val")
wealth <- merge(wealth, data6, by='HHID', all = TRUE)

# urban rural variable
data7 <- read.dta('Uganda_2009/GSEC1.dta')
data7 <- data7[,c(1,3)]
colnames(data7) <- c("HHID", "urban")
wealth <- merge(wealth, data7, by='HHID', all = TRUE)

# clean data set
wealth <- wealth[nchar(wealth[,1])>9,]

# add wealth
wealth$wealth_t <- apply(wealth[,2:7], 1, sum, na.rm=TRUE)
wealth$wealth_t[wealth$wealth_t==0] <- NA
wealth$wealth_t <- log(wealth$wealth_t)

# trimming
boxplot(wealth$wealth_t)
hist(wealth$wealth_t)
hist(wealth$wealth_t[wealth$urban=="Rural"])
hist(wealth$wealth_t[wealth$urban=="Urban"])

#qh <- quantile(wealth$wealth_t, 0.99, na.rm=TRUE)
#ql <- quantile(wealth$wealth_t, 0.01, na.rm=TRUE)
#wealth$wealth_t[wealth$wealth_t>qh] <- NA
#wealth$wealth_t[wealth$wealth_t<ql] <- NA

qh <- quantile(wealth$wealth_t[wealth$urban=="Rural"], 0.995, na.rm=TRUE)
ql <- quantile(wealth$wealth_t[wealth$urban=="Rural"], 0.005, na.rm=TRUE)
wealth$wealth_t[(wealth$wealth_t>qh)&(wealth$urban=="Rural")] <- NA
wealth$wealth_t[(wealth$wealth_t<ql)&(wealth$urban=="Rural")] <- NA

boxplot(wealth$wealth_t)
hist(wealth$wealth_t)
var(wealth$wealth_t, na.rm=TRUE)

hist(wealth$wealth_t[wealth$urban=="Urban"])
var(wealth$wealth_t[wealth$urban=="Urban"], na.rm=TRUE)

hist(wealth$wealth_t[wealth$urban=="Rural"])
var(wealth$wealth_t[wealth$urban=="Rural"], na.rm=TRUE)


# CONSUMPTION #######################################################################

#data11 <- read.dta('Uganda_2009/GSEC1.dta')
#data11 <- data.frame(data11$HHID, data11$h1bq2b)
#colnames(data11) <- c("HHID", "month")

# food
data8 <- read.dta('Uganda_2009/GSEC15b.dta')
data8 <- data8[,c(1,6,8,10,12)]
colnames(data8)[1] <- "HHID"

data8 <- data.frame(data8[,1],apply(data8[,2:5], 1, sum, na.rm=TRUE))
colnames(data8) <- c("HHID", "fd")
#data8 <- merge(data11, data8, by="HHID", all = TRUE)

data8 <- aggregate(data8$fd, by=list(Category=data8$HHID), FUN=sum, na.rm=TRUE)
colnames(data8) <- c("HHID", "fd")
data8$fd <- data8$fd*52 # convert weekly consumption to annual
consumption <- data8

# non-food non-durables
data9 <- read.dta('Uganda_2009/GSEC15c.dta')
data9 <- data9[,c(1,5,7,9)]
data9 <- data.frame(data9[,1],apply(data9[,2:4], 1, sum, na.rm=TRUE))
colnames(data9) <- c("HHID", "nfnd")
data9 <- aggregate(data9$nfnd, by=list(Category=data9$HHID), FUN=sum, na.rm=TRUE)
colnames(data9) <- c("HHID", "nfnd")
data9$nfnd <- data9$nfnd*12
consumption <- merge(consumption, data9, by="HHID", all = TRUE)

# semi-durables
data10 <- read.dta('Uganda_2009/GSEC15d.dta',convert.factors = F)
data10$h15dq2 <- floor(data10$h15dq2/100)
data10$durable <- 0 #We eliminate durable
data10$durable[(data10$h15dq2>2)&(data10$h15dq2<5)] <- 1
data10 <- data10[data10$durable==1,c(1,3,4,5)]
data10 <- data.frame(data10[,1],apply(data10[,2:4], 1, sum, na.rm=TRUE))
colnames(data10) <- c("HHID", "sd")
data10 <- aggregate(data10$sd, by=list(Category=data10$HHID), FUN=sum, na.rm=TRUE)
colnames(data10) <- c("HHID", "sd")
consumption <- merge(consumption, data10, by="HHID", all = TRUE)

# sum up consumption
consumption$cons_t <- apply(consumption[,2:4], 1, sum, na.rm=TRUE)
consumption$cons_t[consumption$cons_t==0] <- NA
consumption$cons_t <- log(consumption$cons_t)

# urban rural variable
consumption <- merge(data7, consumption, by='HHID', all = TRUE)

# trimming (not necesary)
boxplot(consumption$cons_t)
hist(consumption$cons_t)
#qh <- quantile(consumption$cons_t, 0.99, na.rm=TRUE)
#ql <- quantile(consumption$cons_t, 0.01, na.rm=TRUE)
#consumption$cons_t[consumption$cons_t>qh] <- NA
#consumption$cons_t[consumption$cons_t<ql] <- NA
boxplot(consumption$cons_t)
hist(consumption$cons_t)
var(consumption$cons_t, na.rm=TRUE)

var(consumption$cons_t[consumption$urban=="Urban"], na.rm=TRUE)
var(consumption$cons_t[consumption$urban=="Rural"], na.rm=TRUE)

hist(consumption$cons_t[consumption$urban=="Urban"])
hist(consumption$cons_t[consumption$urban=="Rural"])

# INCOME ###########################################################################
# labour income - ok
data12 <- read.dta('Uganda_2009/GSEC8.dta')

# main
data12$wph <- NA
data12$h8q31c <- as.character(data12$h8q31c)
data12$h8q31c[is.na(data12$h8q31c)] <- "none"
for(i in 1:nrow(data12)){
  if (data12$h8q31c[i]=="Hour") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])}
  if (data12$h8q31c[i]=="Day") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])/8}
  if (data12$h8q31c[i]=="Week") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])/(5*8)} # They work on saturdays and, on avg, half on sundays
  if (data12$h8q31c[i]=="Month") {
    data12$wph[i] <- (data12$h8q31a[i]+data12$h8q31b[i])/(5*8*4.5)}
}

data12$hpw <- data12$h8q36a+data12$h8q36b+data12$h8q36c+data12$h8q36d+
    data12$h8q36e+data12$h8q36f+data12$h8q36g
data12$hpw <- data12$h8q70
data12$main <- data12$wph*data12$h8q70*data12$h8q69*data12$h8q52
data12$main_horas <- data12$h8q70*data12$h8q69*data12$h8q30

# secondary
data12$wph <- NA
data12$h8q45c <- as.character(data12$h8q45c)
data12$h8q45c[is.na(data12$h8q45c)] <- "none"
for(i in 1:nrow(data12)){
  if (data12$h8q45c[i]=="1") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])}
  if (data12$h8q45c[i]=="2") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])/8}
  if (data12$h8q45c[i]=="3") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])/(5*8)}
  if (data12$h8q45c[i]=="4") {
    data12$wph[i] <- (data12$h8q45a[i]+data12$h8q45b[i])/(5*8*4.5)}
}

data12$hpw <- data12$h8q73
#data12$secondary <- data12$wph*data12$hpw*4.5*data12$h8q44
data12$secondary <- data12$wph*data12$h8q73*data12$h8q72*data12$h8q44
data12$secondary_horas <- data12$h8q73*data12$h8q72*data12$h8q44

data12$h8q04 <- abs(as.numeric(data12$h8q04)-2)

lab_supply <- data.frame(data12$HHID,data12$PID,data12$main_horas, data12$secondary_horas,data12$h8q04)
colnames(lab_supply) <- c("HHID", "PID","hours1","hours2","employed")

data12 <- data.frame(data12$HHID, data12$main, data12$secondary,data12$main_horas, data12$secondary_horas,data12$h8q04)
colnames(data12) <- c("HHID", "main", "secondary","hours1","hours2","emp")
labour_income <- data.frame(data12[,1], apply(data12[,2:3], 1, sum, na.rm=TRUE),apply(data12[,4:5], 1, sum, na.rm=TRUE),data12$emp)
colnames(labour_income) <- c("HHID", "labour","hours","emp")
labour_inc1 <- aggregate(labour_income$labour, by=list(Category=labour_income$HHID), FUN=sum, na.rm=TRUE)
labour_inc2 <- aggregate(labour_income$hours, by=list(Category=labour_income$HHID), FUN=sum, na.rm=TRUE)
labour_inc3 <- aggregate(labour_income$emp, by=list(Category=labour_income$HHID), FUN=mean, na.rm=TRUE)
labour_income <- data.frame(labour_inc1,labour_inc2[,2],labour_inc3[,2])
colnames(labour_income) <- c("HHID", "labour","hours","emp")

# income from crop farming - ok

# first visit
data15 <- read.dta('Uganda_2009/AGSEC5A.dta')

average_conversion <- aggregate(data15$a5aq6d, by=list(data15$a5aq6c), FUN=mean, na.rm=TRUE)
data15$a5aq7d <- NA

for(i in 1:nrow(data15)){
  if (!is.na(data15$a5aq6c[i])&!is.na(data15$a5aq7c[i])){
    if (data15$a5aq6c[i]==data15$a5aq7c[i]){
      data15$a5aq7d[i] <- data15$a5aq6d[i]
    }else{
      if (length(average_conversion[data15$a5aq7c[i]==average_conversion[,1],2])!=0){
        data15$a5aq7d[i] <- average_conversion[data15$a5aq7c[i]==average_conversion[,1],2]
      }else{
        data15$a5aq7d[i] <- NA
      }
    }
  }else{
    data15$a5aq7d[i] <- NA
  }
}
data15$ppk<- data15$a5aq8/(data15$a5aq7a*data15$a5aq7d) # changed to 7d

data16 <- read.dta('Uganda_2009/GSEC1.dta')
data16 <- data.frame(data16$HHID, data16$region)
colnames(data16) <- c("HHID", "region")
data15 <- merge(data16, data15, by="HHID", all = TRUE)

regions <- unique(data15$region)
crops <- unique(data15$a5aq4)

crops <- crops[-c(1,2)] 

data15$pag <- NA

for(i in 1:length(crops)){ # compute median price per crop and region
  for(j in 1:length(regions)){
    data15$pag[data15$region==regions[[j]]&data15$a5aq4==crops[[i]]] <- median(data15$ppk[data15$region==regions[[j]]&data15$a5aq4==crops[[i]]], na.rm=TRUE)
  }
}

data15$ag_income <- rowSums(data.frame(data15$a5aq8, 
                                       (data15$a5aq6a*data15$a5aq6d-data15$a5aq7a*data15$a5aq7d)*data15$pag,
                                       -data15$a5aq10), na.rm=TRUE) # value of products sold+value of products eaten-transport costs

crop_incomefv <- aggregate(data15$ag_income, by=list(Category=data15$HHID), FUN=sum, na.rm=TRUE)

# second visit
data15 <- read.dta('Uganda_2009/AGSEC5B.dta')

average_conversion <- aggregate(data15$a5bq6d, by=list(data15$a5bq6c), FUN=mean, na.rm=TRUE)
data15$a5bq7d <- NA

for(i in 1:nrow(data15)){
  if (!is.na(data15$a5bq6c[i])&!is.na(data15$a5bq7c[i])){
    if (data15$a5bq6c[i]==data15$a5bq7c[i]){
      data15$a5bq7d[i] <- data15$a5bq6d[i]
    }else{
      if (length(average_conversion[data15$a5bq7c[i]==average_conversion[,1],2])!=0){
        data15$a5bq7d[i] <- average_conversion[data15$a5bq7c[i]==average_conversion[,1],2]
      }else{
        data15$a5bq7d[i] <- NA
      }
    }
  }else{
    data15$a5bq7d[i] <- NA
  }
}
data15$ppk<- data15$a5bq8/(data15$a5bq7a*data15$a5bq7d) # changed to 7d

data16 <- read.dta('Uganda_2009/GSEC1.dta')
data16 <- data.frame(data16$HHID, data16$region)
colnames(data16) <- c("HHID", "region")
data15 <- merge(data16, data15, by="HHID", all = TRUE)

regions <- unique(data15$region)
crops <- unique(data15$a5bq4)

crops <- crops[-c(1,2)] 

data15$pag <- NA

for(i in 1:length(crops)){ # compute median price per crop and region
  for(j in 1:length(regions)){
    data15$pag[data15$region==regions[[j]]&data15$a5bq4==crops[[i]]] <- median(data15$ppk[data15$region==regions[[j]]&data15$a5bq4==crops[[i]]], na.rm=TRUE)
  }
}

data15$ag_income <- rowSums(data.frame(data15$a5bq8, 
                                       (data15$a5bq6a*data15$a5bq6d-data15$a5bq7a*data15$a5bq7d)*data15$pag,
                                       -data15$a5bq10), na.rm=TRUE) # value of products sold+value of products eaten-transport costs

crop_incomesv <- aggregate(data15$ag_income, by=list(Category=data15$HHID), FUN=sum, na.rm=TRUE)

crop_income_minus_transport <- merge(crop_incomefv, crop_incomesv, by="Category", all = TRUE) # revenues+home consumption-transport in 
colnames(crop_income_minus_transport) <- c("HHID", "crop sales and home consumption minus transport in first season",
                                           "crop sales and home consumption minus transport in second season")

# first and second visit

data21 <- read.dta('Uganda_2009/AGSEC3A.dta')
data21$cost_3a <- rowSums(data.frame(data21$a3aq8, # organic fertilizer, first visit
                                     data21$a3aq19, # chemical fertilizer, first visit
                                     data21$a3aq31, # pesticides, first visit
                                     data21$a3aq43), na.rm=TRUE) # hired labour paid, first visit

cost_a <- aggregate(data21$cost_3a, by=list(Category=data21$HHID), FUN=sum, na.rm=TRUE)
colnames(cost_a) <- c("HHID", "cost of fertilizer, pesticides and labour in first season")

data22 <- read.dta('Uganda_2009/AGSEC3B.dta')
data22$cost_3b <- rowSums(data.frame(data22$a3bq8, # organic fertilizer, second visit
                                     data22$a3bq19, # chemical fertilizer, second visit
                                     data22$a3bq31, # pesticides, second visit
                                     data22$a3bq43), na.rm=TRUE) # hired labour paid, second visit

cost_b <- aggregate(data22$cost_3b, by=list(Category=data22$HHID), FUN=sum, na.rm=TRUE)
colnames(cost_b) <- c("HHID", "cost of fertilizer, pesticides and labour in second season")

data25 <- read.dta('Uganda_2009/AGSEC4A.dta')
seeds_a <- aggregate(data25$a4aq11, by=list(Category=data25$HHID), FUN=sum, na.rm=TRUE)
colnames(seeds_a) <- c("HHID", "seed cost in first season")
# seeds, first visit
data27 <- read.dta('Uganda_2009/AGSEC4B.dta')
seeds_b <- aggregate(data27$a4bq11, by=list(Category=data27$HHID), FUN=sum, na.rm=TRUE)
colnames(seeds_b) <- c("HHID", "seed cost in second season")
# seeds, second visit

data29 <- read.dta('Uganda_2009/AGSEC2B.dta')
land <- aggregate(data29$a2bq9, by=list(Category=data29$HHID), FUN=sum, na.rm=TRUE)
colnames(land) <- c("HHID", "cost for land")
# rent for land to be paid per parcel and hh

crop_income <- merge(crop_income_minus_transport,cost_a, by="HHID", all=TRUE)
crop_income <- merge(crop_income,cost_b, by="HHID", all=TRUE)
crop_income <- merge(crop_income,seeds_a, by="HHID", all=TRUE)
crop_income <- merge(crop_income,seeds_b, by="HHID", all=TRUE)
crop_income <- merge(crop_income,land, by="HHID", all=TRUE)

crop_income[,9] <- apply(crop_income[,2:3], 1, sum, na.rm=TRUE)-apply(crop_income[,4:8], 1, sum, na.rm=TRUE)
colnames(crop_income)[9] <- c("total income from crop farming")

# livestock sales ok 
big_animals <- read.dta('Uganda_2009/AGSEC6A.dta')
big_animals$income <- rowSums(data.frame(big_animals$a6aq15,-big_animals$a6aq13), na.rm=TRUE)
big_animals <- aggregate(big_animals$income, by=list(Category=big_animals$HHID), FUN=sum, na.rm=TRUE)
colnames(big_animals) <- c("HHID", "net sales of big animals")

small_animals <- read.dta('Uganda_2009/AGSEC6B.dta')
small_animals$income <- rowSums(data.frame(small_animals$a6bq15,-small_animals$a6bq13), na.rm=TRUE)
small_animals <- aggregate(small_animals$income, by=list(Category=small_animals$HHID), FUN=sum, na.rm=TRUE)
colnames(small_animals) <- c("HHID", "net sales of small animals")

poultry <- read.dta('Uganda_2009/AGSEC6C.dta')
poultry$income <- rowSums(data.frame(poultry$a6cq15,-poultry$a6cq13), na.rm=TRUE)
poultry <- aggregate(poultry$income, by=list(Category=poultry$HHID), FUN=sum, na.rm=TRUE)
colnames(poultry) <- c("HHID", "net sales of poultry")

animals_cost <- read.dta('Uganda_2009/AGSEC7.dta')
animals_cost <- aggregate(animals_cost$a7q4, by=list(Category=animals_cost$HHID), FUN=sum, na.rm=TRUE)
colnames(animals_cost) <- c("HHID", "cost of holding animals")

livestock_sales <- merge(big_animals, small_animals, by="HHID", all=TRUE)
livestock_sales <- merge(livestock_sales, poultry, by="HHID", all=TRUE)
livestock_sales <- merge(livestock_sales, animals_cost, by="HHID", all=TRUE)
livestock_sales[,6] <- apply(data.frame(apply(livestock_sales[,2:4], 1, sum, na.rm=TRUE),
                                        -livestock_sales[,5]), 1, sum, na.rm=TRUE)
colnames(livestock_sales)[6] <- c("net income from livestock sales")

# livestock products ok 
animal_products <- read.dta('Uganda_2009/AGSEC8.dta')
animal_products$price <- animal_products$a8q7/animal_products$a8q6
animal_products$price[animal_products$price==Inf] <- NA
animal_products$price[animal_products$price==0] <- NA

animal_products$income <- rowSums(data.frame(animal_products$a8q7, animal_products$a8q8*animal_products$price), na.rm=TRUE)*
  animal_products$a8q3
livestock_products <- aggregate(animal_products$income, by=list(Category=animal_products$HHID),
                                FUN=sum, na.rm=TRUE)
colnames(livestock_products) <- c("HHID", "income from livestock products")

# income from fishery ok
fishery_A <- read.dta('Uganda_2009/AGSEC9A.dta')
fishery_B <- read.dta('Uganda_2009/AGSEC9B.dta')
fishery_B <- reshape(fishery_B, idvar = "HHID", timevar = "a9q6purp", direction = "wide")
fishery_C <- read.dta('Uganda_2009/AGSEC9C.dta')
fishery_D <- read.dta('Uganda_2009/AGSEC9D.dta')
fishery_D <- fishery_D[,c(1,3,4)]
fishery_D <- reshape(fishery_D, idvar = "HHID", timevar = "a9q11a", direction = "wide")

fishery <- merge(fishery_A, fishery_B, by="HHID", all=TRUE)
fishery <- merge(fishery, fishery_D, by="HHID", all=TRUE)

fishery$a9q9[fishery$a9q9==0] <- NA
fishery$a9q8[fishery$a9q8==0] <- NA
fishery$price <- median(fishery$a9q9/(fishery$a9q5*fishery$a9q6prop.Sold/100), na.rm =  TRUE)
fishery$sold <- fishery$a9q3*fishery$a9q4*fishery$a9q9
fishery$eaten <- fishery$a9q3*fishery$a9q4*fishery$a9q5*fishery$a9q6prop.Eaten*fishery$price
fishery$total <- rowSums(data.frame(fishery$sold, fishery$eaten), na.rm=TRUE)
fishery_income <- data.frame(fishery$HHID, 
                             rowSums(data.frame(fishery$total, -fishery[,12:19]), na.rm=TRUE))
colnames(fishery_income) <- c("HHID", "fishery income")
# income from business activity - ok
business <- read.dta('Uganda_2009/GSEC12.dta')
business$income <- rowSums(data.frame(business$h12q13,- business$h12q15, - business$h12q16,
                   - business$h12q17), na.rm=TRUE)
business_income <- aggregate(business$income, by=list(Category=business$HHID), FUN=sum, na.rm=TRUE)
colnames(business_income) <- c("HHID", "business income")

# non-labour non-agricultural income - ok
data14 <- read.dta('Uganda_2009/GSEC11.dta')
data14$h11aq05[data14$h11aq03=="Crop farming Enterprises"] <- NA
data14$h11aq06[data14$h11aq03=="Crop farming Enterprises"] <- NA
data14$h11aq05[data14$h11aq03=="Other Agricultural Enterprises"] <- NA
data14$h11aq06[data14$h11aq03=="Other Agricultural Enterprises"] <- NA
data14$h11aq05[data14$h11aq03=="Non-agricultural Enterprises"] <- NA
data14$h11aq06[data14$h11aq03=="Non-agricultural Enterprises"] <- NA
data14$nli <- rowSums(data.frame(data14$h11aq05,data14$h11aq06), na.rm=TRUE)

data14 <- data.frame(data14$HHID, data14$nli)
colnames(data14) <- c("HHID", "nli")
non_labour <- aggregate(data14$nli, by=list(Category=data14$HHID), FUN=sum, na.rm=TRUE)
colnames(non_labour) <- c("HHID", "nli")

# merge all types of income
income <- merge(labour_income[,1:2], crop_income[,c(1,9)], by="HHID", all = TRUE)
income <- merge(income, livestock_sales[,c(1,6)], by="HHID", all = TRUE)
income <- merge(income, livestock_products, by="HHID", all = TRUE)
income <- merge(income, fishery_income, by="HHID", all = TRUE)
income <- merge(income, business_income, by="HHID", all = TRUE)
income <- merge(income, non_labour, by="HHID", all = TRUE)

income$inc_t <- apply(income[,2:8], 1, sum, na.rm=TRUE)
income$inc_t[income$inc_t==0] <- NA
income$inc_t[income$inc_t<0] <- NA
income$inc_t <- log(income$inc_t)

# urban rural variable
income <- merge(data7, income, by='HHID', all = TRUE)

# trimming (only in rural there are outliers)
boxplot(income$inc_t)
hist(income$inc_t)
hist(income$inc_t[income$urban=="Urban"])
hist(income$inc_t[income$urban=="Rural"])

qh <- quantile(income$inc_t[income$urban=="Rural"], 0.995, na.rm=TRUE)
ql <- quantile(income$inc_t[income$urban=="Rural"], 0.005, na.rm=TRUE)
income$inc_t[(income$inc_t>qh)&(income$urban=="Rural")] <- NA
income$inc_t[(income$inc_t<ql)&(income$urban=="Rural")] <- NA

qh <- quantile(income$inc_t[income$urban=="Urban"], 0.995, na.rm=TRUE)
income$inc_t[(income$inc_t>qh)&(income$urban=="Urban")] <- NA


boxplot(income$inc_t)
hist(income$inc_t)
var(income$inc_t, na.rm=TRUE)

var(income$inc_t[income$urban=="Urban"], na.rm=TRUE)
var(income$inc_t[income$urban=="Rural"], na.rm=TRUE)

# Report average CIW per household separately for rural and urban areas. ###########


gral <- read.dta('Uganda_2009/GSEC1.dta')
gral <- gral[,1:8]

personas <- read.dta('Uganda_2009/GSEC2.dta')
educ <- read.dta('Uganda_2009/GSEC4.dta')
trabajo <- merge(personas[,c(2,4,9)], lab_supply, by='PID', all = TRUE)
trabajo <- merge(trabajo,educ[,c(2,9)])
trabajo <- merge(data7, trabajo, by='HHID', all = TRUE)

cabeza_temp <- personas[personas$h2q4=="Head",]
cabeza <- data.frame(cabeza_temp[,1], cabeza_temp$h2q8)
colnames(cabeza) <- c("HHID", "age")

basico <- merge(gral, cabeza, by='HHID', all = TRUE)

TODO_inc <- merge(basico, income, by='HHID', all = TRUE)
TODO_wealth <- merge(basico, wealth, by='HHID', all = TRUE)
TODO_cons <- merge(basico, consumption, by='HHID', all = TRUE)
TODO_lab <- merge(basico,labour_income[,c(1,3,4)], by='HHID', all = TRUE)

write.dta(TODO_cons, "cons.dta")
write.dta(TODO_wealth, "wealth.dta")
write.dta(TODO_inc, "inc.dta")
write.dta(trabajo, "lab.dta")
write.dta(TODO_lab, "trab.dta")


