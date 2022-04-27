#Data from SDR 2020 from OECD countries
setwd("C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data")

library(readxl)
data_SDR<-read.csv("SDR2020Database_test.csv", header=TRUE, na.strings=c("..","NA","","?")) #Everything coming in as factor vars
#Data ranges from 2000 to 2020

par.defaults <- par(no.readonly=TRUE)
save(par.defaults, file="R.default.par.RData")

library(Amelia)
missmap(data_SDR)
#summary(data_SDR)
data<-data_SDR

#eliminate countries identified by SDR as having more than 15% missing data
Countries_miss<-grep("^(Comoros|Brunei Darussalam|Bahrain|Qatar|Vanuatu|Somalia|Barbados|Fiji|Papua New Guinea|South Sudan)",data$Country)
data<-data[-Countries_miss,]

#pick out OECD Countries
OECD_countries<-grep("^(United States|Mexico|Japan|Germany|Turkey|France|United Kingdom|Italy|Korea, Rep.|Spain|
|Poland|Canada|Australia|Chile|Netherlands|Belgium|Greece|Czech Republic|Portugal|Sweden|Hungary|
|Austria|Israel|Switzerland|Denmark|Finland|Slovak|Norway|Ireland|New Zealand|Lithuania|Slovenia|
|Latvia|Estonia|Luxembourg|Iceland)",data$Country)
unique(data$Country[OECD_countries])

data<-data[OECD_countries,]

#eliminate variables with many missing values
vars<- is.na(data)
sum(vars[1,])
sum(vars[,15])
sum(vars)
length(data)
variables<-logical()
names(data)
threshold<-length(data$Country)/1.3333 #eliminate if more than 75% missing

for (variable in names(data)){
  if (sum(vars[,variable])>(threshold)){
    variables[variable]=T
  }else{
    variables[variable]=F
  }
}
print(unname(variables))#the list of whether or not each variable has more (T) or less (F) than 75% missing values
variables

missmap(data[variables==F]) #27% missing, down from 51%
str(data[variables==F])
summary(data[variables==F])

### Sorted list of variables by number of missing values ###
list<-sapply(data[variables==F], is.na, USE.NAMES = T)
list<-as.data.frame(list)
list2<-colSums(list, na.rm=F,dims=1)
dim(list)
sort(list2)
list3<-100*list2/length(data$Country)
sort(list3)

length(list3)

#Get rid of 2020
a = nrow(data)
b = which.max(data$Year)
no2020 = seq(-b, -a, by=-b)
data_no2020 <- data[no2020,]
missmap(data_no2020[variables==F]) #24% missing, down from 27%

#Get rid of 2019
a2 = nrow(data)
b2 = which.max(data$Year)-1
no2019 = seq(-b2, -a2, by=-b)
data_no2019_20 <- data[c(no2019,no2020),]
missmap(data_no2019_20[variables==F]) #21% missing, down from 24%
#View(data_no2019_20[variables==F])

#Get rid of 2018
a3 = nrow(data)
b3 = which.max(data$Year)-2
no2018 = seq(-b3, -a3, by=-b)
data_no2018_19_20 <- data[c(no2018,no2019,no2020),]
missmap(data_no2018_19_20[variables==F]) #19% missing
#View(data_no2018_19_20[variables==F])
#data$Year[which(rowSums(is.na(data))==84)]

#translate Carbon metric to be consistent with OECD metric: kg per GDP PPP
data.translate<-data_no2019_20[variables==F]
data.translate$sdg13_co2pc<-(data.translate$sdg13_co2pc*data.translate$Population)*(1000/data.translate$GDP_PPP)

library(gdata)
library(dplyr)

data.translate<- data.translate %>% rename(sdg13_co2kgPerGDPPPP = sdg13_co2pc)

list<-sapply(data.translate[variables==F], is.na, USE.NAMES = T)
list<-as.data.frame(list)
list2<-colSums(list, na.rm=F,dims=1)
dim(list)
sort(list2)
list3<-100*list2/length(data.translate$Country)
sort(list3)

length(list3)


library(ggplot2)
theme_set(theme_minimal())
ggplot(data.translate[1:660,], aes(x=Year, y=sdg13_co2kgPerGDPPPP, colour=id)) +
  geom_line(show.legend = F) +
  xlab("Year")+
  ylab("kg CO2 per GDP PPP")+
  ggtitle("SDR CO2 trends")


#mice package
data.imp<-data.translate
library(mice)
library(lattice)
num_countries<-length(unique(data.imp[,1]))
num_years<-max(data.imp$Year)-min(data.imp$Year)+1
list_test<-ceiling(seq(1,num_countries+1, by=(1/num_years))-1)
country_indices<-list_test[-1]
country_codes<-data.imp[,2]
blocks<-country_indices
emissions<-as.matrix(data.imp$sdg13_co2kgPerGDPPPP)
#predictor_matrix<-as.matrix(cbind(blocks,data.imp[,c(-1,-2,-47)])) #cut out emissions from predictors? I have on first attempt
# predictor_matrix<-as.matrix(data.imp[,c(-1,-2,-3,-4)])
# v0<-which(colnames(data.imp) == "Year")
# v1<-which(colnames(data.imp) == "Population")
# v2<-which(colnames(data.imp) == "sdg2_trophic")
# v3<-which(colnames(data.imp) == "sdg7_elecac")
# v4<-which(colnames(data.imp) == "sdg7_cleanfuel")
# v5<-which(colnames(data.imp) == "sdg7_co2twh")
# v6<-which(colnames(data.imp) == "sdg11_pm25")
# v7<-which(colnames(data.imp) == "sdg11_pipedwat")
# v8<-which(colnames(data.imp) == "sdg13_co2kgPerGDPPPP")
# v9<-which(colnames(data.imp) == "sdg15_redlist")
# predictor_list<-c(v0,
#                   #v1, # can't use both population and year with norm. Will be singular
#                   v2,
#                   v3,
#                   v4,
#                   v5,
#                   v6,
#                   v7,
#                   v8,
#                   v9)
# predictor_matrix_small<-as.matrix(data.imp[,predictor_list])
# #mice_data<-as.data.frame(cbind(blocks,predictor_matrix_small))
# mice_data<-predictor_matrix_small
# 
# imp<-mice(mice_data, method="pmm", seed=1994, print=F)
# imp_test<-mice(mice_data[20:38,], method="pmm", seed=1994, print=F)
# c.broad.test<-complete(imp_test, "broad")
# c.broad.test1<-cbind(data.imp$id,c.broad.test)
# 
# theme_set(theme_minimal())
# ggplot(data.imp[20:38,], aes(x=Year, y=sdg2_trophic, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(c.broad.test1, aes(x=Year.1, y=sdg2_trophic.1, colour=data.imp$id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends after imputation")
# 
# 
# c.broad<-complete(imp, "broad")
# c.long<-complete(imp, "long")
# imp2<-mice(predictor_matrix, method = "rf", m=1, seed=1994, print=F)
# c2<-complete(imp2)

#### Try pan package ####
library(pan)
library(matlab)
subj = blocks
col_ones = ones(nrow(data.imp),1)
#predi = as.matrix(data.imp[,c(v0,v1,v2,v3,v4,v5,v6,v7,v9)])
#pred = matrix(nrow=nrow(predi),ncol=ncol(predi)+1)
xcol = 1:2
zcol = 1
id2 <- matrix(c(1,0,0,1),ncol=2,nrow=2)
a<-2
c<-2
Binv <- a*id2
Dinv <- c*id2
prior = list(a=a,Binv=Binv,c=c,Dinv=Dinv)
predi_year = as.matrix(data.imp[,3])
pred_year = matrix(nrow=nrow(predi_year),ncol=2)
pred_year[,1] = col_ones
pred_year[,2] = predi_year
y = data.imp$sdg13_co2kgPerGDPPPP
imp_pan_sdg13_co2kgPerGDPPPP<-pan(y,subj,pred_year,xcol,zcol,prior,seed=1234,iter=100)

# hist(data.imp$sdg13_co2kgPerGDPPPP)
# hist(imp_pan_sdg13_co2kgPerGDPPPP$y)
# summary(imp_pan$y)
imputed_pan <- array(data=NA,dim=c(nrow(data.imp),ncol(data.imp)-3))
for (h in 4:ncol(data.imp)){
  y=data.imp[,h]
  imp_pan<-pan(y,subj,pred_year,xcol,zcol,prior,seed=1234,iter=100)
  imputed_pan[,h-3]<-imp_pan$last$y
  }
colnames(imputed_pan)<-names(data.imp[,4:75])

Year=pred_year[,2]
sdg13_co2kgGDPPPP_pan<-imp_pan_sdg13_co2kgPerGDPPPP$last$y
id=data.imp[,2]

data.pan<-data.frame(id,Year,imputed_pan)

# truncate imputed values at their natural cutoffs (100%)
data.pan[which(data.pan$sdg13_co2kgPerGDPPPP<0),"sdg13_co2kgPerGDPPPP"]<-0
data.pan[which(data.pan$sdg7_elecac>100),"sdg7_elecac"]<-100
data.pan[which(data.pan$sdg11_pipedwat>99),"sdg11_pipedwat"]<-99
data.pan[which(data.pan$sdg14_cleanwat>100),"sdg14_cleanwat"]<-100
data.pan[which(data.pan$sdg14_fishstocks>100),"sdg14_fishstocks"]<-100
data.pan[which(data.pan$sdg14_trawl>100),"sdg14_trawl"]<-100
data.pan[which(data.pan$sdg9_intuse>100),"sdg9_intuse"]<-100
data.pan[which(data.pan$sdg7_cleanfuel>100),"sdg7_cleanfuel"]<-100
data.pan[which(data.pan$sdg6_safesan>100),"sdg6_safesan"]<-100
data.pan[which(data.pan$sdg6_safewat>100),"sdg6_safewat"]<-100
data.pan[which(data.pan$sdg6_scarcew>100),"sdg6_scarcew"]<-100
data.pan[which(data.pan$sdg5_edat>100),"sdg5_edat"]<-100
data.pan[which(data.pan$sdg5_fplmodel>100),"sdg5_fplmodel"]<-100
data.pan[which(data.pan$sdg4_earlyedu>100),"sdg4_earlyedu"]<-100
data.pan[which(data.pan$sdg4_second>100),"sdg4_second"]<-100
data.pan[which(data.pan$sdg4_primary>100),"sdg4_primary"]<-100
data.pan[which(data.pan$sdg3_births>100),"sdg3_births"]<-100
data.pan[which(data.pan$sdg1_oecdpov>25),"sdg1_oecdpov"]<-25 #questionable cutoff. The highest poverty in actual data is 19.5%
data.pan[which(data.pan$sdg1_320pov<0),"sdg1_320pov"]<-0
data.pan[which(data.pan$sdg1_oecdpov<0),"sdg1_oecdpov"]<-0
data.pan[which(data.pan$sdg1_wpc<0),"sdg1_wpc"]<-0
data.pan[which(data.pan$sdg3_incomeg<0),"sdg3_incomeg"]<-0
data.pan[which(data.pan$sdg4_earlyedu<0),"sdg4_earlyedu"]<-0
data.pan[which(data.pan$sdg8_impacc<0),"sdg8_impacc"]<-0
data.pan[which(data.pan$sdg9_netacc<0),"sdg9_netacc"]<-0
data.pan[which(data.pan$sdg9_netacc>100),"sdg9_netacc"]<-100
data.pan[which(data.pan$sdg10_palma<0.9),"sdg10_palma"]<-0.9
data.pan[which(data.pan$sdg10_elder<0),"sdg10_elder"]<-0
data.pan[which(data.pan$sdg11_rentover<0),"sdg11_rentover"]<-0
data.pan[which(data.pan$sdg14_fishstocks<0),"sdg14_fishstocks"]<-0
data.pan[which(data.pan$sdg14_trawl<0),"sdg14_trawl"]<-0
data.pan[which(data.pan$sdg16_safe<0),"sdg16_safe"]<-0
data.pan[which(data.pan$sdg16_cpi<0),"sdg16_cpi"]<-0
data.pan[which(data.pan$sdg16_rsf<0),"sdg16_rsf"]<-0
data.pan[which(data.pan$sdg17_oda<0),"sdg17_oda"]<-0
data.pan[which(data.pan$sdg2_stuntihme<0),"sdg2_stuntihme"]<-0
data.pan[which(data.pan$sdg2_snmi<0),"sdg2_snmi"]<-0
data.pan[which(data.pan$sdg3_matmort<0),"sdg3_matmort"]<-0
data.pan[which(data.pan$sdg3_hiv<0),"sdg3_hiv"]<-0
data.pan[which(data.pan$sdg3_fertility<0),"sdg3_fertility"]<-0
data.pan[which(data.pan$sdg5_parl<0),"sdg5_parl"]<-0
data.pan[which(data.pan$sdg5_paygap<0),"sdg5_paygap"]<-0
data.pan[which(data.pan$sdg6_scarcew<0),"sdg6_scarcew"]<-0
data.pan[which(data.pan$sdg7_co2twh<0),"sdg7_co2twh"]<-0
data.pan[which(data.pan$sdg9_rdex<0),"sdg9_rdex"]<-0
data.pan[which(data.pan$sdg9_rdres<0),"sdg9_rdres"]<-0
data.pan[which(data.pan$sdg9_patents<0),"sdg9_patents"]<-0
data.pan[which(data.pan$sdg16_homicides<0),"sdg16_homicides"]<-0


summary(data.imp)
summary(data.pan)

# # Try looping strategy #]
# library(matlab)
# int_col=ones(nrow(predictor_matrix),1)
# #mice_data_country<-cbind(int_col,predictor_matrix) #for using Co2 selected variables
# mice_data_country<-cbind(int_col,data.imp[,c(-4,-2:-1)]) #for using all variables
# #imp_i<-list()
# m=5
# c3<-matrix(nrow=m*nrow(mice_data_country),ncol=ncol(mice_data_country))
# only_time<-matrix(nrow=ncol(mice_data_country),ncol=ncol(mice_data_country))
# only_time[,2]<-1
# only_time[,-2]<-0
# for (i in 1:(num_countries)){
#   country = seq(i*num_years-(num_years-1),i*num_years) # Force mice to only use time/year as an input to preddict trends
#   imp_i = mice(mice_data_country[country,],m=5,method="cart",seed=1994,print=F,predictorMatrix=only_time)#quickpred(mice_data_country[country,], mincor=.3))#c4 = complete(imp_i)
#   c3=complete(imp_i, "broad")
#   if (i==1){
#     c4=c3}
#   else{
#     c4=rbind(c4,c3)}
# }
# missmap(c4) #cart 19% missing after 23 countries.
# missmap(data.imp)
# data.imp.looping<-cbind(data.imp[,1:4],c4[,1:72],data.imp[,1:4],c4[,73:144],data.imp[,1:4],c4[,145:216],data.imp[,1:4],c4[,217:288],data.imp[,1:4],c4[,289:360])
# 
# 
# ###### Plots to check Imputation ######
# imp40<-mice.mids(imp,maxit=35,print=F,seed=1994)
# plot(imp40)
# stripplot(imp, sdg11_pm25~.imp, pch=20, cex=2)
# stripplot(imp)
# 
# hist(c.long$sdg11_pm25[1:19])
# hist(data.imp$sdg11_pm25[1:19])
# plot(c.long$Year[1:19],c.long$sdg11_pm25[1:19])
# plot(data.imp$Year[1:19],c.long$sdg11_pm25[1:19])
# 
# c.long_country<-cbind(data.imp$id,c.long)

# # Trophic #
# theme_set(theme_minimal())
# ggplot(data.imp, aes(x=Year, y=sdg2_trophic, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.looping[1:72], aes(x=Year.1, y=sdg2_trophic.1, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends after loop mice imputation")
# 
# theme_set(theme_minimal())
# ggplot(data.pan, aes(x=Year, y=sdg2_trophic, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends after pan imputation")
# 
# # Electricity Access #
# theme_set(theme_minimal())
# ggplot(data.imp, aes(x=Year, y=sdg7_elecac, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Electricity Access (%)")+
#   ggtitle("SDR_OECD Electricity Access index trends before iputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.looping[1:72], aes(x=Year.1, y=sdg7_elecac.1, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Electricity Access (%)")+
#   ggtitle("SDR_OECD Electricity Access index trends after loop miceimputation")
# 
# theme_set(theme_minimal())
# ggplot(data.pan, aes(x=Year, y=sdg7_elecac, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Electricity Access (%)")+
#   ggtitle("SDR_OECD Electricity Access index trends after pan imputation")
# 
# # Clean Fuel Access #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg7_cleanfuel, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Clean Fuel Access (%)")+
#   ggtitle("SDR_OECD Clean Fuel Access index trends before iputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.looping[1:72], aes(x=Year.1, y=sdg7_cleanfuel.1, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Clean Fuel Access (%)")+
#   ggtitle("SDR_OECD Clean Fuel Access index trends after loop mice imputation")
# 
# theme_set(theme_minimal())
# ggplot(data.pan, aes(x=Year, y=sdg7_cleanfuel, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Clean Fuel Access (%)")+
#   ggtitle("SDR_OECD Clean Fuel Access index trends after pan imputation")
# 
# # CO2 TWH #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg7_co2twh, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("CO2 TWH")+
#   ggtitle("SDR_OECD CO2 TWH index trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.looping[1:72], aes(x=Year.1, y=sdg7_co2twh.1, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("CO2 TWH")+
#   ggtitle("SDR_OECD CO2 TWH index trends after loop mice imputation")
# 
# theme_set(theme_minimal())
# ggplot(data.pan, aes(x=Year, y=sdg7_co2twh, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("CO2 TWH")+
#   ggtitle("SDR_OECD CO2 TWH index trends after pan imputation")
# 
# # PM2.5 #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg11_pm25, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("PM 2.5 exposure")+
#   ggtitle("SDR_OECD PM 2.5 trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.looping[1:72], aes(x=Year.1, y=sdg11_pm25.1, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("PM 2.5 exposure")+
#   ggtitle("SDR_OECD PM 2.5 trends after loop mice imputation")
# 
# theme_set(theme_minimal())
# ggplot(data.pan, aes(x=Year, y=sdg11_pm25, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("PM 2.5 exposure")+
#   ggtitle("SDR_OECD PM 2.5 trends after pan imputation")
# 
# # Piped water #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg11_pipedwat, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Piped water access (%)")+
#   ggtitle("SDR_OECD Piped water trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.looping[1:72], aes(x=Year.1, y=sdg11_pipedwat.1, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Piped water access (%)")+
#   ggtitle("SDR_OECD Piped water trends after loop mice imputation")
# 
# theme_set(theme_minimal())
# ggplot(data.pan, aes(x=Year, y=sdg11_pipedwat, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Piped water access (%)")+
#   ggtitle("SDR_OECD Piped water trends after pan imputation")



write.csv(data.imp,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/data_imp_OECD.csv", row.names = FALSE)
write.csv(c.long,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/clong_OECD.csv", row.names = FALSE)
write.csv(c.broad,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/cbroad_OECD.csv", row.names = FALSE)
write.csv(c2,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/c2_OECD.csv", row.names = FALSE)
write.csv(c4,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/c4_OECD.csv", row.names = FALSE)
#write.csv(imp,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/imp_OECD.csv", row.names = FALSE)

#data.imp<-read.csv("data_imp_OECD.csv", header=TRUE, na.strings=c("..","NA","","?"))
#c.long<-read.csv("clong_OECD.csv", header=TRUE, na.strings=c("..","NA","","?"))
#c.broad<-read.csv("cbroad_OECD.csv", header=TRUE, na.strings=c("..","NA","","?"))
#c2<-read.csv("c2_OECD.csv", header=TRUE, na.strings=c("..","NA","","?"))
#c4<-read.csv("c4_OECD.csv", header=TRUE, na.strings=c("..","NA","","?"))



# #### Custom Method with Interpolation ####
# Country_start = 1
# polycomp <- function(x,period) {lm(x ~ poly(Year,2,raw=T), data.imp[period,])}#,na.action=)}
# indx4<- grep("^(sdg2_trophic|sdg7_elec_ac|sdg7_cleanfuel|sdg15_redlist|sdg7_co2twh|sdg11_pm25|sdg11_pipedwat|sdg13_co2kgPerGDPPPP)",colnames(data.imp))#c(25,26,30,32,33) #indices of input vars. Access to electricity, Clean fuel use, PM 2.5 exposure, Per GDP CO2, imported CO2
# 
# data.imp.cust<-data.imp
# #Country_inx<-1:42
# for (n in 1:num_countries){
#   period = seq(n*num_years-(num_years-1),n*num_years)
#   #for (i in indx4){
#   for (i in 3:ncol(data.imp)){
#     #name<-names(data.imp)[i]
#     if (sum(is.na(data.imp[period,i]))<(length(period)-2)){
#       #poly<- sapply(data.imp[period,indx4],polycomp,period=period)
#       #poly <- polycomp(data.imp[period,i],period=period)
#       for (j in period){
#         if (is.na(data.imp[j,i])==T){
#           poly <- polycomp(data.imp[period,i],period=period)
#           #string1<-as.name(paste('poly$',name,'$coefficients[1]',sep=""))
#           #string2<-as.name(paste('poly$',name,'$coefficients[2]',sep=""))
#           string1<-as.name(paste('poly$','coefficients[1]',sep=""))
#           string2<-as.name(paste('poly$','coefficients[2]',sep=""))
#           string3<-as.name(paste('poly$','coefficients[3]',sep=""))
#           #polyimp <- function(x1,x2) {(x1)+(x2)*data.imp$Year[j]} # use for linear
#           polyimp <- function(x1,x2,x3) {(x1)+(x2)*data.imp$Year[j]+(x3)*data.imp$Year[j]^2} # use for quadratic
#           #data.imp.test[j,i]<-polyimp(eval(parse(text=string1)),eval(parse(text=string2))) # use for linear
#           data.imp.cust[j,i]<-polyimp(eval(parse(text=string1)),eval(parse(text=string2)),eval(parse(text=string3))) # use for quadratic
#         }
#       }
#     }
#   }
# }
# missmap(data.imp.cust[,indx4])
# missmap(data.imp.cust)
# 
# # truncate imputed values at their natural cutoffs (0%, 100%)
# data.imp.cust[which(data.imp.cust$sdg7_elecac>100),"sdg7_elecac"]<-100
# data.imp.cust[which(data.imp.cust$sdg11_pipedwat>99),"sdg11_pipedwat"]<-99
# data.imp.cust[which(data.imp.cust$sdg14_cleanwat>100),"sdg14_cleanwat"]<-100
# data.imp.cust[which(data.imp.cust$sdg1_oecdpov>25),"sdg1_oecdpov"]<-25 #questionable cutoff. The highest poverty in actual data is 19.5%
# data.imp.cust[which(data.imp.cust$sdg1_320pov<0),"sdg1_320pov"]<-0
# data.imp.cust[which(data.imp.cust$sdg1_oecdpov<0),"sdg1_oecdpov"]<-0
# data.imp.cust[which(data.imp.cust$sdg1_wpc<0),"sdg1_wpc"]<-0
# data.imp.cust[which(data.imp.cust$sdg3_incomeg<0),"sdg3_incomeg"]<-0
# data.imp.cust[which(data.imp.cust$sdg4_earlyedu<0),"sdg4_earlyedu"]<-0
# data.imp.cust[which(data.imp.cust$sdg8_impacc<0),"sdg8_impacc"]<-0
# data.imp.cust[which(data.imp.cust$sdg9_netacc<0),"sdg9_netacc"]<-0
# data.imp.cust[which(data.imp.cust$sdg9_netacc>100),"sdg9_netacc"]<-100
# data.imp.cust[which(data.imp.cust$sdg10_palma<0.9),"sdg10_palma"]<-0.9
# data.imp.cust[which(data.imp.cust$sdg10_elder<0),"sdg10_elder"]<-0
# data.imp.cust[which(data.imp.cust$sdg11_rentover<0),"sdg11_rentover"]<-0
# data.imp.cust[which(data.imp.cust$sdg14_fishstocks<0),"sdg14_fishstocks"]<-0
# data.imp.cust[which(data.imp.cust$sdg14_trawl<0),"sdg14_trawl"]<-0
# data.imp.cust[which(data.imp.cust$sdg16_safe<0),"sdg16_safe"]<-0
# data.imp.cust[which(data.imp.cust$sdg16_cpi<0),"sdg16_cpi"]<-0
# data.imp.cust[which(data.imp.cust$sdg16_rsf<0),"sdg16_rsf"]<-0
# data.imp.cust[which(data.imp.cust$sdg17_oda<0),"sdg17_oda"]<-0
# 
# 
# # plots to check manual imputation #
# # Trophic #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg2_trophic, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg2_trophic, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Trophic Index")+
#   ggtitle("SDR_OECD trophic index trends after imputation")
# 
# # Electricity Access #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg7_elecac, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Electricity Access (%)")+
#   ggtitle("SDR_OECD Electricity Access index trends before iputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg7_elecac, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Electricity Access (%)")+
#   ggtitle("SDR_OECD Electricity Access index trends after imputation")
# 
# # Clean Fuel Access #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg7_cleanfuel, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Clean Fuel Access (%)")+
#   ggtitle("SDR_OECD Clean Fuel Access index trends before iputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg7_cleanfuel, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Clean Fuel Access (%)")+
#   ggtitle("SDR_OECD Clean Fuel Access index trends after imputation")
# 
# # CO2 TWH #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg7_co2twh, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("CO2 TWH")+
#   ggtitle("SDR_OECD CO2 TWH index trends before iputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg7_co2twh, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("CO2 TWH")+
#   ggtitle("SDR_OECD CO2 TWH index trends after imputation")
# 
# # PM2.5 #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg11_pm25, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("PM 2.5 exposure")+
#   ggtitle("SDR_OECD PM 2.5 trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg11_pm25, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("PM 2.5 exposure")+
#   ggtitle("SDR_OECD PM 2.5 trends after imputation")
# 
# # Piped water #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg11_pipedwat, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Piped water access (%)")+
#   ggtitle("SDR_OECD Piped water trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg11_pipedwat, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("Piped water access (%)")+
#   ggtitle("SDR_OECD Piped water trends after imputation")
# 
# # OECD Poverty #
# theme_set(theme_minimal())
# ggplot(data.imp[1:684,], aes(x=Year, y=sdg1_oecdpov, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("OECD adjusted Poverty rate (%)")+
#   ggtitle("SDR_OECD OECD poverty trends before imputation")
# 
# 
# theme_set(theme_minimal())
# ggplot(data.imp.cust[1:684,], aes(x=Year, y=sdg1_oecdpov, colour=id)) +
#   geom_line(show.legend = F) +
#   xlab("Year")+
#   ylab("OECD adjusted Poverty rate (%)")+
#   ggtitle("SDR_OECD OECD poverty trends after imputation")

#seems like clean fuel access and pm 2.5 could use different trends besides linear. Cleanfuel>logarithmic
#quadratic works, but makes some imputations nonsensical, like decreasing clean fuel acces in final year
  #quadratic trends hurting oecd pov imputation. Linear would likely be better for that indicator.


#Optional Transformation
library(caret)
data.xform <- data.imp.cust
indx<-sapply(data.xform, is.numeric)
TransformParams <- preProcess(data.xform[,indx], method=c("YeoJohnson"))
#TransformParams$yj
sort(abs(TransformParams$yj), decreasing = F)
library(e1071)
TPs<-preProcess(data.xform[,indx], method=c("BoxCox"))
#TPs$bc

#Transformation of pan imputed data
data.xform.pan <- data.pan
indx.pan<-sapply(data.xform.pan, is.numeric)
TransformParams.pan <- preProcess(data.xform.pan[,indx.pan], method=c("YeoJohnson"))
#TransformParams.pan$yj
sort(abs(TransformParams.pan$yj), decreasing = F)
TPs.pan<-preProcess(data.xform.pan[,indx.pan], method=c("BoxCox"))


######################################################################
######################################################################
######################################################################
# par(mfrow=c(1,1),mar=c(4.1,4.1,3.1,2.1))
hist(data.xform.pan$GDP_PPP)
hist(data.xform.pan$sdg7_ren) #May not need transformation
hist(data.xform.pan$Population)
hist(data.xform.pan$sdg6_scarcew)
hist(data.xform.pan$sdg9_rdex) #May not need transformation
hist(data.xform.pan$sdg8_yneet)
hist(data.xform.pan$sdg9_patents)
hist(data.xform.pan$sdg16_rsf)
hist(data.xform.pan$sdg2_trophic)
hist(data.xform.pan$sdg10_elder)
hist(data.xform.pan$sdg11_rentover)
hist(data.xform.pan$sdg3_fertility)
hist(data.xform.pan$sdg16_prison)
hist(data.xform.pan$sdg11_pm25)
hist(data.xform.pan$sdg8_impacc)
hist(data.xform.pan$sdg17_govex)
hist(data.xform.pan$sdg5_paygap)
hist(data.xform.pan$sdg3_matmort)
hist(data.xform.pan$sdg13_co2import)
hist(data.xform.pan$sdg3_tb)
hist(data.xform.pan$sdg3_incomeg)
hist(data.xform.pan$sdg14_fishstocks)
hist(data.xform.pan$sdg5_parl)
hist(data.xform.pan$sdg2_crlyld)
hist(data.xform.pan$sdg9_articles)
hist(data.xform.pan$sdg4_earlyedu)
hist(data.xform.pan$sdg2_stuntihme)
hist(data.xform.pan$sdg1_oecdpov)
hist(data.xform.pan$sdg9_rdres) #already normal
hist(data.xform.pan$sdg7_co2twh)
hist(data.xform.pan$sdg14_trawl)
hist(data.xform.pan$sdg3_smoke) #already normal
hist(data.xform.pan$sdg9_netacc)
hist(data.xform.pan$sdg3_neonat)
hist(data.xform.pan$sdg10_adjgini)
hist(data.xform.pan$sdg14_cleanwat)
hist(data.xform.pan$sdg3_u5mort)
hist(data.xform.pan$sdg15_cpfa)
#
hist(data.xform.pan$sdg4_tertiary)
hist(data.xform.pan$sdg16_cpi)
hist(data.xform.pan$sdg15_cpta)
hist(data.xform.pan$sdg16_homicides)
hist(data.xform.pan$sdg16_safe)
hist(data.xform.pan$sdg14_cpma)
#
hist(data.xform.pan$sdg1_320pov)
#
hist(data.xform.pan$sdg11_transport)
hist(data.xform.pan$sdg9_intuse)
hist(data.xform.pan$sdg2_obesity)
hist(data.xform.pan$sdg2_wasteihme)
hist(data.xform.pan$sdg2_snmi)
#
hist(data.xform.pan$sdg1_wpc)

#
hist(data.xform.pan$sdg10_palma)
hist(data.pan$sdg10_palma)
# 
# 
# # Apply transformations for custom imputed data in order of sorted abs(transformparams$yj)
# TPs$bc$GDP_PPP$lambda
# data.xform$GDP_PPP <- log(data.imp.cust$GDP_PPP)
# min(data.xform$sdg7_ren)
# TPs$bc$sdg7_ren$lambda
# data.xform$sdg7_ren <- log(data.imp.cust$sdg7_ren)
# TPs$bc$Population$lambda
# data.xform$Population <- log(data.imp.cust$Population)
# min(data.xform$sdg6_scarcew)
# TPs$bc$sdg6_scarcew$lambda
# data.xform$sdg6_scarcew <- log(data.imp.cust$sdg6_scarcew)
# min(data.xform$sdg9_rdex)
# TPs$bc$sdg9_rdex$lambda
# data.xform$sdg9_rdex <- (data.imp.cust$sdg9_rdex^(TPs$bc$sdg9_rdex$lambda)-1)/TPs$bc$sdg9_rdex$lambda
# summary(data.xform$sdg8_yneet)
# TPs$bc$sdg8_yneet$lambda
# data.xform$sdg8_yneet <- log(data.imp.cust$sdg8_yneet)
# summary(data.xform$sdg9_patents)
# TransformParams[["yj"]][["sdg9_patents"]]
# data.xform$sdg9_patents <- -((-data.imp.cust$sdg9_patents+1)^(2-TransformParams[["yj"]][["sdg9_patents"]])-1)/(2-TransformParams[["yj"]][["sdg9_patents"]])
# #data.xform$sdg9_patents <- (((data.imp.cust$sdg9_patents+1)^(TransformParams[["yj"]][["sdg9_patents"]]))-1)/TransformParams[["yj"]][["sdg9_patents"]]
# #data.xform$sdg9_patents <- log(data.imp.cust$sdg9_patents +1)
# TransformParams[["yj"]][["sdg16_rsf"]]
# data.xform$sdg16_rsf <- log(data.imp.cust$sdg16_rsf+1)
# #trophic index does not need transformation. Already normally distributed
# TransformParams[["yj"]][["sdg10_elder"]]
# #data.xform$sdg10_elder <- (((data.imp.cust$sdg10_elder+1)^(TransformParams[["yj"]][["sdg10_elder"]]))-1)/TransformParams[["yj"]][["sdg10_elder"]]
# data.xform$sdg10_elder <- log(data.imp.cust$sdg10_elder+1)
# TransformParams[["yj"]][["sdg11_rentover"]]
# data.xform$sdg11_rentover <- log(data.imp.cust$sdg11_rentover+1)
# min(data.xform$sdg3_fertility)
# TPs$bc$sdg3_fertility$lambda
# data.xform$sdg3_fertility <- log(data.imp.cust$sdg3_fertility)
# min(data.xform$sdg16_prison)
# TPs$bc$sdg16_prison$lambda
# data.xform$sdg16_prison <- (data.imp.cust$sdg16_prison^(TPs$bc$sdg16_prison$lambda)-1)/TPs$bc$sdg16_prison$lambda
# TPs$bc$sdg11_pm25$lambda
# data.xform$sdg11_pm25 <- (data.imp.cust$sdg11_pm25^(TPs$bc$sdg11_pm25$lambda)-1)/TPs$bc$sdg11_pm25$lambda
# min(data.xform$sdg8_impacc)
# TransformParams[["yj"]][["sdg8_impacc"]]
# data.xform$sdg8_impacc <- (((data.imp.cust$sdg8_impacc+1)^(TransformParams[["yj"]][["sdg8_impacc"]]))-1)/TransformParams[["yj"]][["sdg8_impacc"]]
# #sdg17_govex does not need transformation. Already normally distributed
# min(data.xform$sdg5_paygap)
# TPs$bc$sdg5_paygap$lambda
# data.xform$sdg5_paygap <- (data.imp.cust$sdg5_paygap^(TPs$bc$sdg5_paygap$lambda)-1)/TPs$bc$sdg5_paygap$lambda
# min(data.xform$sdg3_matmort)
# TPs$bc$sdg3_matmort$lambda
# data.xform$sdg3_matmort <- (data.imp.cust$sdg3_matmort^(TPs$bc$sdg3_matmort$lambda)-1)/TPs$bc$sdg3_matmort$lambda
# TPs$bc$sdg13_co2import$lambda
# data.xform$sdg13_co2import <- log(data.imp.cust$sdg13_co2import)
# min(data.xform$sdg3_tb)
# TPs$bc$sdg3_tb$lambda
# data.xform$sdg3_tb <- (data.imp.cust$sdg3_tb^(TPs$bc$sdg3_tb$lambda)-1)/TPs$bc$sdg3_tb$lambda
# summary(data.xform$sdg3_incomeg)
# TransformParams[["yj"]][["sdg3_incomeg"]]
# data.xform$sdg3_incomeg <- (((data.imp.cust$sdg3_incomeg+1)^(TransformParams[["yj"]][["sdg3_incomeg"]]))-1)/TransformParams[["yj"]][["sdg3_incomeg"]]
# summary(data.xform$sdg14_fishstocks)
# TPs$bc$sdg14_fishstocks$lambda
# TransformParams[["yj"]][["sdg14_fishstocks"]]
# data.xform$sdg14_fishstocks <- (((data.imp.cust$sdg14_fishstocks+1)^(TransformParams[["yj"]][["sdg14_fishstocks"]]))-1)/TransformParams[["yj"]][["sdg14_fishstocks"]]
# #sdg5_parl does not need transformation. Already normally distributed
# min(data.xform$sdg2_crlyld)
# TPs$bc$sdg2_crlyld$lambda
# data.xform$sdg2_crlyld <- (data.imp.cust$sdg2_crlyld^(TPs$bc$sdg2_crlyld$lambda)-1)/TPs$bc$sdg2_crlyld$lambda #not much change. Could consider not transforming
# min(data.xform$sdg9_articles)
# TPs$bc$sdg9_articles$lambda
# data.xform$sdg9_articles <- (data.imp.cust$sdg9_articles^(TPs$bc$sdg9_articles$lambda)-1)/TPs$bc$sdg9_articles$lambda
# summary(data.xform$sdg4_earlyedu)
# TransformParams[["yj"]][["sdg4_earlyedu"]]
# data.xform$sdg4_earlyedu <- (((data.imp.cust$sdg4_earlyedu+1)^(TransformParams[["yj"]][["sdg4_earlyedu"]]))-1)/TransformParams[["yj"]][["sdg4_earlyedu"]]
# min(data.xform$sdg2_stuntihme)
# TPs$bc$sdg2_stuntihme$lambda
# TransformParams[["yj"]][["sdg2_stuntihme"]]
# #data.xform$sdg2_stuntihme <- log(data.imp.cust$sdg2_stuntihme)
# #data.xform$sdg2_stuntihme <- (((data.imp.cust$sdg2_stuntihme+1)^(TransformParams[["yj"]][["sdg2_stuntihme"]]))-1)/TransformParams[["yj"]][["sdg2_stuntihme"]]
# data.xform$sdg2_stuntihme <- (data.imp.cust$sdg2_stuntihme^(TPs$bc$sdg2_stuntihme$lambda)-1)/TPs$bc$sdg2_stuntihme$lambda
# min(data.xform$sdg1_oecdpov)
# TransformParams[["yj"]][["sdg1_oecdpov"]]
# data.xform$sdg1_oecdpov <- (((data.imp.cust$sdg1_oecdpov+1)^(TransformParams[["yj"]][["sdg1_oecdpov"]]))-1)/TransformParams[["yj"]][["sdg1_oecdpov"]]
# #data.xform$sdg1_oecdpov <- log(data.imp.cust$sdg1_oecdpov +1) #slightly worse than non zero lambda transform
# #sdg9_res does not need transformation. Already normally distributed
# min(data.xform$sdg7_co2twh)
# TPs$bc$sdg7_co2twh$lambda
# data.xform$sdg7_co2twh <- log(data.imp.cust$sdg7_co2twh)
# summary(data.xform$sdg14_trawl)
# TransformParams[["yj"]][["sdg14_trawl"]]
# data.xform$sdg14_trawl <- (((data.imp.cust$sdg14_trawl+1)^(TransformParams[["yj"]][["sdg14_trawl"]]))-1)/TransformParams[["yj"]][["sdg14_trawl"]]
# #sdg3_smoke does not need transformation. Already normally distributed
# min(data.xform$sdg9_netacc)
# #TPs$bc$sdg9_netacc$lambda
# TransformParams[["yj"]][["sdg9_netacc"]]
# data.xform$sdg9_netacc <- (((data.imp.cust$sdg9_netacc+1)^(TransformParams[["yj"]][["sdg9_netacc"]]))-1)/TransformParams[["yj"]][["sdg9_netacc"]]
# min(data.xform$sdg3_neonat)
# TPs$bc$sdg3_neonat$lambda
# data.xform$sdg3_neonat <- (data.imp.cust$sdg3_neonat^(TPs$bc$sdg3_neonat$lambda)-1)/TPs$bc$sdg3_neonat$lambda
# min(data.xform$sdg10_adjgini)
# TPs$bc$sdg10_adjgini$lambda
# data.xform$sdg10_adjgini <- (data.imp.cust$sdg10_adjgini^(TPs$bc$sdg10_adjgini$lambda)-1)/TPs$bc$sdg10_adjgini$lambda
# summary(data.xform$sdg14_cleanwat)
# TPs$bc$sdg14_cleanwat$lambda
# #TransformParams[["yj"]][["sdg14_cleanwat"]]
# data.xform$sdg14_cleanwat <- (data.imp.cust$sdg14_cleanwat^(TPs$bc$sdg14_cleanwat$lambda)-1)/TPs$bc$sdg14_cleanwat$lambda
# min(data.xform$sdg3_u5mort)
# TransformParams[["yj"]][["sdg3_u5mort"]]
# data.xform$sdg3_u5mort <- (((data.imp.cust$sdg3_u5mort+1)^(TransformParams[["yj"]][["sdg3_u5mort"]]))-1)/TransformParams[["yj"]][["sdg3_u5mort"]]
# min(data.xform$sdg15_cpfa)
# TransformParams[["yj"]][["sdg15_cpfa"]]
# data.xform$sdg15_cpfa <- (((data.imp.cust$sdg15_cpfa+1)^(TransformParams[["yj"]][["sdg15_cpfa"]]))-1)/TransformParams[["yj"]][["sdg15_cpfa"]]
# min(data.xform$sdg4_tertiary)
# TransformParams[["yj"]][["sdg4_tertiary"]]
# data.xform$sdg4_tertiary <- (((data.imp.cust$sdg4_tertiary+1)^(TransformParams[["yj"]][["sdg4_tertiary"]]))-1)/TransformParams[["yj"]][["sdg4_tertiary"]]
# min(data.xform$sdg16_cpi)
# TransformParams[["yj"]][["sdg16_cpi"]]
# data.xform$sdg16_cpi <- (((data.imp.cust$sdg16_cpi+1)^(TransformParams[["yj"]][["sdg16_cpi"]]))-1)/TransformParams[["yj"]][["sdg16_cpi"]]
# min(data.xform$sdg15_cpta)
# TransformParams[["yj"]][["sdg15_cpta"]]
# data.xform$sdg15_cpta <- (((data.imp.cust$sdg15_cpta+1)^(TransformParams[["yj"]][["sdg15_cpta"]]))-1)/TransformParams[["yj"]][["sdg15_cpta"]]
# min(data.xform$sdg16_homicides)
# TransformParams[["yj"]][["sdg16_homicides"]]
# data.xform$sdg16_homicides <- (((data.imp.cust$sdg16_homicides+1)^(TransformParams[["yj"]][["sdg16_homicides"]]))-1)/TransformParams[["yj"]][["sdg16_homicides"]]
# min(data.xform$sdg16_safe)
# TransformParams[["yj"]][["sdg16_safe"]]
# data.xform$sdg16_safe <- (((data.imp.cust$sdg16_safe+1)^(TransformParams[["yj"]][["sdg16_safe"]]))-1)/TransformParams[["yj"]][["sdg16_safe"]]
# min(data.xform$sdg14_cpma)
# TransformParams[["yj"]][["sdg14_cpma"]]
# data.xform$sdg14_cpma <- (((data.imp.cust$sdg14_cpma+1)^(TransformParams[["yj"]][["sdg14_cpma"]]))-1)/TransformParams[["yj"]][["sdg14_cpma"]]
# min(data.xform$sdg1_320pov)
# TransformParams[["yj"]][["sdg1_320pov"]]
# data.xform$sdg1_320pov <- (((data.imp.cust$sdg1_320pov+1)^(TransformParams[["yj"]][["sdg1_320pov"]]))-1)/TransformParams[["yj"]][["sdg1_320pov"]]
# min(data.xform$sdg11_transport)
# TransformParams[["yj"]][["sdg11_transport"]]
# data.xform$sdg11_transport <- (((data.imp.cust$sdg11_transport+1)^(TransformParams[["yj"]][["sdg11_transport"]]))-1)/TransformParams[["yj"]][["sdg11_transport"]]
# min(data.xform$sdg9_intuse)
# TransformParams[["yj"]][["sdg9_intuse"]]
# data.xform$sdg9_intuse <- (((data.imp.cust$sdg9_intuse+1)^(TransformParams[["yj"]][["sdg9_intuse"]]))-1)/TransformParams[["yj"]][["sdg9_intuse"]]
# min(data.xform$sdg2_obesity)
# TransformParams[["yj"]][["sdg2_obesity"]]
# data.xform$sdg2_obesity <- (((data.imp.cust$sdg2_obesity+1)^(TransformParams[["yj"]][["sdg2_obesity"]]))-1)/TransformParams[["yj"]][["sdg2_obesity"]]
# min(data.xform$sdg2_wasteihme)
# TransformParams[["yj"]][["sdg2_wasteihme"]]
# data.xform$sdg2_wasteihme <- (((data.imp.cust$sdg2_wasteihme+1)^(TransformParams[["yj"]][["sdg2_wasteihme"]]))-1)/TransformParams[["yj"]][["sdg2_wasteihme"]]
# min(data.xform$sdg2_wasteihme)
# TransformParams[["yj"]][["sdg2_wasteihme"]]
# data.xform$sdg2_wasteihme <- (((data.imp.cust$sdg2_wasteihme+1)^(TransformParams[["yj"]][["sdg2_wasteihme"]]))-1)/TransformParams[["yj"]][["sdg2_wasteihme"]]
# min(data.xform$sdg2_snmi)
# TransformParams[["yj"]][["sdg2_snmi"]]
# data.xform$sdg2_snmi <- (((data.imp.cust$sdg2_snmi+1)^(TransformParams[["yj"]][["sdg2_snmi"]]))-1)/TransformParams[["yj"]][["sdg2_snmi"]]
# min(data.xform$sdg1_wpc)
# #TPs$bc$sdg1_wpc$lambda
# TransformParams[["yj"]][["sdg1_wpc"]]
# data.xform$sdg1_wpc <- (((data.imp.cust$sdg1_wpc+1)^(TransformParams[["yj"]][["sdg1_wpc"]]))-1)/TransformParams[["yj"]][["sdg1_wpc"]]
# min(data.xform$sdg8_empop)
# TransformParams[["yj"]][["sdg8_empop"]]
# data.xform$sdg8_empop <- (((data.imp.cust$sdg8_empop+1)^(TransformParams[["yj"]][["sdg8_empop"]]))-1)/TransformParams[["yj"]][["sdg8_empop"]]
# min(data.xform$sdg3_swb)
# TransformParams[["yj"]][["sdg3_swb"]]
# data.xform$sdg3_swb <- (((data.imp.cust$sdg3_swb+1)^(TransformParams[["yj"]][["sdg3_swb"]]))-1)/TransformParams[["yj"]][["sdg3_swb"]]
# min(data.xform$sdg17_oda)
# TransformParams[["yj"]][["sdg17_oda"]]
# data.xform$sdg17_oda <- (((data.imp.cust$sdg17_oda+1)^(TransformParams[["yj"]][["sdg17_oda"]]))-1)/TransformParams[["yj"]][["sdg17_oda"]]
# 
# 
# TPs$bc$sdg13_co2kgPerGDPPPP$lambda
# data.xform$sdg13_co2kgPerGDPPPP <- log(data.imp.cust$sdg13_co2kgPerGDPPPP) #(((data.poly$sdg13_co2kgPerGDPPPP+1)^TransformParams$yj[4])-1)/TransformParams$yj[4]
# TPs$bc$sdg7_cleanfuel$lambda
# data.xform$sdg7_cleanfuel <- (data.imp.cust$sdg7_cleanfuel^(TPs$bc$sdg7_cleanfuel$lambda)-1)/TPs$bc$sdg7_cleanfuel$lambda
# TPs$bc$sdg7_elecac$lambda
# data.xform$sdg7_elecac <- (data.imp.cust$sdg7_elecac^(TPs$bc$sdg7_elecac$lambda)-1)/TPs$bc$sdg7_elecac$lambda
# 
# 
# min(data.xform$sdg10_palma)
# TPs$bc$sdg10_palma$lambda
# data.xform$sdg10_palma <- (data.xform$sdg10_palma^(TPs$bc$sdg10_palma$lambda)-1)/TPs$bc$sdg10_palma$lambda
# 
# 
# 
# #plot difference in distribution between before and after transformations
# par(mfrow=c(1,2),mar=c(4.1,4.1,3.1,2.1))
# 
# hist(data.imp.cust$sdg11_pm25)
# hist(data.xform$sdg11_pm25)
# hist(data.imp.cust$sdg13_co2kgPerGDPPPP)
# hist(data.xform$sdg13_co2kgPerGDPPPP)
# hist(data.imp.cust$sdg7_cleanfuel)
# hist(data.xform$sdg7_cleanfuel)
# hist(data.imp.cust$sdg13_co2import)
# hist(data.xform$sdg13_co2import)
# hist(data.imp.cust$sdg7_elecac)
# hist(data.xform$sdg7_elecac)
# hist(data.imp.cust$Population)
# hist(data.xform$Population)
# hist(data.imp.cust$sdg6_scarcew)
# hist(data.xform$sdg6_scarcew)
# hist(data.imp.cust$sdg9_rdex)
# hist(data.xform$sdg9_rdex)
# hist(data.imp.cust$sdg9_patents)
# hist(data.xform$sdg9_patents)
# hist(data.imp.cust$sdg2_trophic)
# hist(data.xform$sdg2_trophic) #not transformed
# hist(data.imp.cust$sdg10_elder)
# hist(data.xform$sdg10_elder)
# hist(data.imp.cust$sdg11_rentover)
# hist(data.xform$sdg11_rentover)
# hist(data.imp.cust$sdg3_fertility)
# hist(data.xform$sdg3_fertility)
# hist(data.imp.cust$sdg16_prison)
# hist(data.xform$sdg16_prison)
# hist(data.imp.cust$sdg17_govex)
# hist(data.xform$sdg17_govex) #not transformed
# hist(data.imp.cust$sdg5_paygap)
# hist(data.xform$sdg5_paygap)
# hist(data.imp.cust$sdg3_matmort)
# hist(data.xform$sdg3_matmort)
# hist(data.imp.cust$sdg3_tb)
# hist(data.xform$sdg3_tb)
# hist(data.imp.cust$sdg5_parl)
# hist(data.xform$sdg5_parl) #not transformed
# hist(data.imp.cust$sdg10_palma)
# hist(data.xform$sdg10_palma)
# hist(data.imp.cust$sdg1_320pov)
# hist(data.xform$sdg1_320pov)
# hist(data.imp.cust$sdg2_crlyld)
# hist(data.xform$sdg2_crlyld) #transformation may be unnecessary
# hist(data.imp.cust$sdg14_fishstocks)
# hist(data.xform$sdg14_fishstocks)
# hist(data.imp.cust$sdg14_cleanwat)
# hist(data.xform$sdg14_cleanwat) #transformation may be unnecessary
# hist(data.imp.cust$sdg9_netacc)
# hist(data.xform$sdg9_netacc) #transformation may be unnecessary
# hist(data.imp.cust$sdg9_articles)
# hist(data.xform$sdg9_articles) #transformation may be unnecessary
# hist(data.imp.cust$sdg2_stuntihme)
# hist(data.xform$sdg2_stuntihme) #not much improvement
# hist(data.imp.cust$sdg1_wpc)
# hist(data.xform$sdg1_wpc)
# hist(data.imp.cust$sdg7_co2twh)
# hist(data.xform$sdg7_co2twh)
# hist(data.imp.cust$sdg3_incomeg)
# hist(data.xform$sdg3_incomeg)
# hist(data.imp.cust$sdg1_oecdpov)
# hist(data.xform$sdg1_oecdpov)
# hist(data.imp.cust$sdg3_neonat)
# hist(data.xform$sdg3_neonat)
# hist(data.imp.cust$sdg10_adjgini)
# hist(data.xform$sdg10_adjgini)
# hist(data.imp.cust$sdg8_impacc)
# hist(data.xform$sdg8_impacc)
# hist(data.imp.cust$sdg16_rsf)
# hist(data.xform$sdg16_rsf)
# hist(data.imp.cust$sdg14_trawl)
# hist(data.xform$sdg14_trawl) #not much improvement
# hist(data.imp.cust$sdg3_u5mort)
# hist(data.xform$sdg3_u5mort)
# hist(data.imp.cust$sdg15_cpfa)
# hist(data.xform$sdg15_cpfa) #not much improvement
# hist(data.imp.cust$sdg4_tertiary)
# hist(data.xform$sdg4_tertiary)
# hist(data.imp.cust$sdg16_cpi)
# hist(data.xform$sdg16_cpi)
# hist(data.imp.cust$sdg15_cpta)
# hist(data.xform$sdg15_cpta)
# hist(data.imp.cust$sdg16_homicides)
# hist(data.xform$sdg16_homicides)
# hist(data.imp.cust$sdg16_safe)
# hist(data.xform$sdg16_safe)
# hist(data.imp.cust$sdg14_cpma)
# hist(data.xform$sdg14_cpma)
# hist(data.imp.cust$sdg1_320pov)
# hist(data.xform$sdg1_320pov)
# hist(data.imp.cust$sdg11_transport)
# hist(data.xform$sdg11_transport)
# hist(data.imp.cust$sdg9_intuse)
# hist(data.xform$sdg9_intuse)
# hist(data.imp.cust$sdg2_obesity)
# hist(data.xform$sdg2_obesity)
# hist(data.imp.cust$sdg2_wasteihme)
# hist(data.xform$sdg2_wasteihme)
# hist(data.imp.cust$sdg2_snmi)
# hist(data.xform$sdg2_snmi)
# hist(data.imp.cust$sdg1_wpc)
# hist(data.xform$sdg1_wpc)
# hist(data.imp.cust$sdg8_empop)
# hist(data.xform$sdg8_empop)
# hist(data.imp.cust$sdg3_swb)
# hist(data.xform$sdg3_swb)
# hist(data.imp.cust$sdg17_oda)
# hist(data.xform$sdg17_oda)


# Apply transformations for pan imputed data in order of sorted abs(transformparams.pan$yj)
sort(abs(TransformParams.pan$yj))
TPs.pan$bc$GDP_PPP$lambda
data.xform.pan$GDP_PPP <- log(data.pan$GDP_PPP)
hist(data.xform.pan$GDP_PPP)
min(data.xform.pan$sdg7_ren)
TPs.pan$bc$sdg7_ren$lambda
data.xform.pan$sdg7_ren <- log(data.pan$sdg7_ren)
hist(data.xform.pan$sdg7_ren)
min(data.xform.pan$Population)
TPs.pan$bc$Population$lambda
data.xform.pan$Population <- log(data.pan$Population)
hist(data.xform.pan$Population)
TPs.pan$bc$sdg2_undernsh
hist(data.pan$sdg2_undernsh)
data.xform.pan$sdg2_undernsh<- (data.pan$sdg2_undernsh^TPs.pan$bc$sdg2_undernsh$lambda-1)/TPs.pan$bc$sdg2_undernsh$lambda
hist(data.xform.pan$sdg2_undernsh) #still bad. The SDR database imputed too many values at 2.5%
#HIV left untransformed. Didn't get a transform parameter from either bc or yj
hist(data.pan$sdg3_hiv)
TPs.pan$bc$sdg3_lifee
hist(data.pan$sdg3_lifee)
data.xform.pan$sdg3_lifee<- (data.pan$sdg3_lifee^TPs.pan$bc$sdg3_lifee$lambda-1)/TPs.pan$bc$sdg3_lifee$lambda
hist(data.xform.pan$sdg3_lifee)
#no change to births
# TPs.pan$bc$sdg3_births
# hist(data.pan$sdg3_births)
# data.xform.pan$sdg3_births<- (data.pan$sdg3_births^TPs.pan$bc$sdg3_births$lambda-1)/TPs.pan$bc$sdg3_births$lambda
# hist(data.xform.pan$sdg3_births)
TPs.pan$bc$sdg3_vac
hist(data.pan$sdg3_vac)
data.xform.pan$sdg3_vac<- (data.pan$sdg3_vac^TPs.pan$bc$sdg3_vac$lambda-1)/TPs.pan$bc$sdg3_vac$lambda
hist(data.xform.pan$sdg3_vac)
# These education transformations had no effect, similar distribution to births above
#TPs.pan$bc$sdg4_earlyedu$lambda
#min(data.pan$sdg4_earlyedu)
# data.xform.pan$sdg4_earlyedu <- (data.pan$sdg4_earlyedu^TPs.pan$bc$sdg4_earlyedu$lambda-1)/TPs.pan$bc$sdg4_earlyedu$lambda
# TPs.pan$bc$sdg4_primary$lambda
# data.xform.pan$sdg4_primary <- (data.pan$sdg4_primary^TPs.pan$bc$sdg4_primary$lambda-1)/TPs.pan$bc$sdg4_primary$lambda
# TPs.pan$bc$sdg4_second$lambda
# data.xform.pan$sdg4_second <- (data.pan$sdg4_second^TPs.pan$bc$sdg4_second$lambda-1)/TPs.pan$bc$sdg4_second$lambda
#no change to edat either
# TPs.pan$bc$sdg5_edat
# hist(data.pan$sdg5_edat)
# data.xform.pan$sdg5_edat<- (data.pan$sdg5_edat^TPs.pan$bc$sdg5_edat$lambda-1)/TPs.pan$bc$sdg5_edat$lambda
# hist(data.xform.pan$sdg5_edat)
TPs.pan$bc$sdg5_lfpr
hist(data.pan$sdg5_lfpr)
data.xform.pan$sdg5_lfpr<- (data.pan$sdg5_lfpr^TPs.pan$bc$sdg5_lfpr$lambda-1)/TPs.pan$bc$sdg5_lfpr$lambda
hist(data.xform.pan$sdg5_lfpr)
#No change for lfpr or safewat
# TPs.pan$bc$sdg6_safewat
# hist(data.pan$sdg6_safewat)
# data.xform.pan$sdg6_safewat<- (data.pan$sdg6_safewat^TPs.pan$bc$sdg6_safewat$lambda-1)/TPs.pan$bc$sdg6_safewat$lambda
# hist(data.xform.pan$sdg6_safewat)
# TPs.pan$bc$sdg6_safewat
# hist(data.pan$sdg6_safewat)
# data.xform.pan$sdg6_safewat<- (data.pan$sdg6_safewat^TPs.pan$bc$sdg6_safewat$lambda-1)/TPs.pan$bc$sdg6_safewat$lambda
# hist(data.xform.pan$sdg6_safewat)
TPs.pan$bc$sdg6_safesan
hist(data.pan$sdg6_safesan)
data.xform.pan$sdg6_safesan<- (data.pan$sdg6_safesan^TPs.pan$bc$sdg6_safesan$lambda-1)/TPs.pan$bc$sdg6_safesan$lambda
hist(data.xform.pan$sdg6_safesan)
#No change to elecac or cleanfuel
# TPs.pan$bc$sdg7_elecac
# hist(data.pan$sdg7_elecac)
# data.xform.pan$sdg7_elecac<- (data.pan$sdg7_elecac^TPs.pan$bc$sdg7_elecac$lambda-1)/TPs.pan$bc$sdg7_elecac$lambda
# hist(data.xform.pan$sdg7_elecac)
# TPs.pan$bc$sdg7_cleanfuel
# hist(data.pan$sdg7_cleanfuel)
# data.xform.pan$sdg7_cleanfuel<- (data.pan$sdg7_cleanfuel^TPs.pan$bc$sdg7_cleanfuel$lambda-1)/TPs.pan$bc$sdg7_cleanfuel$lambda
# hist(data.xform.pan$sdg7_cleanfuel)
TPs.pan$bc$sdg10_palma$lambda
hist(data.pan$sdg10_palma)
data.xform.pan$sdg10_palma <- (data.pan$sdg10_palma^TPs.pan$bc$sdg10_palma$lambda-1)/TPs.pan$bc$sdg10_palma$lambda
hist(data.xform.pan$sdg10_palma)
#No change for pipedwat
# TPs.pan$bc$sdg11_pipedwat
# hist(data.pan$sdg11_pipedwat)
# data.xform.pan$sdg11_pipedwat<- (data.pan$sdg11_pipedwat^TPs.pan$bc$sdg11_pipedwat$lambda-1)/TPs.pan$bc$sdg11_pipedwat$lambda
# hist(data.xform.pan$sdg11_pipedwat)
TPs.pan$bc$sdg15_redlist$lambda
hist(data.pan$sdg15_redlist)
data.xform.pan$sdg15_redlist <- (data.pan$sdg15_redlist^TPs.pan$bc$sdg15_redlist$lambda-1)/TPs.pan$bc$sdg15_redlist$lambda
hist(data.xform.pan$sdg15_redlist)

min(data.xform.pan$sdg3_fertility)
TPs.pan$bc$sdg3_fertility$lambda
TransformParams.pan[["yj"]][["sdg3_fertility"]]
data.xform.pan$sdg3_fertility <- log(data.pan$sdg3_fertility+1)
hist(data.xform.pan$sdg3_fertility)
min(data.xform.pan$sdg9_patents)
TransformParams.pan[["yj"]][["sdg9_patents"]]
data.xform.pan$sdg9_patents <- ((data.pan$sdg9_patents+1)^(TransformParams.pan[["yj"]][["sdg9_patents"]])-1)/(TransformParams.pan[["yj"]][["sdg9_patents"]])
min(data.xform.pan$sdg9_rdex)
TransformParams.pan[["yj"]][["sdg9_rdex"]]
data.xform.pan$sdg9_rdex <- ((data.pan$sdg9_rdex+1)^(TransformParams.pan[["yj"]][["sdg9_rdex"]])-1)/(TransformParams.pan[["yj"]][["sdg9_rdex"]])
min(data.xform.pan$sdg3_matmort)
TransformParams.pan[["yj"]][["sdg3_matmort"]]
data.xform.pan$sdg3_matmort <- ((data.pan$sdg3_matmort+1)^(TransformParams.pan[["yj"]][["sdg3_matmort"]])-1)/(TransformParams.pan[["yj"]][["sdg3_matmort"]])
TransformParams.pan[["yj"]][["sdg16_prison"]]
data.xform.pan$sdg16_prison <- ((data.pan$sdg16_prison+1)^(TransformParams.pan[["yj"]][["sdg16_prison"]])-1)/(TransformParams.pan[["yj"]][["sdg16_prison"]])
TransformParams.pan[["yj"]][["sdg8_yneet"]]
data.xform.pan$sdg8_yneet <- ((data.pan$sdg8_yneet+1)^(TransformParams.pan[["yj"]][["sdg8_yneet"]])-1)/(TransformParams.pan[["yj"]][["sdg8_yneet"]])
min(data.xform.pan$sdg8_impacc)
TransformParams.pan[["yj"]][["sdg8_impacc"]]
data.xform.pan$sdg8_impacc <- ((data.pan$sdg8_impacc+1)^(TransformParams.pan[["yj"]][["sdg8_impacc"]])-1)/(TransformParams.pan[["yj"]][["sdg8_impacc"]])
TransformParams.pan[["yj"]][["sdg2_trophic"]]
data.xform.pan$sdg2_trophic <- ((data.pan$sdg2_trophic+1)^(TransformParams.pan[["yj"]][["sdg2_trophic"]])-1)/(TransformParams.pan[["yj"]][["sdg2_trophic"]])
TransformParams.pan[["yj"]][["sdg11_pm25"]]
data.xform.pan$sdg11_pm25 <- ((data.pan$sdg11_pm25+1)^(TransformParams.pan[["yj"]][["sdg11_pm25"]])-1)/(TransformParams.pan[["yj"]][["sdg11_pm25"]])
TransformParams.pan[["yj"]][["sdg10_elder"]]
data.xform.pan$sdg10_elder <- ((data.pan$sdg10_elder+1)^(TransformParams.pan[["yj"]][["sdg10_elder"]])-1)/(TransformParams.pan[["yj"]][["sdg10_elder"]])
TransformParams.pan[["yj"]][["sdg11_transport"]]
data.xform.pan$sdg11_transport <- ((data.pan$sdg11_transport+1)^(TransformParams.pan[["yj"]][["sdg11_transport"]])-1)/(TransformParams.pan[["yj"]][["sdg11_transport"]])
TransformParams.pan[["yj"]][["sdg16_rsf"]]
data.xform.pan$sdg16_rsf <- ((data.pan$sdg16_rsf+1)^(TransformParams.pan[["yj"]][["sdg16_rsf"]])-1)/(TransformParams.pan[["yj"]][["sdg16_rsf"]])
TransformParams.pan[["yj"]][["sdg6_scarcew"]]
data.xform.pan$sdg6_scarcew <- ((data.pan$sdg6_scarcew+1)^(TransformParams.pan[["yj"]][["sdg6_scarcew"]])-1)/(TransformParams.pan[["yj"]][["sdg6_scarcew"]])
TransformParams.pan[["yj"]][["sdg1_oecdpov"]]
data.xform.pan$sdg1_oecdpov <- ((data.pan$sdg1_oecdpov+1)^(TransformParams.pan[["yj"]][["sdg1_oecdpov"]])-1)/(TransformParams.pan[["yj"]][["sdg1_oecdpov"]])
TransformParams.pan[["yj"]][["sdg17_govex"]]
data.xform.pan$sdg17_govex <- ((data.pan$sdg17_govex+1)^(TransformParams.pan[["yj"]][["sdg17_govex"]])-1)/(TransformParams.pan[["yj"]][["sdg17_govex"]])
TransformParams.pan[["yj"]][["sdg5_paygap"]]
data.xform.pan$sdg5_paygap <- ((data.pan$sdg5_paygap+1)^(TransformParams.pan[["yj"]][["sdg5_paygap"]])-1)/(TransformParams.pan[["yj"]][["sdg5_paygap"]])
TransformParams.pan[["yj"]][["sdg13_co2import"]]
data.xform.pan$sdg13_co2import <- ((data.pan$sdg13_co2import+1)^(TransformParams.pan[["yj"]][["sdg13_co2import"]])-1)/(TransformParams.pan[["yj"]][["sdg13_co2import"]])
TransformParams.pan[["yj"]][["sdg2_stuntihme"]]
data.xform.pan$sdg2_stuntihme <- ((data.pan$sdg2_stuntihme+1)^(TransformParams.pan[["yj"]][["sdg2_stuntihme"]])-1)/(TransformParams.pan[["yj"]][["sdg2_stuntihme"]])
TransformParams.pan[["yj"]][["sdg3_tb"]]
data.xform.pan$sdg3_tb <- ((data.pan$sdg3_tb+1)^(TransformParams.pan[["yj"]][["sdg3_tb"]])-1)/(TransformParams.pan[["yj"]][["sdg3_tb"]])
TransformParams.pan[["yj"]][["sdg14_cleanwat"]]
data.xform.pan$sdg14_cleanwat <- ((data.pan$sdg14_cleanwat+1)^(TransformParams.pan[["yj"]][["sdg14_cleanwat"]])-1)/(TransformParams.pan[["yj"]][["sdg14_cleanwat"]])
TransformParams.pan[["yj"]][["sdg7_co2twh"]]
data.xform.pan$sdg7_co2twh <- ((data.pan$sdg7_co2twh+1)^(TransformParams.pan[["yj"]][["sdg7_co2twh"]])-1)/(TransformParams.pan[["yj"]][["sdg7_co2twh"]])
TransformParams.pan[["yj"]][["sdg5_parl"]]
data.xform.pan$sdg5_parl <- ((data.pan$sdg5_parl+1)^(TransformParams.pan[["yj"]][["sdg5_parl"]])-1)/(TransformParams.pan[["yj"]][["sdg5_parl"]])
TransformParams.pan[["yj"]][["sdg11_rentover"]]
data.xform.pan$sdg11_rentover <- ((data.pan$sdg11_rentover+1)^(TransformParams.pan[["yj"]][["sdg11_rentover"]])-1)/(TransformParams.pan[["yj"]][["sdg11_rentover"]])
TransformParams.pan[["yj"]][["sdg2_crlyld"]]
data.xform.pan$sdg2_crlyld <- ((data.pan$sdg2_crlyld+1)^(TransformParams.pan[["yj"]][["sdg2_crlyld"]])-1)/(TransformParams.pan[["yj"]][["sdg2_crlyld"]])
TransformParams.pan[["yj"]][["sdg3_incomeg"]]
data.xform.pan$sdg3_incomeg <- ((data.pan$sdg3_incomeg+1)^(TransformParams.pan[["yj"]][["sdg3_incomeg"]])-1)/(TransformParams.pan[["yj"]][["sdg3_incomeg"]])
TransformParams.pan[["yj"]][["sdg9_articles"]]
data.xform.pan$sdg9_articles <- ((data.pan$sdg9_articles+1)^(TransformParams.pan[["yj"]][["sdg9_articles"]])-1)/(TransformParams.pan[["yj"]][["sdg9_articles"]])
TransformParams.pan[["yj"]][["sdg14_trawl"]]
data.xform.pan$sdg14_trawl <- ((data.pan$sdg14_trawl+1)^(TransformParams.pan[["yj"]][["sdg14_trawl"]])-1)/(TransformParams.pan[["yj"]][["sdg14_trawl"]])
TransformParams.pan[["yj"]][["sdg14_fishstocks"]]
data.xform.pan$sdg14_fishstocks <- ((data.pan$sdg14_fishstocks+1)^(TransformParams.pan[["yj"]][["sdg14_fishstocks"]])-1)/(TransformParams.pan[["yj"]][["sdg14_fishstocks"]])
TransformParams.pan[["yj"]][["sdg9_rdres"]]
data.xform.pan$sdg14_fishstocks <- ((data.pan$sdg14_fishstocks+1)^(TransformParams.pan[["yj"]][["sdg14_fishstocks"]])-1)/(TransformParams.pan[["yj"]][["sdg14_fishstocks"]])
TransformParams.pan[["yj"]][["sdg1_320pov"]]
data.xform.pan$sdg1_320pov <- ((data.pan$sdg1_320pov+1)^(TransformParams.pan[["yj"]][["sdg1_320pov"]])-1)/(TransformParams.pan[["yj"]][["sdg1_320pov"]])
TransformParams.pan[["yj"]][["sdg3_neonat"]]
data.xform.pan$sdg3_neonat <- ((data.pan$sdg3_neonat+1)^(TransformParams.pan[["yj"]][["sdg3_neonat"]])-1)/(TransformParams.pan[["yj"]][["sdg3_neonat"]])
TransformParams.pan[["yj"]][["sdg3_smoke"]]
data.xform.pan$sdg3_smoke <- ((data.pan$sdg3_smoke+1)^(TransformParams.pan[["yj"]][["sdg3_smoke"]])-1)/(TransformParams.pan[["yj"]][["sdg3_smoke"]])
TransformParams.pan[["yj"]][["sdg16_homicides"]]
data.xform.pan$sdg16_homicides <- ((data.pan$sdg16_homicides+1)^(TransformParams.pan[["yj"]][["sdg16_homicides"]])-1)/(TransformParams.pan[["yj"]][["sdg16_homicides"]])
TransformParams.pan[["yj"]][["sdg9_netacc"]]
data.xform.pan$sdg9_netacc <- ((data.pan$sdg9_netacc+1)^(TransformParams.pan[["yj"]][["sdg9_netacc"]])-1)/(TransformParams.pan[["yj"]][["sdg9_netacc"]])
TransformParams.pan[["yj"]][["sdg3_u5mort"]]
data.xform.pan$sdg3_u5mort <- ((data.pan$sdg3_u5mort+1)^(TransformParams.pan[["yj"]][["sdg3_u5mort"]])-1)/(TransformParams.pan[["yj"]][["sdg3_u5mort"]])
TransformParams.pan[["yj"]][["sdg4_tertiary"]]
data.xform.pan$sdg4_tertiary <- ((data.pan$sdg4_tertiary+1)^(TransformParams.pan[["yj"]][["sdg4_tertiary"]])-1)/(TransformParams.pan[["yj"]][["sdg4_tertiary"]])
TransformParams.pan[["yj"]][["sdg15_cpfa"]]
data.xform.pan$sdg15_cpfa <- ((data.pan$sdg15_cpfa+1)^(TransformParams.pan[["yj"]][["sdg15_cpfa"]])-1)/(TransformParams.pan[["yj"]][["sdg15_cpfa"]])
TransformParams.pan[["yj"]][["sdg15_cpta"]]
data.xform.pan$sdg15_cpta <- ((data.pan$sdg15_cpta+1)^(TransformParams.pan[["yj"]][["sdg15_cpta"]])-1)/(TransformParams.pan[["yj"]][["sdg15_cpta"]])
TransformParams.pan[["yj"]][["sdg14_cpma"]]
data.xform.pan$sdg14_cpma <- ((data.pan$sdg14_cpma+1)^(TransformParams.pan[["yj"]][["sdg14_cpma"]])-1)/(TransformParams.pan[["yj"]][["sdg14_cpma"]])
TransformParams.pan[["yj"]][["sdg10_adjgini"]]
data.xform.pan$sdg10_adjgini <- ((data.pan$sdg10_adjgini+1)^(TransformParams.pan[["yj"]][["sdg10_adjgini"]])-1)/(TransformParams.pan[["yj"]][["sdg10_adjgini"]])
TransformParams.pan[["yj"]][["sdg2_wasteihme"]]
data.xform.pan$sdg2_wasteihme <- ((data.pan$sdg2_wasteihme+1)^(TransformParams.pan[["yj"]][["sdg2_wasteihme"]])-1)/(TransformParams.pan[["yj"]][["sdg2_wasteihme"]])
TransformParams.pan[["yj"]][["sdg1_wpc"]]
data.xform.pan$sdg1_wpc <- ((data.pan$sdg1_wpc+1)^(TransformParams.pan[["yj"]][["sdg1_wpc"]])-1)/(TransformParams.pan[["yj"]][["sdg1_wpc"]])
TransformParams.pan[["yj"]][["sdg9_intuse"]]
data.xform.pan$sdg9_intuse <- ((data.pan$sdg9_intuse+1)^(TransformParams.pan[["yj"]][["sdg9_intuse"]])-1)/(TransformParams.pan[["yj"]][["sdg9_intuse"]])
TransformParams.pan[["yj"]][["sdg16_safe"]]
data.xform.pan$sdg16_safe <- ((data.pan$sdg16_safe+1)^(TransformParams.pan[["yj"]][["sdg16_safe"]])-1)/(TransformParams.pan[["yj"]][["sdg16_safe"]])
TransformParams.pan[["yj"]][["sdg2_obesity"]]
data.xform.pan$sdg2_obesity <- ((data.pan$sdg2_obesity+1)^(TransformParams.pan[["yj"]][["sdg2_obesity"]])-1)/(TransformParams.pan[["yj"]][["sdg2_obesity"]])
TransformParams.pan[["yj"]][["sdg16_cpi"]]
data.xform.pan$sdg16_cpi <- ((data.pan$sdg16_cpi+1)^(TransformParams.pan[["yj"]][["sdg16_cpi"]])-1)/(TransformParams.pan[["yj"]][["sdg16_cpi"]])
TransformParams.pan[["yj"]][["sdg2_snmi"]]
data.xform.pan$sdg2_snmi <- ((data.pan$sdg2_snmi+1)^(TransformParams.pan[["yj"]][["sdg2_snmi"]])-1)/(TransformParams.pan[["yj"]][["sdg2_snmi"]])
TransformParams.pan[["yj"]][["sdg8_empop"]]
data.xform.pan$sdg8_empop <- ((data.pan$sdg8_empop+1)^(TransformParams.pan[["yj"]][["sdg8_empop"]])-1)/(TransformParams.pan[["yj"]][["sdg8_empop"]])
TransformParams.pan[["yj"]][["sdg17_oda"]]
data.xform.pan$sdg17_oda <- ((data.pan$sdg17_oda+1)^(TransformParams.pan[["yj"]][["sdg17_oda"]])-1)/(TransformParams.pan[["yj"]][["sdg17_oda"]])
TransformParams.pan[["yj"]][["sdg13_co2kgPerGDPPPP"]]
data.xform.pan$sdg13_co2kgPerGDPPPP <- ((data.pan$sdg13_co2kgPerGDPPPP+1)^(TransformParams.pan[["yj"]][["sdg13_co2kgPerGDPPPP"]])-1)/(TransformParams.pan[["yj"]][["sdg13_co2kgPerGDPPPP"]])



hist(data.pan$sdg8_yneet)
hist(data.xform.pan$sdg8_yneet)

###################
##### Scaling #####
###################

#scaling untransformed custom imputed data
summary(data.imp.cust)
indx[3]<-F #switch year to be false so it is not scaled
ScaleParams <- preProcess(data.imp.cust[,indx], method=c("range"), rangeBounds = c(0,1))
#ScaleParams <- preProcess(data.imp.cust[,indx], method=c("center","scale"))
data.imp.cust[,indx]<-predict(ScaleParams, data.imp.cust[,indx])
summary(data.imp.cust)
data.imp.final<-data.imp.cust

#Scaling transformed custom imputed data
summary(data.xform)
ScaleParams <- preProcess(data.xform[,indx], method=c("range"), rangeBounds = c(0,1))
#ScaleParams <- preProcess(data.xform[,indx], method=c("center","scale"))
data.xform[,indx]<-predict(ScaleParams, data.xform[,indx])
summary(data.xform)
data.imp.final.xf<-data.xform

which_miss<-which(is.na(data.imp$sdg7_cleanfuel))
min(mean((data.imp$GDP_PPP[which_miss]/data.imp$Population[which_miss]))-mean((data.imp$GDP_PPP[-which_miss]/data.imp$Population[-which_miss])))

#scaling untransformed pan imputed data
summary(data.pan)
indx.pan[2]<-F #switch year to be false so it is not scaled
ScaleParams <- preProcess(data.pan[,indx.pan], method=c("range"), rangeBounds = c(0,1))
#ScaleParams <- preProcess(data.pan[,indx.pan], method=c("center","scale"))
data.pan[,indx.pan]<-predict(ScaleParams, data.pan[,indx.pan])
summary(data.pan)
data.pan.final<-data.pan

# the transformations were never applied to the data that was used in the models due to an error below. However, the transformations weren't working anyway. Creating NaN and Inf

#scaling transformed pan imputed data
summary(data.xform.pan)
indx.pan[2]<-F #switch year to be false so it is not scaled
ScaleParams <- preProcess(data.xform.pan[,indx.pan], method=c("range"), rangeBounds = c(0,1))
#ScaleParams <- preProcess(data.pan[,indx.pan], method=c("center","scale"))
data.xform.pan[,indx.pan]<-predict(ScaleParams, data.xform.pan[,indx.pan]) #This line has the error that kept transformations from being applied. Transformations were applied to data.xform.pan, then overwritten
summary(data.xform.pan)
data.pan.final.xf<-data.xform.pan

# Check distributions #
par(mfrow=c(1,1),mar=c(4.1,4.1,3.1,2.1))
hist(data.pan.final.xf$GDP_PPP)
hist(data.pan.final.xf$sdg7_ren)
hist(data.pan.final.xf$Population)
hist(data.pan.final.xf$sdg6_scarcew)
hist(data.pan.final.xf$sdg9_rdex)
hist(data.pan.final.xf$sdg8_yneet)
hist(data.pan.final.xf$sdg9_patents)
hist(data.pan.final.xf$sdg16_rsf)
hist(data.pan.final.xf$sdg2_trophic)
hist(data.pan.final.xf$sdg10_elder)
hist(data.pan.final.xf$sdg11_rentover)
hist(data.pan.final.xf$sdg3_fertility)
hist(data.pan.final.xf$sdg16_prison)
hist(data.pan.final.xf$sdg11_pm25)
hist(data.pan.final.xf$sdg8_impacc)
hist(data.pan.final.xf$sdg17_govex)
hist(data.pan.final.xf$sdg5_paygap)
hist(data.pan.final.xf$sdg3_matmort)
hist(data.pan.final.xf$sdg13_co2import)
hist(data.pan.final.xf$sdg3_tb)
hist(data.pan.final.xf$sdg3_incomeg)
hist(data.pan.final.xf$sdg14_fishstocks)
hist(data.pan.final.xf$sdg5_parl)
hist(data.pan.final.xf$sdg2_crlyld)
hist(data.pan.final.xf$sdg9_articles)
hist(data.pan.final.xf$sdg4_earlyedu)
hist(data.pan.final.xf$sdg2_stuntihme)
hist(data.pan.final.xf$sdg1_oecdpov)
hist(data.pan.final.xf$sdg9_rdres) #already normal
hist(data.pan.final.xf$sdg7_co2twh)
hist(data.pan.final.xf$sdg14_trawl)
hist(data.pan.final.xf$sdg3_smoke) #already normal
hist(data.pan.final.xf$sdg9_netacc)
hist(data.pan.final.xf$sdg3_neonat)
hist(data.pan.final.xf$sdg10_adjgini)
hist(data.pan.final.xf$sdg14_cleanwat)
hist(data.pan.final.xf$sdg3_u5mort)
hist(data.pan.final.xf$sdg15_cpfa)
#
hist(data.pan.final.xf$sdg4_tertiary)
hist(data.pan.final.xf$sdg16_cpi)
hist(data.pan.final.xf$sdg15_cpta)
hist(data.pan.final.xf$sdg16_homicides)
hist(data.pan.final.xf$sdg16_safe)
hist(data.pan.final.xf$sdg14_cpma)
#
hist(data.pan.final.xf$sdg1_320pov)
#
hist(data.pan.final.xf$sdg11_transport)
hist(data.pan.final.xf$sdg9_intuse)
hist(data.pan.final.xf$sdg2_obesity)
hist(data.pan.final.xf$sdg2_wasteihme)
hist(data.pan.final.xf$sdg2_snmi)
#
hist(data.pan.final.xf$sdg1_wpc)

#
hist(data.pan.final.xf$sdg10_palma)
hist(data.pan$sdg10_palma)



#####################################################################################################################################
##### Lasso #####

# #Lasso using glmnet
# #Data is transformed and imputed using the custom quadratic interpolation (poor)
library(glmnet)
# data.frame(colnames(data.imp.final.xf))
# #l_output_vars<-c(40:43,56,61)
# vars_nomiss<-which(colSums(is.na(data.imp.final.xf)) == 0)
# vars_sdg13<-vars_nomiss[c(-1:-3,-42)] #CO2 is 42nd member of the vars_nomiss vector
# lasso_input_sdg13 <- data.imp.final.xf[,vars_sdg13] #Use all vars except year, country, id, co2 target
# lasso_target_sdg13 <- data.imp.final.xf[,60] #CO2 is the 60th column of data.imp.final.xf dataframe
# nvars = ncol(lasso_input_sdg13)
# dfmax = nvars+1
# nobs<-nrow(data.imp.final.xf)
#   
# Lasso_sdg13_co2kgPerGDPPPP<-glmnet(
#   lasso_input_sdg13,
#   lasso_target_sdg13,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg13_co2kgPerGDPPPP, s = c(0.01,0.02)) #alter s(lambda to get a resonable number of vars)
# 
# #Lasso on sdg1_oecdpov which currently has low R squared value
# targ1col<-grep("^(sdg1_oecdpov)",colnames(data.imp.final.xf))
# targ1colnm<-grep("^(sdg1_oecdpov)",names(vars_nomiss))
# vars_sdg1<-vars_nomiss[c(-1:-3,-targ1colnm)] #oecdpov is 8th member of the vars_nomiss vector
# lasso_input_sdg1 <- data.imp.final.xf[,vars_sdg1] #Use all vars except year, country, id, target
# lasso_target_sdg1 <- data.imp.final.xf[,targ1col] #oecdpov is the 8th column of data.imp.final.xf dataframe
# Lasso_sdg1_oecdpov<-glmnet(
#   lasso_input_sdg1,
#   lasso_target_sdg1,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg1_oecdpov, s = c(0.01,0.02))
# plot(Lasso_sdg1_oecdpov)
# 
# #Lasso on sdg2_undernsh
# targ2col<-grep("^(sdg2_undernsh)",colnames(data.imp.final.xf))
# targ2colnm<-grep("^(sdg2_undernsh)",names(vars_nomiss))
# vars_sdg2<-vars_nomiss[c(-1:-3,-targ2colnm)] 
# lasso_input_sdg2 <- data.imp.final.xf[,vars_sdg2] #Use all vars except year, country, id, target
# lasso_target_sdg2 <- data.imp.final.xf[,targ2col] 
# Lasso_sdg2_undernsh<-glmnet(
#   lasso_input_sdg2,
#   lasso_target_sdg2,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg2_undernsh, s = c(0.01,0.02))
# 
# #Lasso on sdg3_lifee
# targ3col<-grep("^(sdg3_lifee)",colnames(data.imp.final.xf))
# targ3colnm<-grep("^(sdg3_lifee)",names(vars_nomiss))
# vars_sdg3<-vars_nomiss[c(-1:-3,-targ3colnm)] 
# lasso_input_sdg3 <- data.imp.final.xf[,vars_sdg3] #Use all vars except year, country, id, target
# lasso_target_sdg3 <- data.imp.final.xf[,targ3col] 
# Lasso_sdg3_lifee<-glmnet(
#   lasso_input_sdg3,
#   lasso_target_sdg3,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg3_lifee, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg4_tertiary
# targ4col<-grep("^(sdg4_tertiary)",colnames(data.imp.final.xf))
# targ4colnm<-grep("^(sdg4_tertiary)",names(vars_nomiss))
# vars_sdg4<-vars_nomiss[c(-1:-3,-targ4colnm)] 
# lasso_input_sdg4 <- data.imp.final.xf[,vars_sdg4] #Use all vars except year, country, id, target
# lasso_target_sdg4 <- data.imp.final.xf[,targ4col] 
# Lasso_sdg4_tertiary<-glmnet(
#   lasso_input_sdg4,
#   lasso_target_sdg4,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg4_tertiary, s = c(0.01,0.02,0.03))
# 
# #Lasso on sdg5_parl
# targ5col<-grep("^(sdg5_parl)",colnames(data.imp.final.xf))
# targ5colnm<-grep("^(sdg5_parl)",names(vars_nomiss))
# vars_sdg5<-vars_nomiss[c(-1:-3,-targ5colnm)] 
# lasso_input_sdg5 <- data.imp.final.xf[,vars_sdg5] #Use all vars except year, country, id, target
# lasso_target_sdg5 <- data.imp.final.xf[,targ5col] 
# Lasso_sdg5_parl<-glmnet(
#   lasso_input_sdg5,
#   lasso_target_sdg5,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg5_parl, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg6_safesan
# targ6col<-grep("^(sdg6_safesan)",colnames(data.imp.final.xf))
# targ6colnm<-grep("^(sdg6_safesan)",names(vars_nomiss))
# vars_sdg6<-vars_nomiss[c(-1:-3,-targ6colnm)] 
# lasso_input_sdg6 <- data.imp.final.xf[,vars_sdg6] #Use all vars except year, country, id, target
# lasso_target_sdg6 <- data.imp.final.xf[,targ6col] 
# Lasso_sdg6_safesan<-glmnet(
#   lasso_input_sdg6,
#   lasso_target_sdg6,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg6_safesan, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg7_ren
# targ7col<-grep("^(sdg7_ren)",colnames(data.imp.final.xf))
# targ7colnm<-grep("^(sdg7_ren)",names(vars_nomiss))
# vars_sdg7<-vars_nomiss[c(-1:-3,-targ7colnm)] 
# lasso_input_sdg7 <- data.imp.final.xf[,vars_sdg7] #Use all vars except year, country, id, target
# lasso_target_sdg7 <- data.imp.final.xf[,targ7col] 
# Lasso_sdg7_ren<-glmnet(
#   lasso_input_sdg7,
#   lasso_target_sdg7,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg7_ren, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg8_empop
# targ8col<-grep("^(sdg8_empop)",colnames(data.imp.final.xf))
# targ8colnm<-grep("^(sdg8_empop)",names(vars_nomiss))
# vars_sdg8<-vars_nomiss[c(-1:-3,-targ8colnm)] 
# lasso_input_sdg8 <- data.imp.final.xf[,vars_sdg8] #Use all vars except year, country, id, target
# lasso_target_sdg8 <- data.imp.final.xf[,targ8col] 
# Lasso_sdg8_empop<-glmnet(
#   lasso_input_sdg8,
#   lasso_target_sdg8,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg8_empop, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg9_rdex
# targ9col<-grep("^(sdg9_rdex)",colnames(data.imp.final.xf))
# targ9colnm<-grep("^(sdg9_rdex)",names(vars_nomiss))
# vars_sdg9<-vars_nomiss[c(-1:-3,-targ9colnm)] 
# lasso_input_sdg9 <- data.imp.final.xf[,vars_sdg9] #Use all vars except year, country, id, target
# lasso_target_sdg9 <- data.imp.final.xf[,targ9col] 
# Lasso_sdg9_rdex<-glmnet(
#   lasso_input_sdg9,
#   lasso_target_sdg9,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg9_rdex, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg10_palma
# targ10col<-grep("^(sdg10_palma)",colnames(data.imp.final.xf))
# targ10colnm<-grep("^(sdg10_palma)",names(vars_nomiss))
# vars_sdg10<-vars_nomiss[c(-1:-3,-targ10colnm)] 
# lasso_input_sdg10 <- data.imp.final.xf[,vars_sdg10] #Use all vars except year, country, id, target
# lasso_target_sdg10 <- data.imp.final.xf[,targ10col] 
# Lasso_sdg10_palma<-glmnet(
#   lasso_input_sdg10,
#   lasso_target_sdg10,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg10_palma, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg11_pm25
# targ11col<-grep("^(sdg11_pm25)",colnames(data.imp.final.xf))
# targ11colnm<-grep("^(sdg11_pm25)",names(vars_nomiss))
# vars_sdg11<-vars_nomiss[c(-1:-3,-targ11colnm)] 
# lasso_input_sdg11 <- data.imp.final.xf[,vars_sdg11] #Use all vars except year, country, id, target
# lasso_target_sdg11 <- data.imp.final.xf[,targ11col] 
# Lasso_sdg11_pm25<-glmnet(
#   lasso_input_sdg11,
#   lasso_target_sdg11,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg11_pm25, s = c(0.01,0.02,0.05))
# 
# #No sdg12 target
# #sdg13 above
# 
# #Lasso on sdg14_cpma. Not working because there are too many NA's
# targ14col<-grep("^(sdg14_cpma)",colnames(data.imp.final.xf))
# targ14colnm<-grep("^(sdg14_cpma)",names(vars_nomiss))
# vars_sdg14<-vars_nomiss[c(-1:-3,-targ14colnm)] 
# lasso_input_sdg14 <- data.imp.final.xf[,vars_sdg14] #Use all vars except year, country, id, target
# lasso_target_sdg14 <- data.imp.final.xf[,targ14col] 
# Lasso_sdg14_cpma<-glmnet(
#   lasso_input_sdg14,
#   lasso_target_sdg14,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg14_cpma, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg15_redlist
# targ15col<-grep("^(sdg15_redlist)",colnames(data.imp.final.xf))
# targ15colnm<-grep("^(sdg15_redlist)",names(vars_nomiss))
# vars_sdg15<-vars_nomiss[c(-1:-3,-targ15colnm)] 
# lasso_input_sdg15 <- data.imp.final.xf[,vars_sdg15] #Use all vars except year, country, id, target
# lasso_target_sdg15 <- data.imp.final.xf[,targ15col] 
# Lasso_sdg15_redlist<-glmnet(
#   lasso_input_sdg15,
#   lasso_target_sdg15,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg15_redlist, s = c(0.01,0.02,0.05))
# 
# #Lasso on sdg16_homicides
# vars_sdg16<-vars_nomiss[c(-1:-3,-47)] #homicides is 47th member of the vars_nomiss vector
# lasso_input_sdg16 <- data.imp.final.xf[,vars_sdg16] #Use all vars except year, country, id, target
# lasso_target_sdg16 <- data.imp.final.xf[,69] #homicides is the 69th column of data.imp.final.xf dataframe
# Lasso_sdg16_homicides<-glmnet(
#   lasso_input_sdg16,
#   lasso_target_sdg16,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg16_homicides, s = c(0.01,0.015,0.02))
# 
# #Lasso on sdg17_govex
# targ17col<-grep("^(sdg17_govex)",colnames(data.imp.final.xf))
# targ17colnm<-grep("^(sdg17_govex)",names(vars_nomiss))
# vars_sdg17<-vars_nomiss[c(-1:-3,-targ17colnm)] 
# lasso_input_sdg17 <- data.imp.final.xf[,vars_sdg17] #Use all vars except year, country, id, target
# lasso_target_sdg17 <- data.imp.final.xf[,targ17col] 
# Lasso_sdg17_govex<-glmnet(
#   lasso_input_sdg17,
#   lasso_target_sdg17,
#   family = c("gaussian"))
# 
# coef(Lasso_sdg17_govex, s = c(0.01,0.02,0.05))

###########################
#### Pan Imputed Lasso ####
###########################

#Lasso on pan imputed data, sdg13 target
data.frame(colnames(data.pan.final.xf))

vars_sdg13_pan<-c(-1:-2,-59) #co2 is 59th member of the vars_sdg13_pan
lasso_input_sdg13_pan <- data.pan.final.xf[,vars_sdg13_pan] #Use all vars except year, country, id, co2 target
lasso_target_sdg13_pan <- data.pan.final.xf[,59]
nvars = ncol(lasso_input_sdg13_pan)
dfmax = nvars+1
nobs<-nrow(data.pan.final.xf)

Lasso_sdg13_co2kgPerGDPPPP_pan<-glmnet(
  lasso_input_sdg13_pan,
  lasso_target_sdg13_pan,
  family = c("gaussian"))

Melt(coef(Lasso_sdg13_co2kgPerGDPPPP_pan, s = 0.03)) #alter s(lambda to get a resonable number of vars)
#Coefficients are very small. seems like it needs transformation to be comparable and to fit gaussian family dist

#### SDG1 OECDpov ####
targ1col<-grep("^(sdg1_oecdpov)",colnames(data.pan.final.xf))
lasso_input_sdg1 <- data.pan.final.xf[,c(-1:-2,-targ1col)] #Use all vars except year, country, id, target
lasso_target_sdg1 <- data.pan.final.xf[,targ1col] #oecdpov is the 8th column of data.imp.final.xf dataframe
Lasso_sdg1_oecdpov<-glmnet(
  lasso_input_sdg1,
  lasso_target_sdg1,
  family = c("gaussian"))

Melt(coef(Lasso_sdg1_oecdpov, s = c(0.015)))
#plot(Lasso_sdg1_oecdpov)

#### SDG2 Undernsh ####
targ2col<-grep("^(sdg2_undernsh)",colnames(data.pan.final.xf))
lasso_input_sdg2 <- data.pan.final.xf[,c(-1:-2,-targ2col)] #Use all vars except year, country, id, target
lasso_target_sdg2 <- data.pan.final.xf[,targ2col] 
Lasso_sdg2_undernsh<-glmnet(
  lasso_input_sdg2,
  lasso_target_sdg2,
  family = c("gaussian"))

Melt(coef(Lasso_sdg2_undernsh, s = c(0.0115)))
#plot(Lasso_sdg2_undernsh)

#### SDG2 Wasting ####
targ2.2col<-grep("^(sdg2_wasteihme)",colnames(data.pan.final.xf))
lasso_input_sdg2.2 <- data.pan.final.xf[,c(-1:-2,-targ2.2col)] #Use all vars except year, country, id, target
lasso_target_sdg2.2 <- data.pan.final.xf[,targ2.2col] 
Lasso_sdg2.2_wasteihme<-glmnet(
  lasso_input_sdg2.2,
  lasso_target_sdg2.2,
  family = c("gaussian"))

Melt(coef(Lasso_sdg2.2_wasteihme, s = c(0.025)))
#plot(Lasso_sdg2_undernsh)


#### SDG3 lifee ####
targ3col<-grep("^(sdg3_lifee)",colnames(data.pan.final.xf))
lasso_input_sdg3 <- data.pan.final.xf[,c(-1:-2,-targ3col)] #Use all vars except year, country, id, target
lasso_target_sdg3 <- data.pan.final.xf[,targ3col] 
Lasso_sdg3_lifee<-glmnet(
  lasso_input_sdg3,
  lasso_target_sdg3,
  family = c("gaussian"))

coef(Lasso_sdg3_lifee, s = c(0.01,0.02))
#plot(Lasso_sdg3_lifee)

#### SDG4 tertiary ####
targ4col<-grep("^(sdg4_tertiary)",colnames(data.pan.final.xf))
lasso_input_sdg4 <- data.pan.final.xf[,c(-1:-2,-targ4col)] #Use all vars except year, country, id, target
lasso_target_sdg4 <- data.pan.final.xf[,targ4col] 
Lasso_sdg4_tertiary<-glmnet(
  lasso_input_sdg4,
  lasso_target_sdg4,
  family = c("gaussian"))

coef(Lasso_sdg4_tertiary, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg4_tertiary)

#### SDG5 parl ####
targ5col<-grep("^(sdg5_parl)",colnames(data.pan.final.xf))
lasso_input_sdg5 <- data.pan.final.xf[,c(-1:-2,-targ5col)] #Use all vars except year, country, id, target
lasso_target_sdg5 <- data.pan.final.xf[,targ5col] 
Lasso_sdg5_parl<-glmnet(
  lasso_input_sdg5,
  lasso_target_sdg5,
  family = c("gaussian"))

coef(Lasso_sdg5_parl, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg5_parl)

#### SDG6 safesan ####
targ6col<-grep("^(sdg6_safesan)",colnames(data.pan.final.xf))
lasso_input_sdg6 <- data.pan.final.xf[,c(-1:-2,-targ6col)] #Use all vars except year, country, id, target
lasso_target_sdg6 <- data.pan.final.xf[,targ6col] 
Lasso_sdg6_safesan<-glmnet(
  lasso_input_sdg6,
  lasso_target_sdg6,
  family = c("gaussian"))

coef(Lasso_sdg6_safesan, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg6_safesan)

#### SDG7 ren ####
targ7col<-grep("^(sdg7_ren)",colnames(data.pan.final.xf))
lasso_input_sdg7 <- data.pan.final.xf[,c(-1:-2,-targ7col)] #Use all vars except year, country, id, target
lasso_target_sdg7 <- data.pan.final.xf[,targ7col] 
Lasso_sdg7_ren<-glmnet(
  lasso_input_sdg7,
  lasso_target_sdg7,
  family = c("gaussian"))

coef(Lasso_sdg7_ren, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg7_ren)

#### SDG8 empop ####
targ8col<-grep("^(sdg8_empop)",colnames(data.pan.final.xf))
lasso_input_sdg8 <- data.pan.final.xf[,c(-1:-2,-targ8col)] #Use all vars except year, country, id, target
lasso_target_sdg8 <- data.pan.final.xf[,targ8col] 
Lasso_sdg8_empop<-glmnet(
  lasso_input_sdg8,
  lasso_target_sdg8,
  family = c("gaussian"))

coef(Lasso_sdg8_empop, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg8_empop)

#### SDG9 rdex ####
targ9col<-grep("^(sdg9_rdex)",colnames(data.pan.final.xf))
lasso_input_sdg9 <- data.pan.final.xf[,c(-1:-2,-targ9col)] #Use all vars except year, country, id, target
lasso_target_sdg9 <- data.pan.final.xf[,targ9col] 
Lasso_sdg9_rdex<-glmnet(
  lasso_input_sdg9,
  lasso_target_sdg9,
  family = c("gaussian"))

coef(Lasso_sdg9_rdex, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg9_rdex)

#### SDG10 palma ####
targ10col<-grep("^(sdg10_palma)",colnames(data.pan.final.xf))
lasso_input_sdg10 <- data.pan.final.xf[,c(-1:-2,-targ10col)] #Use all vars except year, country, id, target
lasso_target_sdg10 <- data.pan.final.xf[,targ10col] 
Lasso_sdg10_palma<-glmnet(
  lasso_input_sdg10,
  lasso_target_sdg10,
  family = c("gaussian"))

coef(Lasso_sdg10_palma, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg10_palma)

#### SDG11 pm2.5 #### POOR PERFORMANCE - REVISIT
targ11col<-grep("^(sdg11_pm25)",colnames(data.pan.final.xf))
lasso_input_sdg11 <- data.pan.final.xf[,c(-1:-2,-targ11col)] #Use all vars except year, country, id, target
lasso_target_sdg11 <- data.pan.final.xf[,targ11col] 
Lasso_sdg11_pm25<-glmnet(
  lasso_input_sdg11,
  lasso_target_sdg11,
  family = c("gaussian"))

coef(Lasso_sdg11_pm25, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg11_pm25)

#### SDG12 no target ####
#### SDG13 at the top ####

#### SDG14 cpma ####
targ14col<-grep("^(sdg14_cpma)",colnames(data.pan.final.xf))
lasso_input_sdg14 <- data.pan.final.xf[,c(-1:-2,-targ14col)] #Use all vars except year, country, id, target
lasso_target_sdg14 <- data.pan.final.xf[,targ14col] 
Lasso_sdg14_cpma<-glmnet(
  lasso_input_sdg14,
  lasso_target_sdg14,
  family = c("gaussian"))

coef(Lasso_sdg14_cpma, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg14_cpma)

#### SDG15 redlist ####
targ15col<-grep("^(sdg15_redlist)",colnames(data.pan.final.xf))
lasso_input_sdg15 <- data.pan.final.xf[,c(-1:-2,-targ15col)] #Use all vars except year, country, id, target
lasso_target_sdg15 <- data.pan.final.xf[,targ15col] 
Lasso_sdg15_redlist<-glmnet(
  lasso_input_sdg15,
  lasso_target_sdg15,
  family = c("gaussian"))

coef(Lasso_sdg15_redlist, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg15_redlist)

#### SDG16 homicides ####
targ16col<-grep("^(sdg16_homicides)",colnames(data.pan.final.xf))
lasso_input_sdg16 <- data.pan.final.xf[,c(-1:-2,-targ16col)] #Use all vars except year, country, id, target
lasso_target_sdg16 <- data.pan.final.xf[,targ16col] 
Lasso_sdg16_homicides<-glmnet(
  lasso_input_sdg16,
  lasso_target_sdg16,
  family = c("gaussian"))

coef(Lasso_sdg16_homicides, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg16_homicides)

#### SDG17 govex ####
targ17col<-grep("^(sdg17_govex)",colnames(data.pan.final.xf))
lasso_input_sdg17 <- data.pan.final.xf[,c(-1:-2,-targ17col)] #Use all vars except year, country, id, target
lasso_target_sdg17 <- data.pan.final.xf[,targ17col] 
Lasso_sdg17_govex<-glmnet(
  lasso_input_sdg17,
  lasso_target_sdg17,
  family = c("gaussian"))

coef(Lasso_sdg17_govex, s = c(0.01,0.02,0.03))
#plot(Lasso_sdg17_govex)



##############################################################
library(plm)
fit <- with(imp, lm(sdg11_pm25 ~ sdg2_trophic + sdg7_elecac))
fit
class(fit)
ls(fit)

data.poly<-read.csv("C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/imputed_SDR_data.csv", header=TRUE, na.strings=c("..","NA","","?"))

###Panel model with all imps of multiply imputed data treating all countries together in one pool (NOT RECOMMENDED)###
data.plm.imp<-cbind(data.imp[,2],c.broad)
fit_plm_with<-with(imp,plm(sdg7_co2twh ~ sdg7_elecac + sdg7_cleanfuel + sdg11_pm25 + sdg2_trophic,# + lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                           model='random',
                           effect='twoway',
                           data=data.plm.imp))
summary(fit_plm_with)
MSR_fit_plm_with<-mean(fit_plm_with$analyses[[1]]$residuals^2)

plm_nonimp<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_elecac + sdg7_cleanfuel + sdg11_pm25 + sdg2_trophic, #+ lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                model='random', 
                effect='twoway', 
                data=data.imp[,-1])
summary(plm_nonimp)
MSR_plm_nonimp<-mean(plm_nonimp$residuals^2)
MSR_plm_nonimp


# Panel model with imputed data by looping mice over each country
plm_looping<-cbind(data.imp[,1:4],c4)
fit_plm_looping<-with(c4,plm(sdg13_co2kgPerGDPPPP ~ sdg7_elecac + sdg7_cleanfuel + sdg11_pm25 + sdg2_trophic, #+ lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                     model='random', 
                     effect='twoway', 
                     data=plm_looping[,-1]))
summary(fit_plm_looping)
MSR_plm_looping<-mean(fit_plm_looping$residuals^2)
MSR_plm_looping

plot(fit_plm_looping$residuals)

fit_plm_imp_cust<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren + sdg7_cleanfuel + sdg11_pm25 
                      + sdg5_paygap + GDP_PPP + sdg7_co2twh + sdg4_tertiary + sdg3_lifee
                      + sdg3_matmort + sdg3_u5mort + sdg8_empop,#lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                           model='random',
                           effect='twoway',
                           data=data.imp.final[,-1])
summary(fit_plm_imp_cust)
MSR_fit_plm_imp_cust<-mean(fit_plm_imp_cust$residuals^2)
MSR_fit_plm_imp_cust

#try best input vars from manual testing with transformed data
fit_plm_imp_cust_xf<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren + sdg7_cleanfuel + sdg5_paygap + GDP_PPP 
                        + sdg7_co2twh + sdg4_tertiary + sdg3_lifee + sdg3_u5mort + sdg8_empop,#lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                      model='random',
                      effect='twoway',
                      data=data.imp.final.xf[,-1])
summary(fit_plm_imp_cust_xf)
MSR_fit_plm_imp_cust_xf<-mean(fit_plm_imp_cust_xf$residuals^2)
MSR_fit_plm_imp_cust_xf
# slightly better MSR, much better R squared, changes the importance of variables, much worse interpretability
plot(data.imp.final.xf$sdg7_ren,fit_plm_imp_cust_xf$residuals)


#### Try vars from lasso#### Untransformed data
fit_plm_imp_cust13.1L<-plm(sdg13_co2kgPerGDPPPP ~  sdg1_320pov + sdg2_wasteihme + sdg3_lifee +sdg16_prison
                           + sdg3_fertility + sdg7_ren + sdg11_pm25 + sdg7_co2twh + sdg8_impacc
                           + sdg5_paygap + sdg9_intuse + sdg11_transport + sdg15_cpfa,
                           model='random',
                           effect='twoway',
                           data=data.imp.final[,-1])
summary(fit_plm_imp_cust13.1L)
MSR_fit_plm_imp_cust13.1L<-mean(fit_plm_imp_cust13.1L$residuals^2)
MSR_fit_plm_imp_cust13.1L

plot(fit_plm_imp_cust13.1L$residuals)

#### Try vars from lasso#### Transformed data
fit_plm_imp_cust13.1xfL<-plm(sdg13_co2kgPerGDPPPP ~  sdg1_320pov + sdg2_wasteihme + sdg3_lifee + sdg16_prison
                       + sdg3_fertility + sdg7_ren + sdg11_pm25 + sdg7_co2twh + sdg8_impacc
                       + sdg5_paygap + sdg9_intuse + sdg11_transport + sdg15_cpfa,
                       model='random',
                       effect='twoway',
                       data=data.imp.final.xf[,-1])
summary(fit_plm_imp_cust13.1xfL)
MSR_fit_plm_imp_cust13.1xfL<-mean(fit_plm_imp_cust13.1xfL$residuals^2)
MSR_fit_plm_imp_cust13.1xfL
## Better MSE and R-squared than other transformed data test, but not all vars are significant

#try again with fewer vars
fit_plm_imp_cust13.2L<-plm(sdg13_co2kgPerGDPPPP ~  sdg1_320pov + sdg2_wasteihme + sdg3_lifee 
                       + sdg7_ren + sdg7_co2twh + sdg8_impacc
                       + sdg9_intuse,
                       model='random',
                       effect='twoway',
                       data=data.imp.final.xf[,-1])
summary(fit_plm_imp_cust13.2L)
MSR_fit_plm_imp_cust13.2L<-mean(fit_plm_imp_cust13.2L$residuals^2)
MSR_fit_plm_imp_cust13.2L
# the extra vars are not significant, but they contribute to raising the R-squared


#Vars from lasso on pan imputed, but untransformed data. Eliminated 3 vars that appeared in lasso but were insignificant (netacc, transport, prisons)
fit_plm_imp_cust13.3L<-plm(sdg13_co2kgPerGDPPPP ~  sdg2_crlyld + sdg2_wasteihme + sdg3_lifee + sdg3_hiv
                       + sdg4_earlyedu + sdg5_paygap + sdg7_cleanfuel + sdg7_ren
                       + sdg9_intuse + sdg14_cleanwat 
                       + sdg16_rsf,
                       model='random',
                       effect='twoway',
                       data=data.pan)
summary(fit_plm_imp_cust13.3L)
MSR_fit_plm_imp_cust13.3L<-mean(fit_plm_imp_cust13.3L$residuals^2)
MSR_fit_plm_imp_cust13.3L

#best vars from manual testing, untransformed data
fit_plm_imp_cust2<-plm(sdg13_co2kgPerGDPPPP ~  + sdg7_ren + sdg11_pm25 
                      + sdg5_paygap + GDP_PPP + sdg3_lifee + sdg2_undernsh + sdg1_oecdpov
                      + sdg3_u5mort + sdg8_empop,#lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                      model='random',
                      effect='twoway',
                      data=data.imp.final[,-1])
summary(fit_plm_imp_cust2)
MSR_fit_plm_imp_cust2<-mean(fit_plm_imp_cust2$residuals^2)
MSR_fit_plm_imp_cust2

#best vars from manual testing, transformed data
fit_plm_imp_cust2.xf<-plm(sdg13_co2kgPerGDPPPP ~  + sdg7_ren + sdg11_pm25 
                       + sdg5_paygap + GDP_PPP + sdg3_lifee + sdg2_undernsh + sdg1_oecdpov
                       + sdg3_u5mort + sdg8_empop,#lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                       model='random',
                       effect='twoway',
                       data=data.imp.final.xf[,-1])
summary(fit_plm_imp_cust2.xf)
MSR_fit_plm_imp_cust2.xf<-mean(fit_plm_imp_cust2.xf$residuals^2)
MSR_fit_plm_imp_cust2.xf

####test interaction terms####
fit_plm_imp_cust_int1<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren*sdg7_elecac + sdg7_cleanfuel + GDP_PPP 
                       + sdg3_lifee + sdg3_u5mort + sdg8_empop + sdg2_trophic*sdg2_obesity,
                       model='random',
                       effect='twoway',
                       data=data.imp.final[,-1])
summary(fit_plm_imp_cust_int1)
MSR_fit_plm_imp_cust_int1<-mean(fit_plm_imp_cust_int1$residuals^2)
MSR_fit_plm_imp_cust_int1

#interaction terms only
fit_plm_imp_cust_int2<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren*sdg7_elecac + sdg7_cleanfuel*sdg11_pm25 +  
                        + sdg3_lifee*sdg1_oecdpov + sdg2_trophic*sdg2_obesity,
                        model='random',
                        effect='twoway',
                        data=data.imp.final[,-1])
summary(fit_plm_imp_cust_int2)
MSR_fit_plm_imp_cust_int2<-mean(fit_plm_imp_cust_int2$residuals^2)
MSR_fit_plm_imp_cust_int2

####test time effects####
fit_plm_imp_cust_time1<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren + sdg5_paygap + GDP_PPP 
                       + sdg3_lifee + sdg2_undernsh + sdg1_oecdpov + sdg8_empop
                       + lag(sdg7_ren)  + lag(sdg5_paygap) + lag(GDP_PPP) + lag(sdg3_lifee)
                       + lag(sdg2_undernsh) + lag(sdg1_oecdpov) + lag(sdg8_empop),
                       model='random',
                       effect='twoway',
                       data=data.imp.final[,-1])
summary(fit_plm_imp_cust_time1)
MSR_fit_plm_imp_cust_time1<-mean(fit_plm_imp_cust_time1$residuals^2)
MSR_fit_plm_imp_cust_time1


plot(fit_plm_imp_cust$residuals) #how to interpret this?
plot(fit_plm_imp1$residuals)


#############################################################################################
###Other targets###
#SDG1#
# fit_plm_imp_cust_sdg1<-plm(sdg1_oecdpov ~ sdg1_wpc + sdg1_320pov + sdg4_tertiary + GDP_PPP,
#                       model='random',
#                       effect='twoway',
#                       data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg1)
# MSR_fit_plm_imp_cust_sdg1<-mean(fit_plm_imp_cust_sdg1$residuals^2)
# MSR_fit_plm_imp_cust_sdg1
# 
# 
# #Lasso inputs with transformed custom imputed vars
# fit_plm_imp_cust_sdg1L<-plm(sdg1_oecdpov ~ sdg1_wpc + sdg2_trophic + sdg3_u5mort + sdg10_palma + sdg10_elder 
#                            + sdg16_prison 
#                            + sdg4_tertiary + sdg5_paygap + sdg7_co2twh + sdg9_intuse + sdg11_transport + sdg16_cpi,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg1L)
# MSR_fit_plm_imp_cust_sdg1L<-mean(fit_plm_imp_cust_sdg1L$residuals^2)
# MSR_fit_plm_imp_cust_sdg1L
# #Lasso improves R^2 value from 0.057 to 0.410, a ~75x increase. MSR also improves. Using s=0.015 might be best. s=.01 too many vars but better performance
# 
# #SDG2#
# fit_plm_imp_cust_sdg2<-plm(sdg2_undernsh ~ sdg1_oecdpov + sdg3_u5mort + sdg3_lifee,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg2)
# MSR_fit_plm_imp_cust_sdg2<-mean(fit_plm_imp_cust_sdg2$residuals^2)
# MSR_fit_plm_imp_cust_sdg2
# 
# #Lasso inputs with transformed custom imputed vars
# fit_plm_imp_cust_sdg2L<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg3_fertility + sdg4_tertiary + sdg5_paygap + sdg7_elecac 
#                             + sdg9_rdex + sdg11_transport + sdg16_homicides + sdg16_cpi,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg2L)
# MSR_fit_plm_imp_cust_sdg2L<-mean(fit_plm_imp_cust_sdg2L$residuals^2)
# MSR_fit_plm_imp_cust_sdg2L
# 
# #SDG3#
# fit_plm_imp_cust_sdg3<-plm(sdg3_lifee ~ sdg1_oecdpov + sdg2_undernsh + sdg4_primary + sdg4_tertiary + sdg5_edat + sdg6_safesan,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg3)
# MSR_fit_plm_imp_cust_sdg3<-mean(fit_plm_imp_cust_sdg3$residuals^2)
# MSR_fit_plm_imp_cust_sdg3
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05 (higher than others))
# fit_plm_imp_cust_sdg3L<-plm(sdg3_lifee ~ GDP_PPP + sdg2_stuntihme + sdg2_wasteihme + sdg3_matmort + sdg3_neonat 
#                             + sdg3_u5mort + sdg3_tb + sdg4_tertiary + sdg9_articles + sdg15_redlist + sdg16_homicides
#                             + sdg16_cpi + sdg16_prison,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg3L)
# MSR_fit_plm_imp_cust_sdg3L<-mean(fit_plm_imp_cust_sdg3L$residuals^2)
# MSR_fit_plm_imp_cust_sdg3L
# 
# #SDG4#
# fit_plm_imp_cust_sdg4<-plm(sdg4_tertiary ~ sdg1_oecdpov + sdg5_edat + sdg9_intuse + sdg9_rdex + sdg16_homicides,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg4)
# MSR_fit_plm_imp_cust_sdg4<-mean(fit_plm_imp_cust_sdg4$residuals^2)
# MSR_fit_plm_imp_cust_sdg4
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg4L<-plm(sdg4_tertiary ~ sdg1_oecdpov + sdg2_undernsh + sdg3_lifee + sdg3_fertility + sdg5_edat 
#                             + sdg5_paygap + sdg7_ren + sdg9_intuse + sdg9_rdex + sdg11_pm25 + sdg15_redlist,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg4L)
# MSR_fit_plm_imp_cust_sdg4L<-mean(fit_plm_imp_cust_sdg4L$residuals^2)
# MSR_fit_plm_imp_cust_sdg4L
# #not many significant vars, but decent R^2
# 
# #SDG5#
# fit_plm_imp_cust_sdg5<-plm(sdg5_parl ~ sdg1_oecdpov + sdg3_matmort + sdg3_fertility + sdg5_edat + sdg16_homicides + sdg16_safe,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg5)
# MSR_fit_plm_imp_cust_sdg5<-mean(fit_plm_imp_cust_sdg5$residuals^2)
# MSR_fit_plm_imp_cust_sdg5
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05)
# fit_plm_imp_cust_sdg5L<-plm(sdg5_parl ~ sdg2_stuntihme + sdg2_trophic + sdg2_snmi + sdg3_swb + sdg7_ren + sdg9_intuse
#                             + sdg9_articles + sdg10_palma + sdg11_pm25 +sdg17_govex,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg5L)
# MSR_fit_plm_imp_cust_sdg5L<-mean(fit_plm_imp_cust_sdg5L$residuals^2)
# MSR_fit_plm_imp_cust_sdg5L
# #good improvement of R^2
# 
# #SDG6#
# fit_plm_imp_cust_sdg6<-plm(sdg6_safesan ~ sdg2_stuntihme + sdg3_u5mort + sdg3_lifee + sdg4_second + sdg11_pipedwat + GDP_PPP,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg6)
# MSR_fit_plm_imp_cust_sdg6<-mean(fit_plm_imp_cust_sdg6$residuals^2)
# MSR_fit_plm_imp_cust_sdg6
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg6L<-plm(sdg6_safesan ~ GDP_PPP + sdg3_u5mort + sdg3_fertility + sdg7_elecac + sdg9_rdex + sdg15_cpta
#                             + sdg15_cpfa + sdg16_prison,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg6L)
# MSR_fit_plm_imp_cust_sdg6L<-mean(fit_plm_imp_cust_sdg6L$residuals^2)
# MSR_fit_plm_imp_cust_sdg6L
# #good improvement of R^2
# 
# #SDG7#
# fit_plm_imp_cust_sdg7<-plm(sdg7_ren ~ sdg1_oecdpov + sdg3_lifee + sdg11_pipedwat + sdg7_elecac + sdg11_pm25,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg7)
# MSR_fit_plm_imp_cust_sdg7<-mean(fit_plm_imp_cust_sdg7$residuals^2)
# MSR_fit_plm_imp_cust_sdg7
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg7L<-plm(sdg7_ren ~ GDP_PPP + sdg2_obesity + sdg3_fertility + sdg4_tertiary + sdg5_parl + sdg5_paygap
#                             + sdg7_co2twh + sdg8_impacc + sdg9_rdex + sdg11_pm25 + sdg13_co2kgPerGDPPPP,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg7L)
# MSR_fit_plm_imp_cust_sdg7L<-mean(fit_plm_imp_cust_sdg7L$residuals^2)
# MSR_fit_plm_imp_cust_sdg7L
# #Good improvement of R^2, few significant vars, not much change in MSR
# 
# #SDG8#
# fit_plm_imp_cust_sdg8<-plm(sdg8_empop ~ sdg1_320pov + sdg2_trophic + sdg3_lifee + sdg3_fertility + sdg16_rsf + sdg5_edat + sdg10_palma,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg8)
# MSR_fit_plm_imp_cust_sdg8<-mean(fit_plm_imp_cust_sdg8$residuals^2)
# MSR_fit_plm_imp_cust_sdg8
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg8L<-plm(sdg8_empop ~ sdg2_trophic + sdg3_swb + sdg5_lfpr + sdg5_paygap + sdg7_co2twh + sdg9_intuse + sdg11_pm25
#                             + sdg13_co2import + sdg17_govex,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg8L)
# MSR_fit_plm_imp_cust_sdg8L<-mean(fit_plm_imp_cust_sdg8L$residuals^2)
# MSR_fit_plm_imp_cust_sdg8L
# #small boost to R^2, very small improvement of MSR
# 
# #SDG9#
# fit_plm_imp_cust_sdg9<-plm(sdg9_rdex ~ sdg9_articles + sdg3_lifee + sdg7_co2twh + sdg2_obesity + sdg5_edat + sdg10_palma,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg9)
# MSR_fit_plm_imp_cust_sdg9<-mean(fit_plm_imp_cust_sdg9$residuals^2)
# MSR_fit_plm_imp_cust_sdg9
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05)
# fit_plm_imp_cust_sdg9L<-plm(sdg9_rdex ~ sdg3_neonat + sdg3_lifee + sdg3_fertility + sdg4_tertiary + sdg5_paygap + sdg9_articles
#                             + sdg16_safe + sdg17_govex,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg9L)
# MSR_fit_plm_imp_cust_sdg9L<-mean(fit_plm_imp_cust_sdg9L$residuals^2)
# MSR_fit_plm_imp_cust_sdg9L
# # Worse performance on R^2 and MSR than hand selected vars
# 
# #SDG10#
# fit_plm_imp_cust_sdg10<-plm(sdg10_palma ~ sdg1_320pov + sdg9_articles + sdg7_co2twh + sdg5_edat,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg10)
# MSR_fit_plm_imp_cust_sdg10<-mean(fit_plm_imp_cust_sdg10$residuals^2)
# MSR_fit_plm_imp_cust_sdg10
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg10L<-plm(sdg10_palma ~ sdg1_320pov + sdg1_oecdpov + sdg2_wasteihme + sdg2_snmi + sdg3_matmort + sdg3_fertility 
#                              + sdg5_parl + sdg6_scarcew + sdg9_articles + sdg11_transport + sdg16_cpi + sdg16_prison,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg10L)
# MSR_fit_plm_imp_cust_sdg10L<-mean(fit_plm_imp_cust_sdg10L$residuals^2)
# MSR_fit_plm_imp_cust_sdg10L
# 
# #SDG11#
# fit_plm_imp_cust_sdg11<-plm(sdg11_pm25 ~ sdg3_fertility + sdg7_ren + sdg3_lifee + sdg5_parl + sdg9_rdex + sdg8_empop 
#                             + sdg11_transport + sdg13_co2kgPerGDPPPP + sdg16_cpi,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg11)
# MSR_fit_plm_imp_cust_sdg11<-mean(fit_plm_imp_cust_sdg11$residuals^2)
# MSR_fit_plm_imp_cust_sdg11
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05)
# fit_plm_imp_cust_sdg11L<-plm(sdg11_pm25 ~ sdg2_trophic + sdg3_swb + sdg5_edat + sdg5_lfpr + sdg5_parl + sdg7_ren 
#                              + sdg8_empop + sdg17_govex,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg11L)
# MSR_fit_plm_imp_cust_sdg11L<-mean(fit_plm_imp_cust_sdg11L$residuals^2)
# MSR_fit_plm_imp_cust_sdg11L
# # Worse performance on R^2 and MSR than hand selected vars. Co2kgperGDPPPP seems important
# 
# 
# #SDG12#
# #No target
# 
# #SDG13#
# #Already Done#
# 
# #SDG14#
# fit_plm_imp_cust_sdg14<-plm(sdg14_cpma ~ sdg15_cpfa + sdg3_lifee + sdg15_redlist + sdg9_rdex + sdg11_transport + sdg14_cleanwat + sdg15_cpta,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg14)
# MSR_fit_plm_imp_cust_sdg14<-mean(fit_plm_imp_cust_sdg14$residuals^2)
# MSR_fit_plm_imp_cust_sdg14
# 
# #SDG15#
# fit_plm_imp_cust_sdg15<-plm(sdg15_redlist ~ sdg15_cpfa + sdg3_lifee + sdg14_cpma + sdg11_transport + sdg14_cleanwat + sdg15_cpta,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg15)
# MSR_fit_plm_imp_cust_sdg15<-mean(fit_plm_imp_cust_sdg15$residuals^2)
# MSR_fit_plm_imp_cust_sdg15
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.03)
# fit_plm_imp_cust_sdg15L<-plm(sdg15_redlist ~ Population + sdg2_trophic + sdg3_lifee + sdg6_scarcew + sdg6_safesan + sdg10_palma 
#                              + sdg10_elder + sdg15_cpfa,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg15L)
# MSR_fit_plm_imp_cust_sdg15L<-mean(fit_plm_imp_cust_sdg15L$residuals^2)
# MSR_fit_plm_imp_cust_sdg15L
# 
# #SDG16#
# fit_plm_imp_cust_sdg16<-plm(sdg16_homicides ~ sdg16_cpi + sdg3_lifee + sdg1_oecdpov + sdg2_obesity + sdg5_edat+ sdg8_empop,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg16)
# MSR_fit_plm_imp_cust_sdg16<-mean(fit_plm_imp_cust_sdg16$residuals^2)
# MSR_fit_plm_imp_cust_sdg16
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg16L<-plm(sdg16_homicides ~ sdg1_320pov + sdg2_obesity + sdg3_matmort + sdg3_neonat + sdg3_lifee + 
#                                sdg3_fertility + sdg5_edat+ sdg10_palma + sdg13_co2kgPerGDPPPP +sdg16_prison,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg16L)
# MSR_fit_plm_imp_cust_sdg16L<-mean(fit_plm_imp_cust_sdg16L$residuals^2)
# MSR_fit_plm_imp_cust_sdg16L
# 
# #SDG17#
# fit_plm_imp_cust_sdg17<-plm(sdg17_govex ~ sdg16_cpi + sdg3_lifee + sdg1_oecdpov + sdg2_obesity + sdg5_edat+ sdg8_empop + sdg9_rdex,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg17)
# MSR_fit_plm_imp_cust_sdg17<-mean(fit_plm_imp_cust_sdg17$residuals^2)
# MSR_fit_plm_imp_cust_sdg17
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.03)
# fit_plm_imp_cust_sdg17L<-plm(sdg17_govex ~ sdg2_wasteihme + sdg2_trophic + sdg3_tb + sdg3_swb + sdg5_lfpr + sdg5_parl + sdg7_co2twh
#                              + sdg9_articles + sdg11_pm25 +sdg16_safe,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final[,-1])
# summary(fit_plm_imp_cust_sdg17L)
# MSR_fit_plm_imp_cust_sdg17L<-mean(fit_plm_imp_cust_sdg17L$residuals^2)
# MSR_fit_plm_imp_cust_sdg17L
# 
# 
# ### Other Targets with Transformed Data ###
# ### other targets ###
# #SDG1#
# fit_plm_imp_cust_sdg1.xf<-plm(sdg1_oecdpov ~ sdg1_wpc + sdg1_320pov + sdg4_tertiary + sdg5_edat + sdg3_fertility + sdg3_lifee,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg1.xf)
# MSR_fit_plm_imp_cust_sdg1.xf<-mean(fit_plm_imp_cust_sdg1.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg1.xf
# 
# #Lasso inputs with transformed custom imputed vars
# fit_plm_imp_cust_sdg1.xfL<-plm(sdg1_oecdpov ~ sdg1_wpc + sdg2_trophic + sdg3_u5mort + sdg10_palma + sdg10_elder 
#                             + sdg16_prison 
#                             + sdg4_tertiary + sdg5_paygap + sdg7_co2twh + sdg9_intuse + sdg11_transport + sdg16_cpi,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg1.xfL)
# MSR_fit_plm_imp_cust_sdg1.xfL<-mean(fit_plm_imp_cust_sdg1.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg1.xfL
# 
# #SDG2#
# fit_plm_imp_cust_sdg2.xf<-plm(sdg2_undernsh ~ sdg1_oecdpov + sdg3_u5mort + sdg3_lifee + sdg2_crlyld + sdg14_fishstocks
#                               + sdg11_pipedwat + sdg15_redlist,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg2.xf)
# MSR_fit_plm_imp_cust_sdg2.xf<-mean(fit_plm_imp_cust_sdg2.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg2.xf
# 
# #Lasso inputs with transformed custom imputed vars
# fit_plm_imp_cust_sdg2.xfL<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg3_fertility + sdg4_tertiary + sdg5_paygap + sdg7_elecac 
#                             + sdg9_rdex + sdg11_transport + sdg16_homicides + sdg16_cpi,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg2.xfL)
# MSR_fit_plm_imp_cust_sdg2.xfL<-mean(fit_plm_imp_cust_sdg2.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg2.xfL
# 
# #SDG3#
# fit_plm_imp_cust_sdg3.xf<-plm(sdg3_lifee ~ sdg1_oecdpov + sdg2_undernsh + sdg4_primary + sdg4_tertiary + sdg5_edat + sdg6_safesan,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg3.xf)
# MSR_fit_plm_imp_cust_sdg3.xf<-mean(fit_plm_imp_cust_sdg3.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg3.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05 (higher than others))
# fit_plm_imp_cust_sdg3.xfL<-plm(sdg3_lifee ~ GDP_PPP + sdg2_stuntihme + sdg2_wasteihme + sdg3_matmort + sdg3_neonat 
#                             + sdg3_u5mort + sdg3_tb + sdg4_tertiary + sdg9_articles + sdg15_redlist + sdg16_homicides
#                             + sdg16_cpi + sdg16_prison,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg3.xfL)
# MSR_fit_plm_imp_cust_sdg3.xfL<-mean(fit_plm_imp_cust_sdg3.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg3.xfL
# 
# #SDG4#
# fit_plm_imp_cust_sdg4.xf<-plm(sdg4_tertiary ~ sdg1_oecdpov + sdg5_edat + sdg9_intuse + sdg9_rdex + sdg16_homicides,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg4.xf)
# MSR_fit_plm_imp_cust_sdg4.xf<-mean(fit_plm_imp_cust_sdg4.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg4.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg4.xfL<-plm(sdg4_tertiary ~ sdg1_oecdpov + sdg2_undernsh + sdg3_lifee + sdg3_fertility + sdg5_edat 
#                             + sdg5_paygap + sdg7_ren + sdg9_intuse + sdg9_rdex + sdg11_pm25 + sdg15_redlist,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg4.xfL)
# MSR_fit_plm_imp_cust_sdg4.xfL<-mean(fit_plm_imp_cust_sdg4.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg4.xfL
# 
# #SDG5#
# fit_plm_imp_cust_sdg5.xf<-plm(sdg5_parl ~ sdg1_oecdpov + sdg3_matmort + sdg3_fertility + sdg5_edat + sdg16_homicides + sdg16_safe,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg5.xf)
# MSR_fit_plm_imp_cust_sdg5.xf<-mean(fit_plm_imp_cust_sdg5.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg5.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05)
# fit_plm_imp_cust_sdg5.xfL<-plm(sdg5_parl ~ sdg2_stuntihme + sdg2_trophic + sdg2_snmi + sdg3_swb + sdg7_ren + sdg9_intuse
#                             + sdg9_articles + sdg10_palma + sdg11_pm25 +sdg17_govex,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg5.xfL)
# MSR_fit_plm_imp_cust_sdg5.xfL<-mean(fit_plm_imp_cust_sdg5.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg5.xfL
# 
# #SDG6#
# fit_plm_imp_cust_sdg6.xf<-plm(sdg6_safesan ~ sdg2_stuntihme + sdg3_u5mort + sdg3_lifee + sdg4_second + sdg11_pipedwat + GDP_PPP,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg6.xf)
# MSR_fit_plm_imp_cust_sdg6.xf<-mean(fit_plm_imp_cust_sdg6.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg6.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg6.xfL<-plm(sdg6_safesan ~ GDP_PPP + sdg3_u5mort + sdg3_fertility + sdg7_elecac + sdg9_rdex + sdg15_cpta
#                             + sdg15_cpfa + sdg16_prison,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg6.xfL)
# MSR_fit_plm_imp_cust_sdg6.xfL<-mean(fit_plm_imp_cust_sdg6.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg6.xfL
# 
# #SDG7#
# fit_plm_imp_cust_sdg7.xf<-plm(sdg7_ren ~ sdg1_oecdpov + sdg3_lifee + sdg11_pipedwat + sdg7_elecac + sdg11_pm25,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg7.xf)
# MSR_fit_plm_imp_cust_sdg7.xf<-mean(fit_plm_imp_cust_sdg7.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg7.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg7.xfL<-plm(sdg7_ren ~ GDP_PPP + sdg2_obesity + sdg3_fertility + sdg4_tertiary + sdg5_parl + sdg5_paygap
#                             + sdg7_co2twh + sdg8_impacc + sdg9_rdex + sdg11_pm25 + sdg13_co2kgPerGDPPPP,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg7.xfL)
# MSR_fit_plm_imp_cust_sdg7.xfL<-mean(fit_plm_imp_cust_sdg7.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg7.xfL
# 
# #SDG8#
# fit_plm_imp_cust_sdg8.xf<-plm(sdg8_empop ~ sdg1_320pov + sdg2_trophic + sdg3_lifee + sdg3_fertility + sdg16_rsf + sdg5_edat + sdg10_palma,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg8.xf)
# MSR_fit_plm_imp_cust_sdg8.xf<-mean(fit_plm_imp_cust_sdg8.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg8.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg8L.xf<-plm(sdg8_empop ~ sdg2_trophic + sdg3_swb + sdg5_lfpr + sdg5_paygap + sdg7_co2twh + sdg9_intuse + sdg11_pm25
#                             + sdg13_co2import + sdg17_govex,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg8L.xf)
# MSR_fit_plm_imp_cust_sdg8L.xf<-mean(fit_plm_imp_cust_sdg8L.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg8L.xf
# 
# #SDG9#
# fit_plm_imp_cust_sdg9.xf<-plm(sdg9_rdex ~ sdg9_articles + sdg3_lifee + sdg7_co2twh + sdg2_obesity + sdg5_edat + sdg10_palma,
#                            model='random',
#                            effect='twoway',
#                            data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg9.xf)
# MSR_fit_plm_imp_cust_sdg9.xf<-mean(fit_plm_imp_cust_sdg9.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg9.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05)
# fit_plm_imp_cust_sdg9L.xf<-plm(sdg9_rdex ~ sdg3_neonat + sdg3_lifee + sdg3_fertility + sdg4_tertiary + sdg5_paygap + sdg9_articles
#                             + sdg16_safe + sdg17_govex,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg9L.xf)
# MSR_fit_plm_imp_cust_sdg9L.xf<-mean(fit_plm_imp_cust_sdg9L.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg9L.xf
# 
# #SDG10#
# fit_plm_imp_cust_sdg10.xf<-plm(sdg10_palma ~ sdg1_320pov + sdg9_articles + sdg7_co2twh + sdg5_edat,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg10.xf)
# MSR_fit_plm_imp_cust_sdg10.xf<-mean(fit_plm_imp_cust_sdg10.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg10.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.02)
# fit_plm_imp_cust_sdg10.xfL<-plm(sdg10_palma ~ sdg1_320pov + sdg1_oecdpov + sdg2_wasteihme + sdg2_snmi + sdg3_matmort + sdg3_fertility 
#                              + sdg5_parl + sdg6_scarcew + sdg9_articles + sdg11_transport + sdg16_cpi + sdg16_prison,
#                              model='random',
#                              effect='twoway',
#                              data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg10.xfL)
# MSR_fit_plm_imp_cust_sdg10.xfL<-mean(fit_plm_imp_cust_sdg10.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg10.xfL
# 
# #SDG11#
# fit_plm_imp_cust_sdg11.xf<-plm(sdg11_pm25 ~ sdg3_fertility + sdg7_ren + sdg3_lifee + sdg5_parl + sdg9_rdex + sdg8_empop + sdg11_transport + sdg13_co2kgPerGDPPPP + sdg16_cpi,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg11.xf)
# MSR_fit_plm_imp_cust_sdg11.xf<-mean(fit_plm_imp_cust_sdg11.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg11.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.05)
# fit_plm_imp_cust_sdg11.xfL<-plm(sdg11_pm25 ~ sdg2_trophic + sdg3_swb + sdg5_edat + sdg5_lfpr + sdg5_parl + sdg7_ren 
#                              + sdg8_empop + sdg17_govex,
#                              model='random',
#                              effect='twoway',
#                              data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg11.xfL)
# MSR_fit_plm_imp_cust_sdg11.xfL<-mean(fit_plm_imp_cust_sdg11.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg11.xfL
# 
# #SDG12#
# #No target
# 
# #SDG13#
# #Already Done#
# 
# #SDG14#
# fit_plm_imp_cust_sdg14.xf<-plm(sdg14_cpma ~ sdg15_cpfa + sdg3_lifee + sdg15_redlist + sdg9_rdex + sdg11_transport + sdg14_cleanwat + sdg15_cpta,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg14.xf)
# MSR_fit_plm_imp_cust_sdg14.xf<-mean(fit_plm_imp_cust_sdg14.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg14.xf
# 
# #SDG15#
# fit_plm_imp_cust_sdg15.xf<-plm(sdg15_redlist ~ sdg15_cpfa + sdg3_lifee + sdg14_cpma + sdg11_transport + sdg14_cleanwat + sdg15_cpta,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg15.xf)
# MSR_fit_plm_imp_cust_sdg15.xf<-mean(fit_plm_imp_cust_sdg15.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg15.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.03)
# fit_plm_imp_cust_sdg15.xfL<-plm(sdg15_redlist ~ Population + sdg2_trophic + sdg3_lifee + sdg6_scarcew + sdg6_safesan + sdg10_palma 
#                              + sdg10_elder + sdg15_cpfa,
#                              model='random',
#                              effect='twoway',
#                              data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg15.xfL)
# MSR_fit_plm_imp_cust_sdg15.xfL<-mean(fit_plm_imp_cust_sdg15.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg15.xfL
# 
# #SDG16#
# fit_plm_imp_cust_sdg16.xf<-plm(sdg16_homicides ~ sdg16_cpi + sdg3_lifee + sdg1_oecdpov + sdg2_obesity + sdg5_edat+ sdg8_empop,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg16.xf)
# MSR_fit_plm_imp_cust_sdg16.xf<-mean(fit_plm_imp_cust_sdg16.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg16.xf
# 
# # sdg 16 with lasso variables. s=0.02
# fit_plm_imp_cust_sdg16.xfL<-plm(sdg16_homicides ~ sdg1_wpc + sdg1_320pov + sdg2_undernsh + sdg3_matmort + sdg3_u5mort + sdg3_lifee
#                                + sdg3_fertility + sdg10_palma,
#                                model='random',
#                                effect='twoway',
#                                data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg16.xfL)
# MSR_fit_plm_imp_cust_sdg16.xfL<-mean(fit_plm_imp_cust_sdg16.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg16.xfL
# 
# #SDG17#
# fit_plm_imp_cust_sdg17.xf<-plm(sdg17_govex ~ sdg16_cpi + sdg3_lifee + sdg1_oecdpov + sdg2_obesity + sdg5_edat+ sdg8_empop + sdg9_rdex,
#                             model='random',
#                             effect='twoway',
#                             data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg17.xf)
# MSR_fit_plm_imp_cust_sdg17.xf<-mean(fit_plm_imp_cust_sdg17.xf$residuals^2)
# MSR_fit_plm_imp_cust_sdg17.xf
# 
# #Lasso inputs with transformed custom imputed vars (lambda = 0.03)
# fit_plm_imp_cust_sdg17.xfL<-plm(sdg17_govex ~ sdg2_wasteihme + sdg2_trophic + sdg3_tb + sdg3_swb + sdg5_lfpr + sdg5_parl + sdg7_co2twh
#                              + sdg9_articles + sdg11_pm25 +sdg16_safe,
#                              model='random',
#                              effect='twoway',
#                              data=data.imp.final.xf[,-1])
# summary(fit_plm_imp_cust_sdg17.xfL)
# MSR_fit_plm_imp_cust_sdg17.xfL<-mean(fit_plm_imp_cust_sdg17.xfL$residuals^2)
# MSR_fit_plm_imp_cust_sdg17.xfL


##########################################################
#### Panel Models on pan imputed and transformed data ####
##########################################################


#SDG1#
fit_plm_imp_pan_sdg1.xf<-plm(sdg1_oecdpov ~ sdg4_tertiary
                             + sdg3_lifee + sdg2_undernsh + sdg10_palma + GDP_PPP,
                          model='random',
                          effect='twoway',
                          data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg1.xf)
MSR_fit_plm_imp_pan_sdg1.xf<-mean(fit_plm_imp_pan_sdg1.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg1.xf

#Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg1.xfL<-plm(sdg1_oecdpov ~ sdg1_wpc + sdg4_tertiary + sdg5_fplmodel + sdg8_empop
                              + sdg10_adjgini + sdg10_palma + sdg10_elder + sdg16_prison  + sdg16_rsf,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg1.xfL)
MSR_fit_plm_imp_pan_sdg1.xfL<-mean(fit_plm_imp_pan_sdg1.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg1.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg1.xfLP<-plm(sdg1_oecdpov ~ sdg1_wpc + sdg4_primary + sdg4_tertiary + sdg6_safewat
                               + sdg10_adjgini + sdg10_palma + sdg10_elder +  sdg16_rsf,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg1.xfLP)
MSR_fit_plm_imp_pan_sdg1.xfLP<-mean(fit_plm_imp_pan_sdg1.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg1.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg1.xfLP2<-plm(sdg1_oecdpov ~ sdg4_tertiary + sdg10_palma + sdg10_elder +  sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg1.xfLP2)
MSR_fit_plm_imp_pan_sdg1.xfLP2<-mean(fit_plm_imp_pan_sdg1.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg1.xfLP2

#SDG2#
fit_plm_imp_pan_sdg2.xf<-plm(sdg2_undernsh ~ sdg1_oecdpov + sdg3_lifee + sdg3_u5mort + sdg6_safewat + sdg7_cleanfuel
                             + sdg14_fishstocks + sdg11_pipedwat + sdg14_trawl ,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.xf)
MSR_fit_plm_imp_pan_sdg2.xf<-mean(fit_plm_imp_pan_sdg2.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg2.xf

#Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xfL<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg3_fertility + sdg4_primary + sdg4_second + sdg4_tertiary + sdg7_elecac
                              + sdg7_cleanfuel + sdg14_cleanwat,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.xfL)
MSR_fit_plm_imp_pan_sdg2.xfL<-mean(fit_plm_imp_pan_sdg2.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg2.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xfLP<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg2_trophic + sdg3_fertility 
                               + sdg3_swb  + sdg7_cleanfuel + sdg14_cleanwat + sdg17_oda,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.xfLP)
MSR_fit_plm_imp_pan_sdg2.xfLP<-mean(fit_plm_imp_pan_sdg2.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg2.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xfLP2<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg2_trophic + sdg7_cleanfuel + sdg17_oda,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.xfLP2)
MSR_fit_plm_imp_pan_sdg2.xfLP2<-mean(fit_plm_imp_pan_sdg2.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg2.xfLP2

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xfLP3<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg2_obesity + sdg2_stuntihme + sdg7_cleanfuel + sdg13_co2kgPerGDPPPP,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.xfLP3)
MSR_fit_plm_imp_pan_sdg2.xfLP3<-mean(fit_plm_imp_pan_sdg2.xfLP3$residuals^2)
MSR_fit_plm_imp_pan_sdg2.xfLP3

#########################################################################################################################################
##### SDG 2 Wasting Target Models #####
#######################################

#handpicked
fit_plm_imp_pan_sdg2.2.xf<-plm(sdg2_wasteihme ~ sdg3_u5mort + sdg2_stuntihme + sdg6_safesan + sdg2_obesity
                             + sdg11_pipedwat + sdg3_lifee + sdg4_tertiary,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.2.xf)
MSR_fit_plm_imp_pan_sdg2.2.xf<-mean(fit_plm_imp_pan_sdg2.2.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg2.2.xf

# simple lasso
fit_plm_imp_pan_sdg2.2.xfL<-plm(sdg2_wasteihme ~ GDP_PPP + sdg2_stuntihme + sdg2_obesity + sdg3_tb + sdg3_vac
                               + sdg9_patents + sdg9_netacc + sdg17_oda,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.2.xfL)
MSR_fit_plm_imp_pan_sdg2.2.xfL<-mean(fit_plm_imp_pan_sdg2.2.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg2.2.xfL

# Pruned lasso
fit_plm_imp_pan_sdg2.2.xfLP<-plm(sdg2_wasteihme ~ GDP_PPP + sdg2_undernsh + sdg2_stuntihme + sdg2_obesity
                                 + sdg8_impacc + sdg9_patents + sdg15_cpta + sdg15_cpfa,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.2.xfLP)
MSR_fit_plm_imp_pan_sdg2.2.xfLP<-mean(fit_plm_imp_pan_sdg2.2.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg2.2.xfLP

# Pruned lasso
fit_plm_imp_pan_sdg2.2.xfLP2<-plm(sdg2_wasteihme ~ sdg2_undernsh + sdg2_stuntihme + sdg2_obesity,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg2.2.xfLP2)
MSR_fit_plm_imp_pan_sdg2.2.xfLP2<-mean(fit_plm_imp_pan_sdg2.2.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg2.2.xfLP2

#SDG3#
fit_plm_imp_pan_sdg3.xf<-plm(sdg3_lifee ~ sdg1_oecdpov + sdg2_undernsh + sdg2_obesity + sdg4_primary + sdg4_tertiary
                             + sdg5_edat,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg3.xf)
MSR_fit_plm_imp_pan_sdg3.xf<-mean(fit_plm_imp_pan_sdg3.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg3.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.07 (higher than others))
fit_plm_imp_pan_sdg3.xfL<-plm(sdg3_lifee ~ sdg2_stuntihme + sdg3_tb + sdg4_tertiary + sdg9_intuse + sdg9_articles + sdg9_netacc 
                              + sdg16_safe,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg3.xfL)
MSR_fit_plm_imp_pan_sdg3.xfL<-mean(fit_plm_imp_pan_sdg3.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg3.xfL

# Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg3.xfLP<-plm(sdg3_lifee ~ sdg3_u5mort + sdg3_smoke +sdg4_tertiary + sdg9_intuse + sdg9_articles 
                              + sdg13_co2kgPerGDPPPP + sdg15_redlist + sdg16_homicides + sdg16_safe,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg3.xfLP)
MSR_fit_plm_imp_pan_sdg3.xfLP<-mean(fit_plm_imp_pan_sdg3.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg3.xfLP

# Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg3.xfLP2<-plm(sdg3_lifee ~ sdg4_tertiary + sdg9_intuse + sdg9_articles + sdg13_co2kgPerGDPPPP,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg3.xfLP2)
MSR_fit_plm_imp_pan_sdg3.xfLP2<-mean(fit_plm_imp_pan_sdg3.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg3.xfLP2

# SDG 3 David Collste Test
fit_plm_imp_pan_sdg3.collste<-plm(sdg3_lifee ~ sdg4_tertiary + sdg9_intuse,
                               #+ sdg7_elecac + sdg7_cleanfuel + sdg7_co2twh + sdg7_ren + sdg13_co2kgPerGDPPPP,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg3.collste)
MSR_fit_plm_imp_pan_sdg3.collste<-mean(fit_plm_imp_pan_sdg3.collste$residuals^2)
MSR_fit_plm_imp_pan_sdg3.collste

#SDG4#
fit_plm_imp_pan_sdg4.xf<-plm(sdg4_tertiary ~ sdg5_edat + sdg9_intuse + sdg9_articles + sdg9_rdex + sdg9_netacc
                             + sdg11_pm25 + sdg16_homicides,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg4.xf)
MSR_fit_plm_imp_pan_sdg4.xf<-mean(fit_plm_imp_pan_sdg4.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg4.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg4.xfL<-plm(sdg4_tertiary ~ sdg1_oecdpov + sdg2_undernsh + sdg3_lifee + sdg3_neonat
                              + sdg5_paygap + sdg9_intuse + sdg9_rdres,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg4.xfL)
MSR_fit_plm_imp_pan_sdg4.xfL<-mean(fit_plm_imp_pan_sdg4.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg4.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg4.xfLP<-plm(sdg4_tertiary ~ sdg3_hiv + sdg3_lifee + sdg5_fplmodel + sdg5_paygap 
                              + sdg7_cleanfuel + sdg8_yneet + sdg9_intuse + sdg9_rdres,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg4.xfLP)
MSR_fit_plm_imp_pan_sdg4.xfLP<-mean(fit_plm_imp_pan_sdg4.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg4.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg4.xfLP2<-plm(sdg4_tertiary ~ sdg3_hiv + sdg3_lifee + sdg9_intuse,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg4.xfLP2)
MSR_fit_plm_imp_pan_sdg4.xfLP2<-mean(fit_plm_imp_pan_sdg4.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg4.xfLP2

#SDG 4 David Collste test
fit_plm_imp_pan_sdg4.collste<-plm(sdg4_tertiary ~ sdg3_lifee + sdg9_intuse,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg4.collste)
MSR_fit_plm_imp_pan_sdg4.collste<-mean(fit_plm_imp_pan_sdg4.collste$residuals^2)
MSR_fit_plm_imp_pan_sdg4.collste

#SDG5#
fit_plm_imp_pan_sdg5.xf<-plm(sdg5_parl ~ sdg2_obesity + sdg2_wasteihme + sdg3_matmort + sdg3_fertility 
                             + sdg5_edat + sdg5_paygap + sdg9_intuse + sdg16_homicides + sdg16_safe,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg5.xf)
MSR_fit_plm_imp_pan_sdg5.xf<-mean(fit_plm_imp_pan_sdg5.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg5.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.055)
fit_plm_imp_pan_sdg5.xfL<-plm(sdg5_parl ~ sdg2_trophic + sdg3_swb + sdg5_fplmodel + sdg5_lfpr + sdg7_ren + sdg9_intuse
                              + sdg9_articles + sdg17_govex + sdg17_oda,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg5.xfL)
MSR_fit_plm_imp_pan_sdg5.xfL<-mean(fit_plm_imp_pan_sdg5.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg5.xfL

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.055)
fit_plm_imp_pan_sdg5.xfLP<-plm(sdg5_parl ~  sdg2_trophic + sdg3_swb + sdg4_earlyedu + sdg5_fplmodel + sdg5_lfpr + sdg9_intuse
                              + sdg14_trawl + sdg17_oda,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg5.xfLP)
MSR_fit_plm_imp_pan_sdg5.xfLP<-mean(fit_plm_imp_pan_sdg5.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg5.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.055)
fit_plm_imp_pan_sdg5.xfLP2<-plm(sdg5_parl ~ sdg5_fplmodel + sdg5_lfpr + sdg9_intuse + sdg14_trawl,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg5.xfLP2)
MSR_fit_plm_imp_pan_sdg5.xfLP2<-mean(fit_plm_imp_pan_sdg5.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg5.xfLP2

#SDG6#
fit_plm_imp_pan_sdg6.xf<-plm(sdg6_safesan ~ sdg2_stuntihme + sdg3_lifee + sdg3_tb + sdg3_neonat + sdg4_tertiary
                             + sdg11_pipedwat + GDP_PPP,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg6.xf)
MSR_fit_plm_imp_pan_sdg6.xf<-mean(fit_plm_imp_pan_sdg6.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg6.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.03)
fit_plm_imp_pan_sdg6.xfL<-plm(sdg6_safesan ~ sdg1_320pov + sdg3_u5mort + sdg3_fertility + sdg7_elecac + +sdg7_ren + sdg9_patents
                              + sdg11_pipedwat + sdg15_cpta,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg6.xfL)
MSR_fit_plm_imp_pan_sdg6.xfL<-mean(fit_plm_imp_pan_sdg6.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg6.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg6.xfLP<-plm(sdg6_safesan ~ sdg2_snmi + sdg3_fertility 
                              + sdg5_paygap
                              + sdg7_elecac + sdg9_intuse + sdg9_patents + sdg9_netacc
                              + sdg16_prison,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg6.xfLP)
MSR_fit_plm_imp_pan_sdg6.xfLP<-mean(fit_plm_imp_pan_sdg6.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg6.xfLP

#Fixed Effects version
fit_plm_imp_pan_sdg6.xfLP.fix<-plm(sdg6_safesan ~ sdg2_snmi + sdg3_fertility 
                               + sdg5_paygap
                               + sdg7_elecac + sdg9_intuse + sdg9_patents + sdg9_netacc
                               + sdg16_prison,
                               model='within',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg6.xfLP.fix)
MSR_fit_plm_imp_pan_sdg6.xfLP.fix<-mean(fit_plm_imp_pan_sdg6.xfLP.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg6.xfLP.fix

form<-sdg6_safesan ~ sdg2_snmi + sdg3_fertility + sdg5_paygap + sdg7_elecac + sdg9_intuse + sdg9_patents + sdg9_netacc + sdg16_prison
RorF6.xfLP<-phtest(fit_plm_imp_pan_sdg6.xfLP.fix,fit_plm_imp_pan_sdg6.xfLP)#, model = c("within","random"))
RorF6.xfLP<-phtest(form, data.pan.final.xf)#,model = c("random","within"))
RorF6.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg6.xfLP2<-plm(sdg6_safesan ~ sdg2_snmi + sdg3_fertility + sdg7_elecac + sdg9_intuse,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg6.xfLP2)
MSR_fit_plm_imp_pan_sdg6.xfLP2<-mean(fit_plm_imp_pan_sdg6.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg6.xfLP2

#SDG7#
fit_plm_imp_pan_sdg7.xf<-plm(sdg7_ren ~ sdg1_oecdpov + sdg3_lifee + sdg11_pm25
                             + sdg13_co2import + sdg13_co2kgPerGDPPPP + sdg7_cleanfuel + sdg9_articles,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg7.xf)
MSR_fit_plm_imp_pan_sdg7.xf<-mean(fit_plm_imp_pan_sdg7.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg7.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.03)
fit_plm_imp_pan_sdg7.xfL<-plm(sdg7_ren ~ Population + sdg2_trophic + sdg5_parl + sdg5_paygap + sdg6_safesan
                              + sdg8_empop + sdg13_co2kgPerGDPPPP + sdg14_cpma + sdg14_cleanwat,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg7.xfL)
MSR_fit_plm_imp_pan_sdg7.xfL<-mean(fit_plm_imp_pan_sdg7.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg7.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg7.xfLP<-plm(sdg7_ren ~ Population + sdg2_trophic + sdg4_tertiary + sdg5_fplmodel
                              + sdg7_cleanfuel + sdg2_stuntihme + sdg8_empop + sdg9_patents 
                              + sdg11_pipedwat + sdg13_co2kgPerGDPPPP + sdg15_cpta,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg7.xfLP)
MSR_fit_plm_imp_pan_sdg7.xfLP<-mean(fit_plm_imp_pan_sdg7.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg7.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg7.xfLP2<-plm(sdg7_ren ~ sdg2_trophic + sdg4_tertiary + sdg2_stuntihme + sdg8_empop + sdg13_co2kgPerGDPPPP,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg7.xfLP2)
MSR_fit_plm_imp_pan_sdg7.xfLP2<-mean(fit_plm_imp_pan_sdg7.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg7.xfLP2

#SDG7 Davd Collste test
fit_plm_imp_pan_sdg7.collste<-plm(sdg7_elecac ~ sdg3_lifee + sdg9_intuse,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg7.collste)
MSR_fit_plm_imp_pan_sdg7.collste<-mean(fit_plm_imp_pan_sdg7.collste$residuals^2)
MSR_fit_plm_imp_pan_sdg7.collste

#SDG8# #Should yneet be allowed to be a predictor? it is a very similar metric. obesity contributes much to R^2 without being significant
fit_plm_imp_pan_sdg8.xf<-plm(sdg8_empop ~ sdg1_320pov + sdg2_trophic + sdg2_obesity + sdg3_tb + sdg3_fertility
                             + sdg4_tertiary + sdg9_articles + sdg9_intuse + sdg11_pm25 + sdg10_adjgini,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg8.xf)
MSR_fit_plm_imp_pan_sdg8.xf<-mean(fit_plm_imp_pan_sdg8.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg8.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.033)
fit_plm_imp_pan_sdg8.xfL<-plm(sdg8_empop ~ sdg2_trophic + sdg3_swb + sdg7_ren + sdg8_yneet + sdg9_intuse
                              + sdg9_articles + sdg11_pm25 + sdg16_cpi,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg8.xfL)
MSR_fit_plm_imp_pan_sdg8.xfL<-mean(fit_plm_imp_pan_sdg8.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg8.xfL

#Pruned Lasso inputs with transformed pan imputed vars. Lagging yneet helped R^2, MSR, and didn't make model insignificant
fit_plm_imp_pan_sdg8.xfLP<-plm(sdg8_empop ~ sdg3_swb + sdg3_incomeg + sdg3_smoke
                              + sdg5_paygap + sdg7_ren + sdg8_yneet + sdg9_intuse + sdg9_articles 
                              + sdg9_patents + sdg11_pm25 + sdg14_cpma + sdg16_safe,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg8.xfLP)
MSR_fit_plm_imp_pan_sdg8.xfLP<-mean(fit_plm_imp_pan_sdg8.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg8.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars. Add intuse back if wanted
fit_plm_imp_pan_sdg8.xfLP2<-plm(sdg8_empop ~ sdg3_swb + sdg3_incomeg + sdg8_yneet + sdg11_pm25 + sdg14_cpma,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg8.xfLP2)
MSR_fit_plm_imp_pan_sdg8.xfLP2<-mean(fit_plm_imp_pan_sdg8.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg8.xfLP2

#SDG9#
fit_plm_imp_pan_sdg9.xf<-plm(sdg9_rdex ~ sdg9_patents + sdg3_lifee + sdg8_empop + sdg2_obesity + sdg5_edat
                             + sdg11_pm25 + sdg17_govex + sdg16_prison,
                             model='random',
                             effect='twoway',
                             data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg9.xf)
MSR_fit_plm_imp_pan_sdg9.xf<-mean(fit_plm_imp_pan_sdg9.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg9.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.025)
fit_plm_imp_pan_sdg9.xfL<-plm(sdg9_rdex ~ sdg2_undernsh + sdg3_lifee + sdg5_paygap + sdg9_articles + sdg9_rdres + sdg9_patents
                              + sdg10_elder + sdg15_cpta,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg9.xfL)
MSR_fit_plm_imp_pan_sdg9.xfL<-mean(fit_plm_imp_pan_sdg9.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg9.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg9.xfLP<-plm(sdg9_rdex ~ GDP_PPP + sdg2_undernsh + sdg3_lifee
                              + sdg5_paygap + sdg9_rdres + sdg9_patents
                              + sdg16_rsf,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg9.xfLP)
MSR_fit_plm_imp_pan_sdg9.xfLP<-mean(fit_plm_imp_pan_sdg9.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg9.xfLP

#Fixed Effects version
fit_plm_imp_pan_sdg9.xfLP.fix<-plm(sdg9_rdex ~ GDP_PPP + sdg2_undernsh + sdg3_lifee
                               + sdg5_paygap + sdg9_rdres + sdg9_patents
                               + sdg16_rsf,
                               model='within',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg9.xfLP.fix)
MSR_fit_plm_imp_pan_sdg9.xfLP.fix<-mean(fit_plm_imp_pan_sdg9.xfLP.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg9.xfLP.fix


form9.xfLP<-sdg9_rdex ~ GDP_PPP + sdg2_undernsh + sdg3_lifee + sdg5_paygap + sdg9_rdres + sdg9_patents + sdg16_rsf
phtest(form9.xfLP,data.pan.final.xf,model = c("within","random"))
phtest(fit_plm_imp_pan_sdg9.xfLP.fix,fit_plm_imp_pan_sdg9.xfLP)

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg9.xfLP2<-plm(sdg9_rdex ~ sdg3_lifee + sdg9_rdres + sdg9_patents,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg9.xfLP2)
MSR_fit_plm_imp_pan_sdg9.xfLP2<-mean(fit_plm_imp_pan_sdg9.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg9.xfLP2

#Fixed Effects Version

#SDG9 David Collste Test
fit_plm_imp_pan_sdg9.collste<-plm(sdg9_intuse ~ sdg3_lifee + sdg4_tertiary + sdg7_elecac,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg9.collste)
MSR_fit_plm_imp_pan_sdg9.collste<-mean(fit_plm_imp_pan_sdg9.collste$residuals^2)
MSR_fit_plm_imp_pan_sdg9.collste

#SDG10# Should adjgini be included? too similar to palma. OECDpov insanely better than 320pov. OECD pov is relative and is too close to palma
fit_plm_imp_pan_sdg10.xf<-plm(sdg10_palma ~ sdg1_320pov + sdg3_fertility + sdg9_articles
                              + sdg6_safewat + sdg9_patents + sdg16_cpi,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg10.xf)
MSR_fit_plm_imp_pan_sdg10.xf<-mean(fit_plm_imp_pan_sdg10.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg10.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.015)
fit_plm_imp_pan_sdg10.xfL<-plm(sdg10_palma ~ sdg1_oecdpov + sdg2_wasteihme + sdg2_crlyld + sdg3_fertility 
                               + sdg5_fplmodel + sdg7_elecac + sdg10_adjgini + sdg15_redlist,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg10.xfL)
MSR_fit_plm_imp_pan_sdg10.xfL<-mean(fit_plm_imp_pan_sdg10.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg10.xfL

#Pruned Lasso inputs with transformed pan imputed vars. Left unpruned to show that pruning can hurt R^2. unpruned R^2=0.685, Pruned=0.65
fit_plm_imp_pan_sdg10.xfLP<-plm(sdg10_palma ~ sdg1_oecdpov + sdg2_wasteihme + sdg2_crlyld + sdg2_snmi
                               + sdg3_fertility + sdg3_smoke + sdg4_second
                               + sdg5_fplmodel + sdg5_lfpr + sdg7_elecac + sdg7_cleanfuel + sdg10_adjgini
                               + sdg11_pm25 + sdg13_co2kgPerGDPPPP + sdg14_cleanwat + sdg14_fishstocks + sdg15_redlist,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg10.xfLP)
MSR_fit_plm_imp_pan_sdg10.xfLP<-mean(fit_plm_imp_pan_sdg10.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg10.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars.
fit_plm_imp_pan_sdg10.xfLP2<-plm(sdg10_palma ~ sdg1_oecdpov + sdg3_fertility + sdg3_smoke,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg10.xfLP2)
MSR_fit_plm_imp_pan_sdg10.xfLP2<-mean(fit_plm_imp_pan_sdg10.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg10.xfLP2

#SDG11#
fit_plm_imp_pan_sdg11.xf<-plm(sdg11_pm25 ~ sdg2_obesity + sdg7_ren + sdg5_parl + sdg7_cleanfuel
                              + sdg9_rdex + sdg8_empop + sdg15_cpta + sdg13_co2kgPerGDPPPP + sdg16_prison,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg11.xf)
MSR_fit_plm_imp_pan_sdg11.xf<-mean(fit_plm_imp_pan_sdg11.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg11.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg11.xfL<-plm(sdg11_pm25 ~ sdg2_trophic + sdg5_edat + sdg5_lfpr + sdg8_impacc + sdg8_empop 
                               + sdg16_cpi + sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg11.xfL)
MSR_fit_plm_imp_pan_sdg11.xfL<-mean(fit_plm_imp_pan_sdg11.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg11.xfL

#Pruned Lasso inputs with transformed pan imputed vars. Rentover somehow contributes hugely to R^2
fit_plm_imp_pan_sdg11.xfLP<-plm(sdg11_pm25 ~ GDP_PPP + sdg1_wpc + sdg2_trophic + sdg3_lifee + sdg3_vac
                               + sdg5_fplmodel + sdg5_edat + sdg5_lfpr + sdg5_parl + sdg6_scarcew
                               + sdg6_safesan + sdg8_empop + sdg11_rentover + sdg14_cpma
                               + sdg15_cpta,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg11.xfLP)
MSR_fit_plm_imp_pan_sdg11.xfLP<-mean(fit_plm_imp_pan_sdg11.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg11.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars. Very poor R^2.
fit_plm_imp_pan_sdg11.xfLP2<-plm(sdg11_pm25 ~ sdg2_trophic + sdg3_vac + sdg5_fplmodel + sdg13_co2kgPerGDPPPP,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg11.xfLP2)
MSR_fit_plm_imp_pan_sdg11.xfLP2<-mean(fit_plm_imp_pan_sdg11.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg11.xfLP2

#Pruned Lasso inputs with transformed pan imputed vars and lags
fit_plm_imp_pan_sdg11.xfLPt<-plm(sdg11_pm25 ~ GDP_PPP + lag(GDP_PPP) + sdg2_trophic + lag(sdg2_trophic) 
                                + sdg3_lifee + lag(sdg3_lifee) + sdg3_vac + lag(sdg3_vac)
                                + sdg5_fplmodel + lag(sdg5_fplmodel) + sdg6_safesan + lag(sdg6_safesan)
                                + sdg8_empop + lag(sdg8_empop) + sdg13_co2kgPerGDPPPP + lag(sdg13_co2kgPerGDPPPP),
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg11.xfLPt)
MSR_fit_plm_imp_pan_sdg11.xfLPt<-mean(fit_plm_imp_pan_sdg11.xfLPt$residuals^2)
MSR_fit_plm_imp_pan_sdg11.xfLPt

#SDG12#
#No target

#SDG13#
fit_plm_imp_pan_sdg13.xf<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren + sdg7_cleanfuel + sdg5_paygap + GDP_PPP 
                              + sdg7_co2twh + sdg4_tertiary + sdg3_lifee + sdg3_u5mort + sdg8_empop,#lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg13.xf)
MSR_fit_plm_imp_pan_sdg13.xf<-mean(fit_plm_imp_pan_sdg13.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg13.xf

# Fixed Effects version
fit_plm_imp_pan_sdg13.xf.fix<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren + sdg7_cleanfuel + sdg5_paygap + GDP_PPP 
                              + sdg7_co2twh + sdg4_tertiary + sdg3_lifee + sdg3_u5mort + sdg8_empop,#lag(sdg7_co2twh) + lag(sdg7_elecac) + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                              model='within',
                              #effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg13.xf.fix)
MSR_fit_plm_imp_pan_sdg13.xf.fix<-mean(fit_plm_imp_pan_sdg13.xf.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg13.xf.fix

RorF13.xf<-phtest(fit_plm_imp_pan_sdg13.xf.fix,fit_plm_imp_pan_sdg13.xf)
RorF13.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg13.xfL<-plm(sdg13_co2kgPerGDPPPP ~ sdg2_crlyld + sdg3_lifee + sdg3_fertility + sdg4_earlyedu + sdg5_paygap 
                               + sdg7_co2twh + sdg7_ren + sdg9_intuse + sdg16_rsf + sdg17_oda,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg13.xfL)
MSR_fit_plm_imp_pan_sdg13.xfL<-mean(fit_plm_imp_pan_sdg13.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg13.xfL

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.01 then pruned)
fit_plm_imp_pan_sdg13.xfLP<-plm(sdg13_co2kgPerGDPPPP ~ sdg2_crlyld + sdg3_lifee + sdg3_fertility + sdg6_scarcew
                                + sdg7_co2twh+sdg7_ren + sdg9_intuse + sdg14_cleanwat + sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg13.xfLP)
MSR_fit_plm_imp_pan_sdg13.xfLP<-mean(fit_plm_imp_pan_sdg13.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg13.xfLP

#Extra pruned Lasso inputs with transformed pan imputed vars (lambda = 0.01 then pruned)
fit_plm_imp_pan_sdg13.xfLP2<-plm(sdg13_co2kgPerGDPPPP ~ sdg3_lifee + sdg3_fertility
                                 + sdg7_co2twh+sdg7_ren + sdg9_intuse,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg13.xfLP2)
MSR_fit_plm_imp_pan_sdg13.xfLP2<-mean(fit_plm_imp_pan_sdg13.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg13.xfLP2

#SDG14#
fit_plm_imp_pan_sdg14.xf<-plm(sdg14_cpma ~ sdg15_cpfa + sdg3_lifee + sdg15_redlist + sdg9_rdex + sdg11_transport + sdg14_cleanwat + sdg15_cpta,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg14.xf)
MSR_fit_plm_imp_pan_sdg14.xf<-mean(fit_plm_imp_pan_sdg14.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg14.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg14.xfL<-plm(sdg14_cpma ~ sdg3_tb + sdg3_hiv + sdg4_earlyedu + sdg7_ren + sdg9_articles
                               + sdg14_cleanwat + sdg15_cpta +sdg15_cpfa,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg14.xfL)
MSR_fit_plm_imp_pan_sdg14.xfL<-mean(fit_plm_imp_pan_sdg14.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg14.xfL

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg14.xfLP<-plm(sdg14_cpma ~ Population + sdg3_tb + sdg3_hiv + sdg9_articles
                                + sdg14_cleanwat + sdg14_fishstocks + sdg15_cpta + sdg16_homicides,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg14.xfLP)
MSR_fit_plm_imp_pan_sdg14.xfLP<-mean(fit_plm_imp_pan_sdg14.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg14.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg14.xfLP2<-plm(sdg14_cpma ~ sdg3_hiv + sdg9_articles + sdg14_fishstocks + sdg15_cpta,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg14.xfLP2)
MSR_fit_plm_imp_pan_sdg14.xfLP2<-mean(fit_plm_imp_pan_sdg14.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg14.xfLP2

#SDG15#
fit_plm_imp_pan_sdg15.xf<-plm(sdg15_redlist ~ sdg2_snmi + sdg15_cpfa + sdg3_lifee + sdg14_cpma
                              + sdg14_cleanwat + sdg9_articles + sdg9_intuse + sdg9_netacc,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg15.xf)
MSR_fit_plm_imp_pan_sdg15.xf<-mean(fit_plm_imp_pan_sdg15.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg15.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg15.xfL<-plm(sdg15_redlist ~ sdg3_lifee + sdg3_incomeg + sdg6_scarcew + sdg10_palma 
                               + sdg10_elder + sdg15_cpfa + sdg16_rsf + sdg17_oda,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg15.xfL)
MSR_fit_plm_imp_pan_sdg15.xfL<-mean(fit_plm_imp_pan_sdg15.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg15.xfL

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg15.xfLP<-plm(sdg15_redlist ~ sdg2_snmi + sdg3_hiv + sdg3_lifee
                               + sdg3_fertility + sdg3_incomeg + sdg6_safewat + sdg15_cpfa,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg15.xfLP)
MSR_fit_plm_imp_pan_sdg15.xfLP<-mean(fit_plm_imp_pan_sdg15.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg15.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg15.xfLP2<-plm(sdg15_redlist ~ sdg3_lifee + sdg3_fertility + sdg15_cpfa,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg15.xfLP2)
MSR_fit_plm_imp_pan_sdg15.xfLP2<-mean(fit_plm_imp_pan_sdg15.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg15.xfLP2

#SDG16#
fit_plm_imp_pan_sdg16.xf<-plm(sdg16_homicides ~ sdg16_cpi + sdg3_lifee + sdg1_320pov + sdg2_obesity 
                              + sdg3_incomeg + sdg5_parl + sdg11_pm25 + sdg7_elecac,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg16.xf)
MSR_fit_plm_imp_pan_sdg16.xf<-mean(fit_plm_imp_pan_sdg16.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg16.xf

# sdg 16 with lasso variables. s=0.015
fit_plm_imp_pan_sdg16.xfL<-plm(sdg16_homicides ~ sdg1_320pov + sdg2_obesity + sdg3_matmort + sdg3_tb + sdg3_lifee
                               + sdg3_incomeg + sdg6_safewat + sdg16_prison,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg16.xfL)
MSR_fit_plm_imp_pan_sdg16.xfL<-mean(fit_plm_imp_pan_sdg16.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg16.xfL

# Pruned SDG 16 with lasso variables. Left more than 8 vars in
fit_plm_imp_pan_sdg16.xfLP<-plm(sdg16_homicides ~ sdg1_320pov + sdg1_oecdpov + sdg2_obesity + sdg3_matmort 
                               + sdg3_tb + sdg3_lifee + sdg3_incomeg 
                               + sdg5_parl + sdg6_safewat + sdg7_elecac + sdg7_cleanfuel + sdg11_pm25,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg16.xfLP)
MSR_fit_plm_imp_pan_sdg16.xfLP<-mean(fit_plm_imp_pan_sdg16.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg16.xfLP

# Extra Pruned SDG 16 with lasso variables. Left more than 8 vars in
fit_plm_imp_pan_sdg16.xfLP2<-plm(sdg16_homicides ~ sdg1_320pov + sdg2_obesity + sdg3_tb + sdg7_elecac + sdg7_cleanfuel,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg16.xfLP2)
MSR_fit_plm_imp_pan_sdg16.xfLP2<-mean(fit_plm_imp_pan_sdg16.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg16.xfLP2

#SDG17#
fit_plm_imp_pan_sdg17.xf<-plm(sdg17_govex ~ sdg16_cpi + sdg3_lifee + sdg1_oecdpov + sdg2_obesity + sdg5_lfpr
                              + sdg5_parl+ sdg9_netacc + sdg9_rdex + sdg14_cpma + sdg13_co2import,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg17.xf)
MSR_fit_plm_imp_pan_sdg17.xf<-mean(fit_plm_imp_pan_sdg17.xf$residuals^2)
MSR_fit_plm_imp_pan_sdg17.xf

#Lasso inputs with transformed pan imputed vars (lambda = 0.03)
fit_plm_imp_pan_sdg17.xfL<-plm(sdg17_govex ~ sdg2_trophic + sdg2_crlyld + sdg5_lfpr + sdg5_parl
                               + sdg9_articles + sdg9_rdres + sdg11_pm25 +sdg16_safe + sdg16_cpi ,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg17.xfL)
MSR_fit_plm_imp_pan_sdg17.xfL<-mean(fit_plm_imp_pan_sdg17.xfL$residuals^2)
MSR_fit_plm_imp_pan_sdg17.xfL

#Pruned Lasso inputs with transformed pan imputed vars. Left in more than 8. Last 2 to be reomved had large effect on R^2
fit_plm_imp_pan_sdg17.xfLP<-plm(sdg17_govex ~ GDP_PPP + sdg1_oecdpov
                               + sdg5_lfpr + sdg5_parl + sdg5_paygap + sdg7_ren
                               + sdg9_articles + sdg13_co2import 
                               + sdg16_cpi + sdg17_oda,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg17.xfLP)
MSR_fit_plm_imp_pan_sdg17.xfLP<-mean(fit_plm_imp_pan_sdg17.xfLP$residuals^2)
MSR_fit_plm_imp_pan_sdg17.xfLP

#Extra Pruned Lasso inputs with transformed pan imputed vars.
fit_plm_imp_pan_sdg17.xfLP2<-plm(sdg17_govex ~ GDP_PPP + sdg1_oecdpov + sdg5_lfpr + sdg9_articles + sdg17_oda,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf)
summary(fit_plm_imp_pan_sdg17.xfLP2)
MSR_fit_plm_imp_pan_sdg17.xfLP2<-mean(fit_plm_imp_pan_sdg17.xfLP2$residuals^2)
MSR_fit_plm_imp_pan_sdg17.xfLP2

######################################################
#### Make a new Data frame with interaction terms ####
######################################################

newdat = data.pan.final.xf[,c(-1,-2)]
newdatnames = matrix(NA,0,1)
for (k in 1:(length(data.pan.final.xf)-3)){
  for (n in (k+1):(length(data.pan.final.xf)-2)){
    newdat[,length(newdat)+1] = newdat[,k]*newdat[,n]
    newdatnames[length(newdatnames)+1]<- paste(names(newdat[k]),"*",names(newdat[n]), sep="")
  }
}
newdat=c(data.pan.final.xf[1:2],newdat)
names(newdat)[(length(data.pan.final.xf)+1):length(newdat)]<-newdatnames
data.pan.int<-data.frame(newdat)

##############################################################
#### Repeat Lasso on Interactions Data frame for each SDG ####
##############################################################

#Lasso on pan imputed data, sdg13 target
library(mefa4)

targ13col<-grep("^sdg13_co2kgPerGDPPPP",colnames(data.pan.final.xf))
targ13excl<-grep("sdg13_co2kgPerGDPPPP",colnames(data.pan.int))
lasso_input_sdg13_pan_int <- data.pan.int[,c(-1,-2,-targ13excl)] #Use all vars except year, country, id, co2 target and all interactions with co2 target
lasso_target_sdg13_pan_int <- data.pan.int[,targ13col]
nvars = ncol(lasso_input_sdg13_pan_int)
dfmax = nvars+1
nobs<-nrow(data.pan.int)

Lasso_sdg13_co2kgPerGDPPPP_pan_int<-glmnet(
  lasso_input_sdg13_pan_int,
  lasso_target_sdg13_pan_int,
  #maxp = 18,
  family = c("gaussian"))

Melt(coef(Lasso_sdg13_co2kgPerGDPPPP_pan_int, s = 0.06))

#### SDG1 OECDpov ####
targ1col<-grep("^sdg1_oecdpov",colnames(data.pan.final.xf))
targ1excl<-grep("sdg1_oecdpov",colnames(data.pan.int))
targ1excl2<-grep("(sdg10_palma|sdg1_oecdpov|sdg10_adjgini|sdg10_elder)",colnames(data.pan.int))
lasso_input_sdg1 <- data.pan.int[,c(-1:-2,-targ1excl)]
lasso_input_sdg1_2 <- data.pan.int[,c(-1:-2,-targ1excl2)] 
lasso_target_sdg1 <- data.pan.int[,targ1col] 
Lasso_sdg1_oecdpov_int<-glmnet(
  lasso_input_sdg1,
  lasso_target_sdg1,
  family = c("gaussian"))
Lasso_sdg1_oecdpov_int2<-glmnet(
  lasso_input_sdg1_2,
  lasso_target_sdg1,
  family = c("gaussian"))

Melt(coef(Lasso_sdg1_oecdpov_int, s = 0.09))
Melt(coef(Lasso_sdg1_oecdpov_int2, s = 0.091)) #not using uninteresting or "cheating" input vars

#### SDG2 Undernsh ####
targ2col<-grep("^sdg2_undernsh",colnames(data.pan.final.xf))
targ2excl<-grep("sdg2_undernsh",colnames(data.pan.int))
targ2excl2<-grep("(sdg2_undernsh|sdg2_wasteihme|sdg2_stuntihme)",colnames(data.pan.int))
lasso_input_sdg2 <- data.pan.int[,c(-1:-2,-targ2excl)]
lasso_input_sdg2_2 <- data.pan.int[,c(-1:-2,-targ2excl2)] 
lasso_target_sdg2 <- data.pan.int[,targ2col] 
Lasso_sdg2_undernsh_int<-glmnet(
  lasso_input_sdg2,
  lasso_target_sdg2,
  family = c("gaussian"))
Lasso_sdg2_undernsh_int2<-glmnet(
  lasso_input_sdg2_2,
  lasso_target_sdg2,
  family = c("gaussian"))

Melt(coef(Lasso_sdg2_undernsh_int, s = 0.034))
Melt(coef(Lasso_sdg2_undernsh_int2, s = 0.034)) #not using uninteresting or "cheating" input vars
#plot(Lasso_sdg2_undernsh)

#### SDG2 Wasteihme ####
targ2.2col<-grep("^sdg2_wasteihme",colnames(data.pan.final.xf))
targ2.2excl<-grep("sdg2_wasteihme",colnames(data.pan.int))
targ2.2excl2<-grep("(sdg2_undernsh|sdg2_wasteihme|sdg2_stuntihme|sdg2_obesity)",colnames(data.pan.int))
lasso_input_sdg2.2 <- data.pan.int[,c(-1:-2,-targ2.2excl)]
lasso_input_sdg2.2_2 <- data.pan.int[,c(-1:-2,-targ2.2excl2)] 
lasso_target_sdg2.2 <- data.pan.int[,targ2.2col] 
Lasso_sdg2.2_wasteihme_int<-glmnet(
  lasso_input_sdg2.2,
  lasso_target_sdg2.2,
  family = c("gaussian"))
Lasso_sdg2.2_wasteihme_int2<-glmnet(
  lasso_input_sdg2.2_2,
  lasso_target_sdg2.2,
  family = c("gaussian"))

Melt(coef(Lasso_sdg2.2_wasteihme_int, s = 0.034))
Melt(coef(Lasso_sdg2.2_wasteihme_int2, s = 0.034)) #not using uninteresting or "cheating" input vars
#plot(Lasso_sdg2.2_wasteihme_int)

#### SDG3 lifee ####
targ3col<-grep("^sdg3_lifee",colnames(data.pan.final.xf))
targ3excl<-grep("sdg3_lifee",colnames(data.pan.int))
targ3excl2<-grep("(sdg3_lifee|sdg3_matmort|sdg3_u5mort|sdg3_neonat|sdg16_homicides)",colnames(data.pan.int))
lasso_input_sdg3 <- data.pan.int[,c(-1:-2,-targ3excl)]
lasso_input_sdg3_2 <- data.pan.int[,c(-1:-2,-targ3excl2)]
lasso_target_sdg3 <- data.pan.int[,targ3col] 
Lasso_sdg3_lifee_int<-glmnet(
  lasso_input_sdg3,
  lasso_target_sdg3,
  family = c("gaussian"))
Lasso_sdg3_lifee_int2<-glmnet(
  lasso_input_sdg3_2,
  lasso_target_sdg3,
  family = c("gaussian"))

Melt(coef(Lasso_sdg3_lifee_int, s = 0.1))
Melt(coef(Lasso_sdg3_lifee_int2, s = 0.1)) #not using uninteresting or "cheating" input vars


#### SDG4 tertiary ####
targ4col<-grep("^sdg4_tertiary",colnames(data.pan.final.xf))
targ4excl<-grep("sdg4_tertiary",colnames(data.pan.int))
lasso_input_sdg4 <- data.pan.int[,c(-1:-2,-targ4excl)] 
lasso_target_sdg4 <- data.pan.int[,targ4col] 
Lasso_sdg4_tertiary_int<-glmnet(
  lasso_input_sdg4,
  lasso_target_sdg4,
  family = c("gaussian"))

Melt(coef(Lasso_sdg4_tertiary_int, s = 0.075))


#### SDG5 parl ####
targ5col<-grep("^sdg5_parl",colnames(data.pan.final.xf))
targ5excl<-grep("sdg5_parl",colnames(data.pan.int))
lasso_input_sdg5 <- data.pan.int[,c(-1:-2,-targ5excl)]
lasso_target_sdg5 <- data.pan.int[,targ5col] 
Lasso_sdg5_parl_int<-glmnet(
  lasso_input_sdg5,
  lasso_target_sdg5,
  family = c("gaussian"))

Melt(coef(Lasso_sdg5_parl_int, s = 0.11))


#### SDG6 safesan ####
targ6col<-grep("^sdg6_safesan",colnames(data.pan.final.xf))
targ6excl<-grep("sdg6_safesan",colnames(data.pan.int))
lasso_input_sdg6 <- data.pan.int[,c(-1:-2,-targ6excl)] #Use all vars except year, country, id, target
lasso_target_sdg6 <- data.pan.int[,targ6col] 
Lasso_sdg6_safesan_int<-glmnet(
  lasso_input_sdg6,
  lasso_target_sdg6,
  family = c("gaussian"))

Melt(coef(Lasso_sdg6_safesan_int, s = 0.055))


#### SDG7 ren ####
targ7col<-grep("^sdg7_ren",colnames(data.pan.final.xf))
targ7excl<-grep("sdg7_ren",colnames(data.pan.int))
lasso_input_sdg7 <- data.pan.int[,c(-1:-2,-targ7excl)] #Use all vars except year, country, id, target
lasso_target_sdg7 <- data.pan.int[,targ7col] 
Lasso_sdg7_ren_int<-glmnet(
  lasso_input_sdg7,
  lasso_target_sdg7,
  family = c("gaussian"))

Melt(coef(Lasso_sdg7_ren_int, s = 0.045))


#### SDG8 empop ####
targ8col<-grep("^sdg8_empop",colnames(data.pan.final.xf))
targ8excl<-grep("sdg8_empop",colnames(data.pan.int))
targ8excl2<-grep("(sdg8_empop|sdg8_yneet)",colnames(data.pan.int))
lasso_input_sdg8 <- data.pan.int[,c(-1:-2,-targ8excl)]
lasso_input_sdg8_2 <- data.pan.int[,c(-1:-2,-targ8excl2)]
lasso_target_sdg8 <- data.pan.int[,targ8col] 
Lasso_sdg8_empop_int<-glmnet(
  lasso_input_sdg8,
  lasso_target_sdg8,
  family = c("gaussian"))
Lasso_sdg8_empop_int2<-glmnet(
  lasso_input_sdg8_2,
  lasso_target_sdg8,
  family = c("gaussian"))

Melt(coef(Lasso_sdg8_empop_int, s = 0.072))
Melt(coef(Lasso_sdg8_empop_int2, s = 0.077))


#### SDG9 rdex ####
targ9col<-grep("^sdg9_rdex",colnames(data.pan.final.xf))
targ9excl<-grep("sdg9_rdex",colnames(data.pan.int))
targ9excl2<-grep("(sdg9_rdex|sdg9_patents|sdg9_articles|sdg9_rdres)",colnames(data.pan.int))
lasso_input_sdg9 <- data.pan.int[,c(-1:-2,-targ9excl)]
lasso_input_sdg9_2 <- data.pan.int[,c(-1:-2,-targ9excl2)]
lasso_target_sdg9 <- data.pan.int[,targ9col] 
Lasso_sdg9_rdex_int<-glmnet(
  lasso_input_sdg9,
  lasso_target_sdg9,
  family = c("gaussian"))
Lasso_sdg9_rdex_int2<-glmnet(
  lasso_input_sdg9_2,
  lasso_target_sdg9,
  family = c("gaussian"))

Melt(coef(Lasso_sdg9_rdex_int, s = 0.09))
Melt(coef(Lasso_sdg9_rdex_int2, s = 0.09))


#### SDG10 palma ####
targ10col<-grep("^(sdg10_palma)",colnames(data.pan.final.xf))
targ10excl<-grep("sdg10_palma",colnames(data.pan.int))
targ10excl2<-grep("(sdg10_palma|sdg1_oecdpov|sdg10_adjgini)",colnames(data.pan.int))
lasso_input_sdg10 <- data.pan.int[,c(-1:-2,-targ10excl)]
lasso_input_sdg10_2 <- data.pan.int[,c(-1:-2,-targ10excl2)]
lasso_target_sdg10 <- data.pan.int[,targ10col] 
Lasso_sdg10_palma_int<-glmnet(
  lasso_input_sdg10,
  lasso_target_sdg10,
  family = c("gaussian"))
Lasso_sdg10_palma_int2<-glmnet(
  lasso_input_sdg10_2,
  lasso_target_sdg10,
  family = c("gaussian"))

Melt(coef(Lasso_sdg10_palma_int, s = 0.03))
Melt(coef(Lasso_sdg10_palma_int2, s = 0.075))


#### SDG11 pm2.5 #### POOR PERFORMANCE - REVISIT
targ11col<-grep("^(sdg11_pm25)",colnames(data.pan.final.xf))
targ11excl<-grep("sdg11_pm25",colnames(data.pan.int))
lasso_input_sdg11 <- data.pan.int[,c(-1:-2,-targ11excl)] #Use all vars except year, country, id, target
lasso_target_sdg11 <- data.pan.int[,targ11col] 
Lasso_sdg11_pm25_int<-glmnet(
  lasso_input_sdg11,
  lasso_target_sdg11,
  family = c("gaussian"))

Melt(coef(Lasso_sdg11_pm25_int, s = 0.06))


#### SDG12 no target ####
#### SDG13 at the top ####

#### SDG14 cpma ####
targ14col<-grep("^(sdg14_cpma)",colnames(data.pan.final.xf))
targ14excl<-grep("sdg14_cpma",colnames(data.pan.int))
lasso_input_sdg14 <- data.pan.int[,c(-1:-2,-targ14excl)] #Use all vars except year, country, id, target
lasso_target_sdg14 <- data.pan.int[,targ14col] 
Lasso_sdg14_cpma_int<-glmnet(
  lasso_input_sdg14,
  lasso_target_sdg14,
  family = c("gaussian"))

Melt(coef(Lasso_sdg14_cpma_int, s = 0.068))


#### SDG15 redlist ####
targ15col<-grep("^(sdg15_redlist)",colnames(data.pan.final.xf))
targ15excl<-grep("sdg15_redlist",colnames(data.pan.int))
lasso_input_sdg15 <- data.pan.int[,c(-1:-2,-targ15excl)] #Use all vars except year, country, id, target
lasso_target_sdg15 <- data.pan.int[,targ15col] 
Lasso_sdg15_redlist_int<-glmnet(
  lasso_input_sdg15,
  lasso_target_sdg15,
  family = c("gaussian"))

Melt(coef(Lasso_sdg15_redlist_int, s = 0.09))


#### SDG16 homicides ####
targ16col<-grep("^(sdg16_homicides)",colnames(data.pan.final.xf))
targ16excl<-grep("sdg16_homicides",colnames(data.pan.int))
lasso_input_sdg16 <- data.pan.int[,c(-1:-2,-targ16excl)] #Use all vars except year, country, id, target
lasso_target_sdg16 <- data.pan.int[,targ16col] 
Lasso_sdg16_homicides_int<-glmnet(
  lasso_input_sdg16,
  lasso_target_sdg16,
  family = c("gaussian"))

Melt(coef(Lasso_sdg16_homicides_int, s = 0.058))


#### SDG17 govex ####
targ17col<-grep("^(sdg17_govex)",colnames(data.pan.final.xf))
targ17excl<-grep("sdg17_govex",colnames(data.pan.int))
lasso_input_sdg17 <- data.pan.int[,c(-1:-2,-targ17excl)] #Use all vars except year, country, id, target
lasso_target_sdg17 <- data.pan.int[,targ17col] 
Lasso_sdg17_govex_int<-glmnet(
  lasso_input_sdg17,
  lasso_target_sdg17,
  family = c("gaussian"))

Melt(coef(Lasso_sdg17_govex_int, s = 0.09))


##################################
# interaction terms panel models #
##################################

fit_plm_imp_pan_sdg13.int<-plm(sdg13_co2kgPerGDPPPP ~ sdg2_crlyld*sdg7_ren +sdg3_lifee*sdg4_earlyedu + sdg3_lifee*sdg7_ren + sdg3_lifee + sdg17_oda 
                               + sdg4_earlyedu + sdg9_intuse +  sdg5_paygap*sdg7_co2twh,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg13.int)
MSR_fit_plm_imp_pan_sdg13.int<-mean(fit_plm_imp_pan_sdg13.int$residuals^2)
MSR_fit_plm_imp_pan_sdg13.int

# Fixed Effects version
fit_plm_imp_pan_sdg13.int.fix<-plm(sdg13_co2kgPerGDPPPP ~ sdg2_undernsh*sdg5_paygap + sdg3_hiv*sdg5_paygap + sdg3_lifee
                               + sdg7_ren*sdg16_rsf + sdg9_netacc*sdg14_cleanwat,
                               model='within',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg13.int.fix)
MSR_fit_plm_imp_pan_sdg13.int.fix<-mean(fit_plm_imp_pan_sdg13.int.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg13.int.fix

RorF13.int<-phtest(fit_plm_imp_pan_sdg13.int.fix,fit_plm_imp_pan_sdg13.int)
RorF13.int

#### SDG 1 OECDpov ####
fit_plm_imp_pan_sdg1.int<-plm(sdg1_oecdpov ~ sdg10_palma*sdg10_elder + sdg16_rsf,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg1.int)
MSR_fit_plm_imp_pan_sdg1.int<-mean(fit_plm_imp_pan_sdg1.int$residuals^2)
MSR_fit_plm_imp_pan_sdg1.int

# Fixed Effects version
fit_plm_imp_pan_sdg1.int.fix<-plm(sdg1_oecdpov ~ sdg4_tertiary*sdg10_palma + sdg6_safesan*sdg10_palma + sdg8_yneet*sdg10_elder,
                              model='within',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg1.int.fix)
MSR_fit_plm_imp_pan_sdg1.int.fix<-mean(fit_plm_imp_pan_sdg1.int.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg1.int.fix

RorF1.int<-phtest(fit_plm_imp_pan_sdg1.int.fix,fit_plm_imp_pan_sdg1.int)
RorF1.int

#### SDG 1 OECDpov #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg1.int2<-plm(sdg1_oecdpov ~ sdg16_rsf + sdg15_redlist + sdg17_govex + sdg16_rsf*sdg16_prison,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg1.int2)
MSR_fit_plm_imp_pan_sdg1.int2<-mean(fit_plm_imp_pan_sdg1.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg1.int2

#### SDG 1 OECDpov #### With only palma
fit_plm_imp_pan_sdg1.int3<-plm(sdg1_oecdpov ~ sdg10_palma,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg1.int3)
MSR_fit_plm_imp_pan_sdg1.int3<-mean(fit_plm_imp_pan_sdg1.int3$residuals^2)
MSR_fit_plm_imp_pan_sdg1.int3

#### SDG 2 undernsh ####
fit_plm_imp_pan_sdg2.int<-plm(sdg2_undernsh ~ sdg7_cleanfuel + sdg2_wasteihme*sdg3_fertility  + sdg2_wasteihme*sdg14_cleanwat 
                              + sdg7_elecac*sdg7_cleanfuel,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg2.int)
MSR_fit_plm_imp_pan_sdg2.int<-mean(fit_plm_imp_pan_sdg2.int$residuals^2)
MSR_fit_plm_imp_pan_sdg2.int

# Fixed Effects version
fit_plm_imp_pan_sdg2.int.fix<-plm(sdg2_undernsh ~ sdg7_cleanfuel + sdg2_wasteihme*sdg3_fertility  + sdg2_wasteihme*sdg14_cleanwat 
                                  + sdg7_elecac*sdg7_cleanfuel,
                              model='within',
                              #effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg2.int.fix)
MSR_fit_plm_imp_pan_sdg2.int.fix<-mean(fit_plm_imp_pan_sdg2.int.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg2.int.fix

RorF2.int<-phtest(fit_plm_imp_pan_sdg2.int.fix,fit_plm_imp_pan_sdg2.int)
RorF2.int

#### SDG 2 undernsh #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg2.int2<-plm(sdg2_undernsh ~ sdg7_cleanfuel + sdg7_elecac*sdg7_cleanfuel + sdg11_pm25*sdg14_cleanwat,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg2.int2)
MSR_fit_plm_imp_pan_sdg2.int2<-mean(fit_plm_imp_pan_sdg2.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg2.int2


#### SDG 2 Wasting target ####
fit_plm_imp_pan_sdg2.2.int<-plm(sdg2_wasteihme ~ sdg2_stuntihme + sdg2_stuntihme*sdg3_tb
                              + sdg2_stuntihme*sdg9_netacc + sdg2_obesity*sdg9_patents,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg2.2.int)
MSR_fit_plm_imp_pan_sdg2.2.int<-mean(fit_plm_imp_pan_sdg2.2.int$residuals^2)
MSR_fit_plm_imp_pan_sdg2.2.int

#### SDG 2 Wasting target #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg2.2.int2<-plm(sdg2_wasteihme ~ sdg3_neonat*sdg9_patents + sdg3_tb + sdg3_vac
                                 + sdg3_swb + sdg6_safewat + sdg8_impacc*sdg17_govex,
                                model='random',
                                effect='twoway',
                                data=data.pan.int)
summary(fit_plm_imp_pan_sdg2.2.int2)
MSR_fit_plm_imp_pan_sdg2.2.int2<-mean(fit_plm_imp_pan_sdg2.2.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg2.2.int2


#### SDG 3 lifee ####
fit_plm_imp_pan_sdg3.int<-plm(sdg3_lifee ~ sdg2_wasteihme*sdg16_prison + sdg3_matmort*sdg15_redlist + sdg3_smoke*sdg16_homicides + sdg4_tertiary*sdg16_safe,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg3.int)
MSR_fit_plm_imp_pan_sdg3.int<-mean(fit_plm_imp_pan_sdg3.int$residuals^2)
MSR_fit_plm_imp_pan_sdg3.int

# Fixed Effects version
fit_plm_imp_pan_sdg3.int.fix<-plm(sdg3_lifee ~ sdg2_wasteihme*sdg16_prison + sdg3_matmort*sdg15_redlist + sdg3_smoke*sdg16_homicides + sdg4_tertiary*sdg16_safe,
                              model='within',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg3.int.fix)
MSR_fit_plm_imp_pan_sdg3.int.fix<-mean(fit_plm_imp_pan_sdg3.int.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg3.int.fix

RorF3.int<-phtest(fit_plm_imp_pan_sdg3.int.fix,fit_plm_imp_pan_sdg3.int)
RorF3.int

#### SDG 3 lifee #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg3.int2<-plm(sdg3_lifee ~ sdg2_stuntihme*sdg3_tb + sdg2_stuntihme*sdg16_prison + sdg2_obesity*sdg3_tb + sdg3_fertility*sdg15_redlist + sdg4_tertiary*sdg16_safe,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg3.int2)
MSR_fit_plm_imp_pan_sdg3.int2<-mean(fit_plm_imp_pan_sdg3.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg3.int2

#### SDG 4 tertiary ####
fit_plm_imp_pan_sdg4.int<-plm(sdg4_tertiary ~ sdg1_oecdpov*sdg9_rdres + sdg3_lifee*sdg9_intuse
                              + sdg4_primary*sdg9_intuse+ sdg6_safewat*sdg9_intuse,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg4.int)
MSR_fit_plm_imp_pan_sdg4.int<-mean(fit_plm_imp_pan_sdg4.int$residuals^2)
MSR_fit_plm_imp_pan_sdg4.int

# Fixed Effects version
fit_plm_imp_pan_sdg4.int.fix<-plm(sdg4_tertiary ~ sdg1_oecdpov*sdg9_rdres + sdg3_lifee*sdg9_intuse
                              + sdg4_primary*sdg9_intuse+ sdg6_safewat*sdg9_intuse,
                              model='within',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg4.int.fix)
MSR_fit_plm_imp_pan_sdg4.int.fix<-mean(fit_plm_imp_pan_sdg4.int.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg4.int.fix

RorF4.int<-phtest(fit_plm_imp_pan_sdg4.int.fix,fit_plm_imp_pan_sdg4.int)
RorF4.int

#### SDG 5 parl ####
fit_plm_imp_pan_sdg5.int<-plm(sdg5_parl ~ sdg2_stuntihme*sdg3_smoke + sdg3_smoke*sdg16_rsf
                              + sdg5_lfpr*sdg17_oda  + sdg7_ren + sdg9_articles,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg5.int)
MSR_fit_plm_imp_pan_sdg5.int<-mean(fit_plm_imp_pan_sdg5.int$residuals^2)
MSR_fit_plm_imp_pan_sdg5.int

# Fixed Effects version
fit_plm_imp_pan_sdg5.int.fix<-plm(sdg5_parl ~ sdg2_trophic*sdg9_intuse,
                              model='within',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg5.int.fix)
MSR_fit_plm_imp_pan_sdg5.int.fix<-mean(fit_plm_imp_pan_sdg5.int.fix$residuals^2)
MSR_fit_plm_imp_pan_sdg5.int.fix

RorF5.int<-phtest(fit_plm_imp_pan_sdg5.int,fit_plm_imp_pan_sdg5.int.fix)
RorF5.int

#### SDG 6 safesan ####
fit_plm_imp_pan_sdg6.int<-plm(sdg6_safesan ~ sdg1_320pov*sdg7_ren + sdg2_snmi*sdg3_fertility
                              + sdg3_fertility*sdg9_netacc + sdg9_rdex*sdg15_cpta,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg6.int)
MSR_fit_plm_imp_pan_sdg6.int<-mean(fit_plm_imp_pan_sdg6.int$residuals^2)
MSR_fit_plm_imp_pan_sdg6.int

#### SDG 7 ren ####
fit_plm_imp_pan_sdg7.int<-plm(sdg7_ren ~ sdg2_trophic*sdg8_empop + sdg3_lifee*sdg13_co2kgPerGDPPPP + sdg8_impacc*sdg14_cpma + sdg13_co2kgPerGDPPPP*sdg14_cpma,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg7.int)
MSR_fit_plm_imp_pan_sdg7.int<-mean(fit_plm_imp_pan_sdg7.int$residuals^2)
MSR_fit_plm_imp_pan_sdg7.int

#### SDG 8 empop ####
fit_plm_imp_pan_sdg8.int<-plm(sdg8_empop ~ sdg8_yneet + sdg2_trophic*sdg16_cpi + sdg5_lfpr*sdg16_cpi + sdg14_cleanwat*sdg17_govex,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg8.int)
MSR_fit_plm_imp_pan_sdg8.int<-mean(fit_plm_imp_pan_sdg8.int$residuals^2)
MSR_fit_plm_imp_pan_sdg8.int

#### SDG 8 empop #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg8.int2<-plm(sdg8_empop ~ sdg2_trophic*sdg16_cpi + sdg3_smoke*sdg11_pm25
                               + sdg5_lfpr*sdg16_cpi + sdg9_netacc*sdg11_pm25 + sdg3_swb + sdg9_intuse,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg8.int2)
MSR_fit_plm_imp_pan_sdg8.int2<-mean(fit_plm_imp_pan_sdg8.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg8.int2

#### SDG 9 rdex ####
fit_plm_imp_pan_sdg9.int<-plm(sdg9_rdex ~ sdg5_paygap*sdg9_articles + sdg6_safewat*sdg9_rdres + sdg9_patents,
                              model='random',
                              effect='twoway',
                              data=data.pan.int)
summary(fit_plm_imp_pan_sdg9.int)
MSR_fit_plm_imp_pan_sdg9.int<-mean(fit_plm_imp_pan_sdg9.int$residuals^2)
MSR_fit_plm_imp_pan_sdg9.int

#### SDG 9 rdex #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg9.int2<-plm(sdg9_rdex ~ sdg3_lifee*sdg5_lfpr + sdg3_lifee*sdg5_paygap + sdg3_lifee*sdg6_safesan
                               + sdg3_swb*sdg6_safesan + sdg6_safesan*sdg17_govex,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg9.int2)
MSR_fit_plm_imp_pan_sdg9.int2<-mean(fit_plm_imp_pan_sdg9.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg9.int2

#### SDG 10 palma ####
fit_plm_imp_pan_sdg10.int<-plm(sdg10_palma ~ sdg1_oecdpov*sdg3_fertility + sdg1_oecdpov*sdg10_adjgini
                               + sdg3_matmort*sdg3_lifee + sdg10_adjgini*sdg14_cleanwat,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg10.int)
MSR_fit_plm_imp_pan_sdg10.int<-mean(fit_plm_imp_pan_sdg10.int$residuals^2)
MSR_fit_plm_imp_pan_sdg10.int

#### SDG 10 palma #### Without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg10.int2<-plm(sdg10_palma ~ sdg2_snmi*sdg3_fertility + sdg3_matmort*sdg3_lifee
                                + sdg3_matmort*sdg14_cleanwat + sdg5_lfpr + sdg15_redlist + sdg10_elder*sdg16_rsf,
                                model='random',
                                effect='twoway',
                                data=data.pan.int)
summary(fit_plm_imp_pan_sdg10.int2)
MSR_fit_plm_imp_pan_sdg10.int2<-mean(fit_plm_imp_pan_sdg10.int2$residuals^2)
MSR_fit_plm_imp_pan_sdg10.int2

#### SDG 11 pm25 ####
fit_plm_imp_pan_sdg11.int<-plm(sdg11_pm25 ~ sdg5_lfpr + sdg2_trophic*sdg7_elecac + sdg5_fplmodel*sdg5_edat
                               + sdg5_edat*sdg8_empop + sdg5_edat*sdg16_cpi + sdg8_yneet*sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg11.int)
MSR_fit_plm_imp_pan_sdg11.int<-mean(fit_plm_imp_pan_sdg11.int$residuals^2)
MSR_fit_plm_imp_pan_sdg11.int

#### SDG 14 cpma ####
fit_plm_imp_pan_sdg14.int<-plm(sdg14_cpma ~ sdg5_edat*sdg15_cpta + sdg9_articles*sdg15_cpfa,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg14.int)
MSR_fit_plm_imp_pan_sdg14.int<-mean(fit_plm_imp_pan_sdg14.int$residuals^2)
MSR_fit_plm_imp_pan_sdg14.int

#### SDG 15 redlist ####
fit_plm_imp_pan_sdg15.int<-plm(sdg15_redlist ~ sdg3_lifee*sdg10_adjgini + sdg3_lifee*sdg10_palma
                               + sdg3_incomeg*sdg6_safesan,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg15.int)
MSR_fit_plm_imp_pan_sdg15.int<-mean(fit_plm_imp_pan_sdg15.int$residuals^2)
MSR_fit_plm_imp_pan_sdg15.int

#### SDG 16 homicides ####
fit_plm_imp_pan_sdg16.int<-plm(sdg16_homicides ~ sdg1_320pov*sdg2_obesity + sdg1_320pov*sdg3_tb + sdg1_320pov*sdg7_elecac
                               + sdg3_matmort*sdg5_parl + sdg3_u5mort*sdg3_incomeg,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg16.int)
MSR_fit_plm_imp_pan_sdg16.int<-mean(fit_plm_imp_pan_sdg16.int$residuals^2)
MSR_fit_plm_imp_pan_sdg16.int

#### SDG 17 govex ####
fit_plm_imp_pan_sdg17.int<-plm(sdg17_govex ~ sdg2_trophic*sdg2_crlyld  + sdg2_crlyld*sdg9_articles
                               + sdg7_cleanfuel + sdg5_lfpr*sdg16_cpi + sdg9_rdres*sdg16_cpi,
                               model='random',
                               effect='twoway',
                               data=data.pan.int)
summary(fit_plm_imp_pan_sdg17.int)
MSR_fit_plm_imp_pan_sdg17.int<-mean(fit_plm_imp_pan_sdg17.int$residuals^2)
MSR_fit_plm_imp_pan_sdg17.int

MSRs_int<-matrix(c(MSR_fit_plm_imp_pan_sdg1.int,
                   MSR_fit_plm_imp_pan_sdg1.int2,
                   MSR_fit_plm_imp_pan_sdg2.int,
                   MSR_fit_plm_imp_pan_sdg2.int2,
                   MSR_fit_plm_imp_pan_sdg3.int,
                   MSR_fit_plm_imp_pan_sdg3.int2,
                   MSR_fit_plm_imp_pan_sdg4.int,
                   MSR_fit_plm_imp_pan_sdg5.int,
                   MSR_fit_plm_imp_pan_sdg6.int,
                   MSR_fit_plm_imp_pan_sdg7.int,
                   MSR_fit_plm_imp_pan_sdg8.int,
                   MSR_fit_plm_imp_pan_sdg8.int2,
                   MSR_fit_plm_imp_pan_sdg9.int,
                   MSR_fit_plm_imp_pan_sdg9.int2,
                   MSR_fit_plm_imp_pan_sdg10.int,
                   MSR_fit_plm_imp_pan_sdg10.int2,
                   MSR_fit_plm_imp_pan_sdg11.int,
                   MSR_fit_plm_imp_pan_sdg13.int,
                   MSR_fit_plm_imp_pan_sdg14.int,
                   MSR_fit_plm_imp_pan_sdg15.int,
                   MSR_fit_plm_imp_pan_sdg16.int,
                   MSR_fit_plm_imp_pan_sdg17.int),22,1,
                 dimnames = list(c("MSR_fit_plm_imp_pan_sdg1.int",
                                   "MSR_fit_plm_imp_pan_sdg1.int2",
                                   "MSR_fit_plm_imp_pan_sdg2.int",
                                   "MSR_fit_plm_imp_pan_sdg2.int2",
                                   "MSR_fit_plm_imp_pan_sdg3.int",
                                   "MSR_fit_plm_imp_pan_sdg3.int2",
                                   "MSR_fit_plm_imp_pan_sdg4.int",
                                   "MSR_fit_plm_imp_pan_sdg5.int",
                                   "MSR_fit_plm_imp_pan_sdg6.int",
                                   "MSR_fit_plm_imp_pan_sdg7.int",
                                   "MSR_fit_plm_imp_pan_sdg8.int",
                                   "MSR_fit_plm_imp_pan_sdg8.int2",
                                   "MSR_fit_plm_imp_pan_sdg9.int",
                                   "MSR_fit_plm_imp_pan_sdg9.int2",
                                   "MSR_fit_plm_imp_pan_sdg10.int",
                                   "MSR_fit_plm_imp_pan_sdg10.int2",
                                   "MSR_fit_plm_imp_pan_sdg11.int",
                                   "MSR_fit_plm_imp_pan_sdg13.int",
                                   "MSR_fit_plm_imp_pan_sdg14.int",
                                   "MSR_fit_plm_imp_pan_sdg15.int",
                                   "MSR_fit_plm_imp_pan_sdg16.int",
                                   "MSR_fit_plm_imp_pan_sdg17.int")))

##########################################
##### Leave one out cross validation #####
##########################################

#setup training error vectors for non-lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.t<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.t<-zeros(num_countries,1)

#setup validation error vectors for non-lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.v<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.v<-zeros(num_countries,1)

#setup training error vectors for lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.tL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.tL<-zeros(num_countries,1)

#setup validation error vectors for lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.vL<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.vL<-zeros(num_countries,1)

#setup training error vectors for pruned lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.tLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.tLP<-zeros(num_countries,1)

#setup validation error vectors for pruned lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.vLP<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.vLP<-zeros(num_countries,1)

#setup training error vectors for extra pruned lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.tLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.tLP2<-zeros(num_countries,1)

#setup validation error vectors for extra pruned lasso vars
MSR_fit_plm_imp_pan_sdg1.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.vLP2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.vLP2<-zeros(num_countries,1)

#setup training error vectors for interaction terms models
MSR_fit_plm_imp_pan_sdg1.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg1.xf.tLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.tLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.tLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.tLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.tLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.tLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.tLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.tLint<-zeros(num_countries,1)

#setup validation error vectors for interaction terms models
MSR_fit_plm_imp_pan_sdg1.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg1.xf.vLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg2.xf.vLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg3.xf.vLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg4.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg5.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg6.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg7.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg8.xf.vLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg9.xf.vLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg10.xf.vLint2<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg11.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg13.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg14.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg15.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg16.xf.vLint<-zeros(num_countries,1)
MSR_fit_plm_imp_pan_sdg17.xf.vLint<-zeros(num_countries,1)

for (j in 1:num_countries){
val_country = (j-1)*num_years+seq(1,num_years)
train_countries = !(seq(1,nrow(data.pan.final)) %in% val_country)

############
####SDG1####
############
fit_plm_imp_pan_sdg1.xf.t<-plm(sdg1_oecdpov ~ sdg4_tertiary + sdg3_lifee + sdg2_undernsh + 
                                 sdg10_palma + GDP_PPP,
                           model='random',
                           effect='twoway',
                           data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg1.xf.t)
MSR_fit_plm_imp_pan_sdg1.xf.t[j]<-mean(fit_plm_imp_pan_sdg1.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg1.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg1.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg1.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg1_oecdpov[val_country])^2)

#Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg1.xf.tL<-plm(sdg1_oecdpov ~ sdg3_hiv + sdg4_tertiary + sdg5_fplmodel + sdg8_empop
                              + sdg10_adjgini + sdg10_palma + sdg10_elder + sdg16_prison  + sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg1.xf.tL)
MSR_fit_plm_imp_pan_sdg1.xf.tL[j]<-mean(fit_plm_imp_pan_sdg1.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg1.xf.tL[j]
val_pred<-predict(fit_plm_imp_pan_sdg1.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg1.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg1_oecdpov[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg1.xf.tLP<-plm(sdg1_oecdpov ~ GDP_PPP + sdg2_crlyld + sdg4_tertiary 
                               + sdg6_safewat + sdg8_empop + sdg10_palma 
                               + sdg10_elder + sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg1.xf.tLP)
MSR_fit_plm_imp_pan_sdg1.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg1.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg1.xf.tLP[j]
val_pred<-predict(fit_plm_imp_pan_sdg1.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg1.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg1_oecdpov[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg1.xf.tLP2<-plm(sdg1_oecdpov ~  + sdg4_tertiary + sdg10_palma + sdg10_elder + sdg16_rsf,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg1.xf.tLP2)
MSR_fit_plm_imp_pan_sdg1.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg1.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg1.xf.tLP2[j]
val_pred<-predict(fit_plm_imp_pan_sdg1.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg1.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg1_oecdpov[val_country])^2)

#Interaction terms model
fit_plm_imp_pan_sdg1.xf.tLint<-plm(sdg1_oecdpov ~  sdg4_tertiary*sdg10_palma + sdg6_safesan*sdg10_palma + sdg8_yneet*sdg10_elder,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg1.xf.tLint)
MSR_fit_plm_imp_pan_sdg1.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg1.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg1.xf.tLint[j]
val_pred<-predict(fit_plm_imp_pan_sdg1.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg1.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg1_oecdpov[val_country])^2)

#Interaction terms model without uninteresting or "Cheating" inputs
fit_plm_imp_pan_sdg1.xf.tLint2<-plm(sdg1_oecdpov ~  sdg2_snmi*sdg16_prison + sdg15_redlist*sdg17_govex + sdg5_paygap + sdg16_rsf,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg1.xf.tLint2)
MSR_fit_plm_imp_pan_sdg1.xf.tLint2[j]<-mean(fit_plm_imp_pan_sdg1.xf.tLint2$residuals^2)
#MSR_fit_plm_imp_pan_sdg1.xf.tLint2[j]
val_pred<-predict(fit_plm_imp_pan_sdg1.xf.tLint2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg1.xf.vLint2[j]<-mean((val_pred-data.pan.final.xf$sdg1_oecdpov[val_country])^2)

############
####SDG2####
############
fit_plm_imp_pan_sdg2.xf.t<-plm(sdg2_undernsh ~ sdg1_oecdpov + sdg3_lifee + sdg3_u5mort + 
                                 sdg6_safewat + sdg7_cleanfuel + sdg14_fishstocks + sdg11_pipedwat + 
                                 sdg14_trawl,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg2.xf.t)
MSR_fit_plm_imp_pan_sdg2.xf.t[j]<-mean(fit_plm_imp_pan_sdg2.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg2.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg2.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg2.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg2_undernsh[val_country])^2)


#Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xf.tL<-plm(sdg2_undernsh ~ sdg1_wpc + sdg2_wasteihme + sdg3_fertility + sdg4_primary + sdg7_elecac 
                               + sdg7_cleanfuel + sdg13_co2kgPerGDPPPP,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg2.xf.tL)
MSR_fit_plm_imp_pan_sdg2.xf.tL[j]<-mean(fit_plm_imp_pan_sdg2.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg2.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg2.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg2.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg2_undernsh[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xf.tLP<-plm(sdg2_undernsh ~ sdg1_oecdpov + sdg2_wasteihme + sdg2_obesity 
                               + sdg3_smoke + sdg7_elecac + sdg7_cleanfuel + sdg11_pipedwat
                               + sdg13_co2kgPerGDPPPP + sdg14_fishstocks,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg2.xf.tLP)
MSR_fit_plm_imp_pan_sdg2.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg2.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg2.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg2.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg2.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg2_undernsh[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg2.xf.tLP2<-plm(sdg2_undernsh ~ sdg2_wasteihme + sdg2_obesity + sdg7_cleanfuel + sdg13_co2kgPerGDPPPP,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg2.xf.tLP2)
MSR_fit_plm_imp_pan_sdg2.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg2.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg2.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg2.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg2.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg2_undernsh[val_country])^2)

#Interaction terms model
fit_plm_imp_pan_sdg2.xf.tLint<-plm(sdg2_undernsh ~ sdg2_stuntihme*sdg14_cleanwat + sdg3_fertility*sdg13_co2kgPerGDPPPP
                                   + sdg4_primary*sdg7_cleanfuel + sdg7_elecac*sdg7_cleanfuel,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg2.xf.tLint)
MSR_fit_plm_imp_pan_sdg2.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg2.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg2.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg2.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg2.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg2_undernsh[val_country])^2)

#Interaction terms model without uninteresting or "Cheating" inputs
fit_plm_imp_pan_sdg2.xf.tLint2<-plm(sdg2_undernsh ~ sdg1_wpc*sdg14_cleanwat + sdg3_u5mort*sdg5_edat + sdg3_u5mort*sdg14_fishstocks 
                                    + sdg3_fertility*sdg13_co2kgPerGDPPPP + sdg4_primary*sdg7_cleanfuel + sdg7_elecac*sdg7_cleanfuel,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg2.xf.tLint2)
MSR_fit_plm_imp_pan_sdg2.xf.tLint2[j]<-mean(fit_plm_imp_pan_sdg2.xf.tLint2$residuals^2)
#MSR_fit_plm_imp_pan_sdg2.xf.tLint2
val_pred<-predict(fit_plm_imp_pan_sdg2.xf.tLint2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg2.xf.vLint2[j]<-mean((val_pred-data.pan.final.xf$sdg2_undernsh[val_country])^2)

############
####SDG3####
############
fit_plm_imp_pan_sdg3.xf.t<-plm(sdg3_lifee ~ sdg1_oecdpov + sdg2_undernsh + sdg2_obesity + 
                                 sdg4_primary + sdg4_tertiary + sdg5_edat,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg3.xf.t)
MSR_fit_plm_imp_pan_sdg3.xf.t[j]<-mean(fit_plm_imp_pan_sdg3.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg3.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg3.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg3.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg3_lifee[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.07 (higher than others))
fit_plm_imp_pan_sdg3.xf.tL<-plm(sdg3_lifee ~ sdg2_stuntihme + sdg3_tb + sdg4_tertiary + sdg9_intuse + sdg9_articles + sdg9_netacc 
                              + sdg16_safe,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg3.xf.tL)
MSR_fit_plm_imp_pan_sdg3.xf.tL[j]<-mean(fit_plm_imp_pan_sdg3.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg3.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg3.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg3.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg3_lifee[val_country])^2)

# Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg3.xf.tLP<-plm(sdg3_lifee ~ + sdg3_u5mort + sdg3_smoke +sdg4_tertiary + sdg9_intuse + sdg9_articles 
                               + sdg13_co2kgPerGDPPPP + sdg15_redlist + sdg16_homicides + sdg16_safe,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg3.xf.tLP)
MSR_fit_plm_imp_pan_sdg3.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg3.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg3.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg3.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg3.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg3_lifee[val_country])^2)

# Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg3.xf.tLP2<-plm(sdg3_lifee ~ sdg4_tertiary + sdg9_intuse + sdg9_articles + sdg13_co2kgPerGDPPPP,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg3.xf.tLP2)
MSR_fit_plm_imp_pan_sdg3.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg3.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg3.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg3.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg3.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg3_lifee[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg3.xf.tLint<-plm(sdg3_lifee ~ sdg2_wasteihme*sdg16_prison + sdg3_matmort*sdg15_redlist + sdg3_smoke*sdg16_homicides + sdg4_tertiary*sdg16_safe,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg3.xf.tLint)
MSR_fit_plm_imp_pan_sdg3.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg3.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg3.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg3.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg3.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg3_lifee[val_country])^2)

# Interaction terms model without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg3.xf.tLint2<-plm(sdg3_lifee ~ sdg2_stuntihme*sdg3_tb + sdg2_stuntihme*sdg16_prison + sdg2_obesity*sdg3_tb + sdg3_fertility*sdg15_redlist + sdg4_tertiary*sdg16_safe,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg3.xf.tLint2)
MSR_fit_plm_imp_pan_sdg3.xf.tLint2[j]<-mean(fit_plm_imp_pan_sdg3.xf.tLint2$residuals^2)
#MSR_fit_plm_imp_pan_sdg3.xf.tLint2
val_pred<-predict(fit_plm_imp_pan_sdg3.xf.tLint2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg3.xf.vLint2[j]<-mean((val_pred-data.pan.final.xf$sdg3_lifee[val_country])^2)

############
####SDG4####
############
fit_plm_imp_pan_sdg4.xf.t<-plm(sdg4_tertiary ~ sdg5_edat + sdg9_intuse + sdg9_articles + sdg9_rdex + sdg9_netacc
                               + sdg11_pm25 + sdg16_homicides,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg4.xf.t)
MSR_fit_plm_imp_pan_sdg4.xf.t[j]<-mean(fit_plm_imp_pan_sdg4.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg4.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg4.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg4.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg4_tertiary[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg4.xf.tL<-plm(sdg4_tertiary ~ sdg1_oecdpov + sdg2_undernsh + sdg3_lifee + sdg3_neonat
                               + sdg5_paygap + sdg9_intuse + sdg9_rdres,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg4.xf.tL)
MSR_fit_plm_imp_pan_sdg4.xf.tL[j]<-mean(fit_plm_imp_pan_sdg4.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg4.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg4.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg4.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg4_tertiary[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg4.xf.tLP<-plm(sdg4_tertiary ~ sdg3_hiv + sdg3_lifee + sdg5_fplmodel + sdg5_paygap 
                               + sdg7_cleanfuel + sdg8_yneet + sdg9_intuse + sdg9_rdres,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg4.xf.tLP)
MSR_fit_plm_imp_pan_sdg4.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg4.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg4.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg4.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg4.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg4_tertiary[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg4.xf.tLP2<-plm(sdg4_tertiary ~ sdg3_hiv + sdg3_lifee + sdg9_intuse,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg4.xf.tLP2)
MSR_fit_plm_imp_pan_sdg4.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg4.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg4.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg4.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg4.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg4_tertiary[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg4.xf.tLint<-plm(sdg4_tertiary ~ sdg1_oecdpov*sdg9_rdres + sdg3_lifee*sdg9_intuse
                                   + sdg4_primary*sdg9_intuse+ sdg6_safewat*sdg9_intuse,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg4.xf.tLint)
MSR_fit_plm_imp_pan_sdg4.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg4.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg4.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg4.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg4.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg4_tertiary[val_country])^2)

############
####SDG5####
############
fit_plm_imp_pan_sdg5.xf.t<-plm(sdg5_parl ~ sdg2_obesity + sdg2_wasteihme + sdg3_matmort + sdg3_fertility 
                               + sdg5_edat + sdg5_paygap + sdg9_intuse + sdg16_homicides + sdg16_safe,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg5.xf.t)
MSR_fit_plm_imp_pan_sdg5.xf.t[j]<-mean(fit_plm_imp_pan_sdg5.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg5.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg5.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg5.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg5_parl[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.055)
fit_plm_imp_pan_sdg5.xf.tL<-plm(sdg5_parl ~ sdg2_trophic + sdg3_swb + sdg3_smoke + sdg7_ren + sdg9_intuse
                               + sdg9_articles + sdg17_govex + sdg17_oda,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg5.xf.tL)
MSR_fit_plm_imp_pan_sdg5.xf.tL[j]<-mean(fit_plm_imp_pan_sdg5.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg5.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg5.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg5.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg5_parl[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.055)
fit_plm_imp_pan_sdg5.xf.tLP<-plm(sdg5_parl ~ sdg2_wasteihme + sdg2_snmi + sdg5_lfpr + sdg5_paygap + sdg9_intuse
                               + sdg11_pm25 + sdg17_govex + sdg17_oda,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg5.xf.tLP)
MSR_fit_plm_imp_pan_sdg5.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg5.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg5.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg5.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg5.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg5_parl[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.055)
fit_plm_imp_pan_sdg5.xf.tLP2<-plm(sdg5_parl ~ sdg2_wasteihme + sdg5_lfpr + sdg9_intuse + sdg11_pm25,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg5.xf.tLP2)
MSR_fit_plm_imp_pan_sdg5.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg5.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg5.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg5.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg5.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg5_parl[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg5.xf.tLint<-plm(sdg5_parl ~ sdg2_trophic*sdg9_intuse,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg5.xf.tLint)
MSR_fit_plm_imp_pan_sdg5.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg5.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg5.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg5.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg5.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg5_parl[val_country])^2)

############
####SDG6####
############

fit_plm_imp_pan_sdg6.xf.t<-plm(sdg6_safesan ~ sdg2_stuntihme + sdg3_lifee + sdg3_tb + sdg3_neonat + sdg4_tertiary
                               + sdg11_pipedwat + GDP_PPP,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg6.xf.t)
MSR_fit_plm_imp_pan_sdg6.xf.t[j]<-mean(fit_plm_imp_pan_sdg6.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg6.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg6.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg6.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg6_safesan[val_country])^2)


#Lasso inputs with transformed pan imputed vars (lambda = 0.03)
fit_plm_imp_pan_sdg6.xf.tL<-plm(sdg6_safesan ~ sdg1_320pov + sdg3_u5mort + sdg3_fertility + sdg7_elecac + +sdg7_ren + sdg9_patents
                               + sdg11_pipedwat + sdg15_cpta,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg6.xf.tL)
MSR_fit_plm_imp_pan_sdg6.xf.tL[j]<-mean(fit_plm_imp_pan_sdg6.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg6.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg6.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg6.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg6_safesan[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg6.xf.tLP<-plm(sdg6_safesan ~ sdg2_snmi + sdg3_fertility 
                               + sdg5_paygap
                               + sdg7_elecac + sdg9_intuse + sdg9_patents + sdg9_netacc
                               + sdg16_prison,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg6.xf.tLP)
MSR_fit_plm_imp_pan_sdg6.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg6.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg6.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg6.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg6.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg6_safesan[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg6.xf.tLP2<-plm(sdg6_safesan ~ sdg2_snmi + sdg3_fertility + sdg7_elecac + sdg9_intuse,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg6.xf.tLP2)
MSR_fit_plm_imp_pan_sdg6.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg6.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg6.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg6.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg6.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg6_safesan[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg6.xf.tLint<-plm(sdg6_safesan ~ sdg1_320pov*sdg7_ren + sdg2_snmi*sdg3_fertility
                                   + sdg3_fertility*sdg9_netacc + sdg9_rdex*sdg15_cpta,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg6.xf.tLint)
MSR_fit_plm_imp_pan_sdg6.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg6.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg6.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg6.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg6.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg6_safesan[val_country])^2)

############
####SDG7####
############

fit_plm_imp_pan_sdg7.xf.t<-plm(sdg7_ren ~ sdg1_oecdpov + sdg3_lifee + sdg11_pm25
                               + sdg13_co2import + sdg13_co2kgPerGDPPPP + sdg7_cleanfuel + sdg9_articles,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg7.xf.t)
MSR_fit_plm_imp_pan_sdg7.xf.t[j]<-mean(fit_plm_imp_pan_sdg7.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg7.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg7.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg7.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg7_ren[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.03)
fit_plm_imp_pan_sdg7.xf.tL<-plm(sdg7_ren ~ Population + sdg2_trophic + sdg5_parl + sdg5_paygap + sdg6_safesan
                               + sdg8_empop + sdg13_co2kgPerGDPPPP + sdg14_cpma + sdg14_cleanwat,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg7.xf.tL)
MSR_fit_plm_imp_pan_sdg7.xf.tL[j]<-mean(fit_plm_imp_pan_sdg7.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg7.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg7.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg7.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg7_ren[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg7.xf.tLP<-plm(sdg7_ren ~ Population + sdg2_trophic + sdg4_tertiary + sdg5_fplmodel
                               + sdg5_paygap + sdg7_cleanfuel + sdg2_stuntihme
                               + sdg8_empop + sdg9_patents + sdg11_pipedwat + sdg13_co2kgPerGDPPPP 
                               + sdg15_cpta + sdg3_births,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg7.xf.tLP)
MSR_fit_plm_imp_pan_sdg7.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg7.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg7.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg7.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg7.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg7_ren[val_country])^2)


#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg7.xf.tLP2<-plm(sdg7_ren ~ sdg2_trophic + sdg4_tertiary + sdg2_stuntihme + sdg8_empop + sdg13_co2kgPerGDPPPP,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg7.xf.tLP2)
MSR_fit_plm_imp_pan_sdg7.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg7.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg7.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg7.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg7.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg7_ren[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg7.xf.tLint<-plm(sdg7_ren ~ sdg2_trophic*sdg8_empop + sdg3_lifee*sdg13_co2kgPerGDPPPP + sdg8_impacc*sdg14_cpma + sdg13_co2kgPerGDPPPP*sdg14_cpma,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg7.xf.tLint)
MSR_fit_plm_imp_pan_sdg7.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg7.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg7.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg7.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg7.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg7_ren[val_country])^2)

############
####SDG8####
############

fit_plm_imp_pan_sdg8.xf.t<-plm(sdg8_empop ~ sdg1_320pov + sdg2_trophic + sdg2_obesity + sdg3_tb + sdg3_fertility
                               + sdg4_tertiary + sdg9_articles + sdg9_intuse + sdg11_pm25 + sdg10_adjgini,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg8.xf.t)
MSR_fit_plm_imp_pan_sdg8.xf.t[j]<-mean(fit_plm_imp_pan_sdg8.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg8.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg8.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg8.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg8_empop[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.033)
fit_plm_imp_pan_sdg8.xf.tL<-plm(sdg8_empop ~ sdg2_trophic + sdg3_swb + sdg7_ren + sdg8_yneet + sdg9_intuse + sdg9_articles + sdg11_pm25
                               + sdg16_cpi,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg8.xf.tL)
MSR_fit_plm_imp_pan_sdg8.xf.tL[j]<-mean(fit_plm_imp_pan_sdg8.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg8.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg8.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg8.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg8_empop[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars. Lagging yneet helped R^2, MSR, and didn't make model insignificant
fit_plm_imp_pan_sdg8.xf.tLP<-plm(sdg8_empop ~ sdg3_swb + sdg3_incomeg + sdg3_smoke
                               + sdg5_paygap + sdg7_ren + sdg8_yneet + sdg9_intuse + sdg9_articles 
                               + sdg9_patents + sdg11_pm25 + sdg14_cpma + sdg16_safe,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg8.xf.tLP)
MSR_fit_plm_imp_pan_sdg8.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg8.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg8.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg8.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg8.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg8_empop[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars. Lagging yneet helped R^2, MSR, and didn't make model insignificant
fit_plm_imp_pan_sdg8.xf.tLP2<-plm(sdg8_empop ~ sdg3_swb + sdg3_incomeg + sdg8_yneet + sdg11_pm25 + sdg14_cpma,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg8.xf.tLP2)
MSR_fit_plm_imp_pan_sdg8.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg8.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg8.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg8.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg8.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg8_empop[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg8.xf.tLint<-plm(sdg8_empop ~ sdg8_yneet + sdg2_trophic*sdg16_cpi + sdg5_lfpr*sdg16_cpi + sdg14_cleanwat*sdg17_govex,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg8.xf.tLint)
MSR_fit_plm_imp_pan_sdg8.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg8.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg8.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg8.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg8.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg8_empop[val_country])^2)

# Interaction terms model without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg8.xf.tLint2<-plm(sdg8_empop ~ sdg2_trophic*sdg16_cpi + sdg3_smoke*sdg11_pm25
                                    + sdg5_lfpr*sdg16_cpi + sdg9_netacc*sdg11_pm25 + sdg3_swb + sdg9_intuse,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg8.xf.tLint2)
MSR_fit_plm_imp_pan_sdg8.xf.tLint2[j]<-mean(fit_plm_imp_pan_sdg8.xf.tLint2$residuals^2)
#MSR_fit_plm_imp_pan_sdg8.xf.tLint2
val_pred<-predict(fit_plm_imp_pan_sdg8.xf.tLint2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg8.xf.vLint2[j]<-mean((val_pred-data.pan.final.xf$sdg8_empop[val_country])^2)

############
####SDG9####
############

fit_plm_imp_pan_sdg9.xf.t<-plm(sdg9_rdex ~ sdg9_patents + sdg3_lifee + sdg8_empop + sdg2_obesity + sdg5_edat
                               + sdg11_pm25 + sdg17_govex + sdg16_prison,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg9.xf.t)
MSR_fit_plm_imp_pan_sdg9.xf.t[j]<-mean(fit_plm_imp_pan_sdg9.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg9.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg9.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg9.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg9_rdex[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.025)
fit_plm_imp_pan_sdg9.xf.tL<-plm(sdg9_rdex ~ sdg2_undernsh + sdg3_lifee + sdg5_paygap + sdg9_articles + sdg9_rdres + sdg9_patents
                               + sdg10_elder + sdg15_cpta,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg9.xf.tL)
MSR_fit_plm_imp_pan_sdg9.xf.tL[j]<-mean(fit_plm_imp_pan_sdg9.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg9.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg9.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg9.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg9_rdex[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg9.xf.tLP<-plm(sdg9_rdex ~ GDP_PPP + sdg2_undernsh + sdg3_lifee
                               + sdg5_paygap + sdg9_rdres + sdg9_patents
                               + sdg16_rsf,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg9.xf.tLP)
MSR_fit_plm_imp_pan_sdg9.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg9.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg9.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg9.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg9.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg9_rdex[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg9.xf.tLP2<-plm(sdg9_rdex ~ sdg3_lifee + sdg9_rdres + sdg9_patents,
                                 model='random',
                                 effect='twoway',
                                 data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg9.xf.tLP2)
MSR_fit_plm_imp_pan_sdg9.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg9.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg9.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg9.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg9.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg9_rdex[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg9.xf.tLint<-plm(sdg9_rdex ~ sdg5_paygap*sdg9_articles + sdg6_safewat*sdg9_rdres + sdg9_patents,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg9.xf.tLint)
MSR_fit_plm_imp_pan_sdg9.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg9.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg9.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg9.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg9.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg9_rdex[val_country])^2)

# Interaction terms model without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg9.xf.tLint2<-plm(sdg9_rdex ~ sdg3_lifee*sdg5_lfpr + sdg3_lifee*sdg5_paygap + sdg3_lifee*sdg6_safesan
                                    + sdg3_swb*sdg6_safesan + sdg6_safesan*sdg17_govex,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg9.xf.tLint2)
MSR_fit_plm_imp_pan_sdg9.xf.tLint2[j]<-mean(fit_plm_imp_pan_sdg9.xf.tLint2$residuals^2)
#MSR_fit_plm_imp_pan_sdg9.xf.tLint2
val_pred<-predict(fit_plm_imp_pan_sdg9.xf.tLint2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg9.xf.vLint2[j]<-mean((val_pred-data.pan.final.xf$sdg9_rdex[val_country])^2)

#############
####SDG10####
#############

fit_plm_imp_pan_sdg10.xf.t<-plm(sdg10_palma ~ sdg1_320pov + sdg9_articles + sdg7_co2twh + sdg5_edat,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg10.xf.t)
MSR_fit_plm_imp_pan_sdg10.xf.t[j]<-mean(fit_plm_imp_pan_sdg10.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg10.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg10.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg10.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg10_palma[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.015)
fit_plm_imp_pan_sdg10.xf.tL<-plm(sdg10_palma ~ sdg1_oecdpov + sdg2_wasteihme + sdg2_crlyld + sdg3_fertility 
                                + sdg5_fplmodel + sdg7_elecac + sdg10_adjgini + sdg15_redlist,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg10.xf.tL)
MSR_fit_plm_imp_pan_sdg10.xf.tL[j]<-mean(fit_plm_imp_pan_sdg10.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg10.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg10.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg10.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg10_palma[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars. Left unpruned to show that pruning can hurt R^2. unpruned R^2=0.685, Pruned=0.65
fit_plm_imp_pan_sdg10.xf.tLP<-plm(sdg10_palma ~ sdg1_oecdpov + sdg2_wasteihme + sdg2_crlyld + sdg2_snmi
                                + sdg3_fertility + sdg3_smoke + sdg4_second
                                + sdg5_fplmodel + sdg5_lfpr + sdg7_elecac + sdg7_cleanfuel + sdg10_adjgini
                                + sdg11_pm25 + sdg13_co2kgPerGDPPPP + sdg14_cleanwat + sdg14_fishstocks + sdg15_redlist,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg10.xf.tLP)
MSR_fit_plm_imp_pan_sdg10.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg10.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg10.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg10.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg10.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg10_palma[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars.
fit_plm_imp_pan_sdg10.xf.tLP2<-plm(sdg10_palma ~ sdg1_oecdpov + sdg3_fertility + sdg3_smoke,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg10.xf.tLP2)
MSR_fit_plm_imp_pan_sdg10.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg10.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg10.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg10.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg10.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg10_palma[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg10.xf.tLint<-plm(sdg10_palma ~ sdg1_oecdpov*sdg3_fertility + sdg1_oecdpov*sdg10_adjgini
                                    + sdg3_matmort*sdg3_lifee + sdg10_adjgini*sdg14_cleanwat,
                                   model='random',
                                   effect='twoway',
                                   data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg10.xf.tLint)
MSR_fit_plm_imp_pan_sdg10.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg10.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg10.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg10.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg10.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg10_palma[val_country])^2)

# Interaction terms model without uninteresting or "cheating" inputs
fit_plm_imp_pan_sdg10.xf.tLint2<-plm(sdg10_palma ~ sdg2_snmi*sdg3_fertility + sdg3_matmort*sdg3_lifee
                                     + sdg3_matmort*sdg14_cleanwat + sdg5_lfpr + sdg15_redlist + sdg10_elder*sdg16_rsf,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg10.xf.tLint2)
MSR_fit_plm_imp_pan_sdg10.xf.tLint2[j]<-mean(fit_plm_imp_pan_sdg10.xf.tLint2$residuals^2)
#MSR_fit_plm_imp_pan_sdg10.xf.tLint2
val_pred<-predict(fit_plm_imp_pan_sdg10.xf.tLint2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg10.xf.vLint2[j]<-mean((val_pred-data.pan.final.xf$sdg10_palma[val_country])^2)

#############
####SDG11####
#############

fit_plm_imp_pan_sdg11.xf.t<-plm(sdg11_pm25 ~ sdg3_fertility + sdg7_ren + sdg3_lifee + sdg5_parl + sdg9_rdex + sdg8_empop + sdg11_transport + sdg13_co2kgPerGDPPPP + sdg16_cpi,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg11.xf.t)
MSR_fit_plm_imp_pan_sdg11.xf.t[j]<-mean(fit_plm_imp_pan_sdg11.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg11.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg11.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg11.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg11_pm25[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg11.xf.tL<-plm(sdg11_pm25 ~ sdg2_trophic + sdg5_edat + sdg5_lfpr + sdg8_impacc + sdg8_empop 
                                + sdg16_cpi + sdg16_rsf,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg11.xf.tL)
MSR_fit_plm_imp_pan_sdg11.xf.tL[j]<-mean(fit_plm_imp_pan_sdg11.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg11.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg11.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg11.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg11_pm25[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars. Rentover somehow contributes hugely to R^2
fit_plm_imp_pan_sdg11.xf.tLP<-plm(sdg11_pm25 ~ GDP_PPP + sdg1_wpc + sdg2_trophic + sdg3_lifee + sdg3_vac
                                + sdg5_fplmodel + sdg5_edat + sdg5_lfpr + sdg5_parl + sdg6_scarcew
                                + sdg6_safesan + sdg8_empop + sdg11_rentover + sdg14_cpma
                                + sdg15_cpta,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg11.xf.tLP)
MSR_fit_plm_imp_pan_sdg11.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg11.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg11.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg11.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg11.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg11_pm25[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars. Rentover somehow contributes hugely to R^2
fit_plm_imp_pan_sdg11.xf.tLP2<-plm(sdg11_pm25 ~ sdg2_trophic + sdg3_vac + sdg5_fplmodel + sdg13_co2kgPerGDPPPP,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg11.xf.tLP2)
MSR_fit_plm_imp_pan_sdg11.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg11.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg11.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg11.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg11.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg11_pm25[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg11.xf.tLint<-plm(sdg11_pm25 ~ sdg5_lfpr + sdg2_trophic*sdg7_elecac + sdg5_fplmodel*sdg5_edat
                                    + sdg5_edat*sdg8_empop + sdg5_edat*sdg16_cpi + sdg8_yneet*sdg16_rsf,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg11.xf.tLint)
MSR_fit_plm_imp_pan_sdg11.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg11.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg11.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg11.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg11.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg11_pm25[val_country])^2)

#SDG12#
#No target

#############
####SDG13####
#############

fit_plm_imp_pan_sdg13.xf.t<-plm(sdg13_co2kgPerGDPPPP ~ sdg7_ren + sdg7_cleanfuel + sdg5_paygap + GDP_PPP 
                         + sdg7_co2twh + sdg4_tertiary + sdg3_lifee + sdg3_u5mort + sdg8_empop +lag(sdg7_ren) + lag(sdg13_co2kgPerGDPPPP),# + lag(sdg7_cleanfuel) + lag(sdg11_pm25) +lag(sdg2_trophic),
                         model='random',
                         effect='twoways',
                         data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg13.xf.t)
MSR_fit_plm_imp_pan_sdg13.xf.t[j]<-mean(fit_plm_imp_pan_sdg13.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg13.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg13.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg13.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg13_co2kgPerGDPPPP[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg13.xf.tL<-plm(sdg13_co2kgPerGDPPPP ~ sdg3_lifee + sdg3_fertility + sdg5_paygap + sdg7_ren + sdg9_intuse + sdg11_transport
                               + sdg16_rsf + sdg16_prison,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg13.xf.tL)
MSR_fit_plm_imp_pan_sdg13.xf.tL[j]<-mean(fit_plm_imp_pan_sdg13.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg13.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg13.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg13.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg13_co2kgPerGDPPPP[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.01 then pruned)
fit_plm_imp_pan_sdg13.xf.tLP<-plm(sdg13_co2kgPerGDPPPP ~ sdg2_wasteihme + sdg2_crlyld + sdg3_hiv + sdg3_lifee
                                + sdg7_ren + sdg9_intuse + sdg15_cpfa + sdg16_rsf,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg13.xf.tLP)
MSR_fit_plm_imp_pan_sdg13.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg13.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg13.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg13.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg13.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg13_co2kgPerGDPPPP[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.01 then pruned)
fit_plm_imp_pan_sdg13.xf.tLP2<-plm(sdg13_co2kgPerGDPPPP ~ sdg3_hiv + sdg3_lifee + sdg9_intuse + sdg15_cpfa,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg13.xf.tLP2)
MSR_fit_plm_imp_pan_sdg13.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg13.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg13.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg13.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg13.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg13_co2kgPerGDPPPP[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg13.xf.tLint<-plm(sdg13_co2kgPerGDPPPP ~ sdg2_undernsh*sdg5_paygap + sdg3_hiv*sdg5_paygap + sdg3_lifee
                                    + sdg7_ren*sdg16_rsf + sdg9_netacc*sdg14_cleanwat,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg13.xf.tLint)
MSR_fit_plm_imp_pan_sdg13.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg13.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg13.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg13.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg13.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg13_co2kgPerGDPPPP[val_country])^2)

#############
####SDG14####
#############

fit_plm_imp_pan_sdg14.xf.t<-plm(sdg14_cpma ~ sdg15_cpfa + sdg3_lifee + sdg15_redlist + sdg9_rdex + sdg11_transport + sdg14_cleanwat + sdg15_cpta,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg14.xf.t)
MSR_fit_plm_imp_pan_sdg14.xf.t[j]<-mean(fit_plm_imp_pan_sdg14.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg14.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg14.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg14.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg14_cpma[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg14.xf.tL<-plm(sdg14_cpma ~ sdg3_tb + sdg3_hiv + sdg4_earlyedu + sdg7_ren + sdg9_articles
                               + sdg14_cleanwat + sdg15_cpta +sdg15_cpfa,
                              model='random',
                              effect='twoway',
                              data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg14.xf.tL)
MSR_fit_plm_imp_pan_sdg14.xf.tL[j]<-mean(fit_plm_imp_pan_sdg14.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg14.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg14.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg14.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg14_cpma[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg14.xf.tLP<-plm(sdg14_cpma ~ Population + sdg3_tb + sdg3_hiv + sdg9_articles
                                + sdg14_cleanwat + sdg14_fishstocks + sdg15_cpta + sdg16_homicides,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg14.xf.tLP)
MSR_fit_plm_imp_pan_sdg14.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg14.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg14.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg14.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg14.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg14_cpma[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars
fit_plm_imp_pan_sdg14.xf.tLP2<-plm(sdg14_cpma ~ sdg3_hiv + sdg9_articles + sdg14_fishstocks + sdg15_cpta,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg14.xf.tLP2)
MSR_fit_plm_imp_pan_sdg14.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg14.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg14.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg14.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg14.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg14_cpma[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg14.xf.tLint<-plm(sdg14_cpma ~ sdg5_edat*sdg15_cpta + sdg9_articles*sdg15_cpfa,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg14.xf.tLint)
MSR_fit_plm_imp_pan_sdg14.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg14.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg14.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg14.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg14.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg14_cpma[val_country])^2)

#############
####SDG15####
#############

fit_plm_imp_pan_sdg15.xf.t<-plm(sdg15_redlist ~ sdg2_snmi + sdg15_cpfa + sdg3_lifee + sdg14_cpma
                                + sdg14_cleanwat + sdg9_articles + sdg9_intuse + sdg9_netacc,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg15.xf.t)
MSR_fit_plm_imp_pan_sdg15.xf.t[j]<-mean(fit_plm_imp_pan_sdg15.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg15.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg15.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg15.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg15_redlist[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg15.xf.tL<-plm(sdg15_redlist ~ sdg3_lifee + sdg3_incomeg + sdg6_scarcew + sdg10_palma 
                                + sdg10_elder + sdg15_cpfa + sdg16_rsf + sdg17_oda,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg15.xf.tL)
MSR_fit_plm_imp_pan_sdg15.xf.tL[j]<-mean(fit_plm_imp_pan_sdg15.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg15.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg15.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg15.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg15_redlist[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg15.xf.tLP<-plm(sdg15_redlist ~ sdg2_snmi + sdg3_hiv + sdg3_lifee
                                + sdg3_fertility + sdg3_incomeg + sdg6_safewat + sdg15_cpfa,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg15.xf.tLP)
MSR_fit_plm_imp_pan_sdg15.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg15.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg15.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg15.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg15.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg15_redlist[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars (lambda = 0.035)
fit_plm_imp_pan_sdg15.xf.tLP2<-plm(sdg15_redlist ~ sdg3_lifee + sdg3_fertility + sdg15_cpfa,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg15.xf.tLP2)
MSR_fit_plm_imp_pan_sdg15.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg15.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg15.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg15.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg15.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg15_redlist[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg15.xf.tLint<-plm(sdg15_redlist ~ sdg3_lifee*sdg10_adjgini + sdg3_lifee*sdg10_palma + sdg3_incomeg*sdg6_safesan,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg15.xf.tLint)
MSR_fit_plm_imp_pan_sdg15.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg15.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg15.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg15.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg15.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg15_redlist[val_country])^2)

#############
####SDG16####
#############

fit_plm_imp_pan_sdg16.xf.t<-plm(sdg16_homicides ~ sdg16_cpi + sdg3_lifee + sdg1_320pov + sdg2_obesity 
                                + sdg3_incomeg + sdg5_parl + sdg11_pm25 + sdg7_elecac,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg16.xf.t)
MSR_fit_plm_imp_pan_sdg16.xf.t[j]<-mean(fit_plm_imp_pan_sdg16.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg16.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg16.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg16.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg16_homicides[val_country])^2)

# sdg 16 with lasso variables. s=0.015
fit_plm_imp_pan_sdg16.xf.tL<-plm(sdg16_homicides ~ sdg1_320pov + sdg2_obesity + sdg3_matmort + sdg3_tb + sdg3_lifee
                                + sdg3_incomeg + sdg6_safewat + sdg16_prison,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg16.xf.tL)
MSR_fit_plm_imp_pan_sdg16.xf.tL[j]<-mean(fit_plm_imp_pan_sdg16.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg16.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg16.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg16.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg16_homicides[val_country])^2)

# Pruned SDG 16 with lasso variables. Left more than 8 vars in
fit_plm_imp_pan_sdg16.xf.tLP<-plm(sdg16_homicides ~ sdg1_320pov + sdg1_oecdpov + sdg2_obesity + sdg3_matmort 
                                + sdg3_tb + sdg3_lifee + sdg3_incomeg 
                                + sdg5_parl + sdg6_safewat + sdg7_elecac + sdg7_cleanfuel + sdg11_pm25,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg16.xf.tLP)
MSR_fit_plm_imp_pan_sdg16.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg16.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg16.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg16.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg16.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg16_homicides[val_country])^2)

# Pruned SDG 16 with lasso variables. Left more than 8 vars in
fit_plm_imp_pan_sdg16.xf.tLP2<-plm(sdg16_homicides ~ sdg1_320pov + sdg2_obesity + sdg3_tb + sdg7_elecac + sdg7_cleanfuel,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg16.xf.tLP2)
MSR_fit_plm_imp_pan_sdg16.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg16.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg16.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg16.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg16.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg16_homicides[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg16.xf.tLint<-plm(sdg16_homicides ~ sdg1_320pov*sdg2_obesity + sdg1_320pov*sdg3_tb + sdg1_320pov*sdg7_elecac
                                    + sdg3_matmort*sdg5_parl + sdg3_u5mort*sdg3_incomeg,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg16.xf.tLint)
MSR_fit_plm_imp_pan_sdg16.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg16.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg16.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg16.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg16.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg16_homicides[val_country])^2)

#############
####SDG17####
#############

fit_plm_imp_pan_sdg17.xf.t<-plm(sdg17_govex ~ sdg16_cpi + sdg3_lifee + sdg1_oecdpov + sdg2_obesity + sdg5_edat+ sdg8_empop + sdg9_rdex,
                               model='random',
                               effect='twoway',
                               data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg17.xf.t)
MSR_fit_plm_imp_pan_sdg17.xf.t[j]<-mean(fit_plm_imp_pan_sdg17.xf.t$residuals^2)
#MSR_fit_plm_imp_pan_sdg17.xf.t
val_pred<-predict(fit_plm_imp_pan_sdg17.xf.t,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg17.xf.v[j]<-mean((val_pred-data.pan.final.xf$sdg17_govex[val_country])^2)

#Lasso inputs with transformed pan imputed vars (lambda = 0.03)
fit_plm_imp_pan_sdg17.xf.tL<-plm(sdg17_govex ~ sdg2_trophic + sdg2_crlyld + sdg5_lfpr + sdg5_parl
                                + sdg9_articles + sdg9_rdres + sdg11_pm25 + sdg16_safe + sdg16_cpi,# + lag(sdg17_govex) + lag(sdg17_govex,2),
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg17.xf.tL)
MSR_fit_plm_imp_pan_sdg17.xf.tL[j]<-mean(fit_plm_imp_pan_sdg17.xf.tL$residuals^2)
#MSR_fit_plm_imp_pan_sdg17.xf.tL
val_pred<-predict(fit_plm_imp_pan_sdg17.xf.tL,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg17.xf.vL[j]<-mean((val_pred-data.pan.final.xf$sdg17_govex[val_country])^2)

#Pruned Lasso inputs with transformed pan imputed vars. Left in more than 8. Last 2 to be reomved had large effect on R^2
fit_plm_imp_pan_sdg17.xf.tLP<-plm(sdg17_govex ~ GDP_PPP + sdg1_oecdpov + sdg2_crlyld
                                + sdg5_lfpr + sdg5_parl + sdg5_paygap + sdg7_ren
                                + sdg9_articles + sdg13_co2import 
                                + sdg16_cpi + sdg17_oda,
                                model='random',
                                effect='twoway',
                                data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg17.xf.tLP)
MSR_fit_plm_imp_pan_sdg17.xf.tLP[j]<-mean(fit_plm_imp_pan_sdg17.xf.tLP$residuals^2)
#MSR_fit_plm_imp_pan_sdg17.xf.tLP
val_pred<-predict(fit_plm_imp_pan_sdg17.xf.tLP,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg17.xf.vLP[j]<-mean((val_pred-data.pan.final.xf$sdg17_govex[val_country])^2)

#Extra Pruned Lasso inputs with transformed pan imputed vars.
fit_plm_imp_pan_sdg17.xf.tLP2<-plm(sdg17_govex ~ GDP_PPP + sdg1_oecdpov + sdg5_lfpr + sdg9_articles + sdg17_oda,
                                  model='random',
                                  effect='twoway',
                                  data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg17.xf.tLP2)
MSR_fit_plm_imp_pan_sdg17.xf.tLP2[j]<-mean(fit_plm_imp_pan_sdg17.xf.tLP2$residuals^2)
#MSR_fit_plm_imp_pan_sdg17.xf.tLP2
val_pred<-predict(fit_plm_imp_pan_sdg17.xf.tLP2,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg17.xf.vLP2[j]<-mean((val_pred-data.pan.final.xf$sdg17_govex[val_country])^2)

# Interaction terms model
fit_plm_imp_pan_sdg17.xf.tLint<-plm(sdg17_govex ~ sdg2_trophic*sdg2_crlyld  + sdg2_crlyld*sdg9_articles
                                    + sdg7_cleanfuel + sdg5_lfpr*sdg16_cpi + sdg9_rdres*sdg16_cpi,
                                    model='random',
                                    effect='twoway',
                                    data=data.pan.final.xf[train_countries,])
summary(fit_plm_imp_pan_sdg17.xf.tLint)
MSR_fit_plm_imp_pan_sdg17.xf.tLint[j]<-mean(fit_plm_imp_pan_sdg17.xf.tLint$residuals^2)
#MSR_fit_plm_imp_pan_sdg17.xf.tLint
val_pred<-predict(fit_plm_imp_pan_sdg17.xf.tLint,data.pan.final.xf[val_country,])
MSR_fit_plm_imp_pan_sdg17.xf.vLint[j]<-mean((val_pred-data.pan.final.xf$sdg17_govex[val_country])^2)
}
#Plot validation residuals vs inputs for each of the 36 countries
#Trust MSR more than R^2. R^2 may not be calculated considering serial correlation between observations
  #Look into how R^2 is computed, and if it takes into consideration the above
  #Try to find source code
#Try to replicate lagged variables model success from last year
#Understand differences between model types in literature or source code
#Try changing model types to within, pooling. See effects on validation error
  #pooling is dubious for fit to reality
  #within may have temporarily better performance than random, but if the effect is indeed random, it doesn't generalize as well
#When adding an additional variable causes the var significance to drop, could be due to multicollinearity
#Measure collinearity between predictors
  #Mutual information
  #Variance inflation factor

#Artem
# Make a table with mean training error, expected training error and deviation
    # Report validation error in table with deviation
#Presentation


######################################################
#### Statistics on leave-one-out validated models ####
######################################################
library(fBasics)

#Compute mean MSRs for each SDG and each left out country - non-Lasso vars
sdg1_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg1.xf.t)
sdg2_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg2.xf.t)
sdg3_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg3.xf.t)
sdg4_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg4.xf.t)
sdg5_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg5.xf.t)
sdg6_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg6.xf.t)
sdg7_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg7.xf.t)
sdg8_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg8.xf.t)
sdg9_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg9.xf.t)
sdg10_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg10.xf.t)
sdg11_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg11.xf.t)
sdg13_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg13.xf.t)
sdg14_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg14.xf.t)
sdg15_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg15.xf.t)
sdg16_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg16.xf.t)
sdg17_train_MMSR<-mean(MSR_fit_plm_imp_pan_sdg17.xf.t)

sdg1_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg1.xf.v)
sdg2_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg2.xf.v)
sdg3_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg3.xf.v)
sdg4_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg4.xf.v)
sdg5_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg5.xf.v)
sdg6_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg6.xf.v)
sdg7_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg7.xf.v)
sdg8_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg8.xf.v)
sdg9_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg9.xf.v)
sdg10_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg10.xf.v)
sdg11_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg11.xf.v)
sdg13_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg13.xf.v)
sdg14_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg14.xf.v)
sdg15_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg15.xf.v)
sdg16_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg16.xf.v)
sdg17_val_MMSR<-mean(MSR_fit_plm_imp_pan_sdg17.xf.v)

#percent difference between validation error and traiining error for each sdg
sdg1_error_diff<-100*(sdg1_val_MMSR-sdg1_train_MMSR)/sdg1_train_MMSR
sdg2_error_diff<-100*(sdg2_val_MMSR-sdg2_train_MMSR)/sdg2_train_MMSR
sdg3_error_diff<-100*(sdg3_val_MMSR-sdg3_train_MMSR)/sdg3_train_MMSR
sdg4_error_diff<-100*(sdg4_val_MMSR-sdg4_train_MMSR)/sdg4_train_MMSR
sdg5_error_diff<-100*(sdg5_val_MMSR-sdg5_train_MMSR)/sdg5_train_MMSR
sdg6_error_diff<-100*(sdg6_val_MMSR-sdg6_train_MMSR)/sdg6_train_MMSR
sdg7_error_diff<-100*(sdg7_val_MMSR-sdg7_train_MMSR)/sdg7_train_MMSR
sdg8_error_diff<-100*(sdg8_val_MMSR-sdg8_train_MMSR)/sdg8_train_MMSR
sdg9_error_diff<-100*(sdg9_val_MMSR-sdg9_train_MMSR)/sdg9_train_MMSR
sdg10_error_diff<-100*(sdg10_val_MMSR-sdg10_train_MMSR)/sdg10_train_MMSR
sdg11_error_diff<-100*(sdg11_val_MMSR-sdg11_train_MMSR)/sdg11_train_MMSR
sdg13_error_diff<-100*(sdg13_val_MMSR-sdg13_train_MMSR)/sdg13_train_MMSR
sdg14_error_diff<-100*(sdg14_val_MMSR-sdg14_train_MMSR)/sdg14_train_MMSR
sdg15_error_diff<-100*(sdg15_val_MMSR-sdg15_train_MMSR)/sdg15_train_MMSR
sdg16_error_diff<-100*(sdg16_val_MMSR-sdg16_train_MMSR)/sdg16_train_MMSR
sdg17_error_diff<-100*(sdg17_val_MMSR-sdg17_train_MMSR)/sdg17_train_MMSR

error_difs<-c(sdg1_error_diff,sdg2_error_diff,sdg3_error_diff,sdg4_error_diff,sdg5_error_diff,sdg6_error_diff,sdg7_error_diff,sdg8_error_diff
              ,sdg9_error_diff,sdg10_error_diff,sdg11_error_diff,sdg13_error_diff,sdg14_error_diff,sdg15_error_diff,sdg16_error_diff,sdg17_error_diff)


MSRs<-zeros(num_countries,17)
MSRs[,1]<-MSR_fit_plm_imp_pan_sdg1.xf.v
MSRs[,2]<-MSR_fit_plm_imp_pan_sdg2.xf.v
MSRs[,3]<-MSR_fit_plm_imp_pan_sdg3.xf.v
MSRs[,4]<-MSR_fit_plm_imp_pan_sdg4.xf.v
MSRs[,5]<-MSR_fit_plm_imp_pan_sdg5.xf.v
MSRs[,6]<-MSR_fit_plm_imp_pan_sdg6.xf.v
MSRs[,7]<-MSR_fit_plm_imp_pan_sdg7.xf.v
MSRs[,8]<-MSR_fit_plm_imp_pan_sdg8.xf.v
MSRs[,9]<-MSR_fit_plm_imp_pan_sdg9.xf.v
MSRs[,10]<-MSR_fit_plm_imp_pan_sdg10.xf.v
MSRs[,11]<-MSR_fit_plm_imp_pan_sdg11.xf.v
MSRs[,13]<-MSR_fit_plm_imp_pan_sdg13.xf.v
MSRs[,14]<-MSR_fit_plm_imp_pan_sdg14.xf.v
MSRs[,15]<-MSR_fit_plm_imp_pan_sdg15.xf.v
MSRs[,16]<-MSR_fit_plm_imp_pan_sdg16.xf.v
MSRs[,17]<-MSR_fit_plm_imp_pan_sdg17.xf.v

MMSRs.t<-c(sdg1_train_MMSR,sdg2_train_MMSR,sdg3_train_MMSR,sdg4_train_MMSR,sdg5_train_MMSR,sdg6_train_MMSR,sdg7_train_MMSR,sdg8_train_MMSR,sdg9_train_MMSR
           ,sdg10_train_MMSR,sdg11_train_MMSR,sdg13_train_MMSR,sdg14_train_MMSR,sdg15_train_MMSR,sdg16_train_MMSR,sdg17_train_MMSR)
MMSRs.t.stats<-basicStats(MMSRs.t)

MMSRs.v<-c(sdg1_val_MMSR,sdg2_val_MMSR,sdg3_val_MMSR,sdg4_val_MMSR,sdg5_val_MMSR,sdg6_val_MMSR,sdg7_val_MMSR,sdg8_val_MMSR,sdg9_val_MMSR
        ,sdg10_val_MMSR,sdg11_val_MMSR,sdg13_val_MMSR,sdg14_val_MMSR,sdg15_val_MMSR,sdg16_val_MMSR,sdg17_val_MMSR)
MMSRs.v.stats<-basicStats(MMSRs.v)


AUS_val_MMSR<-mean(MSRs[1,])
AUT_val_MMSR<-mean(MSRs[2,])
BEL_val_MMSR<-mean(MSRs[3,])
CAN_val_MMSR<-mean(MSRs[4,])
CHE_val_MMSR<-mean(MSRs[5,])
CHL_val_MMSR<-mean(MSRs[6,])
CZE_val_MMSR<-mean(MSRs[7,])
DEU_val_MMSR<-mean(MSRs[8,])
DNK_val_MMSR<-mean(MSRs[9,])
ESP_val_MMSR<-mean(MSRs[10,])
EST_val_MMSR<-mean(MSRs[11,])
FIN_val_MMSR<-mean(MSRs[12,])
FRA_val_MMSR<-mean(MSRs[13,])
GBR_val_MMSR<-mean(MSRs[14,])
GRC_val_MMSR<-mean(MSRs[15,])
HUN_val_MMSR<-mean(MSRs[16,])
IRL_val_MMSR<-mean(MSRs[17,])
ISL_val_MMSR<-mean(MSRs[18,])
ISR_val_MMSR<-mean(MSRs[19,])
ITA_val_MMSR<-mean(MSRs[20,])
JPN_val_MMSR<-mean(MSRs[21,])
KOR_val_MMSR<-mean(MSRs[22,])
LTU_val_MMSR<-mean(MSRs[23,])
LUX_val_MMSR<-mean(MSRs[24,])
LVA_val_MMSR<-mean(MSRs[25,])
MEX_val_MMSR<-mean(MSRs[26,])
NLD_val_MMSR<-mean(MSRs[27,])
NOR_val_MMSR<-mean(MSRs[28,])
NZL_val_MMSR<-mean(MSRs[29,])
POL_val_MMSR<-mean(MSRs[30,])
PRT_val_MMSR<-mean(MSRs[31,])
SVK_val_MMSR<-mean(MSRs[32,])
SVN_val_MMSR<-mean(MSRs[33,])
SWE_val_MMSR<-mean(MSRs[34,])
TUR_val_MMSR<-mean(MSRs[35,])
USA_val_MMSR<-mean(MSRs[36,])

#Compute mean MSRs for each SDG and each left out country - Lasso vars
sdg1_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg1.xf.tL)
sdg2_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg2.xf.tL)
sdg3_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg3.xf.tL)
sdg4_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg4.xf.tL)
sdg5_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg5.xf.tL)
sdg6_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg6.xf.tL)
sdg7_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg7.xf.tL)
sdg8_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg8.xf.tL)
sdg9_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg9.xf.tL)
sdg10_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg10.xf.tL)
sdg11_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg11.xf.tL)
sdg13_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg13.xf.tL)
sdg14_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg14.xf.tL)
sdg15_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg15.xf.tL)
sdg16_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg16.xf.tL)
sdg17_train_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg17.xf.tL)

sdg1_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg1.xf.vL)
sdg2_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg2.xf.vL)
sdg3_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg3.xf.vL)
sdg4_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg4.xf.vL)
sdg5_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg5.xf.vL)
sdg6_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg6.xf.vL)
sdg7_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg7.xf.vL)
sdg8_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg8.xf.vL)
sdg9_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg9.xf.vL)
sdg10_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg10.xf.vL)
sdg11_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg11.xf.vL)
sdg13_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg13.xf.vL)
sdg14_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg14.xf.vL)
sdg15_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg15.xf.vL)
sdg16_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg16.xf.vL)
sdg17_val_MMSR_Lasso<-mean(MSR_fit_plm_imp_pan_sdg17.xf.vL)

#percent difference between validation error and traiining error for each sdg
sdg1_error_diff_Lasso<-100*(sdg1_val_MMSR_Lasso-sdg1_train_MMSR_Lasso)/sdg1_train_MMSR_Lasso
sdg2_error_diff_Lasso<-100*(sdg2_val_MMSR_Lasso-sdg2_train_MMSR_Lasso)/sdg2_train_MMSR_Lasso
sdg3_error_diff_Lasso<-100*(sdg3_val_MMSR_Lasso-sdg3_train_MMSR_Lasso)/sdg3_train_MMSR_Lasso
sdg4_error_diff_Lasso<-100*(sdg4_val_MMSR_Lasso-sdg4_train_MMSR_Lasso)/sdg4_train_MMSR_Lasso
sdg5_error_diff_Lasso<-100*(sdg5_val_MMSR_Lasso-sdg5_train_MMSR_Lasso)/sdg5_train_MMSR_Lasso
sdg6_error_diff_Lasso<-100*(sdg6_val_MMSR_Lasso-sdg6_train_MMSR_Lasso)/sdg6_train_MMSR_Lasso
sdg7_error_diff_Lasso<-100*(sdg7_val_MMSR_Lasso-sdg7_train_MMSR_Lasso)/sdg7_train_MMSR_Lasso
sdg8_error_diff_Lasso<-100*(sdg8_val_MMSR_Lasso-sdg8_train_MMSR_Lasso)/sdg8_train_MMSR_Lasso
sdg9_error_diff_Lasso<-100*(sdg9_val_MMSR_Lasso-sdg9_train_MMSR_Lasso)/sdg9_train_MMSR_Lasso
sdg10_error_diff_Lasso<-100*(sdg10_val_MMSR_Lasso-sdg10_train_MMSR_Lasso)/sdg10_train_MMSR_Lasso
sdg11_error_diff_Lasso<-100*(sdg11_val_MMSR_Lasso-sdg11_train_MMSR_Lasso)/sdg11_train_MMSR_Lasso
sdg13_error_diff_Lasso<-100*(sdg13_val_MMSR_Lasso-sdg13_train_MMSR_Lasso)/sdg13_train_MMSR_Lasso
sdg14_error_diff_Lasso<-100*(sdg14_val_MMSR_Lasso-sdg14_train_MMSR_Lasso)/sdg14_train_MMSR_Lasso
sdg15_error_diff_Lasso<-100*(sdg15_val_MMSR_Lasso-sdg15_train_MMSR_Lasso)/sdg15_train_MMSR_Lasso
sdg16_error_diff_Lasso<-100*(sdg16_val_MMSR_Lasso-sdg16_train_MMSR_Lasso)/sdg16_train_MMSR_Lasso
sdg17_error_diff_Lasso<-100*(sdg17_val_MMSR_Lasso-sdg17_train_MMSR_Lasso)/sdg17_train_MMSR_Lasso

error_difs_Lasso<-c(sdg1_error_diff_Lasso,sdg2_error_diff_Lasso,sdg3_error_diff_Lasso,sdg4_error_diff_Lasso,sdg5_error_diff_Lasso,sdg6_error_diff_Lasso,sdg7_error_diff_Lasso,sdg8_error_diff_Lasso
              ,sdg9_error_diff_Lasso,sdg10_error_diff_Lasso,sdg11_error_diff_Lasso,sdg13_error_diff_Lasso,sdg14_error_diff_Lasso,sdg15_error_diff_Lasso,sdg16_error_diff_Lasso,sdg17_error_diff_Lasso)


MSRs_Lasso<-zeros(num_countries,17)
MSRs_Lasso[,1]<-MSR_fit_plm_imp_pan_sdg1.xf.vL
MSRs_Lasso[,2]<-MSR_fit_plm_imp_pan_sdg2.xf.vL
MSRs_Lasso[,3]<-MSR_fit_plm_imp_pan_sdg3.xf.vL
MSRs_Lasso[,4]<-MSR_fit_plm_imp_pan_sdg4.xf.vL
MSRs_Lasso[,5]<-MSR_fit_plm_imp_pan_sdg5.xf.vL
MSRs_Lasso[,6]<-MSR_fit_plm_imp_pan_sdg6.xf.vL
MSRs_Lasso[,7]<-MSR_fit_plm_imp_pan_sdg7.xf.vL
MSRs_Lasso[,8]<-MSR_fit_plm_imp_pan_sdg8.xf.vL
MSRs_Lasso[,9]<-MSR_fit_plm_imp_pan_sdg9.xf.vL
MSRs_Lasso[,10]<-MSR_fit_plm_imp_pan_sdg10.xf.vL
MSRs_Lasso[,11]<-MSR_fit_plm_imp_pan_sdg11.xf.vL
MSRs_Lasso[,13]<-MSR_fit_plm_imp_pan_sdg13.xf.vL
MSRs_Lasso[,14]<-MSR_fit_plm_imp_pan_sdg14.xf.vL
MSRs_Lasso[,15]<-MSR_fit_plm_imp_pan_sdg15.xf.vL
MSRs_Lasso[,16]<-MSR_fit_plm_imp_pan_sdg16.xf.vL
MSRs_Lasso[,17]<-MSR_fit_plm_imp_pan_sdg17.xf.vL

MMSRs.tL<-c(sdg1_train_MMSR_Lasso,sdg2_train_MMSR_Lasso,sdg3_train_MMSR_Lasso,sdg4_train_MMSR_Lasso,sdg5_train_MMSR_Lasso,sdg6_train_MMSR_Lasso,sdg7_train_MMSR_Lasso,sdg8_train_MMSR_Lasso,sdg9_train_MMSR_Lasso
            ,sdg10_train_MMSR_Lasso,sdg11_train_MMSR_Lasso,sdg13_train_MMSR_Lasso,sdg14_train_MMSR_Lasso,sdg15_train_MMSR_Lasso,sdg16_train_MMSR_Lasso,sdg17_train_MMSR_Lasso)
MMSRs.tL.stats<-basicStats(MMSRs.tL)


MMSRs.vL<-c(sdg1_val_MMSR_Lasso,sdg2_val_MMSR_Lasso,sdg3_val_MMSR_Lasso,sdg4_val_MMSR_Lasso,sdg5_val_MMSR_Lasso,sdg6_val_MMSR_Lasso,sdg7_val_MMSR_Lasso,sdg8_val_MMSR_Lasso,sdg9_val_MMSR_Lasso
         ,sdg10_val_MMSR_Lasso,sdg11_val_MMSR_Lasso,sdg13_val_MMSR_Lasso,sdg14_val_MMSR_Lasso,sdg15_val_MMSR_Lasso,sdg16_val_MMSR_Lasso,sdg17_val_MMSR_Lasso)
MMSRs.vL.stats<-basicStats(MMSRs.vL)


AUS_val_MMSR_Lasso<-mean(MSRs_Lasso[1,])
AUT_val_MMSR_Lasso<-mean(MSRs_Lasso[2,])
BEL_val_MMSR_Lasso<-mean(MSRs_Lasso[3,])
CAN_val_MMSR_Lasso<-mean(MSRs_Lasso[4,])
CHE_val_MMSR_Lasso<-mean(MSRs_Lasso[5,])
CHL_val_MMSR_Lasso<-mean(MSRs_Lasso[6,])
CZE_val_MMSR_Lasso<-mean(MSRs_Lasso[7,])
DEU_val_MMSR_Lasso<-mean(MSRs_Lasso[8,])
DNK_val_MMSR_Lasso<-mean(MSRs_Lasso[9,])
ESP_val_MMSR_Lasso<-mean(MSRs_Lasso[10,])
EST_val_MMSR_Lasso<-mean(MSRs_Lasso[11,])
FIN_val_MMSR_Lasso<-mean(MSRs_Lasso[12,])
FRA_val_MMSR_Lasso<-mean(MSRs_Lasso[13,])
GBR_val_MMSR_Lasso<-mean(MSRs_Lasso[14,])
GRC_val_MMSR_Lasso<-mean(MSRs_Lasso[15,])
HUN_val_MMSR_Lasso<-mean(MSRs_Lasso[16,])
IRL_val_MMSR_Lasso<-mean(MSRs_Lasso[17,])
ISL_val_MMSR_Lasso<-mean(MSRs_Lasso[18,])
ISR_val_MMSR_Lasso<-mean(MSRs_Lasso[19,])
ITA_val_MMSR_Lasso<-mean(MSRs_Lasso[20,])
JPN_val_MMSR_Lasso<-mean(MSRs_Lasso[21,])
KOR_val_MMSR_Lasso<-mean(MSRs_Lasso[22,])
LTU_val_MMSR_Lasso<-mean(MSRs_Lasso[23,])
LUX_val_MMSR_Lasso<-mean(MSRs_Lasso[24,])
LVA_val_MMSR_Lasso<-mean(MSRs_Lasso[25,])
MEX_val_MMSR_Lasso<-mean(MSRs_Lasso[26,])
NLD_val_MMSR_Lasso<-mean(MSRs_Lasso[27,])
NOR_val_MMSR_Lasso<-mean(MSRs_Lasso[28,])
NZL_val_MMSR_Lasso<-mean(MSRs_Lasso[29,])
POL_val_MMSR_Lasso<-mean(MSRs_Lasso[30,])
PRT_val_MMSR_Lasso<-mean(MSRs_Lasso[31,])
SVK_val_MMSR_Lasso<-mean(MSRs_Lasso[32,])
SVN_val_MMSR_Lasso<-mean(MSRs_Lasso[33,])
SWE_val_MMSR_Lasso<-mean(MSRs_Lasso[34,])
TUR_val_MMSR_Lasso<-mean(MSRs_Lasso[35,])
USA_val_MMSR_Lasso<-mean(MSRs_Lasso[36,])

Countries_MSR_Lasso.i<-c(AUS_val_MMSR_Lasso,AUT_val_MMSR_Lasso,BEL_val_MMSR_Lasso,CAN_val_MMSR_Lasso,CHE_val_MMSR_Lasso
                         ,CHL_val_MMSR_Lasso,CZE_val_MMSR_Lasso,DEU_val_MMSR_Lasso,DNK_val_MMSR_Lasso,ESP_val_MMSR_Lasso
                         ,EST_val_MMSR_Lasso,FIN_val_MMSR_Lasso,FRA_val_MMSR_Lasso,GBR_val_MMSR_Lasso,GRC_val_MMSR_Lasso
                         ,HUN_val_MMSR_Lasso,IRL_val_MMSR_Lasso,ISL_val_MMSR_Lasso,ISR_val_MMSR_Lasso,ITA_val_MMSR_Lasso
                         ,JPN_val_MMSR_Lasso,KOR_val_MMSR_Lasso,LTU_val_MMSR_Lasso,LUX_val_MMSR_Lasso,LVA_val_MMSR_Lasso
                         ,MEX_val_MMSR_Lasso,NLD_val_MMSR_Lasso,NOR_val_MMSR_Lasso,NZL_val_MMSR_Lasso,POL_val_MMSR_Lasso
                         ,PRT_val_MMSR_Lasso,SVK_val_MMSR_Lasso,SVN_val_MMSR_Lasso,SWE_val_MMSR_Lasso,TUR_val_MMSR_Lasso
                         ,USA_val_MMSR_Lasso)
Countries_MSR_Lasso<-data.frame(unique(data.imp$id),Countries_MSR_Lasso.i)
Lasso_Country_stats<-basicStats(Countries_MSR_Lasso$Countries_MSR_Lasso.i)
#USA, Mexico, ISL all high error


#Compute mean MSRs for each SDG and each left out country - Pruned Lasso vars
sdg1_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg1.xf.tLP)
sdg2_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg2.xf.tLP)
sdg3_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg3.xf.tLP)
sdg4_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg4.xf.tLP)
sdg5_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg5.xf.tLP)
sdg6_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg6.xf.tLP)
sdg7_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg7.xf.tLP)
sdg8_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg8.xf.tLP)
sdg9_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg9.xf.tLP)
sdg10_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg10.xf.tLP)
sdg11_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg11.xf.tLP)
sdg13_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg13.xf.tLP)
sdg14_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg14.xf.tLP)
sdg15_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg15.xf.tLP)
sdg16_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg16.xf.tLP)
sdg17_train_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg17.xf.tLP)

sdg1_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg1.xf.vLP)
sdg2_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg2.xf.vLP)
sdg3_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg3.xf.vLP)
sdg4_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg4.xf.vLP)
sdg5_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg5.xf.vLP)
sdg6_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg6.xf.vLP)
sdg7_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg7.xf.vLP)
sdg8_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg8.xf.vLP)
sdg9_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg9.xf.vLP)
sdg10_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg10.xf.vLP)
sdg11_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg11.xf.vLP)
sdg13_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg13.xf.vLP)
sdg14_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg14.xf.vLP)
sdg15_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg15.xf.vLP)
sdg16_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg16.xf.vLP)
sdg17_val_MMSR_Lasso.P<-mean(MSR_fit_plm_imp_pan_sdg17.xf.vLP)

#percent difference between validation error and training error for each sdg
sdg1_error_diff_Lasso.P<-100*(sdg1_val_MMSR_Lasso.P-sdg1_train_MMSR_Lasso.P)/sdg1_train_MMSR_Lasso.P
sdg2_error_diff_Lasso.P<-100*(sdg2_val_MMSR_Lasso.P-sdg2_train_MMSR_Lasso.P)/sdg2_train_MMSR_Lasso.P
sdg3_error_diff_Lasso.P<-100*(sdg3_val_MMSR_Lasso.P-sdg3_train_MMSR_Lasso.P)/sdg3_train_MMSR_Lasso.P
sdg4_error_diff_Lasso.P<-100*(sdg4_val_MMSR_Lasso.P-sdg4_train_MMSR_Lasso.P)/sdg4_train_MMSR_Lasso.P
sdg5_error_diff_Lasso.P<-100*(sdg5_val_MMSR_Lasso.P-sdg5_train_MMSR_Lasso.P)/sdg5_train_MMSR_Lasso.P
sdg6_error_diff_Lasso.P<-100*(sdg6_val_MMSR_Lasso.P-sdg6_train_MMSR_Lasso.P)/sdg6_train_MMSR_Lasso.P
sdg7_error_diff_Lasso.P<-100*(sdg7_val_MMSR_Lasso.P-sdg7_train_MMSR_Lasso.P)/sdg7_train_MMSR_Lasso.P
sdg8_error_diff_Lasso.P<-100*(sdg8_val_MMSR_Lasso.P-sdg8_train_MMSR_Lasso.P)/sdg8_train_MMSR_Lasso.P
sdg9_error_diff_Lasso.P<-100*(sdg9_val_MMSR_Lasso.P-sdg9_train_MMSR_Lasso.P)/sdg9_train_MMSR_Lasso.P
sdg10_error_diff_Lasso.P<-100*(sdg10_val_MMSR_Lasso.P-sdg10_train_MMSR_Lasso.P)/sdg10_train_MMSR_Lasso.P
sdg11_error_diff_Lasso.P<-100*(sdg11_val_MMSR_Lasso.P-sdg11_train_MMSR_Lasso.P)/sdg11_train_MMSR_Lasso.P
sdg13_error_diff_Lasso.P<-100*(sdg13_val_MMSR_Lasso.P-sdg13_train_MMSR_Lasso.P)/sdg13_train_MMSR_Lasso.P
sdg14_error_diff_Lasso.P<-100*(sdg14_val_MMSR_Lasso.P-sdg14_train_MMSR_Lasso.P)/sdg14_train_MMSR_Lasso.P
sdg15_error_diff_Lasso.P<-100*(sdg15_val_MMSR_Lasso.P-sdg15_train_MMSR_Lasso.P)/sdg15_train_MMSR_Lasso.P
sdg16_error_diff_Lasso.P<-100*(sdg16_val_MMSR_Lasso.P-sdg16_train_MMSR_Lasso.P)/sdg16_train_MMSR_Lasso.P
sdg17_error_diff_Lasso.P<-100*(sdg17_val_MMSR_Lasso.P-sdg17_train_MMSR_Lasso.P)/sdg17_train_MMSR_Lasso.P

error_difs_Lasso.P<-c(sdg1_error_diff_Lasso.P,sdg2_error_diff_Lasso.P,sdg3_error_diff_Lasso.P,sdg4_error_diff_Lasso.P,sdg5_error_diff_Lasso.P,sdg6_error_diff_Lasso.P,sdg7_error_diff_Lasso.P,sdg8_error_diff_Lasso.P
                      ,sdg9_error_diff_Lasso.P,sdg10_error_diff_Lasso.P,sdg11_error_diff_Lasso.P,sdg13_error_diff_Lasso.P,sdg14_error_diff_Lasso.P,sdg15_error_diff_Lasso.P,sdg16_error_diff_Lasso.P,sdg17_error_diff_Lasso.P)


MSRs_Lasso.P<-zeros(num_countries,17)
MSRs_Lasso.P[,1]<-MSR_fit_plm_imp_pan_sdg1.xf.vLP
MSRs_Lasso.P[,2]<-MSR_fit_plm_imp_pan_sdg2.xf.vLP
MSRs_Lasso.P[,3]<-MSR_fit_plm_imp_pan_sdg3.xf.vLP
MSRs_Lasso.P[,4]<-MSR_fit_plm_imp_pan_sdg4.xf.vLP
MSRs_Lasso.P[,5]<-MSR_fit_plm_imp_pan_sdg5.xf.vLP
MSRs_Lasso.P[,6]<-MSR_fit_plm_imp_pan_sdg6.xf.vLP
MSRs_Lasso.P[,7]<-MSR_fit_plm_imp_pan_sdg7.xf.vLP
MSRs_Lasso.P[,8]<-MSR_fit_plm_imp_pan_sdg8.xf.vLP
MSRs_Lasso.P[,9]<-MSR_fit_plm_imp_pan_sdg9.xf.vLP
MSRs_Lasso.P[,10]<-MSR_fit_plm_imp_pan_sdg10.xf.vLP
MSRs_Lasso.P[,11]<-MSR_fit_plm_imp_pan_sdg11.xf.vLP
MSRs_Lasso.P[,13]<-MSR_fit_plm_imp_pan_sdg13.xf.vLP
MSRs_Lasso.P[,14]<-MSR_fit_plm_imp_pan_sdg14.xf.vLP
MSRs_Lasso.P[,15]<-MSR_fit_plm_imp_pan_sdg15.xf.vLP
MSRs_Lasso.P[,16]<-MSR_fit_plm_imp_pan_sdg16.xf.vLP
MSRs_Lasso.P[,17]<-MSR_fit_plm_imp_pan_sdg17.xf.vLP

MMSRs.tLP<-c(sdg1_train_MMSR_Lasso.P,sdg2_train_MMSR_Lasso.P,sdg3_train_MMSR_Lasso.P,sdg4_train_MMSR_Lasso.P,sdg5_train_MMSR_Lasso.P,sdg6_train_MMSR_Lasso.P,sdg7_train_MMSR_Lasso.P,sdg8_train_MMSR_Lasso.P,sdg9_train_MMSR_Lasso.P
             ,sdg10_train_MMSR_Lasso.P,sdg11_train_MMSR_Lasso.P,sdg13_train_MMSR_Lasso.P,sdg14_train_MMSR_Lasso.P,sdg15_train_MMSR_Lasso.P,sdg16_train_MMSR_Lasso.P,sdg17_train_MMSR_Lasso.P)
MMSRs.tLP.stats<-basicStats(MMSRs.tLP)
MMSRs.tL-MMSRs.tLP #shows improvement between pruned lasso and unpruned lasso var selection in training error


MMSRs.vLP<-c(sdg1_val_MMSR_Lasso.P,sdg2_val_MMSR_Lasso.P,sdg3_val_MMSR_Lasso.P,sdg4_val_MMSR_Lasso.P,sdg5_val_MMSR_Lasso.P,sdg6_val_MMSR_Lasso.P,sdg7_val_MMSR_Lasso.P,sdg8_val_MMSR_Lasso.P,sdg9_val_MMSR_Lasso.P
             ,sdg10_val_MMSR_Lasso.P,sdg11_val_MMSR_Lasso.P,sdg13_val_MMSR_Lasso.P,sdg14_val_MMSR_Lasso.P,sdg15_val_MMSR_Lasso.P,sdg16_val_MMSR_Lasso.P,sdg17_val_MMSR_Lasso.P)
MMSRs.vLP.stats<-basicStats(MMSRs.vLP)
MMSRs.vL-MMSRs.vLP #shows improvement between pruned lasso and unpruned lasso var selection in validation error

AUS_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[1,])
AUT_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[2,])
BEL_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[3,])
CAN_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[4,])
CHE_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[5,])
CHL_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[6,])
CZE_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[7,])
DEU_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[8,])
DNK_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[9,])
ESP_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[10,])
EST_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[11,])
FIN_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[12,])
FRA_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[13,])
GBR_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[14,])
GRC_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[15,])
HUN_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[16,])
IRL_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[17,])
ISL_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[18,])
ISR_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[19,])
ITA_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[20,])
JPN_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[21,])
KOR_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[22,])
LTU_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[23,])
LUX_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[24,])
LVA_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[25,])
MEX_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[26,])
NLD_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[27,])
NOR_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[28,])
NZL_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[29,])
POL_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[30,])
PRT_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[31,])
SVK_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[32,])
SVN_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[33,])
SWE_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[34,])
TUR_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[35,])
USA_val_MMSR_Lasso.P<-mean(MSRs_Lasso.P[36,])

Countries_MSR_Lasso.P.i<-c(AUS_val_MMSR_Lasso.P,AUT_val_MMSR_Lasso.P,BEL_val_MMSR_Lasso.P,CAN_val_MMSR_Lasso.P,CHE_val_MMSR_Lasso.P
                           ,CHL_val_MMSR_Lasso.P,CZE_val_MMSR_Lasso.P,DEU_val_MMSR_Lasso.P,DNK_val_MMSR_Lasso.P,ESP_val_MMSR_Lasso.P
                           ,EST_val_MMSR_Lasso.P,FIN_val_MMSR_Lasso.P,FRA_val_MMSR_Lasso.P,GBR_val_MMSR_Lasso.P,GRC_val_MMSR_Lasso.P
                           ,HUN_val_MMSR_Lasso.P,IRL_val_MMSR_Lasso.P,ISL_val_MMSR_Lasso.P,ISR_val_MMSR_Lasso.P,ITA_val_MMSR_Lasso.P
                           ,JPN_val_MMSR_Lasso.P,KOR_val_MMSR_Lasso.P,LTU_val_MMSR_Lasso.P,LUX_val_MMSR_Lasso.P,LVA_val_MMSR_Lasso.P
                           ,MEX_val_MMSR_Lasso.P,NLD_val_MMSR_Lasso.P,NOR_val_MMSR_Lasso.P,NZL_val_MMSR_Lasso.P,POL_val_MMSR_Lasso.P
                           ,PRT_val_MMSR_Lasso.P,SVK_val_MMSR_Lasso.P,SVN_val_MMSR_Lasso.P,SWE_val_MMSR_Lasso.P,TUR_val_MMSR_Lasso.P
                           ,USA_val_MMSR_Lasso.P)
Countries_MSR_Lasso.P<-data.frame(unique(data.imp$id),Countries_MSR_Lasso.P.i)
Pruned_Lasso_Country_stats<-basicStats(Countries_MSR_Lasso.P$Countries_MSR_Lasso.P.i)
#Mexico highest error, ISL and USA also high


#Compute mean MSRs for each SDG and each left out country - Extra Pruned Lasso vars
sdg1_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg1.xf.tLP2)
sdg2_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg2.xf.tLP2)
sdg3_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg3.xf.tLP2)
sdg4_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg4.xf.tLP2)
sdg5_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg5.xf.tLP2)
sdg6_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg6.xf.tLP2)
sdg7_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg7.xf.tLP2)
sdg8_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg8.xf.tLP2)
sdg9_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg9.xf.tLP2)
sdg10_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg10.xf.tLP2)
sdg11_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg11.xf.tLP2)
sdg13_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg13.xf.tLP2)
sdg14_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg14.xf.tLP2)
sdg15_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg15.xf.tLP2)
sdg16_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg16.xf.tLP2)
sdg17_train_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg17.xf.tLP2)

sdg1_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg1.xf.vLP2)
sdg2_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg2.xf.vLP2)
sdg3_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg3.xf.vLP2)
sdg4_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg4.xf.vLP2)
sdg5_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg5.xf.vLP2)
sdg6_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg6.xf.vLP2)
sdg7_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg7.xf.vLP2)
sdg8_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg8.xf.vLP2)
sdg9_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg9.xf.vLP2)
sdg10_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg10.xf.vLP2)
sdg11_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg11.xf.vLP2)
sdg13_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg13.xf.vLP2)
sdg14_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg14.xf.vLP2)
sdg15_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg15.xf.vLP2)
sdg16_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg16.xf.vLP2)
sdg17_val_MMSR_Lasso.P2<-mean(MSR_fit_plm_imp_pan_sdg17.xf.vLP2)

#percent difference between validation error and training error for each sdg
sdg1_error_diff_Lasso.P2<-100*(sdg1_val_MMSR_Lasso.P2-sdg1_train_MMSR_Lasso.P2)/sdg1_train_MMSR_Lasso.P2
sdg2_error_diff_Lasso.P2<-100*(sdg2_val_MMSR_Lasso.P2-sdg2_train_MMSR_Lasso.P2)/sdg2_train_MMSR_Lasso.P2
sdg3_error_diff_Lasso.P2<-100*(sdg3_val_MMSR_Lasso.P2-sdg3_train_MMSR_Lasso.P2)/sdg3_train_MMSR_Lasso.P2
sdg4_error_diff_Lasso.P2<-100*(sdg4_val_MMSR_Lasso.P2-sdg4_train_MMSR_Lasso.P2)/sdg4_train_MMSR_Lasso.P2
sdg5_error_diff_Lasso.P2<-100*(sdg5_val_MMSR_Lasso.P2-sdg5_train_MMSR_Lasso.P2)/sdg5_train_MMSR_Lasso.P2
sdg6_error_diff_Lasso.P2<-100*(sdg6_val_MMSR_Lasso.P2-sdg6_train_MMSR_Lasso.P2)/sdg6_train_MMSR_Lasso.P2
sdg7_error_diff_Lasso.P2<-100*(sdg7_val_MMSR_Lasso.P2-sdg7_train_MMSR_Lasso.P2)/sdg7_train_MMSR_Lasso.P2
sdg8_error_diff_Lasso.P2<-100*(sdg8_val_MMSR_Lasso.P2-sdg8_train_MMSR_Lasso.P2)/sdg8_train_MMSR_Lasso.P2
sdg9_error_diff_Lasso.P2<-100*(sdg9_val_MMSR_Lasso.P2-sdg9_train_MMSR_Lasso.P2)/sdg9_train_MMSR_Lasso.P2
sdg10_error_diff_Lasso.P2<-100*(sdg10_val_MMSR_Lasso.P2-sdg10_train_MMSR_Lasso.P2)/sdg10_train_MMSR_Lasso.P2
sdg11_error_diff_Lasso.P2<-100*(sdg11_val_MMSR_Lasso.P2-sdg11_train_MMSR_Lasso.P2)/sdg11_train_MMSR_Lasso.P2
sdg13_error_diff_Lasso.P2<-100*(sdg13_val_MMSR_Lasso.P2-sdg13_train_MMSR_Lasso.P2)/sdg13_train_MMSR_Lasso.P2
sdg14_error_diff_Lasso.P2<-100*(sdg14_val_MMSR_Lasso.P2-sdg14_train_MMSR_Lasso.P2)/sdg14_train_MMSR_Lasso.P2
sdg15_error_diff_Lasso.P2<-100*(sdg15_val_MMSR_Lasso.P2-sdg15_train_MMSR_Lasso.P2)/sdg15_train_MMSR_Lasso.P2
sdg16_error_diff_Lasso.P2<-100*(sdg16_val_MMSR_Lasso.P2-sdg16_train_MMSR_Lasso.P2)/sdg16_train_MMSR_Lasso.P2
sdg17_error_diff_Lasso.P2<-100*(sdg17_val_MMSR_Lasso.P2-sdg17_train_MMSR_Lasso.P2)/sdg17_train_MMSR_Lasso.P2

error_difs_Lasso.P2<-c(sdg1_error_diff_Lasso.P2,sdg2_error_diff_Lasso.P2,sdg3_error_diff_Lasso.P2,sdg4_error_diff_Lasso.P2,sdg5_error_diff_Lasso.P2,sdg6_error_diff_Lasso.P2,sdg7_error_diff_Lasso.P2,sdg8_error_diff_Lasso.P2
                       ,sdg9_error_diff_Lasso.P2,sdg10_error_diff_Lasso.P2,sdg11_error_diff_Lasso.P2,sdg13_error_diff_Lasso.P2,sdg14_error_diff_Lasso.P2,sdg15_error_diff_Lasso.P2,sdg16_error_diff_Lasso.P2,sdg17_error_diff_Lasso.P2)


MSRs_Lasso.P2<-zeros(num_countries,17)
MSRs_Lasso.P2[,1]<-MSR_fit_plm_imp_pan_sdg1.xf.vLP2
MSRs_Lasso.P2[,2]<-MSR_fit_plm_imp_pan_sdg2.xf.vLP2
MSRs_Lasso.P2[,3]<-MSR_fit_plm_imp_pan_sdg3.xf.vLP2
MSRs_Lasso.P2[,4]<-MSR_fit_plm_imp_pan_sdg4.xf.vLP2
MSRs_Lasso.P2[,5]<-MSR_fit_plm_imp_pan_sdg5.xf.vLP2
MSRs_Lasso.P2[,6]<-MSR_fit_plm_imp_pan_sdg6.xf.vLP2
MSRs_Lasso.P2[,7]<-MSR_fit_plm_imp_pan_sdg7.xf.vLP2
MSRs_Lasso.P2[,8]<-MSR_fit_plm_imp_pan_sdg8.xf.vLP2
MSRs_Lasso.P2[,9]<-MSR_fit_plm_imp_pan_sdg9.xf.vLP2
MSRs_Lasso.P2[,10]<-MSR_fit_plm_imp_pan_sdg10.xf.vLP2
MSRs_Lasso.P2[,11]<-MSR_fit_plm_imp_pan_sdg11.xf.vLP2
MSRs_Lasso.P2[,13]<-MSR_fit_plm_imp_pan_sdg13.xf.vLP2
MSRs_Lasso.P2[,14]<-MSR_fit_plm_imp_pan_sdg14.xf.vLP2
MSRs_Lasso.P2[,15]<-MSR_fit_plm_imp_pan_sdg15.xf.vLP2
MSRs_Lasso.P2[,16]<-MSR_fit_plm_imp_pan_sdg16.xf.vLP2
MSRs_Lasso.P2[,17]<-MSR_fit_plm_imp_pan_sdg17.xf.vLP2

MMSRs.tLP2<-c(sdg1_train_MMSR_Lasso.P2,sdg2_train_MMSR_Lasso.P2,sdg3_train_MMSR_Lasso.P2,sdg4_train_MMSR_Lasso.P2,sdg5_train_MMSR_Lasso.P2,sdg6_train_MMSR_Lasso.P2,sdg7_train_MMSR_Lasso.P2,sdg8_train_MMSR_Lasso.P2,sdg9_train_MMSR_Lasso.P2
             ,sdg10_train_MMSR_Lasso.P2,sdg11_train_MMSR_Lasso.P2,sdg13_train_MMSR_Lasso.P2,sdg14_train_MMSR_Lasso.P2,sdg15_train_MMSR_Lasso.P2,sdg16_train_MMSR_Lasso.P2,sdg17_train_MMSR_Lasso.P2)
MMSRs.tLP2.stats<-basicStats(MMSRs.tLP2)
MMSRs.tLP-MMSRs.tLP2 #shows improvement between extra pruned lasso and pruned lasso var selection in training error


MMSRs.vLP2<-c(sdg1_val_MMSR_Lasso.P2,sdg2_val_MMSR_Lasso.P2,sdg3_val_MMSR_Lasso.P2,sdg4_val_MMSR_Lasso.P2,sdg5_val_MMSR_Lasso.P2,sdg6_val_MMSR_Lasso.P2,sdg7_val_MMSR_Lasso.P2,sdg8_val_MMSR_Lasso.P2,sdg9_val_MMSR_Lasso.P2
              ,sdg10_val_MMSR_Lasso.P2,sdg11_val_MMSR_Lasso.P2,sdg13_val_MMSR_Lasso.P2,sdg14_val_MMSR_Lasso.P2,sdg15_val_MMSR_Lasso.P2,sdg16_val_MMSR_Lasso.P2,sdg17_val_MMSR_Lasso.P2)
MMSRs.vLP2.stats<-basicStats(MMSRs.vLP2)
MMSRs.vLP-MMSRs.vLP2 #shows improvement between extra pruned lasso and pruned lasso var selection in validation error

AUS_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[1,])
AUT_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[2,])
BEL_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[3,])
CAN_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[4,])
CHE_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[5,])
CHL_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[6,])
CZE_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[7,])
DEU_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[8,])
DNK_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[9,])
ESP_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[10,])
EST_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[11,])
FIN_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[12,])
FRA_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[13,])
GBR_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[14,])
GRC_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[15,])
HUN_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[16,])
IRL_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[17,])
ISL_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[18,])
ISR_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[19,])
ITA_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[20,])
JPN_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[21,])
KOR_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[22,])
LTU_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[23,])
LUX_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[24,])
LVA_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[25,])
MEX_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[26,])
NLD_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[27,])
NOR_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[28,])
NZL_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[29,])
POL_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[30,])
PRT_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[31,])
SVK_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[32,])
SVN_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[33,])
SWE_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[34,])
TUR_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[35,])
USA_val_MMSR_Lasso.P2<-mean(MSRs_Lasso.P2[36,])


Countries_MSR_Lasso.P2.i<-c(AUS_val_MMSR_Lasso.P2,AUT_val_MMSR_Lasso.P2,BEL_val_MMSR_Lasso.P2,CAN_val_MMSR_Lasso.P2,CHE_val_MMSR_Lasso.P2
                           ,CHL_val_MMSR_Lasso.P2,CZE_val_MMSR_Lasso.P2,DEU_val_MMSR_Lasso.P2,DNK_val_MMSR_Lasso.P2,ESP_val_MMSR_Lasso.P2
                           ,EST_val_MMSR_Lasso.P2,FIN_val_MMSR_Lasso.P2,FRA_val_MMSR_Lasso.P2,GBR_val_MMSR_Lasso.P2,GRC_val_MMSR_Lasso.P2
                           ,HUN_val_MMSR_Lasso.P2,IRL_val_MMSR_Lasso.P2,ISL_val_MMSR_Lasso.P2,ISR_val_MMSR_Lasso.P2,ITA_val_MMSR_Lasso.P2
                           ,JPN_val_MMSR_Lasso.P2,KOR_val_MMSR_Lasso.P2,LTU_val_MMSR_Lasso.P2,LUX_val_MMSR_Lasso.P2,LVA_val_MMSR_Lasso.P2
                           ,MEX_val_MMSR_Lasso.P2,NLD_val_MMSR_Lasso.P2,NOR_val_MMSR_Lasso.P2,NZL_val_MMSR_Lasso.P2,POL_val_MMSR_Lasso.P2
                           ,PRT_val_MMSR_Lasso.P2,SVK_val_MMSR_Lasso.P2,SVN_val_MMSR_Lasso.P2,SWE_val_MMSR_Lasso.P2,TUR_val_MMSR_Lasso.P2
                           ,USA_val_MMSR_Lasso.P2)
Countries_MSR_Lasso.P2<-data.frame(unique(data.imp$id),Countries_MSR_Lasso.P2.i)
Extra_Pruned_Lasso_Country_stats<-basicStats(Countries_MSR_Lasso.P2$Countries_MSR_Lasso.P2.i)
#Mexico way higher error


#Compute mean MSRs for each SDG and each left out country - Interaction inputs
sdg1_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg1.xf.tLint)
sdg1_train_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg1.xf.tLint2)
sdg2_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg2.xf.tLint)
sdg2_train_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg2.xf.tLint2)
sdg3_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg3.xf.tLint)
sdg3_train_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg3.xf.tLint2)
sdg4_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg4.xf.tLint)
sdg5_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg5.xf.tLint)
sdg6_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg6.xf.tLint)
sdg7_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg7.xf.tLint)
sdg8_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg8.xf.tLint)
sdg8_train_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg8.xf.tLint2)
sdg9_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg9.xf.tLint)
sdg9_train_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg9.xf.tLint2)
sdg10_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg10.xf.tLint)
sdg10_train_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg10.xf.tLint2)
sdg11_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg11.xf.tLint)
sdg13_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg13.xf.tLint)
sdg14_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg14.xf.tLint)
sdg15_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg15.xf.tLint)
sdg16_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg16.xf.tLint)
sdg17_train_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg17.xf.tLint)

sdg1_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg1.xf.vLint)
sdg1_val_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg1.xf.vLint2)
sdg2_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg2.xf.vLint)
sdg2_val_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg2.xf.vLint2)
sdg3_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg3.xf.vLint)
sdg3_val_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg3.xf.vLint2)
sdg4_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg4.xf.vLint)
sdg5_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg5.xf.vLint)
sdg6_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg6.xf.vLint)
sdg7_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg7.xf.vLint)
sdg8_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg8.xf.vLint)
sdg8_val_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg8.xf.vLint2)
sdg9_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg9.xf.vLint)
sdg9_val_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg9.xf.vLint2)
sdg10_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg10.xf.vLint)
sdg10_val_MMSR_Lasso.int2<-mean(MSR_fit_plm_imp_pan_sdg10.xf.vLint2)
sdg11_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg11.xf.vLint)
sdg13_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg13.xf.vLint)
sdg14_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg14.xf.vLint)
sdg15_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg15.xf.vLint)
sdg16_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg16.xf.vLint)
sdg17_val_MMSR_Lasso.int<-mean(MSR_fit_plm_imp_pan_sdg17.xf.vLint)

#percent difference between validation error and training error for each sdg
sdg1_error_diff_Lasso.int<-100*(sdg1_val_MMSR_Lasso.int-sdg1_train_MMSR_Lasso.int)/sdg1_train_MMSR_Lasso.int
sdg2_error_diff_Lasso.int<-100*(sdg2_val_MMSR_Lasso.int-sdg2_train_MMSR_Lasso.int)/sdg2_train_MMSR_Lasso.int
sdg3_error_diff_Lasso.int<-100*(sdg3_val_MMSR_Lasso.int-sdg3_train_MMSR_Lasso.int)/sdg3_train_MMSR_Lasso.int
sdg4_error_diff_Lasso.int<-100*(sdg4_val_MMSR_Lasso.int-sdg4_train_MMSR_Lasso.int)/sdg4_train_MMSR_Lasso.int
sdg5_error_diff_Lasso.int<-100*(sdg5_val_MMSR_Lasso.int-sdg5_train_MMSR_Lasso.int)/sdg5_train_MMSR_Lasso.int
sdg6_error_diff_Lasso.int<-100*(sdg6_val_MMSR_Lasso.int-sdg6_train_MMSR_Lasso.int)/sdg6_train_MMSR_Lasso.int
sdg7_error_diff_Lasso.int<-100*(sdg7_val_MMSR_Lasso.int-sdg7_train_MMSR_Lasso.int)/sdg7_train_MMSR_Lasso.int
sdg8_error_diff_Lasso.int<-100*(sdg8_val_MMSR_Lasso.int-sdg8_train_MMSR_Lasso.int)/sdg8_train_MMSR_Lasso.int
sdg9_error_diff_Lasso.int<-100*(sdg9_val_MMSR_Lasso.int-sdg9_train_MMSR_Lasso.int)/sdg9_train_MMSR_Lasso.int
sdg10_error_diff_Lasso.int<-100*(sdg10_val_MMSR_Lasso.int-sdg10_train_MMSR_Lasso.int)/sdg10_train_MMSR_Lasso.int
sdg11_error_diff_Lasso.int<-100*(sdg11_val_MMSR_Lasso.int-sdg11_train_MMSR_Lasso.int)/sdg11_train_MMSR_Lasso.int
sdg13_error_diff_Lasso.int<-100*(sdg13_val_MMSR_Lasso.int-sdg13_train_MMSR_Lasso.int)/sdg13_train_MMSR_Lasso.int
sdg14_error_diff_Lasso.int<-100*(sdg14_val_MMSR_Lasso.int-sdg14_train_MMSR_Lasso.int)/sdg14_train_MMSR_Lasso.int
sdg15_error_diff_Lasso.int<-100*(sdg15_val_MMSR_Lasso.int-sdg15_train_MMSR_Lasso.int)/sdg15_train_MMSR_Lasso.int
sdg16_error_diff_Lasso.int<-100*(sdg16_val_MMSR_Lasso.int-sdg16_train_MMSR_Lasso.int)/sdg16_train_MMSR_Lasso.int
sdg17_error_diff_Lasso.int<-100*(sdg17_val_MMSR_Lasso.int-sdg17_train_MMSR_Lasso.int)/sdg17_train_MMSR_Lasso.int

error_difs_Lasso.int<-c(sdg1_error_diff_Lasso.int,sdg2_error_diff_Lasso.int,sdg3_error_diff_Lasso.int,sdg4_error_diff_Lasso.int,sdg5_error_diff_Lasso.int,sdg6_error_diff_Lasso.int,sdg7_error_diff_Lasso.int,sdg8_error_diff_Lasso.int
                       ,sdg9_error_diff_Lasso.int,sdg10_error_diff_Lasso.int,sdg11_error_diff_Lasso.int,sdg13_error_diff_Lasso.int,sdg14_error_diff_Lasso.int,sdg15_error_diff_Lasso.int,sdg16_error_diff_Lasso.int,sdg17_error_diff_Lasso.int)


MSRs_Lasso.int<-zeros(num_countries,17)
MSRs_Lasso.int[,1]<-MSR_fit_plm_imp_pan_sdg1.xf.vLint
MSRs_Lasso.int[,2]<-MSR_fit_plm_imp_pan_sdg2.xf.vLint
MSRs_Lasso.int[,3]<-MSR_fit_plm_imp_pan_sdg3.xf.vLint
MSRs_Lasso.int[,4]<-MSR_fit_plm_imp_pan_sdg4.xf.vLint
MSRs_Lasso.int[,5]<-MSR_fit_plm_imp_pan_sdg5.xf.vLint
MSRs_Lasso.int[,6]<-MSR_fit_plm_imp_pan_sdg6.xf.vLint
MSRs_Lasso.int[,7]<-MSR_fit_plm_imp_pan_sdg7.xf.vLint
MSRs_Lasso.int[,8]<-MSR_fit_plm_imp_pan_sdg8.xf.vLint
MSRs_Lasso.int[,9]<-MSR_fit_plm_imp_pan_sdg9.xf.vLint
MSRs_Lasso.int[,10]<-MSR_fit_plm_imp_pan_sdg10.xf.vLint
MSRs_Lasso.int[,11]<-MSR_fit_plm_imp_pan_sdg11.xf.vLint
MSRs_Lasso.int[,13]<-MSR_fit_plm_imp_pan_sdg13.xf.vLint
MSRs_Lasso.int[,14]<-MSR_fit_plm_imp_pan_sdg14.xf.vLint
MSRs_Lasso.int[,15]<-MSR_fit_plm_imp_pan_sdg15.xf.vLint
MSRs_Lasso.int[,16]<-MSR_fit_plm_imp_pan_sdg16.xf.vLint
MSRs_Lasso.int[,17]<-MSR_fit_plm_imp_pan_sdg17.xf.vLint

MMSRs.tLint<-c(sdg1_train_MMSR_Lasso.int,sdg2_train_MMSR_Lasso.int,sdg3_train_MMSR_Lasso.int,sdg4_train_MMSR_Lasso.int,sdg5_train_MMSR_Lasso.int,sdg6_train_MMSR_Lasso.int,sdg7_train_MMSR_Lasso.int,sdg8_train_MMSR_Lasso.int,sdg9_train_MMSR_Lasso.int
              ,sdg10_train_MMSR_Lasso.int,sdg11_train_MMSR_Lasso.int,sdg13_train_MMSR_Lasso.int,sdg14_train_MMSR_Lasso.int,sdg15_train_MMSR_Lasso.int,sdg16_train_MMSR_Lasso.int,sdg17_train_MMSR_Lasso.int)
MMSRs.tLint.stats<-basicStats(MMSRs.tLint)
MMSRs.tLP-MMSRs.tLint #shows improvement between interaction terms models and pruned lasso var selection in training error


MMSRs.vLint<-c(sdg1_val_MMSR_Lasso.int,sdg2_val_MMSR_Lasso.int,sdg3_val_MMSR_Lasso.int,sdg4_val_MMSR_Lasso.int,sdg5_val_MMSR_Lasso.int,sdg6_val_MMSR_Lasso.int,sdg7_val_MMSR_Lasso.int,sdg8_val_MMSR_Lasso.int,sdg9_val_MMSR_Lasso.int
              ,sdg10_val_MMSR_Lasso.int,sdg11_val_MMSR_Lasso.int,sdg13_val_MMSR_Lasso.int,sdg14_val_MMSR_Lasso.int,sdg15_val_MMSR_Lasso.int,sdg16_val_MMSR_Lasso.int,sdg17_val_MMSR_Lasso.int)
MMSRs.vLint.stats<-basicStats(MMSRs.vLint)
MMSRs.vLP-MMSRs.vLint #shows improvement between interaction terms models and pruned lasso var selection in validation error

AUS_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[1,])
AUT_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[2,])
BEL_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[3,])
CAN_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[4,])
CHE_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[5,])
CHL_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[6,])
CZE_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[7,])
DEU_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[8,])
DNK_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[9,])
ESP_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[10,])
EST_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[11,])
FIN_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[12,])
FRA_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[13,])
GBR_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[14,])
GRC_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[15,])
HUN_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[16,])
IRL_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[17,])
ISL_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[18,])
ISR_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[19,])
ITA_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[20,])
JPN_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[21,])
KOR_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[22,])
LTU_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[23,])
LUX_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[24,])
LVA_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[25,])
MEX_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[26,])
NLD_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[27,])
NOR_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[28,])
NZL_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[29,])
POL_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[30,])
PRT_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[31,])
SVK_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[32,])
SVN_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[33,])
SWE_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[34,])
TUR_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[35,])
USA_val_MMSR_Lasso.int<-mean(MSRs_Lasso.int[36,])


Countries_MSR_Lasso.int.i<-c(AUS_val_MMSR_Lasso.int,AUT_val_MMSR_Lasso.int,BEL_val_MMSR_Lasso.int,CAN_val_MMSR_Lasso.int,CHE_val_MMSR_Lasso.int
                            ,CHL_val_MMSR_Lasso.int,CZE_val_MMSR_Lasso.int,DEU_val_MMSR_Lasso.int,DNK_val_MMSR_Lasso.int,ESP_val_MMSR_Lasso.int
                            ,EST_val_MMSR_Lasso.int,FIN_val_MMSR_Lasso.int,FRA_val_MMSR_Lasso.int,GBR_val_MMSR_Lasso.int,GRC_val_MMSR_Lasso.int
                            ,HUN_val_MMSR_Lasso.int,IRL_val_MMSR_Lasso.int,ISL_val_MMSR_Lasso.int,ISR_val_MMSR_Lasso.int,ITA_val_MMSR_Lasso.int
                            ,JPN_val_MMSR_Lasso.int,KOR_val_MMSR_Lasso.int,LTU_val_MMSR_Lasso.int,LUX_val_MMSR_Lasso.int,LVA_val_MMSR_Lasso.int
                            ,MEX_val_MMSR_Lasso.int,NLD_val_MMSR_Lasso.int,NOR_val_MMSR_Lasso.int,NZL_val_MMSR_Lasso.int,POL_val_MMSR_Lasso.int
                            ,PRT_val_MMSR_Lasso.int,SVK_val_MMSR_Lasso.int,SVN_val_MMSR_Lasso.int,SWE_val_MMSR_Lasso.int,TUR_val_MMSR_Lasso.int
                            ,USA_val_MMSR_Lasso.int)
Countries_MSR_Lasso.int<-data.frame(unique(data.imp$id),Countries_MSR_Lasso.int.i)
Interaction_Country_stats<-basicStats(Countries_MSR_Lasso.int$Countries_MSR_Lasso.int.i)



###########################
##### Advanced Charts #####
###########################

#Sankey

library(plotly)

prefix = "fit_plm_imp_pan_sdg"
suffix = ".int"
colorx=list()

for (h in c(seq(1:11),seq(13,17))){
sdgnum = as.name(h)
#fignum = parse(text=paste("fig",sdgnum, sep=""))

targ_model = parse(text=paste(prefix,sdgnum,suffix, sep=""))
labels = c(colnames(eval(targ_model)$model)[1],names(eval(targ_model)$coefficients))

source = seq(1,length(labels)-1)
target = zeros(length(labels)-1)[1,]

for (y in 1:(length(names(eval(targ_model)$coefficients)))){
  if ((eval(targ_model)$coefficients[y])<0){
    colorx[y] = "red"
    }else{
      colorx[y] = "green"
      source[y] = 0
      target[y] = y
  }
}


fig <- plot_ly(
  type = "sankey",
  domain = list(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".4f",
  
  node = list(
    label = labels,
    color = c("black",t(colorx)),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = source,
    target = target,
    value =  abs(eval(targ_model)$coefficients)
  )
)
fig <- fig %>% layout(
  title = paste("Sankey Diagram", "SDG", sdgnum, suffix,sep = " "),
  font = list(
    size = 10
  )
)

print(fig)
}

# Chord Diagrams
library(circlize)
library(reshape2)

col.pal = c(SDG1 = rgb(229,36,59,alpha=200,maxColorValue=255), SDG2 = rgb(221,166,58,alpha=200,maxColorValue=255), SDG3 = rgb(76,159,56,alpha=200,maxColorValue=255),
            SDG4 = rgb(197,25,45,alpha=200,maxColorValue=255), SDG5 = rgb(255,38,33,alpha=200,maxColorValue=255), SDG6 = rgb(38,189,226,alpha=200,maxColorValue=255),
            SDG7 = rgb(252,195,11,alpha=200,maxColorValue=255), SDG8 = rgb(162,25,66,alpha=200,maxColorValue=255), SDG9 = rgb(253,105,37,alpha=200,maxColorValue=255),
            SDG10  = rgb(221,19,103,alpha=200,maxColorValue=255), SDG11 = rgb(253,157,36,alpha=200,maxColorValue=255), SDG12 = rgb(191,139,46,alpha=200,maxColorValue=255),
            SDG13 = rgb(63,126,68,alpha=200,maxColorValue=255), SDG14 = rgb(10,151,217,alpha=200,maxColorValue=255), SDG15 = rgb(86,192,43,alpha=200,maxColorValue=255),
            SDG16 = rgb(0,104,157,alpha=200,maxColorValue=255), SDG17 = rgb(25,72,106,alpha=200,maxColorValue=255))

par(par.defaults)

testeroo.finale<-zeros(length(data.pan.final.xf)-2,17)

prefix2 = prefix
suffix2 = ".xfLP2"

for (t in c(seq(1:11),seq(13,17))){
  sdgnum2 = as.name(t)
  targ_model2 = parse(text=paste(prefix2,sdgnum2,suffix2, sep=""))
  
  testeroo<-matrix(0,nrow=length(data.pan.final.xf)-2,dimnames = list(names(data.pan.final.xf[-1:-2]),names(eval(targ_model2)$model[1])))
  testerooni<-matrix(eval(targ_model2)$coefficients[-1],dimnames = list(names(eval(targ_model2)$coefficients[-1]),names(eval(targ_model2)$model[1])))
  testeroo2<-acast(rbind(melt(testeroo),melt(testerooni)),Var1~Var2,sum)
  testeroo.finale[,t]<-testeroo2
}

SDG_list = c("SDG1","SDG2","SDG3","SDG4","SDG5","SDG6","SDG7","SDG8","SDG9","SDG10","SDG11","SDG12","SDG13",
             "SDG14","SDG15","SDG16","SDG17")

all_SGDs_Chord = testeroo.finale
dimnames(all_SGDs_Chord) = list(names(data.pan.final.xf[-1:-2]),SDG_list)
all_SGDs_Chord.kewk<-matrix(nrow=17,ncol=17, dimnames=list(SDG_list,SDG_list))

for (u in 1:17){
  for (q in 1:17){
    sdgnum3<-as.name(u)
    targsdg<-paste("^(sdg",sdgnum3,"_)",sep="")
    targ_rows<-grep(targsdg, rownames(all_SGDs_Chord))
    all_SGDs_Chord.kewk[u,q]<-sum(abs(all_SGDs_Chord[targ_rows,q]))
  }
}
#chordDiagram(SDG1_extrapruned,grid.col = col.pal)
#library(devEMF)
#emf(filename="chord diagram extra pruned.emf")#,width = 3840, height = 2160)
png(filename="chord diagram extra pruned.png", res=300, width = 2160, height = 2160)
chordDiagram(all_SGDs_Chord.kewk,grid.col = col.pal)
title(main = "SDG Interactions from Random Effects Extra Pruned Lasso Models")
dev.off()

# Bar Charts
prefix3 = prefix
suffix3 = ".xfL"
if (suffix3==".xf"){
  suffdisp = "Handpicked Variables Model"
}
if(suffix3==".xfL"){
  suffdisp = "Simple Lasso Model"
}
if (suffix3==".xfLP"){
  suffdisp = "Pruned Lasso Model"
}
if (suffix3==".xfLP2"){
  suffdisp = "Extra Pruned Lasso Model"
}
if (suffix3==".int"){
  suffdisp = "Interaction Terms Model"
}

par(mar=c(11,4,4,2)+0.2)

for (p in c(seq(1:11),seq(13,17))){
  sdgnum3 = as.name(p)
  targ_model3<-parse(text=paste(prefix3,sdgnum3,suffix3, sep=""))
  summary.x<-summary(eval(targ_model3))
  pval<-summary.x[["coefficients"]][,"Pr(>|z|)"] # to remove intercept, [-1,"Pr(>|z|)"]
  width = pval
  width[pval<0.001]=8
  width[pval<0.01&pval>0.001]=6
  width[pval<0.05&pval>0.01]=4
  width[pval<0.1&pval>0.05]=2
  width[pval>0.1]=1
  coeff<-summary.x[["coefficients"]][,"Estimate"] # to remove intercept, [-1,"Estimate"]
  bp<-barplot(coeff,width = width, axisnames = T, #names.arg = names(eval(targ_model3)$coefficients),
          col = "navy", space = 1, ann = T, las = 2,
          axes = T,ylab = "Coefficient", ylim = c(-0.4,1),
          main=paste("Relationships from SDG",sdgnum3,suffdisp,sep=" "))
  abline(h=0)
}
#abline(fit_plm_imp_pan_sdg1.xf)


#######################
##### Stats Tests #####
#######################

#Granger (non-)Causality test
pgrangertest(sdg13_co2kgPerGDPPPP ~ sdg7_ren, data.pan.final.xf, test = c("Ztilde","Zbar","Wbar"), order = 1L)
pgrangertest(sdg13_co2kgPerGDPPPP ~ GDP_PPP, data.pan.final.xf, test = c("Ztilde","Zbar","Wbar"), order = 1L)
pgrangertest(sdg13_co2kgPerGDPPPP ~ sdg10_palma, data.pan.final.xf, test = c("Ztilde","Zbar","Wbar"), order = 1L)
RorF1.int
RorF2.int
RorF3.int

#############################################
# ############ Random Forest ################
# ###########################################
# data.tree<- data.poly
# 
# library(caTools)
# set.seed(8937)
# TrainYears = seq(1,20)
# Split = which(data.tree$Year %in% TrainYears)
# ValYears = seq(21,28)
# NotSplit = which(data.tree$Year %in% ValYears)
# 
# 
# #### Make data into Matrices ####
# targets<- grep("^(Unemployment|CO2kgPerGDPPPP|HomicideRate)", names(data.tree))
# trainX = data.matrix(data.tree[Split,-targets],rownames.force = NA)
# trainY = data.matrix(data.tree[Split,targets],rownames.force = NA)
# n_tree = 11
# m_feature = 4
# min_leaf = 2
# testX = data.matrix(data.tree[NotSplit,-targets],rownames.force = NA)
# 
# 
# #Multi-Target Tree
# library(MultivariateRandomForest)
# library(IntegratedMRF)
# 
# 
# Inv_Cov_Y = solve(cov(trainY))# Check that this is correct
# ff2 = ncol(trainX)
# ff = sort(sample(ff2,m_feature))
# Index = 1:nrow(trainX)
# 
# 
# #Random Forest
# set.seed(8937)
# Prediction = build_forest_predict(trainX, trainY, n_tree, m_feature, min_leaf, testX)
# #Single tree
# Prediction2 = build_single_tree(trainX, trainY, m_feature, min_leaf, Inv_Cov_Y, 2)
# 
# 
# ###### Error Calcs ######
# SE = (data.tree[NotSplit,targets]-Prediction)^2
# MSE = colMeans(SE)
# MSE #Mean Squared Error
# RMSE = sqrt(MSE) #root mean squared error
# 
# #Variable Importance
# theta <- function(trainX){trainX}
# results <- bootstrap::bootstrap(1:nrow(trainX),n_tree,theta)
# b=results$thetastar
# Variable_number=ncol(trainY)
# if (Variable_number>1){
#   Command=2
# }else if(Variable_number==1){
#   Command=1
# }
# NumVariable=ncol(trainX)
# NumRepeatation=matrix(rep(0,n_tree*NumVariable),nrow=n_tree)
# for (i in 1:n_tree){
#   Single_Model=NULL
#   X=trainX[ b[ ,i], ]
#   Y=matrix(trainY[ b[ ,i], ],ncol=Variable_number)
#   Inv_Cov_Y = solve(cov(Y)) # calculate the V inverse
#   if (Command==1){
#     Inv_Cov_Y=matrix(rep(0,4),ncol=2)
#   }
#   Single_Model=build_single_tree(X, Y, m_feature, min_leaf,Inv_Cov_Y,Command)
#   NumRepeatation[i,]=variable_importance_measure(Single_Model,NumVariable)
# }
# Var_Imp = colSums(NumRepeatation)
# Var_Imp2 = cbind(names(data.tree[,-targets]),Var_Imp)
# #Var_Imp3 = sort(Var_Imp2, decreasing=TRUE)
# View(Var_Imp2)
# #Countries to be done in chunks? Western Europe vs Eastern Europe, etc.
# #Too discrete to have each country code
# #Try using Year as a numerical variable
# #Work on models on US data, then apply to larger data set