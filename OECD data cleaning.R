#OECD Data Dr. Nat cleaning#

#Data from the world bank SDG database OECD
setwd("C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/OECD_BRICS_SDG")

#library(readxl)
data_OECD<-read.csv("OECD_BRICS_SDG_Test.csv", header=TRUE, na.strings=c("..","NA","","?")) #Everything coming in as factor vars

par.defaults <- par(no.readonly=TRUE)
save(par.defaults, file="R.default.par.RData")

library(Amelia)
missmap(data_OECD)
summary(data_OECD)
data<-data_OECD
#problem: the variable names are too long and there are too many. Have to narrow down significantly
#problem: there are too many missing values in the selected variables.
#Eliminate variables with more than 25% missing values

vars<- is.na(data)
sum(vars[1,])
sum(vars[,15])
sum(vars)
length(data)
variables<-logical()
names(data)
threshold<-length(data$Country.Name)/4

for (variable in names(data))
{
  if (sum(vars[,variable])>(threshold)){
    variables[variable]=T
  }else{
    variables[variable]=F
  }
}
print(unname(variables))#the list of whether or not each variable has more (T) or less (F) than 50% missing values
variables

missmap(data[variables==F])
str(data[variables==F])
summary(data[variables==F])

### Sorted list of variables by number of missing values ###
list<-sapply(data[variables==F], is.na, USE.NAMES = T)
list<-as.data.frame(list)
list2<-colSums(list, na.rm=F,dims=1)
dim(list)
sort(list2)
list3<-100*list2/length(data$Country.Name)
sort(list3)

length(list3)

##### Rename Data #####
library(gdata)
library(dplyr)


data<- data %>% rename(MortNeonatal = Mortality.rate..neonatal..per.1.000.live.births.)
data<- data %>% rename(MortUnder5 = Mortality.rate..under.5..per.1.000.live.births.)
data<- data %>% rename(PrePrimaryEnroll = School.enrollment..preprimary....gross.)
data<- data %>% rename(GDPCapGrowth = GDP.per.capita.growth..annual...)
data<- data %>% rename(IndValAddPerWorker = Industry..including.construction...value.added.per.worker..constant.2010.US..)
data<- data %>% rename(Unemployment = Unemployment..total....of.total.labor.force...modeled.ILO.estimate.)
data<- data %>% rename(AirPassenger = Air.transport..passengers.carried)
data<- data %>% rename(ManufValAdd = Manufacturing..value.added....of.GDP.)
data<- data %>% rename(IndustryEmploy = Employment.in.industry....of.total.employment...modeled.ILO.estimate.)
data<- data %>% rename(CO2kgPerGDPPPP = CO2.emissions..kg.per.PPP...of.GDP.)
data<- data %>% rename(NRGIntensityMJ = Energy.intensity.level.of.primary.energy..MJ..2011.PPP.GDP.)
data<- data %>% rename(NRGrenewConsume = Renewable.energy.consumption....of.total.final.energy.consumption.)
data<- data %>% rename(PersonalRemit = Personal.remittances..received....of.GDP.)
data<- data %>% rename(PrivateConsump = PPP.conversion.factor..private.consumption..LCU.per.international...)
data<- data %>% rename(WomenParliament = Proportion.of.seats.held.by.women.in.national.parliaments....)
data<- data %>% rename(FisheryProductMT = Total.fisheries.production..metric.tons.)
data<- data %>% rename(HomicideRate = Intentional.homicides..per.100.000.people.)
data<- data %>% rename(TaxRevenuePercGDP = Tax.revenue....of.GDP.)
print(names(data[variables==F]))

#Get rid of 2019
a = nrow(data)
b = which.max(data$Year)
no2019 = seq(-b, -a, by=-b)
data_no2019 <- data[no2019,]
missmap(data_no2019[variables==F]) #7% missing, down from 9%

#Get rid of 2018
a2 = nrow(data)
b2 = which.max(data$Year)-1
no2018 = seq(-b2, -a2, by=-b)
data_no2018_19 <- data[c(no2018,no2019),]
missmap(data_no2018_19[variables==F])
#View(data_no2018_19[variables==F])

#####################
##### Imputation ####
#####################

data_no2018_19<- data[c(no2018,no2019),variables==F]
data_no2018_19<- data_no2018_19[,-2] #remove country name. Country code remains

indx<-sapply(data_no2018_19, is.numeric)
indx2<-colSums(is.na(data_no2018_19))>0
indx2[1]<-T
indx3<-which(colSums(is.na(data_no2018_19))>0)
index.cat<-sapply(data_no2018_19, is.factor)


data.imp<-data_no2018_19
#data.imp[index.cat]<-as.data.frame(mapply(impute,x=data.imp[index.cat],y=Mode)) #for each nominal input, impute using each Mode column and force into a data.frame

# Numeric Input: By Mean #
means <- function(x) { mean(x, na.rm = TRUE) } #na.rm = TRUE removes missing values
Mean<-sapply(data.imp[indx],means) #indx contains only numeric inputs

#data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))

###################################################################################
Country_length = length(seq(min(data.imp$Year),max(data.imp$Year))) #How many years for each country
Country_start = 1
polycomp <- function(x,period) {lm(x ~ poly(Year,1,raw=T), data.imp[period,])}#,na.rm=T)}
indx4<-c(7,8,12,15) #indices of input vars NRGrenewConsume, NRGIntensityMJ, AirPassenger, CO2kgPerGDPPPP
Country_inx<-c(1:4,7:18,21:24,26:28,30:32,34:37,40:42)
#Country_inx<-1:42
for (n in Country_inx){
  period = seq(n*Country_length-(Country_length-1),n*Country_length)
  poly<- sapply(data.imp[period,indx4],polycomp,period=period)
  #country_include[n]<-data.imp$Country.Code[max(period)]
  for (i in indx4){
    name<-names(data.imp)[i]
    for (j in period){
      if (is.na(data.imp[j,i])==T){
        string1<-as.name(paste('poly$',name,'$coefficients[1]',sep=""))
        string2<-as.name(paste('poly$',name,'$coefficients[2]',sep=""))
        polyimp <- function(x1,x2) {(x1)+(x2)*data.imp$Year[j]}
        data.imp[j,i]<-polyimp(eval(parse(text=string1)),eval(parse(text=string2)))
      }
    }
  }
}

not_included<-c(5,6,19,20,25,29,33,38,39)
not_included2<-matrix(nrow=length(not_included),ncol=Country_length)
for (t in 1:length(not_included)){
  not_included2[t,]<-seq(not_included[t]*Country_length-(Country_length-1),not_included[t]*Country_length)
}
row_select<-seq(1,length(data.imp$Year)) %in% not_included2
data.imp.select<-data.imp[!row_select,]

missmap(data.imp.select[,indx4], main="Missing Map")
load("R.default.par.RData")
par(par.defaults)

library(caret)
ggplot(data.imp.select, aes(Year, NRGrenewConsume) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))

#Dr. Nat's Idea#
#Low order polynomial fit of input and output variables against time
#Save the coefficients of the polynomial fits. Inputs go into input matrix. Target goes into output matrix
#Coefficients hold information about the time varying nature of the inputs and target
#Use a neural network with input and output matrices to make predictions
#Model will not be interpretable, hopefully it will be accurate. Tradeoff we are willing to make.
#Multiple inputs and outputs could be used
data.poly<-data.imp.select
summary(data.poly$Year)
data.poly$Year<-(data.poly$Year)-1989
indx[1]<-F #Set year to not be treated as a numerical variable for the purpose of scaling

indx5 <- c(7,8,12,15)
# numeric input xform
TransformParams <- preProcess(data.poly[,indx5], method=c("YeoJohnson"))
TransformParams$yj
sort(abs(TransformParams$yj), decreasing = F)

#Apply the transformations to the appropriate variables according to the literature (close to 0). https://www.stat.umn.edu/arc/yjpower.pdf
#data.poly$CO2kgPerGDPPPP<- -(((-data.poly$CO2kgPerGDPPPP+1)^(2-TransformParams$yj[4]))-1)/(2-TransformParams$yj[4])
data.poly$CO2kgPerGDPPPP<- (((data.poly$CO2kgPerGDPPPP+1)^TransformParams$yj[4])-1)/TransformParams$yj[4]
data.poly[,c(7,8,12)] <- log(data.poly[,c(7,8,12)]+1)

#Scaling #Right now being done on whole set. Should only scale based on training parameters.
ScaleParams <- preProcess(data.poly[,indx], method=c("range"), rangeBounds = c(-0.75,0.75)) #scale only based on training data means, std devs.
#ScaleParams <- preProcess(data.poly[,indx], method=c("center","scale"))
data.poly[indx]<-predict(ScaleParams, data.poly[indx])

###########################################################################################
k_folds<-7
#Year_set<-seq(1,28)
num_countries<-length(data.poly$Year)/Country_length
A_train<-array(dim=c(num_countries,6,7))
b_train<-array(dim=c(num_countries,2,7))
A_val<-array(dim=c(num_countries,6,7))
b_val<-array(dim=c(num_countries,2,7))

for (m in seq(1,num_countries)){
  Year_set<-seq(m*Country_length-(Country_length-1),m*Country_length)
  for (k in seq(1,k_folds)){
    valYears<-seq(min(Year_set)+4*k-4,min(Year_set)-1+4*k,by=1) #validation years
    trainYears<-subset(Year_set, !(Year_set %in% valYears)) #subset of the available years not including the ones used for validation
    
    #Generating ployfits. Change number within poly to change order. 1=linear, 2=quadratic, etc.
    CO2fitTrain<-lm(CO2kgPerGDPPPP ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
    NRGConsumpfitTrain<-lm(NRGIntensityMJ ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
    RenewablefitTrain<-lm(NRGrenewConsume ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
    #GDPGrowthfitTrain<-lm(GDPCapGrowth ~ poly(Year, 1, raw=T), data = data.poly[trainYears,]) #Not a good trend. All over the place. Could use 3-year avg for this one
    APfitTrain<-lm(AirPassenger ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
    
    CO2fitVal<-lm(CO2kgPerGDPPPP ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
    NRGConsumpfitVal<-lm(NRGIntensityMJ ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
    RenewablefitVal<-lm(NRGrenewConsume ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
    #GDPGrowthfitVal<-lm(GDPCapGrowth ~ poly(Year, 1, raw=T), data = data.poly[valYears,]) #Not a good trend. All over the place. Could use 3-year avg for this one
    APfitVal<-lm(AirPassenger ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
    
    #Extracting training coefficients
    CO2coeffT<-CO2fitTrain$coefficients #potentially need to unname these
    NRGConsumpcoeffT<-NRGConsumpfitTrain$coefficients
    RenewablecoeffT<-RenewablefitTrain$coefficients
    #GDPGrowthcoeffT<-GDPGrowthfitTrain$coefficients
    APcoeffT<-APfitTrain$coefficients
    
    #Extracting validation coefficients
    CO2coeffV<-CO2fitVal$coefficients #potentially need to unname these
    NRGConsumpcoeffV<-NRGConsumpfitVal$coefficients
    RenewablecoeffV<-RenewablefitVal$coefficients
    #GDPGrowthcoeffV<-GDPGrowthfitVal$coefficients
    APcoeffV<-APfitVal$coefficients
    
    #placing coefficients in training and validation arrays
    A_train[m,,k]<-t(c(NRGConsumpcoeffT,RenewablecoeffT,APcoeffT)) #Training input vector. Took out GDPGrowthcoeffT
    b_train[m,,k]<-t(c(CO2coeffT)) #Training Output vector
    
    A_val[m,,k]<-t(c(NRGConsumpcoeffV,RenewablecoeffV,APcoeffV)) #Validation input vector. Took out GDPGrowthcoeffV
    b_val[m,,k]<-t(c(CO2coeffV)) #Validation Output vector
  }       
}


ggplot(data.poly, aes(x=Year, y=CO2kgPerGDPPPP, colour=Country.Code)) +
  geom_line() +
  xlab("Year")+
  ylab("kg CO2 per GDP PPP")+
  ggtitle("OECD CO2 trends")


write.csv(data.poly,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/imputed_OECD_data.csv", row.names = FALSE)
write.csv(A_train,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/A_train.csv", row.names = FALSE)
write.csv(A_val,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/A_val.csv", row.names = FALSE)
write.csv(b_train,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/b_train.csv", row.names = FALSE)
write.csv(b_val,"C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/b_val.csv", row.names = FALSE)