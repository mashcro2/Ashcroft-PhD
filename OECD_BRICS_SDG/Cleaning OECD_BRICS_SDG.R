#Cleaning data from the world bank SDG database for OECD + BRICs countries
setwd("C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/OECD_BRICS_SDG")

#library(readxl)
data_OECD<-read.csv("OECD_BRICS_SDG_Test.csv", header=TRUE, na.strings=c("..","NA","","?")) #Everything coming in as factor vars

#Transpose. Not needed if excessive preprocessing has been done in Excel.
#data.T<-data.table::transpose(data)

library(Amelia)
missmap(data_OECD)
summary(data_OECD)
data<-data_OECD
#problem: the variable names are too long and there are too many. Have to narrow down significantly
#problem: there are too many missing values in the selected variables.
#Eliminate variables with more than 50% missing values

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

#Change Year to a factor variable#
data$Year = as.factor(data$Year)

#Get rid of 2019
a = nrow(data)
b = which.max(data$Year)
no2019 = seq(-b, -a, by=-b)
data_no2019 <- data[no2019,]
missmap(data_no2019[variables==F]) #7% missing, down from 9%

#Get rid of 2018
c = which.max(data$Year)-1
no2018 = seq(-c, -a, by=-b)
data_no2018_19 <- data[c(no2018,no2019),]
missmap(data_no2018_19[variables==F]) #6% missing still, down from 7%
#sum(is.na(data))

#Get rid of 2017
# d = which.max(data$Year)-2
# no2017 = seq(-d, -a, by=-b)
# data_no2017_18_19 <- data[c(no2017,no2018,no2019),]
# missmap(data_no2017_18_19[variables==F]) #6% missing still, not worth removing



#####################
##### Imputation ####
#####################

### This imputation method does not take into account the time trends or the separate countries. Should definitely be changed ###

data_no2018_19<- data_no2018_19[variables==F]
data_no2018_19<- data_no2018_19[,-2] #Take out Country name. Country code remains

indx<-sapply(data_no2018_19, is.numeric)
index.cat<-sapply(data_no2018_19, is.factor)

# Nominal Input: By Mode #
impute <- function(x,y) {
  x[is.na(x)]<-y # if the value of x is missing, it will replace it with y
  x
}

#compute the mode (no built in function)
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))] #tabulate calculates frequency of each level
}

Mode<-sapply(data_no2018_19[index.cat], mode) #index.cat contains only nominal (categorical) inputs
data.imp<-data_no2018_19
data.imp[index.cat]<-as.data.frame(mapply(impute,x=data.imp[index.cat],y=Mode)) #for each nominal input, impute using each Mode column and force into a data.frame

# Numeric Input: By Mean #
means <- function(x) { mean(x, na.rm = TRUE) } #na.rm = TRUE removes missing values
Mean<-sapply(data.imp[indx],means) #indx contains only numeric inputs

data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))

missmap(data.imp, main="Missing Map")

#split#
data.tree<-data.imp

library(caTools)
set.seed(8937)
TrainYears = seq(1990,2010)
Split = which(data.tree$Year %in% TrainYears)
ValYears = seq(2011,2017)
NotSplit = which(data.tree$Year %in% ValYears)
#data.train = data.tree[Year=TrainYears]
#data.valid = data.tree[Year=ValYears]

#testY = as.matrix(data.tree[!split,targets])

#########################
##### Transformtion #####
#########################

# numeric input xform
library(caret)
TransformParams <- preProcess(data.tree[Split,indx], method=c("YeoJohnson"))
TransformParams$yj
sort(abs(TransformParams$yj), decreasing = F)

#Apply the transformations to the appropriate variables according to the literature (close to 0)
vars.xf <- grep("^(AirPassenger|NRGNewConsume|IndustryEmploy)", names(data.tree))
data.tree[vars.xf]<-log(data.tree[,vars.xf]+1)

# nominal input xform
# none


## Standardization: numeric inputs
library(caret)
ScaleParams <- preProcess(data.tree[Split,indx], method=c("center", "scale")) #scale all or just training?
data.tree[indx]<-predict(ScaleParams, data.tree[indx])
sd(data.tree$NRGrenewConsume)
mean(data.tree$NRGrenewConsume) #Proper values when tested on just training data
#Do scaling by country
#Try modeling with and without transformation and compare
#Without transformation helps interpretation

#######################
#### Random Forest ####
#######################

#### Make data into Matrices ####
targets<- grep("^(Unemployment|CO2kgPerGDPPPP|HomicideRate)", names(data.tree))
trainX = data.matrix(data.tree[Split,-targets],rownames.force = NA)
trainY = data.matrix(data.tree[Split,targets],rownames.force = NA)
n_tree = 11
m_feature = 4
min_leaf = 2
testX = data.matrix(data.tree[NotSplit,-targets],rownames.force = NA)


#Multi-Target Tree
library(MultivariateRandomForest)
library(IntegratedMRF)


Inv_Cov_Y = solve(cov(trainY))# Check that this is correct
ff2 = ncol(trainX)
ff = sort(sample(ff2,m_feature))
Index = 1:nrow(trainX)


#Random Forest
set.seed(8937)
Prediction = build_forest_predict(trainX, trainY, n_tree, m_feature, min_leaf, testX)
#Single tree
#Prediction2 = build_single_tree(trainX, trainY, m_feature, min_leaf, Inv_Cov_Y, 2)


###### Error Calcs ######
SE = (data.tree[NotSplit,targets]-Prediction)^2
MSE = colMeans(SE)
MSE #Mean Squared Error
RMSE = sqrt(MSE) #root mean squared error

#Variable Importance
theta <- function(trainX){trainX}
results <- bootstrap::bootstrap(1:nrow(trainX),n_tree,theta)
b=results$thetastar
Variable_number=ncol(trainY)
if (Variable_number>1){
  Command=2
}else if(Variable_number==1){
  Command=1
}
NumVariable=ncol(trainX)
NumRepeatation=matrix(rep(0,n_tree*NumVariable),nrow=n_tree)
for (i in 1:n_tree){
  Single_Model=NULL
  X=trainX[ b[ ,i], ]
  Y=matrix(trainY[ b[ ,i], ],ncol=Variable_number)
  Inv_Cov_Y = solve(cov(Y)) # calculate the V inverse
  if (Command==1){
    Inv_Cov_Y=matrix(rep(0,4),ncol=2)
  }
  Single_Model=build_single_tree(X, Y, m_feature, min_leaf,Inv_Cov_Y,Command)
  NumRepeatation[i,]=variable_importance_measure(Single_Model,NumVariable)
}
Var_Imp = colSums(NumRepeatation)
Var_Imp2 = cbind(names(data.tree[,-targets]),Var_Imp)
#Var_Imp3 = sort(Var_Imp2, decreasing=TRUE)
View(Var_Imp2)
#Countries to be done in chunks? Western Europe vs Eastern Europe, etc.
#Too discrete to have each country code
#Try using Year as a numerical variable
#Work on models on US data, then apply to larger data set


#############################################
####### Simple Model to Test Error ##########
#############################################

# Multiple Linear Regression
fit1 <- lm(IndValAddPerWorker ~ IndustryEmploy + ManufValAdd + Unemployment, data=data.tree)
fit2 <- lm(IndValAddPerWorker ~ MortUnder5 + HomicideRate + NRGIntensityMJ, data=data.tree) #designed to be bad
fit3 <- lm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + PrivateConsump + IndustryEmploy + WomenParliament, data=data.tree)

fit1t <- lm(IndValAddPerWorker ~ IndustryEmploy + ManufValAdd + Unemployment, Year, data=data.tree)
fit2t <- lm(IndValAddPerWorker ~ MortUnder5 + HomicideRate + NRGIntensityMJ, Year, data=data.tree) #designed to be bad
fit3t <- lm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + PrivateConsump + IndustryEmploy + WomenParliament, Year, data=data.tree)
#summary(fit) # show results
#fitted(fit)
#plot(fit)
SE_simple1 = (fitted(fit1)-data.tree$IndValAddPerWorker)^2
SE_simple2 = (fitted(fit2)-data.tree$IndValAddPerWorker)^2
SE_simple3 = (fitted(fit3)-data.tree$CO2kgPerGDPPPP)^2
SE_simple1t = (fitted(fit1t)-data.tree$IndValAddPerWorker)^2
SE_simple2t = (fitted(fit2t)-data.tree$IndValAddPerWorker)^2
SE_simple3t = (fitted(fit3t)-data.tree$CO2kgPerGDPPPP)^2

MSE_simple1 = mean(SE_simple1)
RMSE_simple1 = MSE_simple1^0.5
RMSE_simple1
MSE_simple2 = mean(SE_simple2)
RMSE_simple2 = MSE_simple2^0.5
RMSE_simple2
MSE_simple3 = mean(SE_simple3)
RMSE_simple3 = MSE_simple3^0.5
RMSE_simple3
MSE_simple1t = mean(SE_simple1t)
RMSE_simple1t = MSE_simple1t^0.5
RMSE_simple1t
MSE_simple2t = mean(SE_simple2t)
RMSE_simple2t = MSE_simple2t^0.5
RMSE_simple2t
MSE_simple3t = mean(SE_simple3t)
RMSE_simple3t = MSE_simple3t^0.5
RMSE_simple3t

#All errors are bad. This is probably due to the many different countries. Try just one country
fit4 <- lm(IndValAddPerWorker ~ IndustryEmploy + ManufValAdd + Unemployment, data=data.tree[data.tree$Country.Code=='CAN',])
SE_simple4 = (fitted(fit4)-data.tree$IndValAddPerWorker)^2
MSE_simple4 = mean(SE_simple4)
RMSE_simple4 = MSE_simple4^0.5
RMSE_simple4
#Error still bad. Could jsut be not much correlation in these variables
#Could also be that the imputation is ruining trends because the imputation is based on the averages from the whole set

#####################
#### Time Series ####
#####################

data.ts <- data.tree
data.ts1 <- ts(data.ts, frequency=1)
dev.off()
plot(data$Year,data$CO2kgPerGDPPPP)
decomposedRes <- decompose(data.ts,type="mult") #not working, data not periodic? Yearly is not periodic

data.ts.co2<-ts(data.tree$CO2kgPerGDPPPP)

laggedTS <-stats::lag(data.ts1)
# laggedCO2TS1 <-stats::lag(data.ts.co2,1)
# laggedCO2TS2 <-stats::lag(data.ts.co2,2)
# laggedCO2TS3 <-stats::lag(data.ts.co2,3)
# laggedCO2TS4 <-stats::lag(data.ts.co2,4)
# laggedCO2TS5 <-stats::lag(data.ts.co2,5)
library(DataCombine)
CO2DF <-as.data.frame(data.ts.co2)
CO2DF <- slide(CO2DF,"x",NewVar="CO2lead1",slideBy=1)
CO2DF <- slide(CO2DF,"x",NewVar="CO2lead2",slideBy=2)
CO2DF <- slide(CO2DF,"x",NewVar="CO2lead3",slideBy=3)
CO2DF <- slide(CO2DF,"x",NewVar="CO2lead4",slideBy=4)
CO2DF <- slide(CO2DF,"x",NewVar="CO2lead5",slideBy=5)
CO2DF
acfCO2 <- acf(data.ts.co2) #autocorrelation
pacfCO2 <- pacf(data.ts.co2) #partial autocorrelation

#Time series regression with tslm
library(forecast)
fittslm3 <- tslm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + PrivateConsump + IndustryEmploy + WomenParliament, data=data.ts1)
SE_simple3ts = (fitted(fittslm3)-data.tree$CO2kgPerGDPPPP)^2
MSE_simple3ts = mean(SE_simple3ts)
RMSE_simple3ts = MSE_simple3ts^0.5
RMSE_simple3ts
RMSE_simple3 #Why are these the same?? Look into tslm assumptions. Assume independence or correlation?
RMSE_simple3t


# ARX model practice
#https://cran.r-project.org/web/packages/gets/gets.pdf
library(gets)
y = data.ts.co2
#ts_target = grep("^(CO2MTPerCap)", names(data.ts))# grep doesn't work on time series data type
names(as.data.frame(data.ts1))
ts_non_target = c(-1,-15) #15 is the index of CO2
xregsfull = data.ts1[,ts_non_target]
xregsselect = data.ts1[,c(2,7,8,9,17,6)] #Country.Code, NRGrenewConsume, NRGIntensityMJ, GDPCapGrowth, PrivateConsump, WomenParliament
#Country code is a numerical value here. Should ideally be factor
summary(xregsselect)
head(xregsselect)

arx_model_full = arx(y,mc=FALSE, ar=NULL, ewma=NULL, mxreg=xregsfull, vc=FALSE,
arch=NULL, asym=NULL, log.ewma=NULL, vxreg=NULL, zero.adj=0.1,
vc.adj=TRUE, vcov.type=c("ordinary", "white", "newey-west"),
qstat.options=NULL, normality.JarqueB=FALSE, user.estimator=NULL,
user.diagnostics=NULL, tol=1e-07, LAPACK=FALSE, plot=NULL)
#Can't use more variables than there are observations

arx_model_select = arx(y,mxreg=xregsselect)
#Now try with partitioning to check accuracy
#Look at statistics (t, p) to determine significance
TrainYears2 = seq(1,round(0.7*(max(data_OECD$Year)-2-min(data_OECD$Year))))
num_countries = length(data.tree$Country.Code)/28 #28 years w/o 2018 and 2019, 42 countries

train_rows = which(data.ts1[,1] %in% TrainYears2)

y_train = y[train_rows] #training targets
xregs_train = xregsselect[train_rows,] #training inputs
arx_model_select_tr = arx(y_train,mxreg=xregs_train)

#Compute error of ARX model
ValYears2 = seq(max(TrainYears2)+1,(max(data.ts1[,1])),by=1) #Years used for validation (2009-2017)
val_rows = which(data.ts1[,1] %in% ValYears2)
y_val = y[val_rows]
x_val = xregsselect[val_rows,]
y_predict = x_val%*%coef.arx(arx_model_select_tr)

SE_arx = (y_predict - y_val)^2
MSE_arx = mean(SE_arx) #pretty bad
RMSE_arx = MSE_arx^0.5 #pretty bad

#still need to change imputation method
#Is there a way to use country code as a factor? Should a different model be used for each  country?
#Create a discrete predictor for chunks of countries (Western Europe, NA, Eastern Europe, etc.)
#Combine them for individual model fidelity with the advantage of more data overall

