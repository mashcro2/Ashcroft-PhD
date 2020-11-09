#First cut at cleaning data from sdg.data.gov
#2/13/2020
setwd("C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data/US SDG Data")

library(readxl)
data<-read_xlsx("Combined.xlsx", col_names=TRUE, na=c("..", "NA", "", "?"))


library(Amelia)
missmap(data)
summary(data)

#Eliminate variables with more than 50% missing values
vars<- is.na(data)
#sum(vars[1,])
#sum(vars[,15])
sum(vars)
length(data)
variables<-logical()
names(data)
threshold<-length(data$Year)/2

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
list3<-100*list2/length(data$Year)
sort(list3)

#Rename to shorten names
library(gdata)
library(dplyr)

data<- data %>% rename(PercBelowUSPov = `Percent of US population living below the US poverty line`)
data<- data %>% rename(PercHouseholdsFoodInsec = `Percent of households with food insecurity in the US`)
data<- data %>% rename(Perc05Stunting = `Percent of US children ages 0 to 5 experiencing stunting`)
data<- data %>% rename(Perc05Wasting = `Percent of children ages 0 to 5 years with wasting (very low weight-for-length/height) in the US`)
data<- data %>% rename(Perc4thgrdReading = `Percentage 4th grade students at or above "basic" proficiency in reading`)
data<- data %>% rename(Perc5yrEnroll = `Percentage of 5 years olds enrolled in prekindergarten, kindergarten, or first or higher grade in the US\r\n`)
data<- data %>% rename(PercWomenCongress = `Percent of Women in the US Congress`)
data<- data %>% rename(PercWomenManage = `Percent of US women ages 16 and older in full-time, civilian management occupations`)
data<- data %>% rename(PercCleanWater = `Percent of US population that receives drinking water from community water systems in compliance with drinking water standards`)
data<- data %>% rename(PercManuEmploy = `Percent of US manufacturing in total employment`)
data<- data %>% rename(PercSlums = `Percentage of occupied housing units in US urban areas that are severely inadequate`)
data<- data %>% rename(PercSustFishStock = `Percentage of US fish stocks at a sustainable level`)
data<- data %>% rename(PercUnsentenced = `Percent unsentenced detainees of inmates held in US state and federal prisons and local jails`)
data<- data %>% rename(PercMarineProtected = `Percent of U.S. marine waters in a natural or cultural heritage marine protected area`)
data<- data %>% rename(PerceVioCrimeReport = `Percent of US violent victimizations reported to the police`)
data<- data %>% rename(PercWalkSafe = `Percent of persons ages 18 and older who report feeling safe walking near their home`)
data<- data %>% rename(HasDisasterStrat = `Has the US established national and local disaster risk reduction strategies?`)
data<- data %>% rename(HasClimateFoodPlan = `Has the US established a plan to improve the nation's ability to adapt to climate change in a manner that does not adversely affect food production?`)
data<- data %>% rename(HasCapacityPrograms = `Has the US implemented programs to build capacity to implement adaptation, mitigation, technology transfer and development related to climate events?`)
data<- data %>% rename(Mortality0to5 = `Mortality rate for US infants and children younger than 5 years old (per 1000)`)
data<- data %>% rename(CardioMortality = `US crude mortality rate due to cardiovascular disease, malignant neoplasms, diabetes and chronic lower respiratory disease`)
data<- data %>% rename(SuicideRate = `US crude suicide mortality rate`)
data<- data %>% rename(InfantDeathPer1000 = `Number of deaths (infants aged 0 to 27 days old) per 1,000 US live births`)
data<- data %>% rename(NewHIVper100000 = `Number of new HIV diagnoses of HIV infection per 100,000 US population by year of diagnosis`)
data<- data %>% rename(RoadDeathsper100000 = `Age-adjusted rate of deaths due to road traffic injuries per 100,000 US population`)
data<- data %>% rename(TourismEmploy = `Share of US Direct Tourism-related Employment`)
data<- data %>% rename(HoursHousehold = `Hours per day spent on household activities by US women ages 15 and older`)
data<- data %>% rename(RenewableConsump = `US renewable energy consumption as a percentage of total final energy consumption`)
data<- data %>% rename(NRGConsumpPerGDP = `US Total Primary Energy Consumption per Real Dollar of GDP (Thousand Btu per Chained (2009) Dollar)`)
data<- data %>% rename(PM2.5Mean = `Annual mean levels of PM2.5 in US cities weighted by population`)
data<- data %>% rename(HomicideVicPer100000 = `Estimated number of US victims of intentional homicide per 100,000 population`)
data<- data %>% rename(GovReceiptsPercGDP = `US Government current receipts as a percentage of GDP`)
data<- data %>% rename(PoisonDeathRate = `US crude rate of death due to unintentional poisoning`)
data<- data %>% rename(LaborCompinGDPinPrices = `US share of labor compensation in GDP in current national prices`)
data<- data %>% rename(IncomeGrowthPerCap = `US growth rate of income per capita for the total population`)
data<- data %>% rename(CO2MTPerCap = `Carbon Dioxide Emissions in Metric Tons per Million Chained (2009) Dollars`)
data<- data %>% rename(GDPPerCapGrowth = `US annual growth rate of per capita GDP in chained 2009 US dollars`)
data<- data %>% rename(BusinessGrowthPerJob = `US annual growth rate of business sector output per job`)
data<- data %>% rename(ConsumpExpendPerCap = `US personal consumption expenditure (goods) per capita`)
data<- data %>% rename(ManufValueAddPercGDP = `US manufacturing value added as a percentage of GDP`)
data<- data %>% rename(MillionPassAirKilos = `US Passenger-Kilometers by Air in Millions`)

#Need to change type of categorical varibles
data$HasClimateFoodPlan=as.factor(data$HasClimateFoodPlan)
data$HasDisasterStrat=as.factor(data$HasDisasterStrat)
data$HasCapacityPrograms = as.factor(data$HasCapacityPrograms)
data$Year = as.factor(data$Year)

vars<- is.na(data)
variables<-logical()
names(data)
threshold<-length(data$Year)/2.5

for (variable2 in names(data))
{
  if (sum(vars[,variable2])>(threshold)){
    variables[variable2]=T
  }else{
    variables[variable2]=F
  }
}
print(unname(variables))#the list of whether or not each variable has more (T) or less (F) than 50% missing values
variables

missmap(data[variables==F])
str(data[variables==F])
summary(data[variables==F])
data.noNA<-data[variables==F]

### Sorted list of variables by number of missing values ###
list<-sapply(data.noNA, is.na, USE.NAMES = T)
list<-as.data.frame(list)
list2<-colSums(list, na.rm=F,dims=1)
dim(list)
sort(list2)
list3<-100*list2/length(data$Year)
sort(list3)

#View(data[variables==F]) #Completed name changes and sorted by missing values

###########################################################################################
##### Imputation ####
#####################
rows<-c(-1,-19)# get rid of first and last year of data
#data.imp<-data.noNA[rows,]
indx<-sapply(data.noNA, is.numeric)
index.cat<-sapply(data.noNA, is.factor)

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

Mode<-sapply(data.noNA[index.cat], mode) #index.cat contains only nominal (categorical) inputs
data.imp<-data.noNA[rows,]
data.imp[index.cat]<-as.data.frame(mapply(impute,x=data.imp[index.cat],y=Mode)) #for each nominal input, impute using each Mode column and force into a data.frame

# Numeric Input: By Mean #
means <- function(x) { mean(x, na.rm = TRUE) } #na.rm = TRUE removes missing values
Mean<-sapply(data.imp[indx],means) #indx contains only numeric inputs

data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))

missmap(data.imp, main="Missing Map")

# Numeric Input: By Median #
# library(caret)
# ImputeParams<-preProcess(data[-(1:3)], method = "medianImpute") #medianImpute instead of BoxCox
# data.imp3<-predict(ImputeParams,data) #?predict()

# missmap(data.imp3, main="Missing Map")


# Create Missing Value Flag #
#data.imp[c("GiftAvgCard36.NA","DemAge.NA","DemMedIncome.NA")] <- ifelse(
# is.na(data[c("GiftAvgCard36","DemAge","DemMedIncome")]), 1, 0)


#split#
data.tree<-data.imp
targets<- grep("^(InfantDeathPer1000|PM2.5Mean|SuicideRate)", names(data.tree))
library(caTools)
set.seed(8937)
split = sample.split(data.tree$CO2MTPerCap, SplitRatio = 0.7)

#testY = as.matrix(data.tree[!split,targets])

#########################
##### Transformation #####
#########################

# numeric input xform
library(caret)
TransformParams <- preProcess(data.tree[split,indx], method=c("YeoJohnson"))
TransformParams$yj
sort(abs(TransformParams$yj), decreasing = F)

#Apply the transformations to the appropriate variables according to the literature (close to 0)
vars.xf <- grep("^(Business|Road|PercMarine)", names(data.tree))
data.tree[vars.xf]<-log(data.tree[vars.xf]+1)

# nominal input xform
# none


## Standardization: numeric inputs
library(caret)
ScaleParams <- preProcess(data.tree[split,indx], method=c("center", "scale")) #scale only based on training data means, std devs.
data.tree[indx]<-predict(ScaleParams, data.tree[indx])
sd(data.tree$SuicideRate)
mean(data.tree$SuicideRate)


#### Make data into Matrices ####
trainX = data.matrix(data.tree[split,-targets],rownames.force = NA)
trainY = data.matrix(data.tree[split,targets],rownames.force = NA)
n_tree = 1001
m_feature = 6
min_leaf = 2
testX = data.matrix(data.tree[!split,-targets],rownames.force = NA)

###################
#Multi-Target Tree#
###################
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
Prediction2 = build_single_tree(trainX, trainY, m_feature, min_leaf, Inv_Cov_Y, 2)


###### Error Calcs ######
SE = (data.tree[!split,targets]-Prediction)^2
MSE = colMeans(SE)
MSE #Mean Squared Error
RMSE = sqrt(MSE) #root mean squared error

#Try Variable Importance
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
#View(Var_Imp3)

#############################################
####### Simple Model to Test Error ##########
#############################################

# Multiple Linear Regression
fit1 <- lm(Perc05Wasting ~ Perc05Stunting + PercBelowUSPov + PercHouseholdsFoodInsec + Perc5yrEnroll, data=data.tree)
fit2 <- lm(Perc05Wasting ~ PM2.5Mean + CO2MTPerCap + SuicideRate, data=data.tree)
fit3 <- lm(CO2MTPerCap ~ NRGConsumpPerGDP + RenewableConsump + PM2.5Mean + IncomeGrowthPerCap + GDPPerCapGrowth + PercWomenCongress, data=data.tree)
data.tree.yr <- data.tree
data.tree.yr$Year <- 1:17
fit1t <- lm(Perc05Wasting ~ Perc05Stunting + PercBelowUSPov + PercHouseholdsFoodInsec + Perc5yrEnroll + Year, data=data.tree.yr)
fit2t <- lm(Perc05Wasting ~ PM2.5Mean + CO2MTPerCap + SuicideRate + Year, data=data.tree.yr)
fit3t <- lm(CO2MTPerCap ~ NRGConsumpPerGDP + RenewableConsump + PM2.5Mean + IncomeGrowthPerCap + GDPPerCapGrowth + PercWomenCongress + Year, data=data.tree.yr)
#summary(fit) # show results
#fitted(fit)
#plot(fit)
SE_simple1 = (fitted(fit1)-data.tree$Perc05Wasting)^2
SE_simple2 = (fitted(fit2)-data.tree$Perc05Wasting)^2
SE_simple3 = (fitted(fit3)-data.tree$CO2MTPerCap)^2
SE_simple1t = (fitted(fit1t)-data.tree.yr$Perc05Wasting)^2
SE_simple2t = (fitted(fit2t)-data.tree.yr$Perc05Wasting)^2
SE_simple3t = (fitted(fit3t)-data.tree.yr$CO2MTPerCap)^2

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

#Must normalize inputs
#Must try more combinations of targets
#Must figure out singularity issue
    #Check that there is no error in code
    #Normalize the target variables
    #Transform only the input variables with YeoJohnson
    #Check the center and scale normalization methods
    #Read pre-process
#Must visualize tree
#Better way to do variable importance
#Build a very simple model to check code decision tree

#####################################################
########### Time Series Investigation ###############
#####################################################

#http://r-statistics.co/Time-Series-Analysis-With-R.html
data.ts <- data.tree
data.ts <- ts(data.ts, frequency=1,start=2000,end=2016) #letting the code know this is time series data
dev.off()
plot(data$Year,data$CO2MTPerCap)
decomposedRes <- decompose(data.ts,type="mult") #not working, data not periodic? Yearly is not periodic
#View(data.ts)
data.ts.co2<-ts(data.tree$CO2MTPerCap,frequency=1,start=2000)
stlRes <- stl(data.ts.co2,s.window="periodic") #not working, data not periodic? Yearly is not periodic

laggedTS <-stats::lag(data.ts)
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
acfCO2 <- acf(data.ts.co2)
pacfCO2 <- pacf(data.ts.co2)

#Time series regression with tslm
library(forecast)
fittslm3 <- tslm(CO2MTPerCap ~ NRGConsumpPerGDP + RenewableConsump + PM2.5Mean + IncomeGrowthPerCap + GDPPerCapGrowth + PercWomenCongress, data=data.ts)
SE_simple3ts = (fitted(fittslm3)-data.tree.yr$CO2MTPerCap)^2
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
ts_non_target = c(-1,-27)
xregsfull = data.ts[,ts_non_target]
xregsselect = data.ts[,c(15,17,18,19,28,30)] #PercWomenCongress + RenewableConsump + NRGConsumpPerGDP + GDPPerCapGrowth + IncomeGrowthPerCap + PM2.5Mean

#arx_model_full = arx(y,mc=FALSE, ar=NULL, ewma=NULL, mxreg=xregsfull, vc=FALSE,
          # arch=NULL, asym=NULL, log.ewma=NULL, vxreg=NULL, zero.adj=0.1,
          # vc.adj=TRUE, vcov.type=c("ordinary", "white", "newey-west"),
          # qstat.options=NULL, normality.JarqueB=FALSE, user.estimator=NULL,
          # user.diagnostics=NULL, tol=1e-07, LAPACK=FALSE, plot=NULL)
#Can't use more variables than there are observations

arx_model_select = arx(y,mxreg=xregsselect)
#Now try with partitioning to check accuracy
#Look at statistics (t, p) to determine significance
num_rows = 14
y_train = y[1:num_rows] #training targets
xregs_train = xregsselect[1:num_rows,] #training inputs
arx_model_select_tr = arx(y_train, mc=FALSE, ar=NULL, ewma=NULL, mxreg=xregs_train, vc=FALSE,
                       arch=NULL, asym=NULL, log.ewma=NULL, vxreg=NULL, zero.adj=0.1,
                       vc.adj=TRUE, vcov.type=c("ordinary", "white", "newey-west"),
                       qstat.options=NULL, normality.JarqueB=FALSE, user.estimator=NULL,
                       user.diagnostics=NULL, tol=1e-07, LAPACK=FALSE, plot=NULL)

#Compute error of ARX model
y_next = y[num_rows+1]
x_next = xregsselect[num_rows+1,]
y_next_predict = coef.arx(arx_model_select_tr)%*%x_next
SE1_arx = (y_next - y_next_predict)^2

#Compute more predictions
x_next2 = xregsselect[num_rows+2,]
y_next2 = y[num_rows+2]
y_next_predict2 = coef.arx(arx_model_select_tr)%*%x_next2
SE2_arx = (y_next2 - y_next_predict2)^2

x_next3 = xregsselect[num_rows+3,]
y_next3 = y[num_rows+3]
y_next_predict3 = coef.arx(arx_model_select_tr)%*%x_next3
SE3_arx = (y_next3 - y_next_predict3)^2

RMSE_Arx = mean(SE1_arx,SE2_arx,SE3_arx)^0.5



#####################################################################################################################

#Another library and funtion to try
library(sysid)
try2 = arx(trainX, order = c(1, 1, 1), lambda = 0.1, intNoise = FALSE, fixed = NULL)

##Simulate from an AR(1):
set.seed(123)
y <- arima.sim(list(ar=0.4), 70)

##estimate an AR(2) with intercept:
arx(y, mc=TRUE, ar=1:2)


#####################################################################################################################

#Dr. Nat's Idea#
#Low order polynomial fit of input and output variables against time
#Save the coefficients of the polynomial fits. Inputs go into input matrix. Target goes into output matrix
#Coefficients hold information about the time varying nature of the inputs and target
#Use a neural network with input and output matrices to make predictions
#Model will not be interpretable, hopefully it will be accurate. Tradeoff we are willing to make.
#Multiple inputs and outputs could be used
data.poly<-data.tree
summary(data.poly$Year)
data.poly$Year<-as.numeric(data.poly$Year)
trainYears<-seq(1,10,by=1) #Set training years 2000-2009
valYears<-seq(11,17,by=1) #Set validation years 2010-2016

#Generating polyfits. Change number within poly to change order. 1=linear, 2=quadratic, etc.
CO2fitTrain<-lm(CO2MTPerCap ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
NRGConsumpfitTrain<-lm(NRGConsumpPerGDP ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
RenewablefitTrain<-lm(RenewableConsump ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])
#GDPGrowthfitTrain<-lm(GDPPerCapGrowth ~ poly(Year, 1, raw=T), data = data.poly[trainYears,]) #Not a good trend. All over the place. Could use 3-year avg for this one
PMfitTrain<-lm(PM2.5Mean ~ poly(Year, 1, raw=T), data = data.poly[trainYears,])

CO2fitVal<-lm(CO2MTPerCap ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
NRGConsumpfitVal<-lm(NRGConsumpPerGDP ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
RenewablefitVal<-lm(RenewableConsump ~ poly(Year, 1, raw=T), data = data.poly[valYears,])
#GDPGrowthfitVal<-lm(GDPPerCapGrowth ~ poly(Year, 1, raw=T), data = data.poly[valYears,]) #Not a good trend. All over the place. Could use 3-year avg for this one
PMfitVal<-lm(PM2.5Mean ~ poly(Year, 1, raw=T), data = data.poly[valYears,])

ggplot(data.poly, aes(Year, CO2MTPerCap) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))

ggplot(data.poly, aes(Year, NRGConsumpPerGDP) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))

ggplot(data.poly, aes(Year, RenewableConsump) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))

ggplot(data.poly, aes(Year, GDPPerCapGrowth) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE)) #Not a good trend. All over the place. Could use 3-year avg for this one

ggplot(data.poly, aes(Year, PM2.5Mean) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))

CO2coeffT<-CO2fitTrain$coefficients #potentially need to unname these
NRGConsumpcoeffT<-NRGConsumpfitTrain$coefficients
RenewablecoeffT<-RenewablefitTrain$coefficients
#GDPGrowthcoeffT<-GDPGrowthfitTrain$coefficients
PMcoeffT<-PMfitTrain$coefficients

CO2coeffV<-CO2fitVal$coefficients #potentially need to unname these
NRGConsumpcoeffV<-NRGConsumpfitVal$coefficients
RenewablecoeffV<-RenewablefitVal$coefficients
#GDPGrowthcoeffV<-GDPGrowthfitVal$coefficients
PMcoeffV<-PMfitVal$coefficients

## Prepare train/validation sets as matrices ##
#How should inputs be structured? In one lonf vector? In a (#inputs)x(polyfit order) Matrix?
A_train<-t(c(NRGConsumpcoeffT,RenewablecoeffT,PMcoeffT)) #Training input vector. Took out GDPGrowthcoeffT
b_train<-t(c(CO2coeffT)) #Training Output vector

A_val<-t(c(NRGConsumpcoeffV,RenewablecoeffV,PMcoeffV)) #Validation input vector. Took out GDPGrowthcoeffV
b_val<-t(c(CO2coeffV)) #Validation Output vector

###########################################
############ Neural Network ###############
###########################################
data.ann<-data.poly #pass on data. May not even need this


####################
### ANN Building ###
####################
#install(keras)
library(keras)
#install_keras()
#tensorflow::install_tensorflow()

use_session_with_seed(27) #for use in the keras library
ann <- keras_model_sequential() 
#start adding layers to neural network. Build architecture
ann %>% #pipe operator
  layer_dense(units = 3, activation = "tanh", input_shape = c(6)) %>% #units is number of neurons.input shape tells how many inputs there are.
  layer_dense(units = 3, activation = "tanh") %>% #hidden layer 2
  layer_dense(units = 2, activation = "tanh") #output layer


#compile model
ann %>% compile(
  loss = "logcosh", #how we define the error. Not misclassification rate. better to use this.Tried MSE, MAE, logcosh, MAPE
  optimizer = "adam", #how we calculate the gradient descent
  metrics = "mean_squared_error" # Choose model based on error, not loss
)

#Set stopping criteria to choose optimal model
callbacks.list = list(
  callback_early_stopping(
    monitor = "val_loss",
    min_delta = 0.001,
    patience = 5 #if there is no change in error after 5 epochs, stop
  ),
  callback_model_checkpoint(
    filepath="Nat_ann.h5", #save best model permanently
    monitor = "mean_squared_error", #error is stopping criteria
    save_best_only = TRUE
  )
)




# fit model
history<- ann%>% fit(
  x = A_train,
  y = b_train,
  epochs = 200, #number of iterations
  validation_data = list(A_val, b_val),
  verbose = 0, #optional. Using 1 does not suppress output
  callbacks = callbacks.list
)

#load model that has been saved
ann.select<- load_model_hdf5("Nat_ann.h5")
summary(ann.select)
results.validation<-evaluate(ann.select, A_val, b_val) #returns results of loss and accuracy


## Prediction ##
evaluate<-predict(ann.select,A_val)  # predict function for keras package
evaluate
b_val

plotter<-evaluate[1]+evaluate[2]*data.ann$Year #+evaluate[3]*data.ann$Year^2 #include more terms if using quadratic

plot(data.ann$Year,data.ann$CO2MTPerCap,col='green')
lines(data.ann$Year,plotter,col='red')
#terrible performance. Better with linear fits instead of quadratic, but still very bad

