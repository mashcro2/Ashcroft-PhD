#OECD Data Dr. Nat time-polyfit + ANN Model#

#Data from the world bank SDG database OECD
setwd("C:/Users/Matt/Dropbox/Villanova Grad/Sustainable/PhD/PhD Data")

#library(readxl)
data_OECD<-read.csv("imputed_OECD_data.csv", header=TRUE, na.strings=c("..","NA","","?")) #Everything coming in as factor vars


#############################################
####### Simple Model to Test Error ##########
#############################################
data.simple<-data_OECD
# Multiple Linear Regression
fit1 <- lm(IndValAddPerWorker ~ IndustryEmploy + ManufValAdd + Unemployment, data=data.simple)
fit2 <- lm(IndValAddPerWorker ~ MortUnder5 + HomicideRate + NRGIntensityMJ, data=data.simple) #designed to be bad
fit3 <- lm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + PrivateConsump + IndustryEmploy + WomenParliament, data=data.simple)

fit1t <- lm(IndValAddPerWorker ~ IndustryEmploy + ManufValAdd + Unemployment, Year, data=data.simple)
fit2t <- lm(IndValAddPerWorker ~ MortUnder5 + HomicideRate + NRGIntensityMJ, Year, data=data.simple) #designed to be bad
fit3t <- lm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + PrivateConsump + IndustryEmploy + WomenParliament + Year, data=data.simple)
#summary(fit) # show results
#fitted(fit)
#plot(fit)
SE_simple1 = (fitted(fit1)-data.simple$IndValAddPerWorker)^2
SE_simple2 = (fitted(fit2)-data.simple$IndValAddPerWorker)^2
SE_simple3 = (fitted(fit3)-data.simple$CO2kgPerGDPPPP)^2
SE_simple1t = (fitted(fit1t)-data.simple$IndValAddPerWorker)^2
SE_simple2t = (fitted(fit2t)-data.simple$IndValAddPerWorker)^2
SE_simple3t = (fitted(fit3t)-data.simple$CO2kgPerGDPPPP)^2

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


###########################################
############ Random Forest ################
###########################################
data.tree<- data.poly

library(caTools)
set.seed(8937)
TrainYears = seq(1,20)
Split = which(data.tree$Year %in% TrainYears)
ValYears = seq(21,28)
NotSplit = which(data.tree$Year %in% ValYears)


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
Prediction2 = build_single_tree(trainX, trainY, m_feature, min_leaf, Inv_Cov_Y, 2)


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


###########################################
############ Neural Network ###############
###########################################
data.ann<-data.poly
k_folds<-7
Country_length = length(seq(min(data.ann$Year),max(data.ann$Year)))
num_countries<-length(data.ann$Year)/Country_length

####################
### ANN Building ###
####################

evaluate<-array(dim=c(num_countries,2,k_folds)) #initialize outside loop
error_log<-matrix(nrow=k_folds,ncol=2) #initialize outside loop

load("R.default.par.RData"); 
par(par.defaults)


# custom_mse <- custom_metric("custom_mse", function(y_true, y_pred){#}, Year, Target){
#   K<-backend()
#   points_true = K$variable(data.ann$CO2kgPerGDPPPP[valYears])
#   Intercept = y_pred[1]
#   Slope = y_pred[2]
#   Dot  = Slope*data.ann$Year[valYears]
#   points_pred = Intercept + Dot
#   squared_diff = K$square(points_true - points_pred)
#   return(K$mean(squared_diff))
# })
custom_mse<-function (y_true, y_pred, val_years){ #try in matlab and python (keras and pytorch)
  K<-backend()
  points_true = K$variable(data.ann$CO2kgPerGDPPPP[valYears]) #use training years and model will automatically substitute validation when computing val_loss
  #The points_true needs to pull from the specific country and year. valYears not good enough to point to country as it currently is 6/30.
  Intercept = y_pred[1,1]
  Slope = y_pred[1,2]
  Dot  = Slope*data.ann$Year[valYears]
  points_pred = Intercept + Dot #K$variable(Intercept + Dot)
  squared_diff = K$square(points_true - points_pred)
  return(K$mean(squared_diff, axis=-1L))
}
# loss_mse <- custom_metric("loss_mse", function(y_true,y_pred){
# custom_mse(y_true,y_pred)})


library(keras)
Year_set2<-seq(1,28)
for (j in 1:k_folds){
  use_session_with_seed(27) #for use in the keras library
  ann <- keras_model_sequential() 
  
  valYears<-seq(4*j-3,4*j,by=1)
  trainYears<-subset(Year_set2, !(Year_set2 %in% valYears))
  #loss_mse <- custom_metric("loss_mse", function(y_true,y_pred){
    #custom_mse(y_true,y_pred,valYears)})
  
  #with_custom_object_scope(c(custom_loss = loss_mse), { 
    ann %>% #pipe operator
      layer_dense(units = 6, activation = "tanh", input_shape = c(6)) %>% #units is number of neurons.input shape is number of columns in input.
      layer_dense(units = 6, activation = "tanh") %>% #hidden layer 2
      layer_dense(units = 2, activation = "tanh") #output layer. Use linear if data is outside range (-1,1). Try no activation fn
    
    #compile model
    ann %>% compile(
      loss = "logcosh", #how we define the error.Tried MSE, MAE, logcosh, MAPE
      optimizer = "nadam", #how we calculate the gradient descent
      metrics = "mean_squared_error" # Choose model based on error, not loss
    )
    
    filename = paste("Nat_k_fold_OECD",j,"_ann.h5",sep="")
    
    #Set stopping criteria to choose optimal model
    callbacks.list = list(
      callback_early_stopping(
        monitor = "val_loss",
        min_delta = 0.0001,
        patience = 10 #if there is no change in error after 5 epochs, stop
      ),
      callback_model_checkpoint(
        filepath = filename, #save best model permanently
        monitor = "mean_squared_error", #error is stopping criteria
        save_best_only = TRUE
      )
    )
    
    
    # fit model
    history<- ann%>% fit(
      x = A_train[,,j],
      y = b_train[,,j],
      epochs = 125, #number of iterations
      shuffle = F,
      validation_data = list(A_val[,,j], b_val[,,j]),
      verbose = 1, #optional. Using 1 does not suppress output
      callbacks = callbacks.list
    )
    
    #load model that has been saved
    ann.select<- load_model_hdf5(filename) #, custom_objects = loss_mse)
    summary(ann.select)
    results.validation<-evaluate(ann.select, A_val[,,j], b_val[,,j]) #returns results of loss and accuracy
    error_log[j,1]<-results.validation$loss
    error_log[j,2]<-results.validation$mean_squared_error ########## Change back to MSE when needed #############
  #}) #end of "with custom object scope"
    
  ## Prediction ##
  evaluate[,,j]<-predict(ann.select,A_val[,,j])  # predict function for keras package
  
    #Need to redo plotting for individual countries
  plotter<-evaluate[1,1,j]+evaluate[1,2,j]*data.ann$Year #+evaluate[3]*data.ann$Year^2 #include more terms if using quadratic
  plotter2<-b_val[1,1,j]+b_val[1,2,j]*data.ann$Year[valYears]
  plotter3<-b_train[1,1,j]+b_train[1,2,j]*data.ann$Year[trainYears]

  plot(data.ann$Year[1:28],data.ann$CO2kgPerGDPPPP[1:28],col='green', title(filename))
  lines(data.ann$Year,plotter,col='red')
  lines(data.ann$Year[valYears],plotter2,col='blue')
  lines(data.ann$Year[trainYears],plotter3,col='black')
}

evaluate
b_val
b_train
error_log
mean(error_log[,2]) #mean MSE
mean(error_log[,1]) #mean loss
hist(evaluate)
hist(b_val)
########################################################################################################################

#Two models much worse than the rest (2002-2005 and 2014-2017)
#Validation error sometimes lower than training error
#Suggestions for creating a new error calculation
#Compare arx and this method's error on predicting actual points. Use k-folds for arx as well.
#Should try other input variables
#How do I check weights? Make variable importance instead.
#Scale data to Mean=0, stdev = 1.
#inquire about better data from Our World in Data SDG Data Tracker
#Extension to multiple targets requires a good loss function


#####################################################
########### Time Series Investigation ###############
#####################################################

#http://r-statistics.co/Time-Series-Analysis-With-R.html
data.ts <- data.ann
data.ts <- ts(data.ts, frequency=1,start=1990,end=2017) #letting the code know this is time series data
load("R.default.par.RData"); 
par(par.defaults)
plot(data.ann$Year,data.ann$CO2kgPerGDPPPP)

data.ts.co2<-ts(data.ann$CO2kgPerGDPPP,frequency=1,start=1990,end=2017)

laggedTS <-stats::lag(data.ts)

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
fittslm <- tslm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + AirPassenger, data=data.ts) #Other models should do better than this. This only uses one country
SE_simplets = (fitted(fittslm)-data.ann$CO2kgPerGDPPPP[1:28])^2 #not working because using average of all countries for first part and all countries for second
MSE_simplets = mean(SE_simplets)
RMSE_simplets = MSE_simplets^0.5
RMSE_simplets

fitlm<-lm(CO2kgPerGDPPPP ~ NRGIntensityMJ + NRGrenewConsume + AirPassenger, data=data.ann) #Other models should do better than this. This uses all countries
SE_simple = (fitted(fitlm)-data.ann$CO2kgPerGDPPPP)^2 #not working because using average of all countries for first part and all countries for second
MSE_simple = mean(SE_simple)
RMSE_simple = MSE_simple^0.5
RMSE_simple
summary(data.ann$Country.Code) #since this is character var, it 

# ARX model practice
#https://cran.r-project.org/web/packages/gets/gets.pdf
######NOT WORKING ON ALL COUNTRIES##########
library(gets)
y = data.ts.co2

ts_non_target = c(-1,-12)
xregsfull = data.ts[,ts_non_target]
xregsselect = data.ts[,c(7,8,12)] #RenewableConsump + NRGIntesityMJ + AirPassengers

arx_model_select = arx(y,mxreg=xregsselect)
#Now try with partitioning to check accuracy
#Look at statistics (t, p) to determine significance

MSE_arx = matrix(nrow=num_countries,ncol=k_folds)
RMSE_arx = matrix(nrow=num_countries,ncol=k_folds)
Year_set3 = seq(1:28)

excluded = 250 #arbitrary high number
data.ann[-excluded,]

for (j in 1:num_countries){
  data.ts <- data.ann[-excluded,]
  data.ts <- ts(data.ts, frequency=1,start=1990,end=2017)
  ts_non_target = c(-1,-12)
  #xregsfull = data.ts[-excluded,ts_non_target]
  xregsselect = data.ts[,c(7,8,12)]
  data.ts.co2<-ts(data.ann$CO2kgPerGDPPP[-excluded],frequency=1,start=1990,end=2017)
  y=data.ts.co2
  
  for (i in 1:k_folds){
    valYears2<-seq(4*i-3,4*i,by=1)
    trainYears2<-subset(Year_set3, !(Year_set3 %in% valYears2))
    y_train = y[trainYears2]
    xregs_train = xregsselect[trainYears2,] #training inputs
    arx_model_select_tr = arx(y_train, mc=FALSE, ar=NULL, ewma=NULL, mxreg=xregs_train, vc=FALSE,
                              arch=NULL, asym=NULL, log.ewma=NULL, vxreg=NULL, zero.adj=0.1,
                              vc.adj=TRUE, vcov.type=c("ordinary", "white", "newey-west"),
                              qstat.options=NULL, normality.JarqueB=FALSE, user.estimator=NULL,
                              user.diagnostics=NULL, tol=1e-07, LAPACK=FALSE, plot=NULL)
    
    #Compute error of ARX model
    y_val = y[valYears2]
    x_val = xregsselect[valYears2,]
    y_predict = coef.arx(arx_model_select_tr)*x_val
    SE_arx = (y_val - y_predict)^2
    MSE_arx[j,i] = mean(SE_arx)
    RMSE_arx[j,i] = MSE_arx[j,i]^0.5
  }
excluded = 1:(j*Country_length)
}
RMSE_arx
hist(RMSE_arx)

Country_RMSE_arx[,1]<-as.data.frame(unique(data.poly$Country.Code))
Country_RMSE_arx[,2]<-as.data.frame(rowMeans(RMSE_arx))
Country_RMSE_arx[,3]<-as.data.frame(rowMeans(MSE_arx))
colnames(Country_RMSE_arx)<-c('Country.Code','RMSE_arx','MSE_arx')

MSE_arx_overall = mean(MSE_arx)
MSE_arx_overall
RMSE_arx_overall = mean(RMSE_arx)
RMSE_arx_overall

######## Panel lm ########
library(plm)
plm_OECD1<-plm(CO2kgPerGDPPPP ~ AirPassenger + NRGrenewConsume + NRGIntensityMJ + lag(CO2kgPerGDPPPP) + lag(NRGIntensityMJ) + lag(AirPassenger) + lag(NRGrenewConsume),
                 model='within', #random, fd and within best. I think random is most applicable to reality
                 effect='time', #time and nested best (same result)
                 data=data.ann)

summary(plm_OECD1) # Shows N=28, T=32. I think these should be switched. Should be 28 years, 32 countries
error_plm<-fixef(plm_OECD1,vcov=vcovHC(plm_OECD1),return='se')
error_plm

Resid_meansq_plm<-mean(plm_OECD1$residuals^2)
Resid_rtmeansq_plm<-sqrt(mean(plm_OECD1$residuals^2))
Resid_abs_mean_plm<-mean(abs(plm_OECD1$residuals))

#### NOT SURE IF TIME AND COUNTRY ARE BEING USED PROPERLY, COULD BE SWITCHED ####
data.plm<-data.ann
data.plm[,1]<-data.ann[,2]
data.plm[,2]<-data.ann[,1]
data.plm<- data.plm %>% rename(Year. = Country.Code)
data.plm<- data.plm %>% rename(Country.Code = Year)
data.plm<- data.plm %>% rename(Year = Year.)

plm_OECD2<-plm(CO2kgPerGDPPPP ~ AirPassenger + NRGrenewConsume + NRGIntensityMJ + lag(CO2kgPerGDPPPP) + lag(NRGIntensityMJ) + lag(AirPassenger) + lag(NRGrenewConsume),
                 model='within', #random, fd and within best. I think random is most applicable to reality
                 effect='time', #time and nested best (same result)
                 data=data.plm)

summary(plm_OECD2) # Shows N=33, T=27. These are more correct, but T should be 28 for 28 years.
fixed_ef2<-fixef(plm_OECD2,vcov=vcovHC(plm_OECD2),return='se')
fixed_ef2

Resid_meansq_plm2<-mean(plm_OECD2$residuals^2)
Resid_rtmeansq_plm2<-sqrt(mean(plm_OECD2$residuals^2))
Resid_abs_mean_plm2<-mean(abs(plm_OECD2$residuals))

#### Try with other variables ####
plm_OECD3<-plm(Unemployment ~ GDPCapGrowth + IndustryEmploy + ManufValAdd + IndValAddPerWorker + PersonalRemit + PrivateConsump + lag(Unemployment),
                  model='within', #random, fd and within best. I think random is most applicable to reality
                  effect='time', #time and nested best (same result)
                  data=data.plm)

summary(plm_OECD3) # Shows unbalanced panel. I think this is due to missing values in some vars
fixed_ef3<-fixef(plm_OECD3,vcov=vcovHC(plm_OECD3),return='se')
fixed_ef3

Resid_meansq_plm3<-mean(plm_OECD3$residuals^2)
Resid_rtmeansq_plm3<-sqrt(mean(plm_OECD3$residuals^2))
Resid_abs_mean_plm3<-mean(abs(plm_OECD3$residuals))

#### Try with more advanced parameters #### twoway effect and vcovHC in fixef function
plm_OECD4<-plm(CO2kgPerGDPPPP ~ AirPassenger + NRGrenewConsume + NRGIntensityMJ + lag(CO2kgPerGDPPPP) + lag(NRGIntensityMJ) + lag(AirPassenger) + lag(NRGrenewConsume),
                  model='within', #random, fd and within best. I think random is most applicable to reality
                  effect='twoway', #twoway not great
                  data=data.plm)

summary(plm_OECD4) # Shows N=33, T=27. These are more correct, but T should be 28 for 28 years.
fixed_ef4<-fixef(plm_OECD4,vcov=vcovHC(plm_OECD4),return='se', effect="time")
summary(fixed_ef4) # shows when there is a signifacant relationship between the vars in each country

Resid_meansq_plm4<-mean(plm_OECD4$residuals^2)
Resid_rtmeansq_plm4<-sqrt(mean(plm_OECD4$residuals^2))
Resid_abs_mean_plm4<-mean(abs(plm_OECD4$residuals))

first_years<-seq(1,length(data.plm$CO2kgPerGDPPPP),by = Country_length)
#last_years<-seq(Country_length,length(data.plm$CO2kgPerGDPPPP),by = Country_length)
MSE_plm_OECD4<-mean((fitted(plm_OECD4)-data.plm$CO2kgPerGDPPPP[-first_years])^2)
#MSE_plm_OECD4<-mean((fitted(plm_OECD4)-data.plm$CO2kgPerGDPPPP[-last_years])^2)

Error_alt_OECD4<-(plm_OECD4$coefficients[1]*data.plm$AirPassenger + 
  plm_OECD4$coefficients[2]*data.plm$NRGrenewConsume + 
  plm_OECD4$coefficients[3]*data.plm$NRGIntensityMJ + 
  plm_OECD4$coefficients[4]*lag(data.plm$CO2kgPerGDPPPP) + 
  plm_OECD4$coefficients[5]*lag(data.plm$NRGIntensityMJ) +
  plm_OECD4$coefficients[6]*lag(data.plm$AirPassenger) +
  plm_OECD4$coefficients[7]*lag(data.plm$NRGrenewConsume)) -
  data.plm$CO2kgPerGDPPPP
MSE_plm_OECD4_alt <- mean(Error_alt_OECD4^2)

MSE_plm_OECD4_alt2 <- mean((predict(plm_OECD4)-data.plm$CO2kgPerGDPPPP[-first_years])^2)

#### Try with more advanced parameters #### random effects
plm_OECD5<-plm(CO2kgPerGDPPPP ~ AirPassenger + NRGrenewConsume + NRGIntensityMJ + lag(CO2kgPerGDPPPP) + lag(NRGIntensityMJ) + lag(AirPassenger) + lag(NRGrenewConsume),
                  model='random', #random, fd and within best. I think random is most applicable to reality
                  effect='twoways', #time and nested best (same result)
                  data=data.plm)

summary(plm_OECD5)
random_ef5<-ranef(plm_OECD5, effect="time")

Resid_meansq_plm5<-mean(plm_OECD5$residuals^2)
Resid_rtmeansq_plm5<-sqrt(mean(plm_OECD5$residuals^2))
Resid_abs_mean_plm5<-mean(abs(plm_OECD5$residuals))

# plm package very effective in terms of error. However, these models use all of the data. They are not trained on some data 
# and validated on other data. Next step might be to try leaving some data out to validate/test
# Can I try non-linear relationships? Yes- more weird nonlinear terms slightly reduce accuracy and increase significance of intercept

plm_OECD6<-plm(CO2kgPerGDPPPP ~ sin(AirPassenger) + cos(NRGrenewConsume+2) + log(NRGIntensityMJ+2) + lag(CO2kgPerGDPPPP) + lag(NRGIntensityMJ) + lag(AirPassenger) + lag(NRGrenewConsume),
                  model='random', #random, fd and within best. I think random is most applicable to reality
                  effect='twoways', #time and nested best (same result)
                  data=data.plm)

summary(plm_OECD6) # Shows N=33, T=27. These are more correct, but T should be 28 for 28 years.
fixed_ef6<-fixef(plm_OECD6,vcov=vcovHC(plm_OECD6),return='se') #needs "within' model
fixed_ef6
random_ef6i<-ranef(plm_OECD6, effect="individual") #needs 'random' model
random_ef6i
random_ef6t<-ranef(plm_OECD6, effect="time")
random_ef6t

Resid_meansq_plm6<-mean(plm_OECD6$residuals^2)
Resid_rtmeansq_plm6<-sqrt(mean(plm_OECD6$residuals^2))
Resid_abs_mean_plm6<-mean(abs(plm_OECD6$residuals))

# try more lags
plm_OECD7<-plm(CO2kgPerGDPPPP ~ AirPassenger + NRGrenewConsume + NRGIntensityMJ + lag(CO2kgPerGDPPPP,k=c(1,2,3)) + lag(NRGIntensityMJ,k=c(1,2,3)) + lag(AirPassenger,k=c(1,2,3)) + lag(NRGrenewConsume,k=c(1,2,3)),
                  model='random', #random, fd and within best. I think random is most applicable to reality
                  effect='twoways', #time and nested best (same result)
                  data=data.plm)

summary(plm_OECD7) # Shows N=33, T=27. These are more correct, but T should be 28 for 28 years.
random_ef7i<-ranef(plm_OECD7, effect = "individual") 
random_ef7i
random_ef7t<-ranef(plm_OECD7, effect="time") #needs 'random' model
random_ef7t

Resid_meansq_plm7<-mean(plm_OECD7$residuals^2)
Resid_rtmeansq_plm7<-sqrt(mean(plm_OECD7$residuals^2))
Resid_abs_mean_plm7<-mean(abs(plm_OECD7$residuals))

#generally only the first lag is significant, except on the target itself


###################################
###### Duplicate other work #######
###################################

# Correlation matrices #
data.cor<-data.ann[,c(-1,-2)]
Cor_matrix_pearson<-cor(data.cor,use="pairwise.complete.obs", method='pearson')
Cor_matrix_spearman<-cor(data.cor,use="pairwise.complete.obs", method='spearman')
Cor_matrix_pearson_USA<-cor(data.cor[data.ann$Country.Code=="USA",],use="pairwise.complete.obs", method='pearson')
indx<-sapply(data_no2018_19, is.numeric)
indx[1]<-F
Cor_matrix_pearson_raw<-cor(data_no2018_19[,indx],use="pairwise.complete.obs", method='pearson')
library(corrplot)
library(RColorBrewer)
corrplot(Cor_matrix_pearson, method='color', order='FPC',col=brewer.pal(n = 7, name = "RdYlGn"))
title(main='Pearson correlation matrix of processed data')
corrplot(Cor_matrix_spearman, method='color', order='FPC',col=brewer.pal(n = 7, name = "RdYlGn"))
title(main='Spearman correlation matrix of processed data')
#corrplot(Cor_matrix_pearson_USA, method='color', order='FPC',col=brewer.pal(n = 7, name = "RdYlGn"))
#title(main='Pearson correlation matrix of processed USA data')
corrplot(Cor_matrix_pearson_raw, method='color', order='FPC',col=brewer.pal(n = 7, name = "RdYlGn"))
title(main='Pearson correlation matrix of raw data')


###########################################################################################################################
########## RNN ###########

#Want to divide data into chunks then have it predict the next year as the target.
  #Can make the chunks and targets overlap a bit, just have to make sure each chunk is in sequential order.
  #Must also ensure ach chunk has data only from one country
  #28 years for each country. Use 7 year chunk plus 1 target, then overlap starting with 5th training point. COULD OVERLAP EVEN MORE, FULL OVERLAP
    # 1  2  3  4  5  6  7  + 8
    # 5  6  7  8  9  10 11 + 12
    # 9  10 11 12 13 14 15 + 16
    # 13 14 15 16 17 18 19 + 20
    # 17 18 19 20 21 22 23 + 24
    # 21 22 23 24 25 26 27 + 28
  # 6 sets per country 6*33
#Do I have to make separate models for each country or can I train the model on all countries? All countries
  #Should I include country code as an input variable? Probably

# Data Process #
data.rnn<-data.ann
indx5 <- c(7,8,12,15)

# Getting rid of BRIC COuntries
BRIC<-which(data.rnn$Country.Code %in% c('CHN','BRA','IND','RUS'))
data.rnn<-data.rnn[-BRIC,]

# Make into Matrix.Choose whether to include country codes, exclude them, or make dummy vars for them
#data.rnn<-data.matrix(data.rnn[,c(2,indx5)],) #Use to include country code

data.rnn<-data.matrix(data.rnn[,indx5],) #Use to exclude country code


### Set up dummy vars for countries ###
library(fastDummies)
dummies<-dummy_cols(data.rnn) #make dummy cars for country codes
data.rnn<-data.matrix(dummies[,-1])
Emission <- data.rnn[,4] #Use when Using dummies
data.rnn<-data.rnn[,-4]
data.rnn<-cbind(data.rnn,Emission)
######################################

# Cut the text in overlapping sample sequences of max_len characters
max_len = 7 # the number of previous examples we'll look at
batch_size = 1 # number of sequences to look at at one time during training. Try 6 for 1 batch per country.
set.seed(27)

Emission <- data.rnn[,ncol(data.rnn)] #Use when not using dummies


# get a list of start indexes for our (overlapping) chunks
start_indexesi <- seq(1, length(Emission) - (max_len + 1), by = 4) #by 4 for overlap starting at fifth element of previous
start_indexes <- start_indexesi[c(-7*(1:33))] #get rid of start indices which would overlap 2 countries

# create an empty matrix to store our data in
RNN_array<- array(dim = c(length(start_indexes),max_len + 1, ncol(data.rnn))) #197x8x5 OR 197x8x4 #197 will increase with more overlap
RNN_input_train<- array(dim = c(ceiling((2/3)*length(start_indexes))+1,max_len+1, ncol(data.rnn)-1)) #split between 2 countries happens at i=132/133
RNN_target_train<- array(dim = c((2/3)*length(start_indexes),max_len+1, 1))
RNN_input_val<- array(dim = c(length(start_indexes)-ceiling((2/3)*length(start_indexes)), max_len+1,ncol(data.rnn)-1))
RNN_target_val<- array(dim = c(length(start_indexes)-ceiling((2/3)*length(start_indexes)),max_len+1, 1))

# fill our matrix with the overlapping slices of our dataset
for (i in 1:length(start_indexes)){
  RNN_array[i,,] <- data.rnn[start_indexes[i]:(start_indexes[i] + max_len),]
} #Confirmed working correctly 7/28
RNN_input_train<- RNN_array[1:ceiling((2/3)*length(start_indexes)),,1:(ncol(data.rnn)-1)]
RNN_target_train<- RNN_array[1:ceiling((2/3)*length(start_indexes)),,ncol(data.rnn)]
RNN_input_val<- RNN_array[(ceiling((2/3)*length(start_indexes))+1):length(start_indexes),,1:(ncol(data.rnn)-1)]
RNN_target_val<- RNN_array[(ceiling((2/3)*length(start_indexes))+1):length(start_indexes),,ncol(data.rnn)]

#How to handle country code as a number vs character in rnn - dummy vars
#Do all countries follow the same model?
#Create dummy varaiables for each country [0-1]

##### Build Model #####
library(keras)
use_session_with_seed(27)
rnn <- keras_model_sequential() %>%
  layer_dense(units = 8, input_shape = dim(RNN_input_train)[2:3], activation = "tanh") %>% #should feel comfortable using more units or more time step
  layer_simple_rnn(units=32, activation = "tanh", return_sequences = T) %>% #more layers
  layer_simple_rnn(units=32, activation = "tanh") %>% #Not able to add more than 1 rnn layer? In order to stack layers, need to use return sequences argument
  #layer_dense(units=8, activation = "tanh") %>%
  #layer_dense(units=8, activation = "tanh") %>%
  layer_dense(units = 8)#, activation = "tanh") #Last layer should be 8 or 1? 8 if CO2 is not an input at all. Predicts every year

rnn %>% compile(
  optimizer = "adam", #how we calculate the gradient descent. optimizer_rmsprop()
  loss = "mse", #how we define the error. 
  metrics = "mean_squared_error" # Choose model based on error, not loss. Training error instead of validation?
)


filename2 = "my_OECD_rnn.h5"

 callbacks.list.2 = list(
  callback_early_stopping(
    monitor = "val_loss",
    min_delta = 0.0005,
    patience = 15 #if there is no change in error after X epochs, stop
  ),
  callback_model_checkpoint(
    filepath = filename2, #save best model permanently
    monitor = "mean_squared_error", #error is stopping criteria
    save_best_only = TRUE
  )
)


# fit model
history<- rnn %>% fit(
  x = RNN_input_train,
  y = RNN_target_train,
  batch_size = 1, #batch_size,
  shuffle = F,
  epochs = 100, #number of iterations
  validation_data = list(RNN_input_val,RNN_target_val),
  verbose = 1, #optional. Using 1 does not suppress output
  callbacks = callbacks.list.2
)

rnn.select<- load_model_hdf5(filename2) #, custom_objects = loss_mse)
summary(rnn.select)
results.validation<-evaluate(rnn.select, RNN_input_val,RNN_target_val) #returns results of loss and accuracy

## Prediction ##
emission_predict<-predict(rnn.select,RNN_input_val)
mean(abs((emission_predict-RNN_target_val)))#^2) #mean absolute error 
#summary(emission_predict)
#summary(RNN_target_val)
summary(data.rnn[,ncol(data.rnn)])
hist(data.rnn[,ncol(data.rnn)])
hist(RNN_target_val)
hist(emission_predict)

#Try GRu and LSTM layers in rnn
#try adding more layers to rnn
#Try getting custom loss function to work in ann
#Try higher order fits to input vars in ann
#Try getting imputation to work for all countries and variables
  #is linear interpolation appropriate for all variables in all countries? Probably not
#Try no activation fn on final layer of ann
#Try arx models again
#compare against lm() models which have low error
#Include more input variables
#Try to interpret models with other models


# NEW rnn setup
# include CO2 as an input? - NO
# Only predict 1 year, not 8
# Only include 7 years in training set, 8th year is predicted
  # Training input shape: 120x7x3, also could try 120x7x4 with CO2 as 4th
  # Training target shape: 120x1
  # Validation input shape: 59x7x3
  # Validation target shape: 59x1
  # Increase first dimension by increasing overlap


###########################
######### RNN 2 ###########
###########################

#Want to divide data into chunks then have it predict the next year as the target.
#Can make the chunks and targets overlap a bit, just have to make sure each chunk is in sequential order.
#Must also ensure ach chunk has data only from one country
#28 years for each country. Use 7 year chunk plus 1 target, then overlap starting with 2nd training point.
# 1  2  3  4  5  6  7  + 8
# 2  3  4  5  6  7  8  + 9
# 3  4  5  6  7  8  9  + 10
# 4  5  6  7  8  9  10 + 11
# ...
# 21 22 23 24 25 26 27 + 28
# 21 sets per country 21*33
#Do I have to make separate models for each country or can I train the model on all countries? All countries
#Should I include country code as an input variable? Probably

# Data Process #
data.rnn<-data.ann
indx5 <- c(7,8,12,15)

# Getting rid of BRIC COuntries
#BRIC<-which(data.rnn$Country.Code %in% c('CHN','BRA','IND','RUS'))
#data.rnn<-data.rnn[-BRIC,]

# Make into Matrix.Choose whether to include country codes, exclude them, or make dummy vars for them
#data.rnn<-data.matrix(data.rnn[,c(2,indx5)],) #Use to include country code

data.rnn<-data.matrix(data.rnn[,indx5],) #Use to exclude country code


### Set up dummy vars for countries ###
library(fastDummies)
dummies<-dummy_cols(data.rnn) #make dummy cars for country codes
data.rnn<-data.matrix(dummies[,-1])
Emission <- data.rnn[,4] #Use when Using dummies
data.rnn<-data.rnn[,-4]
data.rnn<-cbind(data.rnn,Emission)
######################################

# Cut the text in overlapping sample sequences of max_len characters
max_len = 7 # the number of previous examples we'll look at
batch_size = 21 # number of sequences to look at at one time during training. Try 6 for 1 batch per country.
time_steps_per_country = 21
set.seed(27)

Emission <- data.rnn[,ncol(data.rnn)] #Use when not using dummies


# get a list of start indexes for our (overlapping) chunks
start_indexesi2 <- seq(1, length(Emission) - (max_len + 1), by = 1) #by 1 for overlap starting at 2nd element of previous
exclude<-integer()
for (k in 1:(num_countries+1)){
  exclude<- c(exclude,seq(k*Country_length-(Country_length - (time_steps_per_country+1)),k*Country_length))
  start_indexes2 <- start_indexesi2[c(-exclude)] #get rid of start indices which would overlap 2 countries
}

# create an empty matrix to store our data in
RNN_array2<- array(dim = c(length(start_indexes2),max_len + 1, ncol(data.rnn))) #699x8x4 #699 observations, 8 years per observation, 4 total variables
RNN_input_train2<- array(dim = c((ceiling((2/3)*length(start_indexes2))+4),max_len, ncol(data.rnn)-1)) #split between 2 countries happens at i=469/470. Slightly more than 2/3 training
RNN_target_train2<- array(dim = c(((2/3)*length(start_indexes2)+4),1, 1))
RNN_input_val2<- array(dim = c((length(start_indexes2)-ceiling((2/3)*length(start_indexes2))+3), max_len,ncol(data.rnn)-1))
RNN_target_val2<- array(dim = c((length(start_indexes2)-ceiling((2/3)*length(start_indexes2))+3),1, 1))

# fill our matrix with the overlapping slices of our dataset
for (i in 1:length(start_indexes2)){
  RNN_array2[i,,] <- data.rnn[start_indexes2[i]:(start_indexes2[i] + max_len),]
}
RNN_input_train2<- RNN_array2[1:(ceiling((2/3)*length(start_indexes2))+3),1:max_len,1:(ncol(data.rnn)-1)]
RNN_target_train2<- RNN_array2[1:(ceiling((2/3)*length(start_indexes2))+3),max_len+1,ncol(data.rnn)]
RNN_input_val2<- RNN_array2[((ceiling((2/3)*length(start_indexes2)))+4):length(start_indexes2),1:max_len,1:(ncol(data.rnn)-1)]
RNN_target_val2<- RNN_array2[((ceiling((2/3)*length(start_indexes2)))+4):length(start_indexes2),max_len+1,ncol(data.rnn)]

#How to handle country code as a number vs character in rnn - dummy vars
#Do all countries follow the same model?
#Create dummy varaiables for each country [0-1]

##### Build Model #####
library(keras)
use_session_with_seed(27)
rnn <- keras_model_sequential() %>%
  layer_dense(units = 7, input_shape = dim(RNN_input_train2)[2:3], activation = "tanh") %>% #should feel comfortable using more units or more time step
  layer_lstm(units=28, activation = "tanh", return_sequences = T) %>% #more layers
  layer_lstm(units=28, activation = "tanh", return_sequences = T) %>% #Not able to add more than 1 rnn layer? In order to stack layers, need to use return sequences argument
  layer_lstm(units=28, activation = "tanh", return_sequences = F) %>%
  #layer_dense(units=8, activation = "tanh") %>%
  layer_dense(units = 1)#, activation = "tanh") #Last layer should be 8 or 1? 8 if CO2 is not an input at all. Predicts every year

rnn %>% compile(
  optimizer = "adam", #how we calculate the gradient descent. optimizer_rmsprop()
  loss = "mse", #how we define the error. 
  metrics = "mean_squared_error" # Choose model based on error, not loss. Training error instead of validation?
)


filename3 = "my_OECD_rnn2.h5"

callbacks.list.2 = list(
  callback_early_stopping(
    monitor = "val_loss",
    min_delta = 0.0005,
    patience = 15 #if there is no change in error after X epochs, stop
  ),
  callback_model_checkpoint(
    filepath = filename3, #save best model permanently
    monitor = "mean_squared_error", #error is stopping criteria
    save_best_only = TRUE
  )
)


# fit model
history<- rnn %>% fit(
  x = RNN_input_train2,
  y = RNN_target_train2,
  batch_size = batch_size,
  shuffle = F,
  epochs = 100, #number of iterations
  validation_data = list(RNN_input_val2,RNN_target_val2),
  verbose = 1, #optional. Using 1 does not suppress output
  callbacks = callbacks.list.2
)

rnn.select2<- load_model_hdf5(filename3) #, custom_objects = loss_mse)
summary(rnn.select2)
results.validation<-evaluate(rnn.select2, RNN_input_val2,RNN_target_val2) #returns results of loss and accuracy

## Prediction ##
emission_predict2<-predict(rnn.select2,RNN_input_val2)
mean(abs((emission_predict2-RNN_target_val2)))#^2) #mean absolute error 
#summary(emission_predict)
#summary(RNN_target_val)
summary(data.rnn[,ncol(data.rnn)])
hist(data.rnn[,ncol(data.rnn)])
hist(RNN_target_val2)
hist(emission_predict2)
