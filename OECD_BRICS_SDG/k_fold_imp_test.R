polycomp <- function(x,period) {lm(x ~ poly(Year,1,raw=T), data.imp[period,])}
indx4<-c(7,8,12) #indices of input vars NRGrenewConsume, NRGIntensityMJ, AirPassenger
country_include<-list()
for (n in c(1:4,7:18,21:24,26:28,30:32,34:37,40:42)){
  period = seq(n*Country_length-(Country_length-1),n*Country_length)
  poly<- sapply(data.imp[period,indx4],polycomp,period=period)
  country_include[n]<-data.imp$Country.Code[max(period)]
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
library(caret)
ggplot(data.imp[period,], aes(Year, NRGrenewConsume) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))
