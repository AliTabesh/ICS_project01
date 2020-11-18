#census_2019_1999 <- read.csv("~/1.TU DORTMUND/4.MD-Req 7 Introductory Case Studies/project 1/census_2019_1999.csv")
census_2019_1999 <- read.csv("census_2019_1999.csv")

View(census_2019_1999)
summary(census_2019_1999)

#####################---Task 0.2 clean the data set---##########################

library(tidyr)
data_2019_1999<-census_2019_1999%>%drop_na()

library(dplyr)
data_2019<-filter(data_2019_1999,  year == "2019")
data_1999<-filter(data_2019_1999,  year == "1999")

######################---Task 1---##############################################


#---------female and male life expectancy--------------------------------------------

par(mfrow=c(1,2))
hgF<-hist(data_2019$life.expectancy.female,
          main = "Life expectancy of female | 2019",
          xlab = "Year")

abline(v=mean(data_2019$life.expectancy.female),col="blue")
abline(v=median(data_2019$life.expectancy.female),col="red")

hgM<-hist(data_2019$life.expectancy.male,
          main = "Life expectancy of male | 2019",
          xlab = "median")
?hist
abline(v=mean(data_2019$life.expectancy.male),col="blue")
abline(v=median(data_2019$life.expectancy.male),col="red")

#---------life.expectancy.both.sexes-------------------------------------------

hgB<-hist(data_2019$life.expectancy.both.sexes,
          main = "Life expectancy of both sexes | 2019",
          xlab = "Year")
abline(v=mean(data_2019$life.expectancy.both.sexes),col="blue")
abline(v=median(data_2019$life.expectancy.both.sexes),col="red")

#---------total.fertility.rate.per.woman---------------------------------------

hgFe<-hist(data_2019$total.fertility.rate.per.woman,
          main = "Total fertility rate per women | 2019",
          xlab = "fertility")
abline(v=mean(data_2019$total.fertility.rate.per.woman),col="blue")
abline(v=median(data_2019$total.fertility.rate.per.woman),col="red")




####----DISTRIBUTIONS LINES ON WEDNESDAY ASK------########


#---------life.expectancy.female-----------------
library(fitdistrplus)
fits <- list(
  no = fitdistr(data_2019$life.expectancy.female,"normal"),
  lo = fitdistr(data_2019$life.expectancy.female,"logistic"),
  ca = fitdistr(data_2019$life.expectancy.female,"cauchy"),
  we = fitdistr(data_2019$life.expectancy.female, "weibull"),
  exp = fitdistr(data_2019$life.expectancy.female, "Exponential")
)
sapply(fits, function(i) i$loglik)

fit.in1<- fitdist(data_2019$life.expectancy.female, "norm")
fit.in2 <- fitdist(data_2019$life.expectancy.female, "unif")
fit.in3 <- fitdist(data_2019$life.expectancy.female, "exp")
fit.in4 <- fitdist(data_2019$life.expectancy.female, "cauchy")
fit.in5 <- fitdist(data_2019$life.expectancy.female, "weibull")
fit.in6 <- fitdist(data_2019$life.expectancy.female, "logis")

par(mfrow=c(1,1))
denscomp(list(fit.in1,fit.in2,fit.in3,fit.in4,fit.in5,fit.in6),xlab = "Life expectancy of female",legendtext=c("Normal","Uniform","Exponential","cauchy","weibull","logistic"))



##########################---Task 4---##########################################

#------life expectancy female 1999 vs.2019--------------------------

boxplot(data_1999$life.expectancy.female,
        data_2019$life.expectancy.female, 
        main = "Life expectancy of females \n  1999 | 2019",
        xlab = "Year")
        



par(mfrow=c(1,2))
hg2019F<-boxplot(data_2019$life.expectancy.female,
          main = "Life expectancy of female | 2019",
          xlab = "Year")
?boxplot

hg1999F<-boxplot(data_1999$life.expectancy.female,
          main = "Life expectancy of female | 1999",
          xlab = "Year")

#------life expectancy male 1999 vs.2019-------------------------

boxplot(data_1999$life.expectancy.male,
        data_2019$life.expectancy.male, 
        main = "Life expectancy of males \n  1999 | 2019",
        xlab = "Year")



par(mfrow=c(1,2))
hg2019M<-boxplot(data_2019$life.expectancy.male,
              main = "Life expectancy of male | 2019",
              xlab = "Year")
hg1999M<-boxplot(data_1999$life.expectancy.male,
              main = "Life expectancy of male | 1999",
              xlab = "Year")

#------life expectancy both sexes 1999 vs.2019-------------------------

boxplot(data_1999$life.expectancy.both.sexes,
        data_2019$life.expectancy.both.sexes, 
        main = "Life expectancy of both sexes \n  1999 | 2019",
        xlab = "Year")


par(mfrow=c(1,2))
hg2019B<-boxplot(data_2019$life.expectancy.both.sexes,
              main = "Life expectancy of both sexes | 2019",
              xlab = "Year")

hg1999B<-boxplot(data_1999$life.expectancy.both.sexes,
              main = "Life expectancy of both sexes | 1999",
              xlab = "Year")

#------TOTAL FETILITY PER WOMAN 1999 vs.2019-------------------------

boxplot(data_1999$total.fertility.rate.per.woman,
        data_2019$total.fertility.rate.per.woman, 
        main = "Total fertility per woman \n  1999 | 2019",
        xlab = "Year")



par(mfrow=c(1,2))
hg2019Fe<-boxplot(data_2019$total.fertility.rate.per.woman,
              main = "total fertility per woman | 2019",
              xlab = "range of Year")


hg1999Fe<-boxplot(data_1999$total.fertility.rate.per.woman,
              main = "total fertility per woman | 1999",
              xlab = "range of Year")




