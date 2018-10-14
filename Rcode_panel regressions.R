sink("Output.txt")
library(foreign)
library(plm)
library(lmtest)
library(car)
library(tseries)
library(gplots)
library(plyr)
library(dummies)
library(lattice)
library(ggplot2)
library(stargazer)
library(psych)

#Descriptive statistics and plots
Book1<-cbind(Book1, dummy(Book1$Country, sep = "_")) #created dummy for each country
my_data <- Book1[, c(3:8)] #created a data frame with only numeric variables
cor(my_data) #Correlation matrix of the variables
summary(Book1) #descriptive statistics of the whole data
dt=Book1[, c(1:8)] #data frame with only the dependent and independent variables
describe.by(dt, dt$Country) #descriptive statistics by country
summary(dt)
hist(Book1$GDP, main="Histogram:GDP") #histogram of GDP
hist(Book1$CO2, main="Histogram: CO2 emissions") #histogram of CO2
hist(Book1$`Pop(mln)`, main="Histogram: Population") #histogram of population
Book1$lnGDP=log(Book1$GDP) #logtransformation of GDP
Book1$lnPop=log(Book1$`Pop(mln)`)#logtransformation of population
Book1$lnCO2 = log(Book1$CO2)#logtransformation of CO2
hist(Book1$lnGDP, main="Histogram: lnGDP")
hist(Book1$lnCO2, main="Histogram: lnCO2")
hist(Book1$lnPop, main="Histogram lnPopulation")
plot(Book1$lnCO2, Book1$lnPop, pch=unclass(Book1$Country), data=Book1)# plot of the relationship of lnCO2 and lnPop
#coplot(CO2~Year|Book1$Country=="AUS",type="b",data=Book1)logpopulation
p<-ggplot(data=Book1, aes(x=lnPop, y=lnCO2, group=Country))
p+geom_point() #relationship between lnPop and lnCO2 grouped by country:points
p+geom_line() #relationship between lnPop and lnCO2 grouped by country:lines


#Correlations of CO2 and population by country
Cor_Aus=ddply(Book1, "Book1_AUS", summarize, corr1=cor(lnCO2, lnPop))
Cor_Aut=ddply(Book1, "Book1_AUT", summarize, corr1=cor(lnCO2, lnPop))
Cor_Bel=ddply(Book1, "Book1_BEL", summarize, corr1=cor(lnCO2, lnPop))
Cor_Can=ddply(Book1, "Book1_CAN", summarize, corr1=cor(lnCO2, lnPop))
Cor_Fin=ddply(Book1, "Book1_FIN", summarize, corr1=cor(lnCO2, lnPop))
Cor_Fra=ddply(Book1, "Book1_FRA", summarize, corr1=cor(lnCO2, lnPop))
Cor_Deu=ddply(Book1, "Book1_DEU", summarize, corr1=cor(lnCO2, lnPop))
Cor_Grc=ddply(Book1, "Book1_GRC", summarize, corr1=cor(lnCO2, lnPop))
Cor_Hun=ddply(Book1, "Book1_HUN", summarize, corr1=cor(lnCO2, lnPop))
Cor_Isl=ddply(Book1, "Book1_ISL", summarize, corr1=cor(lnCO2, lnPop))
Cor_Irl=ddply(Book1, "Book1_IRL", summarize, corr1=cor(lnCO2, lnPop))
Cor_Ita=ddply(Book1, "Book1_ITA", summarize, corr1=cor(lnCO2, lnPop))
Cor_Jpn=ddply(Book1, "Book1_JPN", summarize, corr1=cor(lnCO2, lnPop))
Cor_Kor=ddply(Book1, "Book1_KOR", summarize, corr1=cor(lnCO2, lnPop))
Cor_Lux=ddply(Book1, "Book1_LUX", summarize, corr1=cor(lnCO2, lnPop))
Cor_Mex=ddply(Book1, "Book1_MEX", summarize, corr1=cor(lnCO2, lnPop))
Cor_Nld=ddply(Book1, "Book1_NLD", summarize, corr1=cor(lnCO2, lnPop))
Cor_Nzl=ddply(Book1, "Book1_NZL", summarize, corr1=cor(lnCO2, lnPop))
Cor_Nor=ddply(Book1, "Book1_NOR", summarize, corr1=cor(lnCO2, lnPop))
Cor_Pol=ddply(Book1, "Book1_POL", summarize, corr1=cor(lnCO2, lnPop))
Cor_Prt=ddply(Book1, "Book1_PRT", summarize, corr1=cor(lnCO2, lnPop))
Cor_Esp=ddply(Book1, "Book1_ESP", summarize, corr1=cor(lnCO2, lnPop))
Cor_Swe=ddply(Book1, "Book1_SWE", summarize, corr1=cor(lnCO2, lnPop))
Cor_Che=ddply(Book1, "Book1_CHE", summarize, corr1=cor(lnCO2, lnPop))
Cor_Tur=ddply(Book1, "Book1_TUR", summarize, corr1=cor(lnCO2, lnPop))
Cor_GBR=ddply(Book1, "Book1_GBR", summarize, corr1=cor(lnCO2, lnPop))
Cor_USA=ddply(Book1, "Book1_USA", summarize, corr1=cor(lnCO2, lnPop))
Cor_Chl=ddply(Book1, "Book1_CHL", summarize, corr1=cor(lnCO2, lnPop))
Cor_Chn=ddply(Book1, "Book1_CHN", summarize, corr1=cor(lnCO2, lnPop))
Cor_Idn=ddply(Book1, "Book1_IDN", summarize, corr1=cor(lnCO2, lnPop))
Cor_Isr=ddply(Book1, "Book1_ISR", summarize, corr1=cor(lnCO2, lnPop))
Cor_Rus=ddply(Book1, "Book1_RUS", summarize, corr1=cor(lnCO2, lnPop))
Cor_Zaf=ddply(Book1, "Book1_ZAF", summarize, corr1=cor(lnCO2, lnPop))
Cor_Bra=ddply(Book1, "Book1_BRA", summarize, corr1=cor(lnCO2, lnPop))
Cor_SAU=ddply(Book1, "Book1_SAU", summarize, corr1=cor(lnCO2, lnPop))
Cor_Arg=ddply(Book1, "Book1_ARG", summarize, corr1=cor(lnCO2, lnPop))
Cor_Cri=ddply(Book1, "Book1_CRI", summarize, corr1=cor(lnCO2, lnPop))
Cor_Mlt=ddply(Book1, "Book1_MLT", summarize, corr1=cor(lnCO2, lnPop))
df=data.frame(cbind(Cor_Arg,Cor_Aus,Cor_Aut, Cor_Bel, Cor_Bra, Cor_Can, Cor_Che, Cor_Chl, Cor_Chn, Cor_Cri, Cor_Deu, Cor_Esp, Cor_Fin, Cor_Fra, Cor_GBR, Cor_Grc, Cor_Hun, Cor_Idn, Cor_Irl, Cor_Isl, Cor_Isr, Cor_Ita, Cor_Jpn, Cor_Kor, Cor_Lux, Cor_Mex, Cor_Mlt, Cor_Nld, Cor_Nor, Cor_Nzl, Cor_Pol, Cor_Prt, Cor_Rus, Cor_SAU, Cor_Swe, Cor_Tur, Cor_USA, Cor_Zaf))
df
xyplot(lnCO2~lnPop |Year, groups = Country, data=Book1) #scatterplot of lnPop and lnCO2 by country over years
xyplot(lnCO2~Year, data=Book1) #scatterplot of lnPop and lnCO2 by country over years



model2= plm(lnCO2~ lnGDP + Unmplrate +lnPop +Book1$`15-64(%oftotal)`+Book1$`Urban(%oftotal)`, data=Book1, index=c("Country", "Year"), model="within")
summary(model2)
plmtest(model2, c("time"), type=("bp")) #testing time-fixed effects

simple= plm(lnCO2~lnPop + factor(Year), data=Book1, index=c("Country", "Year"), model="within")
summary(simple)

i= plm(lnGDP~lnPop + factor(Year), data=Book1, index=c("Country", "Year"), model="within")
summary(i)

model1= plm(lnCO2~lnPop + lnGDP + factor(Year), data=Book1, index=c("Country", "Year"), model="within")
summary(model1)

modelfull= plm(lnCO2~lnPop  + lnGDP +Unmplrate + Book1$`15-64(%oftotal)`+Book1$`Urban(%oftotal)`+ factor(Year), data=Book1, index=c("Country", "Year"), model="within")
summary(modelfull)

reduced= plm(lnCO2~lnPop  + lnGDP + Unmplrate +Book1$`Urban(%oftotal)` +factor(Year), data=Book1, index=c("Country", "Year"), model="within")
summary(reduced)

pcdtest(reduced, test = c("cd")) #testing for cross-sectional independence: residual across entities are not correlated, using Pasaran CD test
pbgtest(reduced) # testing for serial correlation
panel.set=plm.data(Book1, index = c("Country","Year"))
adf.test(panel.set$CO2, k=2) # testing for unit root/stationarity

coeftest(modelfull)
coeftest(modelfull, vcovHC(modelfull, method = "arellano")) #heteroskedasticity consistent coefficients and serial correlation

final= plm(lnCO2~lnPop  + lnGDP +Book1$`Urban(%oftotal)`+ factor(Year), data=Book1, index=c("Country", "Year"), model="within")
summary(final)
plot(final)


#Residual plots 
plotmeans(CO2 ~ Country, main="Heterogeineity across countries", data=Book1)
plotmeans(CO2~ Year, main="Heterogeineity across years", data=Book1)
hist(residuals(final), xlab='Residuals')
qqnorm(residuals(final, ylab='Residuals'))
qqline(residuals(final))
test=plm(lnCO2 ~ lnPop + lnGDP + Book1$`Urban(%oftotal)`, data = Book1, model = "within", index = c("Country", "Year"))
summary(test)
x=Book1[,c(48, 47, 8)]
yfit=as.matrix(x)%*%coef(test)
plot(yfit, residuals(test))

pool= plm(lnCO2 ~ lnPop + lnGDP + Unmplrate +Book1$`15-64(%oftotal)`+Book1$`Urban(%oftotal)`, data=Book1, index=c("Country", "Year"), model="pooling")
summary(pool)
final$coefficients
fixef(final) #Constants for each country
pFtest(model2, pool) #Testing for fixed effects, null: Pool is better than fixed
random <-plm(lnCO2~lnPop  + lnGDP + Unmplrate + Book1$`15-64(%oftotal)`+Book1$`Urban(%oftotal)`+ factor(Year), data=Book1, index=c("Country", "Year"), model="random")
summary(random)
phtest(model2, random) #Test for using fixed or random effects


vif(plm(lnCO2 ~ lnPop + lnGDP +Unmplrate + Book1$`15-64(%oftotal)`+Book1$`Urban(%oftotal)`+ factor(Year), data=Book1, index=c("Country", "Year"), model="pooling"))

sink()




