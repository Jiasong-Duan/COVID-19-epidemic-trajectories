############################################
# COVID-19 in South Korea
# modeling the epidemic process with semi-parametric gam model
# explore gender and age differences in the epidemic process
# original author: Jiasong Duan 
# revised using ggplots by: Xinhua Yu
# updated tests of interaction effects between gender groups and age groups individually by: Dr. Hongmei Zhang
# Date: 5-18-2020
############################################
rm(list = ls())
library(foreign)
library(dplyr)
library(ggplot2)
library(data.table)
library(incidence)
library(mgcv)



########### Subset data excluding Daegu, from line list files ##############

############ process Data #################
Pdaily = data.table(read.csv("PatientInfo.csv",header=TRUE))
str(Pdaily)

table(Pdaily$confirmed_date)
table(Pdaily$city)
table(Pdaily$province)
table(Pdaily$age)
table(Pdaily$sex)

# restricted to a limited variable sets
Tdaily = Pdaily[,.SD,.SDcols=c("sex","age","city","confirmed_date")] 
# recode variables and exclude missing date and city of Daedeok-gu
Tdaily[,case_date:=as.Date(confirmed_date)][city!="Daedeok-gu" & confirmed_date !="",]
setkey(Tdaily,"case_date")
# recode age groups, no 0 but using 1,2,3,4 for later model encoding 
Tdaily[,agegroup:=recode(age, .default=9,"0s"=1,"10s"=1,"20s"=2,"30s"=2,"40s"=3,"50s"=3,"60s"=4,"70s"=4,"80s"=4,"90s"=4,"100s"=4)]

# limit to cases between Feb 19 to May 1, 2020
STdaily=Tdaily[case_date>=as.Date("2020-02-19") & case_date<as.Date("2020-05-01"),]
# descriptive
st<-table(STdaily$sex)
st
margin.table(st)
prop.table(st)
table(STdaily$age)
at<-table(STdaily$agegroup)
at
prop.table(at)
agesext<-table(STdaily$agegroup,STdaily$sex)
agesext 
margin.table(agesext,2);
prop.table(agesext,2)
margin.table(agesext,1)
prop.table(agesext,1)

# create period, main and after the epidemic 
STdaily[,period:=ifelse(case_date<as.Date("2020-03-15"),1,2)]
pt<-table(STdaily$period)
pt; margin.table(pt);prop.table(pt)

#### total daily cases ####
setkeyv(STdaily,c("case_date"))
Newdaily = STdaily[,Dailycase:=.N, by=c("case_date")][, .SD[.N], by=c("case_date"),.SDcols=c("Dailycase")]

#### frequency by sex and date ####
setkeyv(STdaily,c("sex","case_date"))
# also exclude obs with missing gender 
genderdaily = STdaily[,Dailycase:=.N, by=c("sex","case_date")][, .SD[.N], by=c("sex","case_date"),.SDcols=c("Dailycase")][sex!="",]
# recode, numerical sex
genderdaily[,sex2:=recode(sex, "male"=1,"female"=2)]

maledaily = genderdaily[sex=="male",]
femaledaily = genderdaily[sex=="female",]

# padding NA to make the same length to all gender data
# avoid troubles in later data merge
maxlen2<-max(table(genderdaily$sex2))
maledaily = maledaily[1:maxlen2,]
femaledaily = femaledaily[1:maxlen2,]

#### frequency by agegroup and date ####
# make sure STdaily still has everybody
table(STdaily$sex)  # missing sex
table(STdaily$agegroup)
setkeyv(STdaily,c("agegroup","case_date"))
# exclude missing age group
agedaily = STdaily[,Dailycase:=.N, by=c("agegroup","case_date")][, .SD[.N], by=c("agegroup","case_date"),.SDcols=c("Dailycase")][agegroup!=9,]
age10daily = agedaily[agegroup==1,]
age30daily = agedaily[agegroup==2,]
age50daily = agedaily[agegroup==3,]
age60daily = agedaily[agegroup==4,]

# padding NA to make the same length to all age group data
maxlen<-max(table(agedaily$agegroup))
age10daily = age10daily[1:maxlen,]
age30daily = age30daily[1:maxlen,]
age50daily = age50daily[1:maxlen,]
age60daily = age60daily[1:maxlen,]

#########frequency by gender and age groups ################
table(STdaily$sex)  # missing sex
table(STdaily$agegroup)
setkeyv(STdaily,c("sex","agegroup","case_date"))

# exclude missing age group
sexagedaily = STdaily[,Dailycase:=.N, by=c("sex","agegroup","case_date")][, .SD[.N], by=c("sex","agegroup","case_date"),.SDcols=c("Dailycase")]
sexagedaily = sexagedaily[agegroup!=9 & sex!="",]
sexagedaily[,sex2:=recode(sex, "male"=1,"female"=2)]

maleagedaily = sexagedaily[sex=="male",]
femaleagedaily = sexagedaily[sex=="female",]

mage10daily = sexagedaily[agegroup==1 & sex=="male",]
mage30daily = sexagedaily[agegroup==2 & sex=="male",]
mage50daily = sexagedaily[agegroup==3 & sex=="male",]
mage60daily = sexagedaily[agegroup==4 & sex=="male",]

fage10daily = sexagedaily[agegroup==1 & sex=="female",]
fage30daily = sexagedaily[agegroup==2 & sex=="female",]
fage50daily = sexagedaily[agegroup==3 & sex=="female",]
fage60daily = sexagedaily[agegroup==4 & sex=="female",]

# padding NA to make the same length to all age group data
maxlen<-max(table(sexagedaily$sex,sexagedaily$agegroup))

maleagedaily = maleagedaily[1:maxlen,]
femaleagedaily = femaleagedaily[1:maxlen,]

mage10daily = mage10daily[1:maxlen,]
mage30daily = mage30daily[1:maxlen,]
mage50daily = mage50daily[1:maxlen,]
mage60daily = mage60daily[1:maxlen,]

fage10daily = fage10daily[1:maxlen,]
fage30daily = fage30daily[1:maxlen,]
fage50daily = fage50daily[1:maxlen,]
fage60daily = fage60daily[1:maxlen,]


############ Model for the subset data from line list file ############

##### function for gam model #######
# good for gender, age, and gender-age specific models
####################################
myfitgam <- function(grpdata){
  grpdata[,time:=seq_len(.N)]
  # rename date to be consistent with Jiasong's codes
  setnames(grpdata,"time","datevalue")
  
  Mixmodel <- gam(Dailycase~s(datevalue,bs="tp",k=8,m=2),family=nb,
                  data=grpdata,method="REML")
  
  print(summary(Mixmodel))
  print(anova(Mixmodel))
  
  plot(Mixmodel, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)
  
  newd <-data.frame(datevalue=grpdata$datevalue)
  
  # prediction + SE from NB model 
  fitTN <- predict( Mixmodel ,newd, type="response",se = TRUE )
  fitTN2 <- data.table(cbind(fitTN,newd))
  return(fitTN2)
}

############## total cases ####################

# plotting the epidemic curve with incidence package
covid_kr<-as.incidence(Newdaily$Dailycase,dates=Newdaily$case_date)
epi<-plot(covid_kr,color = "blue",border = "white", n_breaks=18)
# it is a ggplot object
epi + theme_bw() + scale_x_date(date_breaks="1 week",date_labels = "%b %d")

# recode time variable
Newdaily[,time:=seq_len(.N)]

# rename date to be consistent with Jiasong's codes
setnames(Newdaily,"time","datevalue")

# comparing Poisson and NB models with GAM;
# can try different methods, REML, GCV.Cp, ML 
# Poisson
STDmodel1b <- gam(Dailycase ~ s(datevalue,bs="tp",m=2,k=8),family=poisson,data=Newdaily,method="REML")
summary(STDmodel1b)

# NB model
STDmodel2b <- gam(Dailycase ~ s(datevalue,bs="tp",m=2,k=8),family=nb,data=Newdaily,method="REML")
summary(STDmodel2b)

# model comparsions based on AIC, comparing Poisson and NB
AIC(STDmodel1b,STDmodel2b)

# plot the models, NB model has larger confidence intervals
plot(STDmodel1b,shade = TRUE)
plot(STDmodel2b,shade = TRUE)

# plot raw, poission fit and NB fit in the same plot
# prefer GCV.Cp method, or REML method

# prediction + SE from Poisson model 
newd <-data.frame(datevalue=Newdaily$datevalue)
fitTP <- predict( STDmodel1b,newd, type="response",se = TRUE )$fit
seTP <- predict( STDmodel1b ,newd,type="response", se = TRUE)$se.fit

# prediction + SE from NB model 
fitTN <- predict( STDmodel2b ,newd, type="response",se = TRUE )$fit
seTN <- predict( STDmodel2b ,newd,type="response", se = TRUE)$se.fit

# pool all fitted data together;
fitdata <- data.frame(newdate=Newdaily$case_date,
                      fitTP = fitTP,
                      seTP = seTP,
                      fitTN = fitTN,
                      seTN = seTN,
                      Dailycase=Newdaily$Dailycase)

# plot epidemic curves
ggplot(fitdata,aes(newdate, Dailycase)) + 
  geom_point() + geom_line(aes(newdate, Dailycase,color="Observed")) +
  geom_line(aes(newdate, fitTP,color="Poisson"),size = 1) +
  geom_line(aes(newdate, fitTN,color="NegBin"),size=1) +
  geom_ribbon(aes(newdate, fitTP, ymin=fitTP-1.96*seTP, ymax=fitTP+1.96*seTP,fill="Poisson"), alpha=.3) +
  geom_ribbon(aes(newdate, fitTN, ymin=fitTN-1.96*seTN, ymax=fitTN+1.96*seTN,fill="NegBin"), alpha=.1) +
  scale_color_manual("Models",values=c("Observed" = "black","Poisson"="blue","NegBin"="red")) + theme_bw() +
  scale_fill_manual("95%CI",values=c("Poisson"="blue","NegBin"="red")) +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))


################# Gender ###############
# group gam model
# recode time variable
genderdaily[,time:=seq_len(.N),by=sex]
setnames(genderdaily,"time","datevalue")
# recode sex to be 0,1
genderdaily[,sex2:=recode(sex, "male"=1,"female"=2)]

Mixmodel <- gam(Dailycase~  s(datevalue,by=sex2, bs="tp", m=2,k=6) + s(sex2,bs="re",k=6),family=nb,
                data=genderdaily,method="REML")

summary(Mixmodel)
anova(Mixmodel)
summary(Mixmodel)$r.sq
# quite diferent smoothing terms
plot(Mixmodel, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)

newd <-data.frame(datevalue=genderdaily$datevalue,
                  sex2 = genderdaily$sex2)

# prediction + SE from NB model 
fitTN <- predict( Mixmodel ,newd, type="response",se = TRUE )
fitTN2 <- data.table(cbind(fitTN,newd))

# separate predicted values and plot them together
fitTN3<-dcast(fitTN2,datevalue ~ sex2,value.var=c("fit","se.fit") )

# dates from one gender 
sfitdata <- data.frame(newdate=femaledaily$case_date,
                      mfitTN = fitTN3$fit_1,
                      mseTN = fitTN3$se.fit_1,
                      ffitTN = fitTN3$fit_2,
                      fseTN = fitTN3$se.fit_2,
                      fDailycase=femaledaily$Dailycase,
                      mDailycase=maledaily$Dailycase)


ggplot(sfitdata,aes(newdate, fDailycase)) + 
  geom_point() + geom_line(aes(newdate, fDailycase,color="Female"),linetype="dashed") +
  geom_point(aes(newdate, mDailycase)) + geom_line(aes(newdate, mDailycase,color="Male"),linetype="dashed") +
  geom_line(aes(newdate, ffitTN,color="Female"),size = 1,linetype="solid") +
  geom_line(aes(newdate, mfitTN,color="Male"),size=1,linetype="solid") +
  scale_color_manual("Models",values=c("Female" = "red","Male"="blue")) + theme_bw() +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))


# comparing to overall model 
Mixmodel2 <- gam(Dailycase~  s(datevalue, bs="tp", m=2,k=6) + sex2 ,family=nb,
                data=genderdaily,method="REML")
summary(Mixmodel2)

# not work well for NB model
AIC(Mixmodel,Mixmodel2)


# Dr. Hongmei Zhang updated the below tests of interaction effects and main sex effect between genders groups
# testing for interaction
genderdaily[,sex2:=recode(as.factor(sex), "male"=1,"female"=0)]
genderdaily$sex2<-as.factor(genderdaily$sex2)
Mixmodel0 <- gam(Dailycase~ s(datevalue,bs="tp", k=8) + sex2+ s(datevalue,sex2,bs="re"),family=nb,
data=genderdaily,method="GCV.Cp")
summary(Mixmodel0)
Mixmodel0$gcv.ubre
# no statistically significant interaction

#testing for main sex effect.
Mixmodel0 <- gam(Dailycase~ s(datevalue,bs="tp", k=8) + sex2,family=nb,
data=genderdaily,method="GCV.Cp")
summary(Mixmodel0)
# statistically significant main effects.



############## by each gender ##################
# fit gender data separately and plot the fitted date together

fitTNf = myfitgam(femaledaily)
fitTNm = myfitgam(maledaily)

genderfit = data.frame(newdate=femaledaily$case_date,
                       ffitTN = fitTNf$fit,
                       fseTN = fitTNf$se.fit,
                       mfitTN = fitTNm$fit,
                       mseTN = fitTNm$se.fit,
                       fDailycase=femaledaily$Dailycase,
                       mDailycase=maledaily$Dailycase)

# NB
ggplot(genderfit,aes(newdate, fDailycase)) + 
  geom_point() + geom_line(aes(newdate, fDailycase,color="Female"),linetype="dashed") +
  geom_point(aes(newdate, mDailycase)) + geom_line(aes(newdate, mDailycase,color="Male"),linetype="dashed") +
  geom_line(aes(newdate, ffitTN,color="Female"),size = 1,linetype="solid") +
  geom_line(aes(newdate, mfitTN,color="Male"),size=1,linetype="solid") +
  scale_color_manual("Gender",values=c("Female" = "red","Male"="blue")) + theme_bw() +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))


################# Age Groups ###############
# gamm model
# recode time variable
agedaily[,time:=seq_len(.N),by=agegroup]
setnames(agedaily,"time","datevalue")
 
Mixmodel <- gam(Dailycase~ s(agegroup,bs="re",k=8) + s(datevalue,by=agegroup,m=2,bs="tp",k=8),family=nb,
                data=agedaily,method="REML")

summary(Mixmodel)
anova(Mixmodel)
summary(Mixmodel)$r.sq
# quite diferent smoothing terms
plot(Mixmodel, shade = TRUE, pages = 1, scale = 0, seWithMean = TRUE)


newd <-data.frame(datevalue=agedaily$datevalue,
                  agegroup = agedaily$agegroup)

# prediction + SE from NB model 
fitTN <- predict( Mixmodel ,newd, type="response",se = TRUE )
fitTN2 <- data.table(cbind(fitTN,newd))

# separate predicted values and plot them together
fitTN3<-dcast(fitTN2,datevalue ~ agegroup,value.var=c("fit","se.fit") )

# replace NA with 1 to avoid tail missing
fitTN3[,c("fit_1","fit_2","fit_3","fit_4"):=
              list(ifelse(is.na(fit_1),1,fit_1),
                    ifelse(is.na(fit_2),1,fit_2),
                    ifelse(is.na(fit_3),1,fit_3),
                    ifelse(is.na(fit_4),1,fit_4))]

# dates from one age group 
agefitdata <- data.frame(newdate=age10daily$case_date,
                      fitTN10 = fitTN3$fit_1,
                      seTN10 = fitTN3$se.fit_1,
                      fitTN30 = fitTN3$fit_2,
                      seTN30 = fitTN3$se.fit_2,
                      fitTN50 = fitTN3$fit_3,
                      seTN50 = fitTN3$se.fit_3,
                      fitTN60 = fitTN3$fit_4,
                      seTN60 = fitTN3$se.fit_4,
                      Dailycase10=age10daily$Dailycase,
                      Dailycase30=age30daily$Dailycase,
                      Dailycase50=age50daily$Dailycase,
                      Dailycase60=age60daily$Dailycase)

ggplot(agefitdata,aes(newdate, Dailycase10)) + 
  geom_point() + geom_line(aes(newdate, Dailycase10,color="0-19"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase30)) + geom_line(aes(newdate, Dailycase30,color="20-39"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase50)) + geom_line(aes(newdate, Dailycase50,color="40-59"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase60)) + geom_line(aes(newdate, Dailycase60,color="60+"),linetype="dashed") +
  geom_line(aes(newdate, fitTN10,color="0-19"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN30,color="20-39"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN50,color="40-59"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN60,color="60+"),size = 1,linetype="solid") +
  scale_color_manual("Age groups",values=c("0-19" = "green","20-39"="blue","40-59"="orange","60+"="red")) + theme_bw() +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))



# comparing to overall model 
Mixmodel0 <- gam(Dailycase~s(datevalue, bs="tp",m=2,k=8) + s(agegroup,bs="re",k=8) + s(datevalue,by=agegroup,m=1,bs="tp",k=8),family=nb,
                data=agedaily,method="REML")
summary(Mixmodel0)

Mixmodel2 <- gam(Dailycase~  s(datevalue, bs="tp", m=2,k=6) + agegroup ,family=nb,
                 data=agedaily,method="REML")
summary(Mixmodel2)

AIC(Mixmodel,Mixmodel2)

# Dr. Hongmei Zhang updated the below test of interaction effects between age groups
# interactions
agedaily$agegroup<-as.factor(agedaily$agegroup) 
Mixmodel0 <- gam(Dailycase~s(datevalue, bs="tp",k=8) + agegroup + s(datevalue, agegroup,bs="re"),family=nb,
data=agedaily,method="GCV.Cp")

summary(Mixmodel0)
# statistically significant interaction effect


########### by each age group ################

gfitTN10 = myfitgam(age10daily)
gfitTN30 = myfitgam(age30daily)
gfitTN50 = myfitgam(age50daily)
gfitTN60 = myfitgam(age60daily)

# dates from one age group 
gfitdata <- data.frame(newdate=age10daily$case_date,
                      fitTN10 = gfitTN10$fit,
                      seTN10 = gfitTN10$se.fit,
                      fitTN30 = gfitTN30$fit,
                      seTN30 = gfitTN30$se.fit,
                      fitTN50 = gfitTN50$fit,
                      seTN50 = gfitTN50$se.fit,
                      fitTN60 = gfitTN60$fit,
                      seTN60 = gfitTN60$se.fit,
                      Dailycase10=age10daily$Dailycase,
                      Dailycase30=age30daily$Dailycase,
                      Dailycase50=age50daily$Dailycase,
                      Dailycase60=age60daily$Dailycase)

ggplot(gfitdata,aes(newdate, Dailycase10)) + 
  geom_point() + geom_line(aes(newdate, Dailycase10,color="0-19"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase30)) + geom_line(aes(newdate, Dailycase30,color="20-39"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase50)) + geom_line(aes(newdate, Dailycase50,color="40-59"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase60)) + geom_line(aes(newdate, Dailycase60,color="60+"),linetype="dashed") +
  geom_line(aes(newdate, fitTN10,color="0-19"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN30,color="20-39"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN50,color="40-59"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN60,color="60+"),size = 1,linetype="solid") +
  scale_color_manual("Age groups",values=c("0-19" = "green","20-39"="blue","40-59"="orange","60+"="red")) + theme_bw() +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))


####### By sex and age group ##################
### male 

maleagedaily[,time:=seq_len(.N),by=agegroup]
setnames(maleagedaily,"time","datevalue")

#overall
Mixmodel <- gam(Dailycase~ s(agegroup,bs="re",k=8) + s(datevalue,by=agegroup,m=2,bs="tp",k=8),family=nb,
                 data=maleagedaily,method="REML")
summary(Mixmodel)

#interaction
Mixmodel0 <- gam(Dailycase~s(datevalue, bs="tp",m=2,k=8) + s(agegroup,bs="re",k=8) + s(datevalue,by=agegroup,m=1,bs="tp",k=8),family=nb,
                 data=maleagedaily,method="GCV.Cp")
summary(Mixmodel0)
Mixmodel0$gcv.ubre

# separate age model
mfitTN10 = myfitgam(mage10daily)
mfitTN30 = myfitgam(mage30daily)
mfitTN50 = myfitgam(mage50daily)
mfitTN60 = myfitgam(mage60daily)

# dates from one age group 
mfitdata <- data.frame(newdate=mage10daily$case_date,
                      fitTN10 = mfitTN10$fit,
                      seTN10 = mfitTN10$se.fit,
                      fitTN30 = mfitTN30$fit,
                      seTN30 = mfitTN30$se.fit,
                      fitTN50 = mfitTN50$fit,
                      seTN50 = mfitTN50$se.fit,
                      fitTN60 = mfitTN60$fit,
                      seTN60 = mfitTN60$se.fit,
                      Dailycase10=mage10daily$Dailycase,
                      Dailycase30=mage30daily$Dailycase,
                      Dailycase50=mage50daily$Dailycase,
                      Dailycase60=mage60daily$Dailycase)

ggplot(mfitdata,aes(newdate, Dailycase10)) + 
  geom_point() + geom_line(aes(newdate, Dailycase10,color="0-19"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase30)) + geom_line(aes(newdate, Dailycase30,color="20-39"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase50)) + geom_line(aes(newdate, Dailycase50,color="40-59"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase60)) + geom_line(aes(newdate, Dailycase60,color="60+"),linetype="dashed") +
  geom_line(aes(newdate, fitTN10,color="0-19"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN30,color="20-39"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN50,color="40-59"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN60,color="60+"),size = 1,linetype="solid") +
  scale_color_manual("Age groups",values=c("0-19" = "green","20-39"="blue","40-59"="orange","60+"="red")) + theme_bw() +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))



#### female 
femaleagedaily[,time:=seq_len(.N),by=agegroup]
setnames(femaleagedaily,"time","datevalue")

# overall model
Mixmodel <- gam(Dailycase~ s(agegroup,bs="re",k=8) + s(datevalue,by=agegroup,m=2,bs="tp",k=8),family=nb,
                data=femaleagedaily,method="REML")
summary(Mixmodel)

# interaction
Mixmodel0 <- gam(Dailycase~s(datevalue, bs="tp",m=2,k=8) + s(agegroup,bs="re",k=8) + s(datevalue,by=agegroup,m=1,bs="tp",k=8),family=nb,
                 data=femaleagedaily,method="GCV.Cp")
summary(Mixmodel0)
Mixmodel0$gcv.ubre

# separate age model
ffitTN10 = myfitgam(fage10daily)
ffitTN30 = myfitgam(fage30daily)
ffitTN50 = myfitgam(fage50daily)
ffitTN60 = myfitgam(fage60daily)

# dates from one age group 
ffitdata <- data.frame(newdate=fage10daily$case_date,
                      fitTN10 = ffitTN10$fit,
                      seTN10 = ffitTN10$se.fit,
                      fitTN30 = ffitTN30$fit,
                      seTN30 = ffitTN30$se.fit,
                      fitTN50 = ffitTN50$fit,
                      seTN50 = ffitTN50$se.fit,
                      fitTN60 = ffitTN60$fit,
                      seTN60 = ffitTN60$se.fit,
                      Dailycase10=fage10daily$Dailycase,
                      Dailycase30=fage30daily$Dailycase,
                      Dailycase50=fage50daily$Dailycase,
                      Dailycase60=fage60daily$Dailycase)

ggplot(ffitdata,aes(newdate, Dailycase10)) + 
  geom_point() + geom_line(aes(newdate, Dailycase10,color="0-19"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase30)) + geom_line(aes(newdate, Dailycase30,color="20-39"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase50)) + geom_line(aes(newdate, Dailycase50,color="40-59"),linetype="dashed") +
  geom_point(aes(newdate, Dailycase60)) + geom_line(aes(newdate, Dailycase60,color="60+"),linetype="dashed") +
  geom_line(aes(newdate, fitTN10,color="0-19"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN30,color="20-39"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN50,color="40-59"),size = 1,linetype="solid") +
  geom_line(aes(newdate, fitTN60,color="60+"),size = 1,linetype="solid") +
  scale_color_manual("Age groups",values=c("0-19" = "green","20-39"="blue","40-59"="orange","60+"="red")) + theme_bw() +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="7 days",date_labels = "%b %d") + theme(legend.position=c(.8,.7))







############################################
####### Total COVID-19 cases ###############
############################################

Tdaily=data.table(read.csv("Time.csv",header=TRUE))
Tdaily[,case_date:=as.Date(date)]
setkey(Tdaily,"case_date")

# calculate new cases and new deaths, rename variables
Tdaily[,c("newcases","newdeaths"):=list(confirmed-shift(confirmed),deceased-shift(deceased))]
setnames(Tdaily,c("confirmed","deceased"),c("cases","deaths"))

# we started on Feb 19, 2020 till May 1, 2020
STdaily=Tdaily[case_date>=as.Date("2020-02-19") & case_date<as.Date("2020-05-01"),]
summary(STdaily)

# recode time variable
STdaily[,time:=seq_len(.N)]

# plotting the epidemic curve with incidence package
covid_kr<-as.incidence(STdaily$newcases,dates=STdaily$case_date)
epi<-plot(covid_kr,color = "blue",border = "white", n_breaks=18)
# it is a ggplot object
epi + theme_bw() + scale_x_date(date_breaks="1 week",date_labels = "%b %d")

# rename date to be consistent with Jiasong's codes
setnames(STdaily,c("newcases","time"),c("Dailycase","datevalue"))

# comparing Poisson and NB models with GAM;
STDmodel1a <- gam(Dailycase ~ s(datevalue),family=poisson,data=STdaily,method="REML")
summary(STDmodel1a)

# try other methods, GCV.Cp, 
STDmodel1b <- gam(Dailycase ~ s(datevalue),family=poisson,data=STdaily,method="GCV.Cp")
summary(STDmodel1b)
#and ML
STDmodel1c <- gam(Dailycase ~ s(datevalue),family=poisson,data=STdaily,method="ML")
summary(STDmodel1c)

# NB model
STDmodel2a <- gam(Dailycase ~ s(datevalue),family=nb,data=STdaily,method="REML")
summary(STDmodel2a)

STDmodel2b <- gam(Dailycase ~ s(datevalue),family=nb,data=STdaily,method="GCV.Cp")
summary(STDmodel2b)

STDmodel2c <- gam(Dailycase ~ s(datevalue),family=nb,data=STdaily,method="ML")
summary(STDmodel2c)

# model comparsions based on AIC
AIC(STDmodel1a,STDmodel1b,STDmodel1c)
AIC(STDmodel2a,STDmodel2b,STDmodel2c)
# comparing Poisson and NB
AIC(STDmodel1b,STDmodel2b)

# plot the models, NB model has larger confidence intervals
plot(STDmodel1b,shade = TRUE)
plot(STDmodel2b,shade = TRUE)

# plot raw, poission fit and NB fit in the same plot
# prefer GCV.Cp method
# prediction + SE from Poisson model 
newd <-data.frame(datevalue=STdaily$datevalue)
fitTP <- predict( STDmodel1b,newd, type="response",se = TRUE )$fit
seTP <- predict( STDmodel1b ,newd,type="response", se = TRUE)$se.fit

# prediction + SE from NB model 
fitTN <- predict( STDmodel2b ,newd, type="response",se = TRUE )$fit
seTN <- predict( STDmodel2b ,newd,type="response", se = TRUE)$se.fit

# pool all fitted data together;
tfitdata <- data.frame(newdate=STdaily$case_date,
                      fitTP = fitTP,
                      seTP = seTP,
                      fitTN = fitTN,
                      seTN = seTN,
                      Dailycase=STdaily$Dailycase)

# plot epidemic curves
ggplot(tfitdata,aes(newdate, Dailycase)) + 
  geom_point() + geom_line(aes(newdate, Dailycase,color="Observed")) +
  geom_line(aes(newdate, fitTP,color="Poisson"),size = 1) +
  geom_line(aes(newdate, fitTN,color="NegBin"),size=1) +
  geom_ribbon(aes(newdate, fitTP, ymin=fitTP-1.96*seTP, ymax=fitTP+1.96*seTP,fill="Poisson"), alpha=.3) +
  geom_ribbon(aes(newdate, fitTN, ymin=fitTN-1.96*seTN, ymax=fitTN+1.96*seTN,fill="NegBin"), alpha=.1) +
  scale_color_manual("Models",values=c("Observed" = "black","Poisson"="blue","NegBin"="red")) + theme_bw() +
  scale_fill_manual("95%CI",values=c("Poisson"="blue","NegBin"="red")) +
  labs(x = "Date",y="Daily new cases") + scale_x_date(date_breaks="1 week",date_labels = "%b %d") + theme(legend.position=c(.8,.7))

