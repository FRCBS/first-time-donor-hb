library(dplyr)
library(lubridate)
library(tidyr)
library(moments)
library(openxlsx)
library(survival)

## ----parameters-----------------
param=list()
param$wd = getwd()

# getwd() might behave differently depending on the environment where it is run (console vs. Rmd),
# therefore checking if working directory is set to the src folder and moving up if yes.
if (grepl('[/\\]src[/\\]?',param$wd)) {
   param$wd = sub('[/\\]src([/\\]?)$','\\1',param$wd)
}

dir.create(file.path(param$wd,"results"),showWarnings = FALSE)
dir.create(file.path(param$wd,"log"),showWarnings = FALSE)
param$result.file = file.path(param$wd,"results","exported-data.xlsx")

# nb! Please edit your country code below
param$country = 'NV'

param$omit.data=list()
param$max.ord.group.number=15
param$max.sample.size=1e7 # This is still reasonably fast (< 1 min)

param$include.na = TRUE
param$minimum.group.size = 5
param$donation.type.keys = NULL

param$age.minimum = 16
param$age.maximum = 75
param$hb.minimum = -Inf
param$hb.maximum = Inf

# This is the default file location. If you are using a country-specific file
# name or path, please specify it below under country-specific parameters.
# nb! If donationdata is already available in memory, the following line need not be run.
param$data.file = file.path(param$wd,'donationdata.Rdata')

# The number of intended decimals in Hb values
# Typically 0 for g/L, 1 for g/dL and mmol/L
param$hb.decimals = NA

param$donor.cols = c('DateOfBirth','Sex','BloodGroup')
param$donation.cols = c()

# conversion factor from mmol/L to g/L: cf = 10 / 0.6206, see import-data.Rmd for details

# always return 0 (if no hour is available)
# nb! In order to extract hourly data, this function should be redefined as a
# country-specific parameter
param$extractHour = function(simple) {return(0)} 

param$omit.data=list()

# Country-specific parameter settings
if (param$country == 'NL') {
  # param$data.file = '~/data/data_timo.rdata'
  param$data.file = '~/data/donor_prediction_data_timo/donationdata_only_vb.Rdata'
  param$units = 'mmol/L' # one of: mmol/L, or g/L or g/dL
  param$cutoff.male = 8.4 # will need to convert these to correct units
  param$cutoff.female = 7.8
  param$data.sets = c('donation0','donation.r','simple')
  # for NL the correct Sex and DateofBirth is in donations... sorry this is a bit cumbersome
  param$hb.minimum = 1
  param$hb.maximum = 20
  param$hb.decimals = 1 #just to be sure
  # MP: Funky hour extraction for NL...
  param$extractHour = function(simple){ 
    return(ifelse(is.na(simple$DonationTimeStart), NA_integer_, as.integer(substr(simple$DonationTimeStart, 1, 2))))
  }
  param$donor.cols = c('BloodGroup')
  param$donation.cols = c('Sex','DateOfBirth','DonationTimeStart')

  # 2026-01-26 For survival analysis, the data should be restricted to successful first-time donations
  param$donation.type.keys.survival=c("Whole Blood (K)")
} else if (param$country == 'FI') {
  param$data.file = 'C:/git_repot/DATA/donationdata.fortimo.rdata'
  param$units = 'g/L' # one of: mmol/L or g/L or g/dL
  param$cutoff.male = 135
  param$cutoff.female = 125
  param$hb.minimum = 50
  param$hb.maximum = 250
  param$data.sets = c('donation0','donation.r','simple')
  param$donation.type.keys = c('Whole Blood (K)','No Donation (E)','VisitNoDonation')
  param$donation.type.keys.survival=c("Whole Blood (K)")
  param$hb.decimals = 0
  param$extractHour = function(simple) {
    return(as.integer(format(simple$DonationTimeDTTM,'%H')))
  }
  param$donor.cols = c('DateOfBirth','Sex','BloodGroup')
  param$donation.cols = c('DonationTimeDTTM')

  param$omit.data$DonationPlaceType='Garrison'

  # For Finland, omit the donations from garrisons to create a second version of the data set
  param$omit.data$DonationPlaceType='Garrison'
} else if (param$country == 'xx') {
  # nb! edit your settings here
  param$data.file = 'donationdata.rdata'
  param$units = 'g/L' # one of: mmol/L or g/L or g/dL
  param$cutoff.male = 135
  param$cutoff.female = 125
  param$hb.minimum = 50
  param$hb.maximum = 250
  param$data.sets = c('donation0','donation.r','simple')
  param$donation.type.keys = c('Whole Blood (K)','No Donation (E)','VisitNoDonation')

  # 2026-01-26 For survival analysis, the data should be restricted to successful first-time donations
  param$donation.type.keys.survival=c("Whole Blood (K)")

  param$hb.decimals = 0
  param$extractHour = function(simple) {
    return(as.integer(format(simple$DonationTimeDTTM,'%H')))
  }
  param$donor.cols = c('DateOfBirth','Sex','BloodGroup')
  param$donation.cols = c('DonationTimeDTTM')
} else if (param$country == "NV"){
  # nb! edit your settings here
  param$data.file = '~/data/navarra/donationdata.Rdata'
  param$units = 'g/dL' # one of: mmol/L or g/L or g/dL
  param$cutoff.male = 13.5 #Not sure!
  param$cutoff.female = 12.5 #Not sure!
  param$hb.minimum = 5.0
  param$hb.maximum = 25.0
  # MP: Funky hour extraction for NL...
  param$extractHour = function(simple){ 
    return(ifelse(is.na(simple$DonationTimeStart), NA_integer_, as.integer(substr(simple$DonationTimeStart, 1, 2))))
  }
  param$data.sets = c('donation0','donation.r','simple')
  param$donor.cols = c('BloodGroup', 'Sex', 'DateOfBirth')
  param$donation.cols = c('DonationTimeStart')
  param$hb.decimals = 1
  # 2026-01-26 For survival analysis, the data should be restricted to successful first-time donations
  param$donation.type.keys.survival=c("Whole Blood (K)")
}

# automated reasoning in case param$hb.decimals is not set
if (is.na(param$hb.decimals)) {
  if (param$units %in% c('mmol/L','g/dL'))
    param$hb.decimals = 1
  else
    param$hb.decimals = 0
}


## ----load-data------------
# The data is expected to be in the same for as in the long-term forecasts project at
# https://github.com/FRCBS/donor-recruitment-prediction/
# except for the column 'Hb' added in the donation table. The units used in Finland is g/l.
# nb! If donationdata is already available in memory, this chunk/the following line need not be run.
if (!exists('donationdata'))
	load(param$data.file)

charid=unique(donationdata$donor$releaseID)
id.map = data.frame(charid=charid,numid=1:length(charid))
rownames(id.map)=id.map$charid

donationdata$donation$numid=id.map[donationdata$donation$releaseID,'numid']
donationdata$donor$numid=id.map[donationdata$donor$releaseID,'numid']

donationdata$donation$rowid=1e7+1:nrow(donationdata$donation) #as.integer(rownames(donationdata$donation))
donationdata$donor$rowid=2e5+1:nrow(donationdata$donor) # as.integer(rownames(donationdata$donor))

donationdata$donation$DonationPlaceType[is.na(donationdata$donation$DonationPlaceType)]='Office'
for (nm in names(param$omit.data)) {
	donationdata$donation=donationdata$donation %>%
		filter(as.character(!!!syms(nm))!=param$omit.data[[nm]])
}

## ----load-and-process-data
if (is.null(param$donation.type.keys)) 
  param$donation.type.keys = unique(donationdata$donation$BloodDonationTypeKey)

simple = donationdata$donation[, c("releaseID", "BloodDonationTypeKey", "DonationDate", 
                  "DonationPlaceType", "DonationPlaceCode", "Hb", param$donation.cols)] %>%
  filter(param$include.na || !is.na(Hb)) %>%
  filter(BloodDonationTypeKey %in% param$donation.type.keys) %>%
  # VisitNoDonation is used in Finland during the recent years instead of No Donation (E)
  left_join(donationdata$donor[,c('releaseID',param$donor.cols)],by='releaseID')
simple$age= as.numeric(difftime(simple$DonationDate,simple$DateOfBirth),unit="weeks")/52.25
simple$month = as.factor(month(simple$DonationDate))
simple$year = year(simple$DonationDate)
simple$year.factor = as.factor(simple$year)

# 2025-05-15 New line to convert the new datetime column to integer for hour)
simple$hour = param$extractHour(simple)

simple = simple %>%
  filter(age >= param$age.minimum, age <= param$age.maximum) %>%
  filter(Hb >= param$hb.minimum, Hb <= param$hb.maximum) %>%
  mutate(Hb = round(Hb,param$hb.decimals)) %>%
  filter(Sex %in% c('Female','Male')) 

simple$limit = param$cutoff.male
simple$limit[simple$Sex=='Female'] = param$cutoff.female
simple$deferred=0
simple$deferred[simple$Hb<simple$limit]=1

#Filter nan sex
simple <- simple %>% filter(!is.na(Sex))

#For NL should take only BloodDonationTypeKey == "N" for first donation
# date of first donation
#The filter(param$country != NL || BloodDonationTypeKey == "New") gave an error for me so changed to this
if (param$country == "NL") {
  date0 = simple %>%
    # filter(BloodDonationTypeKey == "New") %>%
    group_by(releaseID) %>%
    summarise(date0=min(DonationDate),.groups='drop')
  } else {
    date0 = simple %>%
    group_by(releaseID) %>%
    summarise(date0=min(DonationDate),.groups='drop')
}

# first-time donations
donation0 = simple %>%
  inner_join(date0,join_by(x$DonationDate==y$date0,releaseID))

# repeat donations; sent for sentinel
donation.r = simple %>%
  left_join(cbind(date0,sent=1),join_by(x$DonationDate==y$date0,releaseID)) %>%
  filter(is.na(sent)) %>%
  select(-sent) %>%
  mutate(month=as.integer(month))

# donation0$year=year(donation0$DonationDate)
# donation0$year.factor=as.factor(donation0$year)


## ----prepare-for-export---
# Annual Hb distributions by Sex
data.sets= param$data.sets # c('donation0','donation.r','simple')
hb.freq = NULL
for (ds in data.sets) {
  freq0 = get(ds) %>%
    mutate(data.set=if(ds == 'simple') 'all' else ds) %>%
    mutate(month=as.integer(month)) %>%
    group_by(data.set,year,Sex,Hb) %>%
    summarise(n=n(),nas=sum(1*is.na(Hb)),deferred=sum(deferred,na.rm=TRUE),
              mean.hour=mean(hour,na.rm=TRUE),sd.hour=sd(hour,na.rm=TRUE),
              mean.month=mean(month,na.rm=TRUE),sd.month=sd(month,na.rm=TRUE),
              mean.age=mean(age,na.rm=TRUE),sd.age=sd(age,na.rm=TRUE),limit=min(limit),.groups='drop')
  hb.freq = if(is.null(hb.freq)) freq0 else rbind(hb.freq,freq0)
}

anonymizeDistribution = function(freq) {
  # freq = hb.freq
  hb.inf = 1e6
  wh.sub = which(freq$n < param$minimum.group.size & freq$Hb < freq$limit)
  wh.super = which(freq$n < param$minimum.group.size & freq$Hb > freq$limit)
  
  freq$Hb[wh.sub] = -hb.inf
  freq$Hb[wh.super] = hb.inf
  
  aggregated = freq[c(wh.sub,wh.super),] %>%
    mutate(sgn=sign(Hb)) %>%
    group_by(data.set,year,Sex,sgn) %>%
    summarise(Hb=min(sgn)*hb.inf,n2=sum(n),nas=sum(nas),deferred=sum(deferred),
              mean.age=sum(n*mean.age)/sum(n),sd.age=NA,
              mean.hour=sum(n*mean.hour)/sum(n),sd.hour=NA,
              mean.month=sum(n*mean.month)/sum(n),sd.month=NA,
              limit=min(limit),.groups='drop') %>%
    dplyr::select(-sgn) %>%
    rename(n=n2)

  freq=freq[-c(wh.sub,wh.super),]
  freq = rbind(freq,aggregated)
  freq = freq[freq$n >= param$minimum.group.size,]
  freq[with(freq,order(data.set,year,Sex,Hb)),]
}

hb.freq = anonymizeDistribution(hb.freq)

# Annual age distributions by Sex
data.sets=c('donation0','donation.r','simple')
age.freq = NULL
for (ds in data.sets) {
  freq0 = get(ds) %>%
    mutate(data.set=if(ds == 'simple') 'all' else ds,age=as.integer(age)) %>%
    group_by(data.set,year,Sex,age) %>%
    summarise(n=n(),nas=sum(1*is.na(Hb)),deferred=sum(deferred,na.rm=TRUE),mean.hb=mean(Hb,na.rm=TRUE),sd.hb=sd(Hb,na.rm=TRUE),.groups='drop') %>%
    filter(n >= param$minimum.group.size)
  age.freq = if(is.null(age.freq)) freq0 else rbind(age.freq,freq0)
}

# Month statistics
monthly = NULL
for (ds in data.sets) {
  monthly0 = get(ds) %>%
    mutate(data.set=ds) %>%
    group_by(data.set,year,Sex,month) %>%
    summarise(n=n(),nas=sum(1*is.na(Hb)),deferred=sum(deferred,na.rm=TRUE),mean=mean(Hb,na.rm=TRUE),sd=sd(Hb,na.rm=TRUE),
              mean.age=mean(age,na.rm=TRUE),sd.age=sd(age,na.rm=TRUE),nas=sum(1*is.na(Hb)),.groups='drop') %>%
    filter(n >= param$minimum.group.size) %>%
    arrange(year,Sex,month)
  monthly = if(is.null(monthly)) monthly0 else rbind(monthly,monthly0)
}

# Hourly statistics
hourly = NULL
for (ds in data.sets) {
  hourly0 = get(ds) %>%
    mutate(data.set=ds) %>%
    group_by(data.set,year,Sex,hour) %>%
    summarise(n=n(),nas=sum(1*is.na(Hb)),deferred=sum(deferred,na.rm=TRUE),mean=mean(Hb,na.rm=TRUE),sd=sd(Hb,na.rm=TRUE),
              mean.age=mean(age,na.rm=TRUE),sd.age=sd(age,na.rm=TRUE),nas=sum(1*is.na(Hb)),.groups='drop') %>%
    filter(n >= param$minimum.group.size) %>%
    arrange(year,Sex,hour)
  hourly = if(is.null(hourly)) hourly0 else rbind(hourly,hourly0)
}


## ----export-data----------
write.xlsx(list(parameters=data.frame(name=names(param),value=paste(param,sep=',')),
                annual.hb=hb.freq,annual.age=age.freq,montly.statistics=monthly,hourly.statistics=hourly),
           file=sub('\\.xlsx$','-hb.xlsx',param$result.file))

# remove unnecessary objects to free up some memory
# sort( sapply(ls(),function(x){object.size(get(x))})) 
rm(list=c('simple','donation.r','donation0'))

#####################
# process survival data

# nb! This leads to smoother results, although not sure if it is justified
# dim(dlink) 
# [1] 6516688      23 # the not-so-smooth case
# [1] 5955572      24 # the smooth case
charid=unique(donationdata$donor$releaseID)
id.map = data.frame(charid=charid,numid=1:length(charid))
rownames(id.map)=id.map$charid

donationdata$donation$numid=id.map[donationdata$donation$releaseID,'numid']
donationdata$donor$numid=id.map[donationdata$donor$releaseID,'numid']

donationdata$donation$rowid=1e7+1:nrow(donationdata$donation) #as.integer(rownames(donationdata$donation))
donationdata$donor$rowid=2e5+1:nrow(donationdata$donor) # as.integer(rownames(donationdata$donor))

donationdata$donation$DonationPlaceType[is.na(donationdata$donation$DonationPlaceType)]='Office'
for (nm in names(param$omit.data)) {
	donationdata$donation=donationdata$donation %>%
		filter(as.character(!!!syms(nm))!=param$omit.data[[nm]])
}

# new variant adapted from the new export-data.file
donation.simple = donationdata$donation[, c('rowid','numid',"BloodDonationTypeKey", "DonationDate", 
                  "Hb", param$donation.cols)] %>% #  "DonationPlaceType","DonationPlaceCode"
  filter(param$include.na || !is.na(Hb)) %>% #  # nb! must check again
  filter(BloodDonationTypeKey %in% param$donation.type.keys.survival) %>%
  # VisitNoDonation is used in Finland during the recent years instead of No Donation (E)
  left_join(donationdata$donor[,c('numid',param$donor.cols)],join_by(numid)) %>%
  arrange(numid,DonationDate) %>%
  dplyr::select(-BloodDonationTypeKey)

# 2026-02-07 remove missing values for key variables
for (cn in c('Sex','DonationDate')) { # 'Hb',
	wh=which(is.na(donation.simple[[cn]]))
	if (length(wh) > 0) {
		donation.simple=donation.simple[-wh,]
	}
	param[[paste0('na.lines.',cn)]]=length(wh)
}

wh=which(!is.na(donation.simple$Hb)&donation.simple$Hb<param$hb.minimum)
if (length(wh) > 0) {
	donation.simple=donation.simple[-wh,]
}
param$omit.hb.low=length(wh)

wh=which(!is.na(donation.simple$Hb)&donation.simple$Hb>param$hb.maximum)
if (length(wh) > 0) {
	donation.simple=donation.simple[-wh,]
}
param$omit.hb.high=length(wh)

wh=which(!donation.simple$Sex %in% c('Female','Male'))
if (length(wh) > 0) {
	donation.simple=donation.simple[-wh,]
}
param$omit.empty.or.other.sex=length(wh)

donation.simple = donation.simple %>% 
	group_by(numid) %>%
	mutate(ord = row_number()) %>%
	ungroup() %>%
	rename(date=DonationDate)

donation0=donation.simple %>% filter(ord==1) %>% dplyr::select(numid,date) %>% rename(date0=date)
donation.simple=donation.simple %>%
	inner_join(donation0,join_by(numid))

colnames(donation.simple)=tolower(colnames(donation.simple))

donation.simple$bloodgr='other'
donation.simple$bloodgr[donation.simple$bloodgroup=='O-']='O-'
donation.simple$bloodgr=as.factor(donation.simple$bloodgr)
donation.simple$bloodgr=relevel(donation.simple$bloodgr,ref='other')

# making sure that the donation numbers are counted correctly
# exclude donor that had their first donation during the first two years of data
# except for those who were under 18 year of age at date start
dt.min=min(donation.simple$date)

param$dt.min=dt.min

dt.cutoff=dt.min+2*7*52.25
wh=which(donation.simple$date0<dt.cutoff)
exclude.donors=unique(donation.simple$numid[wh])
donation.simple$age.at.dt.min=as.numeric(difftime(dt.min,donation.simple$dateofbirth),unit="weeks")/52.25
include.donors=unique(donation.simple$numid[which(donation.simple$age.at.dt.min<18)])
hist(donation.simple$age.at.dt.min)
final.exclude=setdiff(exclude.donors,include.donors)

donation.simple = donation.simple %>% filter(!numid %in% final.exclude)

donation.simple$age=as.numeric(difftime(donation.simple$date0,donation.simple$dateofbirth),unit="weeks")/52.25
donation.simple$age.group=cut(donation.simple$age,breaks=c(0,25,40,100))

donation.simple$age.t=as.numeric(difftime(donation.simple$date,donation.simple$dateofbirth),unit="weeks")/52.25
donation.simple$age.group.t=cut(donation.simple$age,breaks=c(seq(15,60,by=5),100))
donation.simple$age.group.t=relevel(donation.simple$age.group.t,ref='(40,45]')

donation.simple$age.stats=round(donation.simple$age,0) # cut(donation.simple$age,breaks=15:75)
donation.simple$age.stats.t=round(donation.simple$age.t,0) # cut(donation.simple$age,breaks=15:75)

# nb! should include stats as well in the results
# maybe a full age distribution (per year) of first and all donations
stats.age=donation.simple %>%
	group_by(sex,age.stats) %>%
	summarise(n=n(),mean.hb=mean(hb,na.rm=TRUE),.groups='drop') %>%
	filter(n>=30) %>%
	data.frame()

stats.age.t=donation.simple %>%
	group_by(sex,age.stats.t) %>%
	summarise(n=n(),mean.hb=mean(hb,na.rm=TRUE),.groups='drop') %>%
	filter(n>=30) %>%
	data.frame()

stats.ord=donation.simple %>%
	group_by(sex,ord) %>%
	summarise(n=n(),mean.hb=mean(hb,na.rm=TRUE),.groups='drop') %>%
	filter(n>=30) %>%
	data.frame()

donation.simple$age.stats=NULL
donation.simple$age.stats.t=NULL

donation.simple$ord.next=donation.simple$ord+1
donation.simple$ord.prev=donation.simple$ord-1

# Compute the running mean hb value
# This might be useful in analysing optouts
# In addition to the level, change, change wrt. the mean, distance from the threshold
# Maybe unsuccessful donations should be included as well? somehow
donation.simple = donation.simple %>%
	mutate(trsum=cumsum(hb),.by=numid) %>%
	mutate(hb.avg=trsum/ord) %>%
	dplyr::select(-trsum)

donation.simple = donation.simple %>% 
	mutate(avg.before=(hb.avg*ord-hb)/(ord-1)) %>%
	mutate(avg.diff=hb-avg.before)

donation.simple$hb.thold=param$cutoff.female
donation.simple$hb.thold[donation.simple$sex=='Male']=param$cutoff.male
donation.simple$hb.surplus=donation.simple$hb-donation.simple$hb.thold

repeat.counts = donation.simple %>%
	group_by(ord) %>%
	summarise(n=n(),.groups='drop') %>%
	mutate(rownr=row_number()) %>%
	filter(ord==rownr,n>100) %>%
	data.frame()

#####
dt.max=max(donation.simple$date)
param$dt.max=dt.max

dlink = donation.simple %>%
	left_join(donation.simple[,c('numid','ord','date')],join_by(numid,x$ord.next==y$ord)) %>%
	left_join(donation.simple[,c('numid','ord','hb')],join_by(numid,x$ord.prev==y$ord),suffix=c('','.prev')) %>%
	dplyr::select(-ord.next,-ord.prev) %>%
	mutate(hb.change=hb-hb.prev)

# free the simple table to free up some memory
rm(donation.simple)

# dlink$hb.change[dlink$ord==1]=NA
dlink$hb.change[is.na(dlink$hb.change)]=NA
dlink$diff=as.integer(dlink$date.y-dlink$date.x)
dlink$event=0
dlink$event[!is.na(dlink$date.y)]=1
dlink$diff[is.na(dlink$diff)]=as.integer(dt.max-dlink$date.x[is.na(dlink$diff)])

ord.group.number=min(nrow(repeat.counts),param$max.ord.group.number)
dlink$ord.group=dlink$ord
dlink$ord.group[dlink$ord.group>ord.group.number]=ord.group.number+1
dlink$ord.group=as.factor(dlink$ord.group)

dlink$dummy=NULL
###

do.coxph.inner = function(data0) {
data00=data0
bsAssign('data00')
	hb.var=colnames(data0)[ncol(data0)]

	if(hb.var != 'sex') {
		sex0=data0$sex[1] 
	} else {
		sex0='Female'
	}
	

print(paste(hb.var,sex0,data0$ord.group[1]))

	breaks.str='-'

	data=data0[[hb.var]]
	if (!is.factor(data) && length(unique(data)) > 10 && hb.var %in% hb.vars) {
		breaks.ord=quantile(data,prob=c(0,0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE)
		breaks.str=df.breaks %>% filter(sex==sex0,var==hb.var) %>% dplyr::select(breaks) %>% as.character()
		breaks.common=strsplit(breaks.str,',')[[1]]
		print(breaks.common)
		data=cut(data,breaks.common)
		levels(data)=c('bottom 10%','bottom 10-25%','mid','top 10-25%','top 10%')
		data=relevel(data,ref='mid')
	}

	data0[[ncol(data0)]]=data

	if (all(is.na(data0[[hb.var]])) || length(unique(data0[[hb.var]])) == 1) {
		if (!hb.var %in% c('ord.group','sex'))
			return(NULL)
	}

	og0=data0$ord.group[1]

	if (hb.var=='sex') {
		wh=which(colnames(data0)=='sex')
		data0=data0[,-wh[1]]
		data0 = data0 %>%
			dplyr::select(-ord.group)
		data0$sex=relevel(as.factor(data0$sex),ref='Female')
	} else {
		data0 = data0 %>%
			dplyr::select(-ord.group,-sex)
	}

bsAssign('data0')
	m=coxph(Surv(diff,event)~.,data=data0)

	if (ncol(data0)==2) {
		# The case where survfit are extracted; spec ~ '-'. Will just return the survival curves
		return(with(survfit(m),data.frame(sex=sex0,ord.group=og0,n,time,n.risk,n.event,n.censor,surv,cumhaz,std.err,std.chaz,lower,upper)))
	}

	sm=summary(m)
	df=data.frame(sm$coeff)
	var=sub('(.+)(top|bottom).+','\\1',rownames(df))
	level=sub(hb.var,'',rownames(df))
	df=cbind(var=hb.var,level=level,ord.group=og0,df)
	colnames(df)=c('var','level','ord.group','coef','exp.coef','se.coef','z','p.value')
	df$breaks=breaks.str

	rdf=cbind(sex=sex0,df,sm$conf.int)
	colnames(rdf)=sub(' \\.','..',colnames(rdf))
	return(rdf[,!grepl('\\(',colnames(rdf))])
}

bsAssign = function(name) {
	obj = get(name,envir=parent.frame())
	assign(name,obj,.GlobalEnv)
}

# interesting results: those with 'more hb tend to donate less frequently
# Is it actually the case that the most active donors get their hb depleted
# nb! This must be done 
vars = c('sex','avg.diff','hb.surplus','hb.change','age.group','bloodgr','age.group.t')
hb.vars = c('avg.diff','hb.surplus','hb.change')
cols.prefix=c('diff','event','sex','ord.group')

spec=data.frame(hb.var=vars)
spec.curves=data.frame(hb.var='-')
spec.age.t=data.frame(hb.var='age.group.t')

# compute the breaks used to group hb-variables in the cox regressions
res.breaks=lapply(hb.vars,function(x) {
		min.x=min(dlink[!is.na(dlink[[x]]),'ord']) # 1 or 2
		data.br=dlink[dlink$ord==min.x&!is.na(dlink[[x]]),c('sex',x)]
		br.list=lapply(c('Male','Female'),function(y) {
				brs=quantile(data.br[data.br$sex==y,x],prob=c(0,0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE)
				data.frame(var=x,sex=y,breaks=paste(brs,collapse=','))
			})
		do.call(rbind,br.list)
	})
df.breaks=do.call(rbind,res.breaks)

getResults=function(dlink,spec,replace.ord.group=NULL) {
	res=by(spec,spec,function(x) {
		if (x=='-') {
			x=NULL
		} else
			x=c(t(x))
		

		df=dlink
		if (!is.null(replace.ord.group)) {
			df$ord.group=NULL
			colnames(df)=sub(replace.ord.group,'ord.group',colnames(df))
			x='ord.pwr'
		}

		coeff.list=by(df[,c(cols.prefix,x)],df[,setdiff(c('sex','ord.group'),x)],do.coxph.inner)
		res=do.call(rbind,coeff.list)
		res$ord.group=as.integer(res$ord.group)
		colnames(res)=sub('^ord.group$','ord',colnames(res))
		return(res)
	})

	return(do.call(rbind,res))
}

res.models=getResults(dlink,spec)
res.curves=getResults(dlink,spec.curves)

### estimate a model with all the ord-levels as a factor
# separately for sex
# the data is sampled to keep the model size (and time required to estimate it) reasonable
max.sample.size=2000
dlink.sampled=do.call(rbind,by(dlink,dlink[,c('sex','ord')],function(x) {
		if (x$ord[1] > 120) 
			return(NULL)

		if (nrow(x) < 100)
			return(NULL)

		wh=1:nrow(x)
		if (nrow(x) > max.sample.size) {
			wh=sample(wh,max.sample.size)
		}
		# print(paste(x$ord[1],':',nrow(x),length(wh)))
		return(x[wh,])
	}))

# This would be too slow; similar results obtained below using res.models.full
# dlink.sampled$ord.group=as.factor(dlink.sampled$ord)
# m=coxph(Surv(diff,event)~ord.group*sex,data=dlink.sampled)
# sm=summary(m)
# m.sex.ord.interaction=m

flist=by(dlink.sampled,dlink.sampled$sex,function(x) {
		x$ord.group=as.factor(x$ord)
		m=coxph(Surv(diff,event)~ord.group,data=x)
		sm=summary(m)
		# print(sm)

		df=data.frame(sm$coeff)
		var='ord.group.full'
		level=sub('ord.group','',rownames(df))
		df=cbind(var=var,level=1,ord=level,df)
		colnames(df)=c('var','level','ord.group','coef','exp.coef','se.coef','z','p.value')
		df$breaks='-' 

		rdf=cbind(sex=x$sex[1],df,sm$conf.int)
		colnames(rdf)=sub(' \\.','..',colnames(rdf))

		rdf$ord.group=as.integer(rdf$ord.group)
		colnames(rdf)=sub('^ord.group$','ord',colnames(rdf)) # $ord.group=NULL

		return(rdf[,!grepl('\\(',colnames(rdf))])
	})
res.models.full=do.call(rbind,flist)

res.models.all=rbind(res.models,res.models.full)

# writing the results: export
cbs=100000 # curve.batch.size # nb! could be a parameter
curve.batches=1:(nrow(res.curves) %/% cbs + 1)
curve.batches=curve.batches[-1]

bsFlatten = function(x) {
		if (is.function(param[[x]])) 
			return(NULL)

		if (is.list(param[[x]])) {
			z=param[[x]]
			df=data.frame(name=paste0(x,':',names(z)),value=as.character(z))
			colnames(df)=c('name','value')
			return(df)
		}
	
		data.frame(name=x,value=param[[x]])
	}

if (!'omit.data' %in% names(param) || is.null(param$omit.data)) {
	param$omit.data='none defined'
}
param$omit.data='dummy'
tst=lapply(names(param),bsFlatten)
df.param=do.call(rbind,tst)

write.xlsx(list(param=df.param,stats.age=stats.age,stats.age.t=stats.age.t,stats.ord=stats.ord,models=res.models.all,curves=res.curves[1:min(cbs,nrow(res.curves)),]),file=sub('\\.xlsx$','-survival.xlsx',param$result.file),rowNames=FALSE)
sapply(curve.batches,FUN=function(x) {
		row.0=((x-1)*cbs+1)
		row.1=min((x*cbs),nrow(res.curves))
		write.xlsx(list(curves=res.curves[row.0:row.1,]),file=sub('\\.xlsx',paste0('-survival-',x,'.xlsx'),param$result.file),rowNames=FALSE)
		return(c(row.0,row.1))

	})
