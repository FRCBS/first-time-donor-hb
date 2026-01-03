library(dplyr)
library(survival)
library(RColorBrewer)

setwd('C:\\git_repot\\first-time-donor-hb\\src')

param=list()
param$data.file = "c:/git_repot/DATA/donationdata.fortimo.rdata" # fi
param$donation.types = c('Whole Blood (K)')
param$max.sample.size=1e7 # This is still reasonably fast (< 1 min)
param$hb.thold.male=135
param$hb.thold.female=125
param$max.ord.group.number=10

load(param$data.file)

  charid=unique(donationdata$donor$releaseID)
  id.map = data.frame(charid=charid,numid=1:length(charid))
  rownames(id.map)=id.map$charid
  
  donationdata$donation$numid=id.map[donationdata$donation$releaseID,'numid']
  donationdata$donor$numid=id.map[donationdata$donor$releaseID,'numid']

  donationdata$donation$rowid=1e7+1:nrow(donationdata$donation) #as.integer(rownames(donationdata$donation))
  donationdata$donor$rowid=2e5+1:nrow(donationdata$donor) # as.integer(rownames(donationdata$donor))

donation.simple = donationdata$donation[donationdata$donation$BloodDonationTypeKey %in% param$donation.types,c('rowid','numid','DonationDate','Hb')] %>% 
	inner_join(donationdata$donor[,c('numid','Sex','BloodGroup')],join_by(numid)) %>%
  	arrange(numid,DonationDate)
  donation.simple$dtEnd = as.Date("2020-01-01") # nb! This is set to NA later
  donation.simple$type = 'donation'
  donation.simple = donation.simple %>% 
    group_by(numid) %>%
    mutate(ord = row_number()) %>%
    ungroup()

colnames(donation.simple)=c('rowid','numid','date','Hb','sex','bloodgroup','dtEnd','type','ord')
donation.simple$ord.next=donation.simple$ord+1
donation.simple$ord.prev=donation.simple$ord-1

# Compute the running mean hb value
# This might be useful in analysing optouts
# In addition to the level, change, change wrt. the mean, distance from the threshold
# Maybe unsuccessful donations should be included as well? somehow
donation.simple = donation.simple %>%
	mutate(trsum=cumsum(Hb),.by=numid) %>%
	mutate(hb.avg=trsum/ord) %>%
	dplyr::select(-trsum)

donation.simple = donation.simple %>% 
	# filter(numid==5) %>%
	mutate(avg.before=(hb.avg*ord-Hb)/(ord-1)) %>%
	mutate(avg.diff=Hb-avg.before)

# 
donation.simple$hb.thold=param$hb.thold.female
donation.simple$hb.thold[donation.simple$sex=='Male']=param$hb.thold.male
donation.simple$hb.surplus=donation.simple$Hb-donation.simple$hb.thold

donation.simple %>% filter(numid==5)

ord.group.number=min(nrow(repeat.counts),param$max.ord.group.number)
dlink$group=dlink$ord
dlink$group[dlink$group>ord.group.number]=ord.group.number+1
dlink$group=as.factor(dlink$group)

#####
dt.max=max(donation.simple$date)

dlink = donation.simple %>%
	left_join(donation.simple[,c('numid','ord','date')],join_by(numid,x$ord.next==y$ord)) %>%
	left_join(donation.simple[,c('numid','ord','Hb')],join_by(numid,x$ord.prev==y$ord),suffix=c('','.prev')) %>%
	dplyr::select(-ord.next,-ord.prev) %>%
	mutate(hb.change=Hb-Hb.prev)

dlink$hb.change[is.na(dlink$hb.change)]=0
dlink$diff=as.integer(dlink$date.y-dlink$date.x)
dlink$event=0
dlink$event[!is.na(dlink$date.y)]=1
dlink$diff[is.na(dlink$diff)]=as.integer(dt.max-dlink$date.x[is.na(dlink$diff)])

repeat.counts = donation.simple %>%
	group_by(ord) %>%
	summarise(n=n(),.groups='drop') %>%
	mutate(rownr=row_number()) %>%
	filter(ord==rownr,n>100) %>%
	data.frame()

dlink %>% filter(numid==5)

# interesting results: those with 'more hb tend to donate less frequently
# Is it actually the case that the most active donors get their hb depleted
# nb! This must be done 
vars = c('avg.diff','hb.surplus','hb.change')
for (hb.var in vars) {
	data=dlink[[hb.var]] # %>% filter(ord==1)
	# data=dlink %>% filter(ord==1)
	# data=round(data,-1)
	# data=as.factor(data)

	# data=cut(data,breaks=c(-Inf,-15,-5,5,15,Inf)) # this was a working one
	# prob=0:10 / 10
	# data=split(data, cut(data, quantile(data,prob=c(0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE),include = TRUE))
	data=cut(data, quantile(data,prob=c(0,0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE))
	levels(data)=c('bottom 10%','bottom 10-25%','mid','top 10-25%','top 10%')
	data=relevel(data,ref='mid')
# str(data)
# table(data)

	# data=relevel(data,ref='(-5,5]')
	print(hb.var)
	hb.var='data'

	frml.char=paste0('Surv(diff,event)~',hb.var)
	m=coxph(formula(frml.char),data=dlink)
	print(summary(m))
}

dlink %>% filter(numid==5)
dlink %>% dplyr::filter(hb.surplus<(-5))
str(dlink)

# nb! This can be slow with a large number of groups
# m=coxph(Surv(diff,event)~sex+group,data=dlink)
sm=summary(m)

wh=2:(nrow(sm$coeff)-1)
grc=sm$coeff[wh,1]
y=log(grc)
y.lower=log(log(sm$conf.int[wh,3]))
y.upper=log(log(sm$conf.int[wh,4]))
x=log(1:length(grc))
m2=lm(y~x)
summary(m2)
plot(y~x)
abline(m2)
	arrows(x,y.lower,x,y,length=0.05,angle=90,code=3,col='black')
	arrows(x,y,x,y.upper,length=0.05,angle=90,code=3,col='black')
# plot(exp(grc))
# sd=survdiff(Surv(diff,event)~sex,data=d0)

# colours <- brewer.pal(n = 9, name = "Blues")
nc=50
colours=colorRampPalette(c("black", "blue1"))(nc)
slist=list()
for (i in 1:nc) {
	di=dlink %>% filter(ord==i)
	m=coxph(Surv(diff,event)~sex+Hb,data=di)
	sf=survfit(m)
	slist[[i]]=sf
	if (i==1) {
		plot(sf,lwd=2,col=colours[i],xlim=c(0,2*365))
	} else {
		lines(sf,lwd=2,col=colours[i])
	}
}

d0=dlink %>% filter(ord==1)
d1=dlink %>% filter(ord==2)

m0=coxph(Surv(diff,event)~sex+Hb,data=d0)
m1=coxph(Surv(diff,event)~sex+Hb,data=d1)

sf0=survfit(m0)
sf1=survfit(m1)

plot(sf0,col='blue2')
lines(sf1,col='red2')

data.frame(pah2[1:100,])

# Compute averages up to each donation
pah2=do.call(rbind,by(don.pah,don.pah$numid,FUN=function(x) {x$csum=cumsum(x$Hb);return(x)}))

str(donation.simple)

wh=min(which(sf0$surv<0.995))
len0=length(sf0$surv)
y=sf0$surv[wh:len0]
x=sf0$time[wh:len0]-wh
m.glm=glm(y~x,family=Gamma(link="log"))
summary(m.glm)
prd.exp=predict(m.glm,newdata=data.frame(x=x)) # seq(1,8001,by=50)
exp(prd.exp) # Tämä vaikuttaa järkevältä
summary(m.glm,dispersion=1)
plot(y~x)
lines(x,exp(prd.exp),col='green2') # This doesn't produce reasonable results

# This works; but the fit is not too great
# 
# input	
#   a numeric vector of values at which to evaluate the model.
# Asym ~ yf	
#   a numeric parameter representing the horizontal asymptote on the right side (very large values of input).
# R0 ~ y0
#   a numeric parameter representing the response when input is zero.
# lrc	~ log_alpha
#   a numeric parameter representing the natural logarithm of the rate constant.

plot(y~x,type='l',lwd=3)

# sqrt.x works even better
sqrt.x=sqrt(x)
m.ss=nls(y ~ SSasymp(sqrt.x,yf,y0,log_alpha))
summary(m.ss)
pred.ss=predict(m.ss,data.frame(x=sqrt.x))
str(pred.ss)
lines(x,pred.ss,col='red2',lwd=2.5)

m=coxph(Surv(diff,event)~sex+group,data=dlink)

dexp=dlink
dexp=dexp[dexp$diff>=60,]
dexp$diff=dexp$diff-59
# e1=exp_model
exp_model <- survreg(Surv(diff,event)~sex+group,data=dexp,dist="exponential")
predict(exp_model,newdata=data.frame(time=1,sex='Male',group='21'))
# This predicts the mean time till next donation presumably?
# Not very useful

# What to do then?
# - Differences between countries are of key interest
#  * For this purpose, it is good to have only a few distinguishing factors to compare the curves
#  * Probably no interactions needed
# -> Single parameters that can be presented in a single table
#  * The donation progression could be saved separately and presented in a plot
#  * The estimated log-log-parameters also as rows in the table
#  * Maybe one or two variables also about the effect of hemoglobin level:
#   - the level (must separate males and females in these analyses)
#   - 
# - Within countries similar questions are of interest as in the donation activity study
#  * Sex
#  * Age at first donation
#  * Blood group (O-/other)
#  * These can modelled as Cox-regressions, where similar~ forms for the survival curves are assumed
# 
# Something like this maybe needed:
# how to test/check the proportional hazards assumption
# ---
# Results in the paper
# 1) Table 1: Demographics, very basic ones
# 2) Relative effects of predictos (sex, blood group, age at first donation, country: the other dimension-x, ord)
#   - Also possible to illustrate these with diamonds or similar, to make the differences visually clear
#   - A separate plot of the progressive effect of ord
# 3) Differences between countries
#   - Visual effects
#   - Exponential fits (possible a way to estimate the sqrt/0.5 parameter from data (taking logs))
# What should be exported
#   - coxph-coefficients; maybe something else
#   - survfit objects for the curves
#    * Should these be truncated for privacy? Probably best to truncate
#    * weights relative to 1/variance; so the square of standard error should do
#   - plots of residuals to make things easier to follow