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
	inner_join(donationdata$donor[,c('numid','Sex','BloodGroup','DateOfBirth')],join_by(numid)) %>%
  	arrange(numid,DonationDate)
  # donation.simple$dtEnd = as.Date("2020-01-01") # nb! This is set to NA later
  # donation.simple$type = 'donation'
  donation.simple$age= as.numeric(difftime(donation.simple$DonationDate,donation.simple$DateOfBirth),unit="weeks")/52.25
  donation.simple = donation.simple %>% 
    group_by(numid) %>%
    mutate(ord = row_number()) %>%
    ungroup() %>%
    dplyr::select(-DateOfBirth)

colnames(donation.simple)=c('rowid','numid','date','hb','sex','bloodgroup','age','ord')

donation.simple$bloodgr='other'
donation.simple$bloodgr[donation.simple$bloodgroup=='O-']='O-'
donation.simple$bloodgr=as.factor(donation.simple$bloodgr)
donation.simple$bloodgr=relevel(donation.simple$bloodgr,ref='other')
donation.simple$age.group=cut(donation.simple$age,breaks=c(0,25,40,100))

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

# 
donation.simple$hb.thold=param$hb.thold.female
donation.simple$hb.thold[donation.simple$sex=='Male']=param$hb.thold.male
donation.simple$hb.surplus=donation.simple$hb-donation.simple$hb.thold

# donation.simple %>% filter(numid==5)

repeat.counts = donation.simple %>%
	group_by(ord) %>%
	summarise(n=n(),.groups='drop') %>%
	mutate(rownr=row_number()) %>%
	filter(ord==rownr,n>100) %>%
	data.frame()

#####
dt.max=max(donation.simple$date)

dlink = donation.simple %>%
	left_join(donation.simple[,c('numid','ord','date')],join_by(numid,x$ord.next==y$ord)) %>%
	left_join(donation.simple[,c('numid','ord','hb')],join_by(numid,x$ord.prev==y$ord),suffix=c('','.prev')) %>%
	dplyr::select(-ord.next,-ord.prev) %>%
	mutate(hb.change=hb-hb.prev)

dlink$hb.change[is.na(dlink$hb.change)]=0
dlink$diff=as.integer(dlink$date.y-dlink$date.x)
dlink$event=0
dlink$event[!is.na(dlink$date.y)]=1
dlink$diff[is.na(dlink$diff)]=as.integer(dt.max-dlink$date.x[is.na(dlink$diff)])

ord.group.number=min(nrow(repeat.counts),param$max.ord.group.number)
dlink$ord.group=dlink$ord
dlink$ord.group[dlink$ord.group>ord.group.number]=ord.group.number+1
dlink$ord.group=as.factor(dlink$ord.group)

dlink$dummy=NULL
# str(dlink)

# dlink %>% filter(numid==5)

# interesting results: those with 'more hb tend to donate less frequently
# Is it actually the case that the most active donors get their hb depleted
# nb! This must be done 
vars = c('avg.diff','hb.surplus','hb.change','age.group','bloodgr')
spec=expand.grid(grp.var=c(NA,'sex'),hb.var=vars)
cols.prefix=c('diff','event','sex','ord.group')

x=spec[1,]

bsAssign = function(name) {
	obj = get(name,envir=parent.frame())
	assign(name,obj,.GlobalEnv)
}

dcox.list=by(spec,spec,function(x) {
		x=x[!is.na(x)]
		coeff.list=by(dlink[,c(cols.prefix,c(t(x)))],dlink[,c('sex','ord.group')],do.coxph.inner)
		res=do.call(rbind,coeff.list)
		return(res)
	})

coeff.list

res=do.call(rbind,dcox.list)
res=do.call(rbind,coeff.list)

res
res %>% filter(p.value<0.05)

colours=list()
colours[['top 10%']]='blue3'
colours[['top 10-25%']]='lightblue'
colours[['bottom 10-25%']]='pink'
colours[['bottom 10%']]='red3'
colours[['(25,40]']]='green3'
colours[['(40,100]']]='gray3'
colours[['O-']]='gray3'

colnames(res)[c(10,11)]=c('lower..95','upper..95')

pdf('../figures.pdf')
by(res,res[,c('sex','var')],function(y) {
	plot(x=NULL,type='n',xlim=c(1,11),ylim=c(0.70,1.30),main=paste(y$sex[1],y$var[1]),xlab='number of donation',ylab='relative likelihood of donation')
	abline(h=1,lwd=2,lty='dotted')
	legend(x='bottomright',legend=unique(y$level),fill=sapply(unique(y$level),function(x) colours[[x]]))
	# bsAssign('y')
	by(y,y[,c('var','level')],function(x) {
			gr=x$level[1]
			col=colours[[gr]]
			ordint=as.integer(x$ord.group)
			lines(ordint,x$exp.coef,lwd=2,col=col)
			lines(ordint,x$lower..95,lwd=2,lty='dotted',col=col)
			lines(ordint,x$upper..95,lwd=2,lty='dotted',col=col)
		})
	})
dev.off()

plot.progression=function(x) {
	plot(x=NULL,type='n',xlim=c(1,11),ylim=c(0.9,1.1))
	by(res[res$sex=='Male',],res[res$sex=='Male',c('var')],function(x) {
			gr=x$var[1]
			gr=sub('.+(top|bottom.+)','\\1',gr)
			col=colours[[gr]]
			ordint=as.integer(x$ord.group)
			lines(ordint,x$exp.coef,lwd=2,col=col)
			lines(ordint,x$lower..95,lwd=2,lty='dotted',col=col)
			lines(ordint,x$upper..95,lwd=2,lty='dotted',col=col)
		})
}

x
x$grp.var
str(dlink)
'group' %in% colnames(dlink)

str(dlink[,c(cols.prefix,c(t(x)))])

save(m.iso,file='../model-126-ord-levels.Rdata')

for (hb.var in vars) {
	data0=dlink[[hb.var]]
	data0$dummy=1

	do.coxph.inner = function(data0) {
		hb.var=colnames(data0)[ncol(data0)]
		data=data0[[hb.var]]
		if (!is.factor(data) && length(unique(data)) > 10) {
			data=cut(data,quantile(data,prob=c(0,0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE))
			levels(data)=c('bottom 10%','bottom 10-25%','mid','top 10-25%','top 10%')
			data=relevel(data,ref='mid')
		}

		data0[[ncol(data0)]]=data
		# print(hb.var)
		# hb.var='data'

		if (all(is.na(data0[[hb.var]])) || length(unique(data0[[hb.var]])) == 1)
			return(NULL)

		sex0=data0$sex[1]
		og0=data0$ord.group[1]

		data0 = data0 %>%
			dplyr::select(-ord.group,-sex)

		frml.char=paste0('Surv(diff,event)~.')
		m=coxph(formula(frml.char),data=data0)
		# print(summary(m))
		sm=summary(m)
		df=data.frame(sm$coeff)

		var=sub('(.+)(top|bottom).+','\\1',rownames(df))
		# level=sub('(.+)(top|bottom)(.+)','\\2\\3',rownames(df))
		level=sub(hb.var,'',rownames(df))
		df=cbind(var=hb.var,level=level,df)
		colnames(df)=c('var','level','coef','exp.coef','se.coef','z','p.value')

		# ci=data.frame(sm$conf.int[,3:4])
		# str(ci)
		rdf=cbind(sex=sex0,ord.group=og0,df,sm$conf.int) # [,c(-3,-4)]
		return(rdf[,-(1+ncol(rdf)-(3:4))])
		# return(cbind(sex=sex0,ord.group=og0,lower..95=ci[,1],upper..95=ci[,2])) # ,'/',group.var # cbind(kind=paste0(hb.var),
	}

	# by(dlink,dlink[,c('sex',)],do.coxph.inner)
}

dlink %>% filter(numid==5)
dlink %>% dplyr::filter(hb.surplus<(-5))
str(dlink)

# nb! This can be slow with a large number of groups
# m=coxph(Surv(diff,event)~sex+group,data=dlink)
sm=summary(m)

###
# Different ord's in the same model, modelling the coefficients
# linear after log-log-transformation.
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
	m=coxph(Surv(diff,event)~sex+hb,data=di)
	sf=survfit(m)
	slist[[i]]=sf
	if (i==1) {
		plot(sf,lwd=2,col=colours[i],xlim=c(0,2*365))
	} else {
		lines(sf,lwd=2,col=colours[i])
	}
}

### 
d0=dlink %>% filter(ord==1)
d1=dlink %>% filter(ord==2)

m0=coxph(Surv(diff,event)~sex+hb,data=d0)
m1=coxph(Surv(diff,event)~sex+hb,data=d1)

sf0=survfit(m0)
sf1=survfit(m1)

plot(sf0,col='blue2')
lines(sf1,col='red2')

# Compute averages up to each donation
# pah2=do.call(rbind,by(don.pah,don.pah$numid,FUN=function(x) {x$csum=cumsum(x$hb);return(x)}))
# data.frame(pah2[1:100,])

# str(donation.simple)

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

# 2026-01-04
# Asioita kannattaa tarkastella luovutuskerran numeron perusteella
# ord voi olla vaaka-akselina
# Eri ryhmien väliset ph:t sitten arvoina maittain aikasarjoina; pikkupaneelit eri muuttujille
# Tästä tulee noin yhden sivun kuva, voi olla tarvittaessa myös kahdessa sarakkeessa
# ord-arvoja voi lisätä tunnelman mukaan

# Sitten erikseen vertailu, miten nopeudet ja asymptootit suhtautuvat toisiinsa
# (Edellisten tulosten perusteella kenties ensimmäinen kerta on jopa edustava suhteille)
# asymptootin lisäksi myös log_alpha-parametri eli laskun jyrkkyys on kiinnostava
# survfit-käyrille ehtii sitten tekemään jotakin myöhemmin

# https://www.rdocumentation.org/packages/survival/versions/3.8-3/topics/survfit.object
