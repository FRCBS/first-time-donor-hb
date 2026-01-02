library(dplyr)
library(survival)
library(RColorBrewer)

setwd('C:\\git_repot\\first-time-donor-hb\\src')

param=list()
param$data.file = "c:/git_repot/DATA/donationdata.fortimo.rdata" # fi
param$donation.types = c('Whole Blood (K)')

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
donation.simple$ord.next=donation.simple$ord+1

str(donation.simple)
  colnames(donation.simple)=c('rowid','numid','date','Hb','sex','bloodgroup','dtEnd','type','ord')

dt.max=max(donation.simple$date)

dlink = donation.simple %>%
	left_join(donation.simple[,c('numid','ord','date')],join_by(numid,x$ord.next==y$ord))
dlink$diff=as.integer(dlink$date.y-dlink$date.x)
dlink$event=0
dlink$event[!is.na(dlink$date.y)]=1
dlink$diff[is.na(dlink$diff)]=as.integer(dt.max-dlink$date.x[is.na(dlink$diff)])
table(dlink$event)

dlink$group=dlink$ord
dlink$group[dlink$group>20]=21
dlink$group=as.factor(dlink$group)
table(dlink$group)
m=coxph(Surv(diff,event)~sex+group,data=dlink)
sm=summary(m)

grc=sm$coeff[2:(nrow(sm$coeff)-1),1]
y=log(grc)
x=log(1:length(grc))
m2=lm(y~x)
summary(m2)
plot(y~x)
abline(m2)
plot(exp(grc))

sd=survdiff(Surv(diff,event)~sex,data=d0)

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

str(d0)

?coxph

str(dlink)
dlink[dlink$numid==1,]
dlink[1:100,]

# Compute the 
pah2 = donation.simple %>%
	mutate(trsum=cumsum(Hb),.by=numid) %>%
	mutate(hb.avg=trsum/ord) %>%
	dplyr::select(-trsum)

data.frame(pah2[1:100,])



# Compute averages up to each donation
pah2=do.call(rbind,by(don.pah,don.pah$numid,FUN=function(x) {x$csum=cumsum(x$Hb);return(x)}))

str(donation.simple)

?cumsum