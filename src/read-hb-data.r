setwd('c:/hy-version/first-time-donor-hb/src')

library(tidyverse)
library(openxlsx)

## ----parameters,echo=FALSE----------------------------------------------------
param=list()
param$data.dir = 'C:/Users/super/OneDrive - University of Helsinki/veripalvelu/paper-2 hemoglobin/data/'
param$shared.dir='C:/Users/super/OneDrive - University of Helsinki/veripalvelu/paper-2 hemoglobin/hb-paper-manuscripts/'
param$shared.dir="c:/hy-version/first-time-donor-hb/submit/"

if (grepl('^VP',Sys.info()[4])) {
	setwd('C:\\git_repot\\first-time-donor-hb\\src')
	param$data.dir = 'C:\\git_repot\\first-time-donor-hb\\results\\'
	param$shared.dir = 'C:\\git_repot\\first-time-donor-hb\\results\\'
}

param$png.resolution=1.4*150
param$figure.format='pdf'

## ----read-files,echo=FALSE----------------------------------------------------
file.names = dir(path=param$data.dir,pattern="*hb.xlsx")
file.names = file.names[!grepl('~',file.names)]
file.names = file.names[!grepl('^old',file.names)]
file.names = file.names[!grepl('survival',file.names)]
file.names = file.names[grepl('.xlsx$',file.names)]
file.paths = paste(param$data.dir,file.names,sep='')

plotByGroups.old = function(data,group.cols=c('sex','country'),xcol='level',ycols=c('Estimate','lower','upper'),
		ltys=list(cm='dashed',fi='solid'),colours=list(Male='blue3',Female='red3'),main='') {

	xmin=min(data[[xcol]][data[[xcol]]>=0])-1
	ylim=c(min(data[,ycols]),max(data[,ycols]))
	yspan=(ylim[2]-ylim[1])
	plot(x=NULL,xlim=c(xmin,max(data[[xcol]])),ylim=c(ylim[1]-0.5*yspan,ylim[2]),
		main=main,xlab=xcol,ylab=ycols[1])
	lgnd=by(data,data[,group.cols],function(x) {
			sex0=x[1,group.cols[1]] 
			country0=x[1,group.cols[2]] 

			col0=pp.cols[[sex0]]

			wh = which(x[[xcol]]==-1)
			if (length(wh) > 0) {
				x0=min(x[[xcol]][-wh])-1+0.1*if(sex0=='Male') 0.2 else 0
				arrows(x0,x[[ycols[2]]][wh],x0,x[[ycols[1]]][wh],length=0.05,angle=90,code=3,col=col0,lwd=1.5)
				arrows(x0,x[[ycols[1]]][wh],x0,x[[ycols[3]]][wh],length=0.05,angle=90,code=3,col=col0,lwd=1.5)
				x=x[-wh,]
			}

			m=lm(x[[ycols[1]]]~x[[xcol]])

			lines(x[[xcol]],x[[ycols[1]]],col=col0,lwd=2,lty=ltys[[country0]])
			lines(x[[xcol]],x[[ycols[3]]],col=col0,lwd=1,lty='dashed')
			lines(x[[xcol]],x[[ycols[2]]],col=col0,lwd=1,lty='dashed')

			sm=summary(m)
			cf=round(sm$coeff,3)
			print(summary(m))
			text=paste0('b=',sprintf(cf[2,1],fmt='%.3f'),', p=',cf[2,4])
			data.frame(text=paste(text,country0),b=cf[2,1],p=cf[2,4],lty=ltys[[country0]],col=col0)
		})

	legend.data=do.call(rbind,lgnd)
	legend(x='bottom',legend=sub('cm','corrected',legend.data$text),col=legend.data$col,lty=legend.data$lty,lwd=2)
}

countries = list()
for (file in file.paths) {
	identifier = sub('.+[/\\]([a-z]+)[^/\\]+$','\\1',file)
	if (nchar(identifier) > 2) 
		next
	
	curr = list()
	
	sheet.names = getSheetNames(file)
	for (sn in sheet.names) {
		data = read.xlsx(file,colNames=TRUE,rowNames=FALSE,sheet = sn)
		if (sn == 'parameters') {
			curr$parameters=data
		} else {
			curr[[sn]]=cbind(country=identifier,data)
		}
	}
	
	countries[[identifier]] = curr
}

# join the data sets from different countries to single variables
for (nm in names(countries[[1]])) {
	if (nm=='parameters')
		next

	tab.list=lapply(countries,FUN=function(x) x[[nm]])
	data=do.call(rbind,tab.list)
	if ('data.set' %in% colnames(data)) {
		data$data.set[data$data.set=='simple']='all'
	}

	colnames(data)=sub('\\.hb$','',colnames(data))
	colnames(data)=tolower(colnames(data))

	assign(nm,data)	
}

annual.hb.dist=annual.hb
monthly=montly.statistics
rm(montly.statistics)

margins=list()
margins[['month']]=monthly
margins[['age']]=annual.age
margins[['hour']]=hourly.statistics

hour.total = hourly.statistics %>%
	group_by(country,data.set,sex) %>%
	summarise(n2=sum(n),.groups='drop')

# groups for hours by country,data.set,hour,sex: separately values
# with at least 0.001 of the total donations; group the rest together
hour.groups=hourly.statistics %>%
	group_by(country,data.set,hour,sex) %>%
	summarise(n=sum(n),.groups='drop') %>%
	inner_join(hour.total,join_by(country,data.set,sex)) %>%
	mutate(prop=n/n2,saved=prop>0.001&!is.na(hour),
		na.group=(!is.na(hour)*(-1))-1,
		nr.group=((saved&!is.na(hour))*coalesce(hour,0)),
		hour.group=(nr.group>0)*(nr.group+1)-1) %>%
	dplyr::select(country,data.set,hour,sex,hour.group) %>%
	data.frame()

hourly.grouped=hourly.statistics %>%
	inner_join(hour.groups,join_by(country,data.set,hour,sex)) %>%
	group_by(country,data.set,year,sex,hour.group) %>%
	summarise(mean=sum(n*mean)/sum(n),n=sum(n),nas=sum(nas),deferred=sum(deferred),.groups='drop') %>%
	rename(hour=hour.group) %>%
	dplyr::select(country,data.set,year,sex,hour,n,nas,deferred,mean)

# use the newly formed grouped hourly statistics instead of the original one
margins[['hour']]=hourly.grouped

annual.hb = annual.hb.dist %>%
	filter(abs(hb)!=1000000) %>%
	group_by(data.set,sex,country,year) %>%
	summarise(n.donor=sum(n),hb=sum(hb*n)/sum(n),.groups='drop') %>%
	data.frame()

totals=lapply(names(margins),FUN=function(nm) {
		margins[[nm]] %>%
			group_by(country,data.set,sex) %>%
			summarise(n2=sum(n),.groups='drop') %>%
			mutate(var=nm)
	})
names(totals)=names(margins)

totals.year=lapply(names(margins),FUN=function(nm) {
		margins[[nm]] %>%
			group_by(country,data.set,sex,year) %>%
			summarise(n2=sum(n),.groups='drop') %>%
			mutate(var=nm)
	})
names(totals.year)=names(margins)

dist=lapply(names(margins),FUN=function(nm) {
		margins[[nm]] %>%
			group_by(country,data.set,sex,!!!syms(nm)) %>%
			summarise(n2=sum(n),.groups='drop') %>%
			mutate(var=nm) %>%
			rename(level=nm) %>%
			full_join(totals[[nm]],join_by(country,data.set,sex,var),suffix=c('','.total')) %>%
			mutate(prop=n2/n2.total)
	})
dist=do.call(rbind,dist)

dist.year=lapply(names(margins),FUN=function(nm) {
		margins[[nm]] %>%
			group_by(country,data.set,sex,year,!!!syms(nm)) %>%
			summarise(n2=sum(n),.groups='drop') %>%
			mutate(var=nm) %>%
			rename(level=nm) %>%
			full_join(totals.year[[nm]],join_by(country,data.set,sex,var,year),suffix=c('','.total')) %>%
			mutate(prop=n2/n2.total)
	})
dist.year=do.call(rbind,dist.year)

# Test plot for the distributions
plot(x=NULL,xlim=c(-1,12),ylim=c(0,0.20))
yd=dist.year %>% filter(data.set=='donation0',sex=='Male',var=='month')
yd0=dist %>% filter(data.set=='donation0',sex=='Male',var=='month') %>%
			mutate(level.num=as.numeric(level)) %>%
			arrange(level.num)

by(yd,yd[,c('country','year','sex')],function(x) {
		x=x%>%
			mutate(level.num=as.numeric(level)) %>%
			arrange(level.num)

		lines(x$level,x$prop)
	})
lines(yd0$level,yd0$prop,col='red3',lwd=3)

dist.diff=inner_join(dist.year,dist,join_by(country,data.set,sex,var,level),suffix=c('','0')) %>%
	mutate(diff=prop-prop0)
# hist(dist.diff$prop-dist.diff$prop0)

dd.sorted=dist.diff %>% arrange(diff)
# plot(dd.sorted$diff)

# names(countries$fi)
getwd()
pdf('../results/margins.pdf')
mar.res=list()
for (nm in names(margins)) {
	print(paste('****',nm))
	df=inner_join(margins[[nm]],annual.hb,join_by(country,sex,year,data.set)) %>% 
		filter(data.set=='donation0') %>% 
		mutate(hb.dev=mean-hb)

	df[[nm]]=as.character(df[[nm]])

	rlist=by(df,df[,c('sex','country')],function(x) {
			frml.char=paste0('hb.dev~',nm,'+0')
			m=lm(formula(frml.char),weights=n,data=x)
			sm=summary(m)
			print(sm)
			df=data.frame(sm$coeff)
			tv=-qt(0.025,df=sm$df[2])
			df=data.frame(country=x$country[1],sex=x$sex[1],var=nm,level=as.integer(sub(nm,'',rownames(df))),df,
				lower=df$Estimate-tv*df$Std..Error,upper=df$Estimate+tv*df$Std..Error) %>%
				arrange(country,sex,level)
			return(df)
		})
	var.data=do.call(rbind,rlist)

	pp.cols=list(Male='blue3',Female='red3')
	xmin=min(var.data$level[var.data$level>=0])-1
	plot(x=NULL,xlim=c(xmin,max(var.data$level)),ylim=c(min(var.data$lower),max(var.data$upper)),
		main=paste(nm),xlab=nm,ylab='deviation from mean hb')
	by(var.data,var.data[,c('country','sex')],function(x) {
			sex0=x$sex[1]
			country0=x$country[1]

			col0=pp.cols[[sex0]]

			wh = which(x$level==-1)
			if (length(wh) > 0) {
				x0=min(x$level[-wh])-1+0.1*if(sex0=='Male') 0.2 else 0
				arrows(x0,x$lower[wh],x0,x$Estimate[wh],length=0.05,angle=90,code=3,col=col0,lwd=1.5)
				arrows(x0,x$Estimate[wh],x0,x$upper[wh],length=0.05,angle=90,code=3,col=col0,lwd=1.5)
				x=x[-wh,]
			}

			lines(x$level,x$Estimate,col=col0,lwd=2)
			lines(x$level,x$upper,col=col0,lwd=1,lty='dashed')
			lines(x$level,x$lower,col=col0,lwd=1,lty='dashed')
		})

	mar.res[[nm]]=var.data
}
dev.off()

crtn=do.call(rbind,mar.res)
crtn$level=as.character(crtn$level)

# the difference is computed as observed-expected
# crtn contains the deviances per margin (mean-hb) estimated from data usin lm
# next compute the corrections per margin: difference in proportion (dist.diff$prop) times the crtn
# the computed corrections will be added (+) to the means
# str(crtn)
# str(dist.diff)

# e.g. 20% donations in January in 2023 vs. 10% on average -> diff=10%
# It was estimated that hb is -1 below average in January: Estimate=-1
# One should like to add 10%*1 to the mean
# Hence the correction should include a multiplier of -1.
# dist.diff can now be used to compute the corrections

crtn.mean=inner_join(dist.diff[dist.diff$data.set=='donation0',],crtn,join_by(country,sex,var,level)) %>%
	mutate(correction=-diff*Estimate) %>%
	dplyr::select(country,data.set,sex,var,level,year,correction) %>%
	arrange(correction)

str(crtn.mean)
plot(crtn.mean$correction)
summary(crtn.mean)
crtn.mean # abs(min~man)=0.37: näihin ei siis tosiaankaan tikahdu
crtn.mean[1:10,]

crtn.annual=crtn.mean %>%
	group_by(country,data.set,sex,year) %>%
	summarise(correction=sum(correction),.groups='drop') %>%
	data.frame()
plot(crtn.annual$correction)

plotByGroups(crtn.annual,group.cols=c('sex','country'),xcol='year',ycols=c('correction'),colours=list(Male='blue3',Female='red3'))
hb.dummy=annual.hb %>%
	filter(data.set=='donation0')
plotByGroups(hb.dummy,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=list(Male='blue3',Female='red3'))

hb.cmp=inner_join(crtn.annual,hb.dummy,join_by(data.set,sex,country,year,)) %>%
	mutate(hb=hb+correction,country=paste0(country,'-corrected')) %>%
	select(!!!syms(colnames(hb.dummy))) %>%
	rbind(hb.dummy) %>%
	dplyr::filter(year>=2002,year<2024) # 2026-02-08 nb! must filter each country based on their own years
# nb! must do the conversion properly as well
hb.cmp$hb[grepl('nl',hb.cmp$country)]= (10 / 0.6206)*hb.cmp$hb[grepl('nl',hb.cmp$country)]

pdf('results/trends-corrected.pdf')
plotByGroups(hb.cmp,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=colours,colour.col='country')
# plotByGroups(hb.cmp,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=list(Male='blue3',Female='red3'),ltys=list(fi='solid'))
dev.off()
### eof

colours=list()
colours$fi='darkblue'
colours$nl='orange'
colours$fr='red3'
colours$au='#007F3B' # 'green3'
colours$nc='black'
colours$ct='purple'
colours$za='turquoise3' # 'violetred3'

cn.names=list()
cn.names$fi='Finland'
cn.names$nl='Netherlands'
cn.names$fr='France'
cn.names$au='Australia'
cn.names$nc='Navarre'
cn.names$ct='Catalonia'
cn.names$za='South Africa'

colfun = function(x) {
	colours[[x]]
}

nuisance.cols = c('Multiplier','MaximumAge','Name','avage')
dim.cols = c('country','age.lower','age.upper','DonationPlaceType','Sex','BloodGroup',nuisance.cols)
dim.keep = c('country','Sex') # this is the changing part

spec.list = list()
spec.list$country = list()
spec.list$country$dim.keep = c('country')
spec.list$country$pch = function(x) {15}
spec.list$country$colours = colours
spec.list$country$col.dim = 'country'
spec.list$country$pch.dim = 'country'

if (FALSE) {
	spec.list$Sex = list()
	spec.list$Sex$dim.keep = c('Sex')
	spec.list$Sex$pch = function(x) {2}
	spec.list$Sex$colours = list(Male='blue3',Female='red3')
	spec.list$Sex$col.dim = 'Sex'
}

spec.list$country.sex = list()
spec.list$country.sex$dim.keep = c('country','Sex')
spec.list$country.sex$pch = function(x) { pchs=list(Female=2,Male=6);  return(pchs[[x]])}
spec.list$country.sex$colours = colours
spec.list$country.sex$col.dim = 'country'
spec.list$country.sex$pch.dim = 'Sex'

spec.list$country.age = list()
spec.list$country.age$dim.keep = c('country','age.lower')
spec.list$country.age$pch = function(x) { pchs=list(a0=7,a25=9,a40=12);  return(pchs[[paste0('a',x)]])}
spec.list$country.age$colours = colours
spec.list$country.age$col.dim = 'country'
spec.list$country.age$pch.dim = 'age.lower'

spec.list$country.bloodgr = list()
spec.list$country.bloodgr$dim.keep = c('country','BloodGroup')
spec.list$country.bloodgr$pch = function(x) { pchs=list(); pchs[['-O-']]=4; pchs[['O-']]=1;  return(pchs[[x]])}
spec.list$country.bloodgr$colours = colours
spec.list$country.bloodgr$col.dim = 'country'
spec.list$country.bloodgr$pch.dim = 'BloodGroup'
spec=spec.list$country
