setwd('c:/hy-version/first-time-donor-hb')
source('src/analysis-functions.r')

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

file.names = dir(path=param$data.dir,pattern="*hb.xlsx")
file.names = file.names[!grepl('~',file.names)]
file.names = file.names[!grepl('^old',file.names)]
file.names = file.names[!grepl('survival',file.names)]
file.names = file.names[grepl('.xlsx$',file.names)]
file.paths = paste(param$data.dir,file.names,sep='')

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

# annual hb and other names come from the xlsx sheet names
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
	summarise(mean=sum(n*mean)/sum(n),sd=sqrt(sum(n^2*sd^2)/sum(n^2)),n=sum(n),nas=sum(nas),deferred=sum(deferred),.groups='drop') %>%
	rename(hour=hour.group) %>%
	dplyr::select(country,data.set,year,sex,hour,n,nas,deferred,mean,sd)

# use the newly formed grouped hourly statistics instead of the original one
margins[['hour']]=hourly.grouped

# 2026-05-25

dt.max.list=lapply(names(countries.surv),function(x) {
	countries.surv[[x]]$param %>%
		rowwise() %>%
		filter(grepl('dt.max',name)) %>%
		mutate(dt.value=as.Date(as.integer(value)),month=month(dt.value),day=day(dt.value)) %>%
		mutate(full.year=if(month==12 && day >= 30) 1 else 0) %>%
		# mutate(country=x,name=firstUp(sub('cutoff.','',name))) %>%
		data.frame()
})
dt.maxs=do.call(rbind,dt.max.list)

use.years=annual.hb %>%
	left_join(dt.maxs,join_by(country)) %>%
	group_by(country) %>%
	summarise(year.min=min(year)+2,year.max=max(year)+coalesce(max(full.year),0)-1,.groups='drop')

margins=lapply(margins,function(x) {
		x %>%
			inner_join(use.years,join_by(country,between(year,y$year.min,y$year.max))) %>%
			select(-year.min) %>%
			select(-year.max) 
	})

annual.hb.dist = annual.hb.dist %>%
	filter(abs(hb)!=1000000) 

####### rectifyDistributions
cutoff.list=lapply(names(countries),function(x) {
	countries[[x]]$param %>%
		rowwise() %>%
		filter(grepl('cutoff',name)) %>%
		mutate(country=x,name=firstUp(sub('cutoff.','',name))) %>%
		data.frame()
})
cutoffs=do.call(rbind,cutoff.list)
# str(cutoffs)

plot.rects=FALSE
rectifyOuter = function(df) {
	df=df[df$data.set=='donation0',]
	sex0=df$sex[1]
	country0=df$country[1]
	year0=df$year[1]
	cutoff=as.numeric(cutoffs[cutoffs$name==sex0&cutoffs$country==country0,'value'])

	hb.decimals=max(sapply(df$hb,decimalPlaces))
	# print(hb.decimals)
	rv=rectifyDistribution(data0=NULL,freq=df,cutoff=cutoff,hb.decimals=hb.decimals,plot=plot.rects)

	return(data.frame(country=country0,sex=sex0,year=year0,mean0=rv$param$mean0,mean1=rv$param$mean))
}

res.list=by(annual.hb.dist,annual.hb.dist[,c('country','sex','year')],rectifyOuter)
rects=do.call(rbind,res.list)
rects = rects %>% 
	inner_join(conversions.df,join_by(country)) %>%
	mutate(diff=mean1-mean0,diff=diff*rate)

pdf('results/hb-rectifications.pdf')
par(mfrow=c(1,2))
ycols='diff'
ylim=c(min(rects[,ycols]),max(rects[,ycols]))
by(rects,rects[,'sex'],function(y) {
	sex0=y[1,'sex']
	main=paste(y$var,sex0)
	plotByGroups(y,group.cols=c(NA,'country'),xcol='year',ycols='diff', # ylim=NULL,
		colours=colours,colour.col='country',trends='',main=main)
})
dev.off()

# source('src/analysis-functions.r')
pdf('results/rectify-distribution-sample.pdf',width=6,height=5)
sample.data=annual.hb.dist %>% filter(country=='fi',sex=='Female',year==2013)
# sample.data=annual.hb.dist %>% filter(country=='nl',sex=='Female',year==2013)

plot.rects=TRUE
rectifyOuter(sample.data)
plot.rects=FALSE
dev.off()

plot.rects=TRUE
#### All the rect-plots in a single sheet
lst=by(annual.hb.dist,annual.hb.dist[,c('country','year','sex')],function(x) {
	sex0=x$sex[1]
	country0=x$country[1]
	year0=x$year[1]

	# print(country0)
	filename=paste0('results/rect/',country0,'-',sex0,'-',year0,'.pdf')
	pdf(filename,width=6,height=5)
	rectifyOuter(x)
	# plot.rects=FALSE
	dev.off()

	return(data.frame(country=country0,sex=sex0,year=year0,file=paste0('<img width=500 src="',filename,'">')))
	return(data.frame(country=country0,sex=sex0,year=year0,file=paste0('\\includegraphics[width=10cm]{',filename,'}')))
})
rect.df=do.call(rbind,lst)
rwide=pivot_wider(rect.df,values_from='file',names_from='country')
colnames(rwide)[3:ncol(rwide)]=sapply(colnames(rwide[3:ncol(rwide)]),FUN=function(cn) {paste0(cn.names[[cn]])})
colnames(rwide)
header=rwide[1,]
header=colnames(rwide)
rwide=rbind(header,rwide)
table.rect=apply(rwide,1,function(x) paste0('<tr><td>',paste0(x,collapse='</td><td>'),'</td></tr>'))
table.rect=paste0('<table>',paste(table.rect,collapse='\n'),'</table>')
html.file=sub('¤table¤',table.rect,html.template)
html.file=gsub('results/rect','../results/rect',html.file,fixed=TRUE)
html.file=gsub('NA','',html.file,fixed=TRUE)
convertOutput(html.file,file=paste0(param$shared.dir,'figure-rr rects.html'),nr.of.columns=7,page.width=100)
# convertOutput(html.file,file=paste0(param$shared.dir,'figure-ml levels margins.html'))

####### rectifyDistributions ends

# redefinition of annual.hb (old)
annual.hb = annual.hb.dist %>%
	group_by(data.set,sex,country,year) %>%
	summarise(n.donor=sum(n),hb=sum(hb*n)/sum(n),.groups='drop') # %>%
	data.frame()

lst=by(annual.hb.dist,annual.hb.dist[,c('country','sex','data.set')],function(x) {
bsAssign('x')
	x$year=as.character(x$year)

	x=x %>% inner_join(conversions.df,join_by(country)) %>%
		mutate(hb=hb*rate)

	m=lm(hb~year+0,data=x,weights=n)
	sm=summary(m)

	mean0 = x %>%
		group_by(data.set,year) %>%
		summarise(mean=sum(n*hb)/sum(n),.groups='drop')
	# sd0=sqrt(sum((mean0-x$hb)^2))

	sd.mean = x %>%
		inner_join(mean0,join_by(data.set,year)) %>%
		group_by(data.set,year) %>%
		summarise(sd=sqrt(sum(n*(hb-mean)^2)/sum(n)),n2=sum(n),.groups='drop') %>%
		mutate(sd.mean=sd/sqrt(n2)) %>%
		mutate(year=as.integer(year))

	df=data.frame(sm$coeff)
	df$year=as.integer(sub('year','',rownames(df)))

	df = df %>% inner_join(sd.mean,join_by(year))

	tv=-qt(0.025,df=sm$df[2])
	qv=-qnorm(0.025)

	df=data.frame(country=x$country[1],sex=x$sex[1],data.set=x$data.set[1],var=nm,df,
	 	lower=df$Estimate-qv*df$sd.mean,upper=df$Estimate+tv*df$sd.mean)

	# df=data.frame(country=x$country[1],sex=x$sex[1],data.set=x$data.set[1],var=nm,df,
	# 	lower=df$Estimate-tv*df$Std..Error,upper=df$Estimate+tv*df$Std..Error)
	colnames(df)=sub('Estimate','hb',colnames(df))
	return(df)
})
annual.hb=do.call(rbind,lst)

# These are the total number of donations by country, sex and dataset (for each margin separately)
totals=lapply(names(margins),FUN=function(nm) {
		margins[[nm]] %>%
			group_by(country,data.set,sex) %>%
			summarise(n2=sum(n),.groups='drop') %>%
			mutate(var=nm)
	})
names(totals)=names(margins)

# These are the total number of donations by country, sex and *year* and dataset (for each margin separately)
totals.year=lapply(names(margins),FUN=function(nm) {
		margins[[nm]] %>%
			group_by(country,data.set,sex,year) %>%
			summarise(n2=sum(n),.groups='drop') %>%
			mutate(var=nm)
	})
names(totals.year)=names(margins)

# This is a single distribution (proportions/density) of the totals (by margin still)
# and country, data.set, level, sex
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

# This is a single distribution (proportions/density) of the totals (by margin still)
# and country, data.set, level, sex
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
if (FALSE) {
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
}

# These are the differences between the mean proportions and overall means
dist.diff=inner_join(dist.year,dist,join_by(country,data.set,sex,var,level),suffix=c('','0')) %>%
	mutate(diff=prop-prop0)

plot.old.style=FALSE
# pdf('results/margins.pdf')
pdf('results/hb-margins-levels.pdf',width=12,height=6)
file.pattern='results/hb-¤phase-¤margin-¤sex.pdf'
# file.pattern=''
mar.res=list()
add.legends=list()
add.legends[['results/hb-levels-age-Female.pdf']]='topleft'

# xlab closer to the axis
# https://stackoverflow.com/questions/30265728/in-r-base-plot-move-axis-label-closer-to-axis
# margins as input ~ these correspond roughly to 
for (nm in names(margins)) {
	# Here, the comparison between monitored hb levels and annual means happens
	df=margins[[nm]] %>%
		inner_join(conversions.df,join_by(country)) %>%
		mutate(mean=mean*rate,sd=sd*rate) %>% # mutate(diff=mean1-mean0,diff=diff*rate)
		inner_join(annual.hb,join_by(country,sex,year,data.set)) %>% 
		filter(data.set=='donation0') %>% 
		mutate(hb.dev=mean-hb)

	df[[nm]]=as.character(df[[nm]])

	rlist=by(df,df[,c('sex','country')],function(x) {
		# 2026-05-24 Handle the case that hourly data is missing
bsAssign('x')
# error(110)
		colnames(x)=sub(nm,'level',colnames(x))

		if (length(table(x[[nm]])) == 1) 
			return(NULL) 

		# The regression happens here: so it is based on
		# margins -> df -> x
		# The result goes to var.data
		# frml.char=paste0('hb.dev~',nm,'+0')
		# m=lm(formula(frml.char),weights=n,data=x)
		# sm=summary(m)

		mean0=x %>%
			group_by(level) %>%
			summarise(hb.mean=sum(n*mean)/sum(n),hb.dev=sum(n*hb.dev)/sum(n),.groups='drop') %>%
			dplyr::select(hb.mean,hb.dev,level)

		sd.mean=x %>%
			group_by(level) %>%
			summarise(sd=sqrt(sum(n^2*sd.x^2)/sum(n^2)),n2=sum(n))
		colnames(sd.mean)=sub(nm,'level',colnames(sd.mean))

		# nb! This should be converted to the new style as well
		# Predict and the computation based on tv seem to give the same results
if (FALSE) {
	m=lm(mean~age+0,weights=n,data=x)
	sm=summary(m)
	new.data=unique(x$age)
	df0=data.frame(predict(m,data.frame(age=new.data),interval='confidence'))
	df0$age=new.data
	df0
}
		# df=data.frame(sm$coeff)
		# tv=-qt(0.025,df=sm$df[2])
		df=data.frame(country=x$country[1],sex=x$sex[1],var=nm,mean0) %>%
			inner_join(sd.mean,join_by(level))
		df$std.err=df$sd/sqrt(df$n2)
		df$level=as.integer(df$level)
		df=df %>% arrange(level)
		df = df %>% pivot_longer(cols=c('hb.mean','hb.dev'))
		qv=-qnorm(0.025)
		df = df %>% rename(est=value)
		df$lower=df$est-qv*df$std.err
		df$upper=df$est+qv*df$std.err
		df = df %>% rename(Estimate=est)

		# df=data.frame(country=x$country[1],sex=x$sex[1],var=nm,level=as.integer(sub(nm,'',rownames(df))),df,
		#	lower=df$Estimate-tv*df$Std..Error,upper=df$Estimate+tv*df$Std..Error) %>%
		#	arrange(country,sex,level)
		return(df)
	})
	var.data=do.call(rbind,rlist)
bsAssign('var.data')

	# 2026-05-27 conversions done here as well
	for (cn in names(conversions)) {
		break # nb! not done here anymore

		convert.cols=c('Estimate','Std..Error','lower','upper')
		var.data[var.data$country==cn,convert.cols]= conversions[[cn]]*var.data[var.data$country==cn,convert.cols]
	}

	# Copy from below - not optimal but should suffice
	# plotting deviations
	# Nothing is plotted here, either
	par(mfrow=c(1,2))
	ylim=lim=c(min(var.data[,'Estimate']),max(var.data[,'Estimate']))

	plotCommon=function(var.name,margins) {
		dev.null=by(var.data,var.data$sex,function(y) {
			y=y %>% filter(name==var.name)

			if (nm == 'age') 
				y = y %>% filter(level<=65)

			y = y %>% filter(level!=-1)

			sex0=y$sex[1]
				
			local.plot=FALSE
			main=paste(nm,sex0)
			if (!is.null(file.pattern) && file.pattern!='') {
				# file.pattern='hb-¤phase-¤margin-¤sex.pdf'
				param=list(phase=if(var.name=='hb.dev') 'margins' else 'levels',margin=nm,sex=sex0)
				filename=gsub('[](%,]','_',subFromList(file.pattern,param))
				local.plot=TRUE

				pdf(filename,width=7,height=if(var.name=='hb.dev') 5 else 5-(63-51)/25.4)
				par(mar=margins) # no space at the top; bottom,left,top,right bottom 2.2->0
				par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
				main=''
			}

			xlim=as.numeric(c(min(y[,'level']),max(y[,'level'])))

			ylim=c(min(y[,'Estimate']),max(y[,'Estimate']))
			plot(NULL,xlim=xlim,ylim=ylim,ylab=if(var.name=='hb.dev') 'deviation in hemoglobin (g/L)' else 'hemoglobin (g/L)',xlab=nm,main=main)
			by (y,y$country,function(x) {
				x$level=as.integer(x$level)

				# sex0=x$sex[1]
				country0=x$country[1]
				col0=colours[[country0]]

				wh = which(x$level==-1)
				if (length(wh) > 0) {
					x0=0 # min(x$level[-wh])-1+0.1*if(sex0=='Male') 0.2 else 0
					arrows(x0,x$lower[wh],x0,x$Estimate[wh],length=0.05,angle=90,code=1,col=col0,lwd=1.5)
					arrows(x0,x$Estimate[wh],x0,x$upper[wh],length=0.05,angle=90,code=2,col=col0,lwd=1.5)
					points(x0,x$Estimate[wh],pch=pchs[[sex0]],col=col0)
					x=x[-wh,]
				}

				# 2026-07-11 lower and upper already available here
				lines(x$level,x$Estimate,col=col0,lwd=2) 
				lines(x$level,x$lower,col=col0,lwd=1,lty='dotted')
				lines(x$level,x$upper,col=col0,lwd=1,lty='dotted')
			})

			if (!is.null(filename) && filename %in% names(add.legends)) {
				cn.ids=sort(unique(y$country))
				legend(add.legends[[filename]],fill=unlist(sapply(cn.ids,FUN=colfun)),legend=sapply(cn.ids,FUN=function(cn) {
					paste0(cn.names[[cn]])}))
				
			}

			if (local.plot) 
				dev.off()
		})
	} # plotCommon

	plotCommon('hb.dev',margins=c(4.1,4.1,.1,0.1))
	plotCommon('hb.mean',margins=c(0.4,4.1,.1,0.1))

	mar.res[[nm]]=var.data
}
dev.off() # margins.pdf

# 2026-06-06 In the large scale, above happens the estimation: margins -> crtn
# margins + annual.hb -> hb.dev~level -> crtn (country,sex,var,level + estimates)
crtn=do.call(rbind,mar.res) %>% filter(name=='hb.dev')
crtn$level=as.character(crtn$level)

# the difference is computed as observed-expected
# crtn contains the deviances per margin (mean-hb) estimated from data usin lm
# next compute the corrections per margin: difference in proportion (dist.diff$prop) times the crtn
# the computed corrections will be added (+) to the means

# e.g. 20% donations in January in 2023 vs. 10% on average -> diff=10%
# It was estimated that hb is -1 below average in January: Estimate=-1
# One should like to add 10%*1 to the mean
# Hence the correction should include a multiplier of -1.
# dist.diff can now be used to compute the corrections

crtn.mean0=inner_join(dist.diff[dist.diff$data.set=='donation0',],crtn,join_by(country,sex,var,level)) %>%
	mutate(correction=-diff*Estimate) %>%
	dplyr::select(country,data.set,sex,var,level,year,correction) %>%
	arrange(correction)

crtn.rects=rects %>%
	mutate(data.set='donation0',correction=diff,var='rectification',level=0) %>%
	inner_join(use.years,join_by(country,between(year,y$year.min,y$year.max))) %>%
	dplyr::select(!!!syms(colnames(crtn.mean0)))

crtn.mean=rbind(crtn.mean0,crtn.rects)

crtn.annual=crtn.mean %>%
	group_by(country,data.set,sex,year) %>%
	summarise(correction=sum(correction),.groups='drop') %>%
	data.frame()
# plot(crtn.annual$correction)
# plotByGroups(crtn.annual,group.cols=c('sex','country'),xcol='year',ycols=c('correction'),colours=list(Male='blue3',Female='red3'))

hb.dummy=annual.hb %>%
	filter(data.set=='donation0') # %>%
	# inner_join(conversions.df,join_by(country)) %>% # nb! are the conversions applied again?
	# mutate(hb=rate*hb) 

hb.cmp=inner_join(crtn.annual,hb.dummy,join_by(data.set,sex,country,year,)) %>%
	mutate(hb=hb+correction,country=paste0(country,'-corrected')) %>%
	select(!!!syms(colnames(hb.dummy))) %>%
	rbind(hb.dummy)

# hb.cmp %>% filter(country=='fi') %>% summarize(min(year))
# crtn.annual %>% filter(country=='fi') %>% summarize(min(year))
# source('src/analysis-functions.r')

# pdf('results/trends-corrected.pdf',width=12)
# par(mar=c(4,4,0.5,0.6)) # no space at the top; bottom,left,top,right bottom 2.2->0
# sms=plotByGroups(df.lst,group.cols=c('sex','country'),xcol='year',ycols=c('Estimate','lower','upper'),colours=colours,colour.col='country',trends='table',legend.position='left')
# dev.off()

pdf('results/trends-corrected.pdf',width=12)
par(mar=c(4,4,0.5,0.6)) # no space at the top; bottom,left,top,right bottom 2.2->0
# sms=plotByGroups(hb.cmp,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=colours,colour.col='country',trends='table',legend.position='left')
sms=plotByGroups(hb.cmp,group.cols=c('sex','country'),xcol='year',ycols=c('hb','lower','upper'),colours=colours,colour.col='country',trends='table',legend.position='left',draw.confint=TRUE)
dev.off()

trends.table = sms %>% 
	filter(par=='hb') %>%
	rowwise() %>%
	mutate(corrected=sub('^..','',sub('-corrected','yes',country))) %>%
	mutate(country=sub('-corrected','',country)) %>%
	# rename(p.value=Pr...t.) %>%
	dplyr::select(country,sex,corrected,Estimate,p.value) %>%
	mutate(p.value=round(p.value,4)) %>%
	arrange(desc(corrected)) %>%
	mutate(country=cn.names[[country]])

wh=which(trends.table$p.value<0.05)
if (length(wh) > 0) {
	trends.table$p.value[wh]=paste0('¤',trends.table$p.value[wh],'%')
}

html.table=paste(capture.output(print(xtable(trends.table,digits=5),type='html',include.rownames=FALSE)),collapse='\n')
html.table=gsub('¤([^%]+)%','<b>\\1</b>',html.table)
# cat(gsub('¤([^%]+)%','<b>\\1</b>',html.table))

caption='<b>Table 2</b> Estimated trends by country and sex for both corrected and uncorrected annual hemoglobin mean values'
html.file=sub('¤table¤',paste0(caption,'\n',html.table),html.template)
cat(html.file,file=paste0('submit/','table-2 trends.html'))
# cat(html.table)

### Table of the correction by sex, country, year and variable (month,hour,age)
ctb=crtn.mean %>%
	arrange(country,year,var) %>%
	group_by(country,year,sex,var) %>%
	summarise(crtn=sum(correction),.groups='drop') %>%
	rowwise() %>%
	mutate(country=cn.names[[country]])
	# sapply(cn.ids,FUN=function(cn) { paste0(cn.names[[cn]])})

ctb2=pivot_wider(ctb %>% mutate(crtn=round(crtn,2)),values_from='crtn',names_from='var')
ctb3=full_join(ctb2 %>% filter(sex == 'Female'),ctb2 %>% filter(sex == 'Male'),join_by(country,year))
colnames(ctb3)=sub('\\.[xy]$','',colnames(ctb3))
colnames(ctb3)=sub('rectification','rect',colnames(ctb3))
ctb3=rbind(colnames(ctb3),data.frame(ctb3))
ctb3
# write.xlsx(ctb3,file='results/corrections.xlsx')

# copied from donor-recruitment-prediction/analysis-script.r and modified
#  - etd$year0 is the vertical axis
#  - etd$cdon is the value to be printed
#  - etd$x is the horizontal axis
# i specifies the colour
# the rows represent the individual cells
# all can be done at once
# maybe ctb3 must ...
# hadj is the vector of horizontal adjustments for the text function
# cdws~column widths
plot.et.data = function(etd,cwds=NULL,hadj=0,bold.first.row=TRUE) {
	# etd=vls.df
	ncols=length(table(etd$x))
	if (is.null(cwds)) 
		cwds=rep(1,ncols)

	cumwd=cumsum(cwds)
	start.offset=cumwd-cwds
	
	etd$x0=start.offset[etd$x] # 0.5+
	etd$x1=etd$x0+cwds[etd$x]

	if (length(hadj) < ncols) {
		hadj=c(hadj,rep(0.5,ncols-length(hadj)))
	}

	etd$hadj=hadj[etd$x]

	if (nrow(etd)==0) {
		print('returning')
		return()
	}

	rect(etd$x0,etd$y-0.5,etd$x1,etd$y-0.5+1,col=col_vector[etd$col],border='white')
	etd$font=0
	if (bold.first.row)
		etd$font[etd$y==1]=2

	by(etd,etd[,c('hadj','font')],function(etd.by) {
		etd.by=etd.by %>% filter(!is.na(value),value!='NA')
		ha=etd.by$hadj[1]
		text((1-ha)*etd.by$x0+ha*etd.by$x1,etd.by$y,labels=etd.by$value,cex=0.75,font=etd.by$font[1]) # 1 
	})
}

library(RColorBrewer)

# 2026-06-27 How the heatmap is formed: ctb3 contains the table as it should be printed
inx=expand.grid(row=1:nrow(ctb3),col=1:ncol(ctb3))
vls=lapply(1:nrow(inx),function(x) data.frame(y=inx[x,'row'],x=inx[x,'col'],value=as.character(ctb3[inx[x,'row'],inx[x,'col']])))
vls.df=do.call(rbind,vls)
vls.df$col=NA

col.minus=colorRampPalette(colors=c('red','white'))(30)
col.plus=colorRampPalette(colors=c('white','blue'))(30)
col_vector=c(col.minus,col.plus[-1],'lightgray')

abs.max=max(abs(min(ctb$crtn)),abs(max(ctb$crtn)))
value.seq=seq(from=-abs.max,to=abs.max,len=length(col_vector))
wh=which(grepl('^-?[0-9](\\.[0-9]+)?$',vls.df$value))
cols=left_join(data.frame(value=as.numeric(vls.df$value[wh])),data.frame(inx=1:length(value.seq),tb=value.seq),join_by(closest(x$value<y$tb))) %>% 
	dplyr::select(inx) %>%
	unlist()
vls.df$col[wh]=cols
vls.df$col[-wh]=length(col_vector)

####
col.widths=c(2,rep(1,11))

pdf('results/heatmap.pdf',height=nrow(ctb3)*5/25.4,width=7)
par(mar=c(0.1,1,0.0,0.0)) # bottom,left,top,right bottom 2.2->0
par(mai=c(0,0,0,0))
plot(NULL,xlim=c(0,sum(col.widths)),ylim=rev(c(1-0.5,nrow(ctb3)+0.5)),axes=FALSE,xaxs = "i",yaxs = "i")
plot.et.data(vls.df,col.widths,hadj=0.5)
dev.off()

country.y=unique(vls.df[,c('value','y')] %>% filter(value %in% cn.names))
lbc=vls.df %>%
	left_join(country.y,join_by(y),suffix=c('','.country'))

by(lbc,lbc$value.country,function(x) {
	if (nrow(x) == 1)
		return(NULL)

	cn=x$value[1]

	y0=min(x$y)
	x$y=2+x$y-y0
	x=rbind(vls.df[vls.df$y==1,],x[,-ncol(x)]) %>% 
		filter(x>1)
	x$x=x$x-1

	col.widths.sg=col.widths[-1]

	pdf(paste0('results/heatmap-',cn,'.pdf'),height=max(x$y)*5/25.4,width=6)
	par(mar=c(0.1,1,0.0,0.0)) # bottom,left,top,right bottom 2.2->0
	par(mai=c(0,0,0,0))
	plot(NULL,xlim=c(0,sum(col.widths.sg)),ylim=rev(c(1-0.5,max(x$y)+0.5)),axes=FALSE,xaxs = "i",yaxs = "i")
	plot.et.data(x,col.widths.sg,hadj=0.5)
	dev.off()
})

# heatmaps for age, month and hour
# copied from a draft file heatmap-dist-draft.r
distHeatmap = function(var0,country0,filename='results/dist-heatmap-¤var-¤country.pdf') {
	dist.hm=dist.year
	dist.hm$cn=sapply(dist.hm$country,function(x) cn.names[[x]])
	dist.hm$prop[dist.hm$sex=='Female']=-1*dist.hm$prop[dist.hm$sex=='Female']
	dist.hm=dist.hm %>% 
		filter(data.set=='donation0') %>%
		dplyr::select(cn,level,sex,year,var,prop) %>%
		filter(var==var0) %>% # parameter
		pivot_wider(names_from='level',values_from='prop')
	dist.hm = dist.hm %>% filter(cn==country0) # parameter
	wh=grep('-?[0-9]+',colnames(dist.hm),value=FALSE)
	level.cols=colnames(dist.hm)[wh]
	level.cols=sort(as.integer(level.cols))
	dist.hm=dist.hm[,c(colnames(dist.hm)[1:(min(wh)-1)],as.character(level.cols))]
	dist.hm=dist.hm %>% dplyr::select(-one_of(c('var')))
	level.cols=as.character(level.cols)
	dist.hm[,level.cols]=round(dist.hm[,level.cols],2)
	dist.hm %>% data.frame()
bsAssign('dist.hm')
	sbs=do.call(cbind,by(dist.hm,dist.hm$sex,function(x) x)) 
	use.columns=grep('([0-9]$)|Female.year|Male.sex',colnames(sbs),value=TRUE)
	dist.hm=sbs %>% 
		dplyr::select(one_of(use.columns)) %>%
		rename(year=Female.year,sep.col=Male.sex)
	dist.hm$sep.col=NA

	wh.num=grep('[0-9]$',colnames(dist.hm))
	abs.max=max(dist.hm[,wh.num],na.rm=TRUE) # max(abs(min(ctb$crtn)),abs(max(ctb$crtn)))

	header=paste0('header',sub('[^0-9\\-]+(-?[0-9]+$)?','\\1',colnames(dist.hm)))
	header[1]='year'
	if (length(header)>50) {
		wh.10=grep('0$',header)
		header[-wh.10]=''
	}
	dist.hm=rbind(header,dist.hm)

	inx=expand.grid(row=1:nrow(dist.hm),col=1:ncol(dist.hm))
	vls=lapply(1:nrow(inx),function(x) data.frame(y=inx[x,'row'],x=inx[x,'col'],value=as.character(dist.hm[inx[x,'row'],inx[x,'col']])))
	vls.df=do.call(rbind,vls)
	vls.df$col=NA

	col.minus=colorRampPalette(colors=c('red','white'))(50)
	col.plus=colorRampPalette(colors=c('white','blue'))(50)
	col_vector=c(col.minus,rep('white',3),col.plus[-1])
	length(col_vector)

	# The expansion by 1/1000 is needed to get a colour for the maximum value as well
bsAssign('col_vector')
bsAssign('vls.df')
	value.seq=seq(from=-1.001*abs.max,to=1.001*abs.max,len=length(col_vector))
	wh=which(grepl('^-?[0-9](\\.[0-9]+)?$',vls.df$value))
	cols=left_join(data.frame(value=as.numeric(vls.df$value[wh])),data.frame(inx=1:length(value.seq),tb=value.seq),join_by(closest(x$value<y$tb))) %>% 
		dplyr::select(inx) %>%
		unlist()

	vls.df$col[wh]=cols
	vls.df$col[-wh]=length(col.minus) # 1#length(col_vector)
	vls.df$value[wh]=NA # dont' print the labels here
	table(vls.df$value,useNA='ifany')

	str(vls.df)
	vls.df$value=sub('^header','',vls.df$value)

	col.widths=rep(1,ncol(dist.hm)) # c(5,3,rep(1,ncol(dist.hm)-2))
	s0=sum(col.widths)
	col.widths[1]=s0/20
	col.widths[colnames(dist.hm)=='sep.col']=s0/50

	if (!is.null(filename)) {
		filename=sub('¤var',var0,sub('¤country',country0,filename))
		pdf(filename,height=nrow(dist.hm)*5/25.4,width=7)
	}

	par(mar=c(0.1,1,0.0,0.0)) # bottom,left,top,right bottom 2.2->0
	par(mai=c(0,0,0,0))

	plot(NULL,xlim=c(0,sum(col.widths)),ylim=rev(c(1-0.5,nrow(dist.hm)+0.5)),axes=FALSE,xaxs = "i",yaxs = "i")
	plot.et.data(vls.df,col.widths,hadj=0.5)

	if (!is.null(filename))
		dev.off()

	return(dist.hm)
}

dist.year %>%
	group_by(country,var) %>%
	summarise(min.level=min(level,na.rm=TRUE),max.level=max(level,na.rm=TRUE),.groups='drop') %>%
	# rowwise() %>%
	filter(min.level!=max.level) %>%
	apply(1,function(x) {bsAssign('x'); str(x); print(x); distHeatmap(x[['var']],cn.names[[x[['country']]]])})
