# TODO must create 

setwd('c:/hy-version/first-time-donor-hb')
source('src/analysis-functions.r')

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
	summarise(mean=sum(n*mean)/sum(n),n=sum(n),nas=sum(nas),deferred=sum(deferred),.groups='drop') %>%
	rename(hour=hour.group) %>%
	dplyr::select(country,data.set,year,sex,hour,n,nas,deferred,mean)

# use the newly formed grouped hourly statistics instead of the original one
margins[['hour']]=hourly.grouped

# 2026-05-25
use.years=annual.hb %>%
	group_by(country) %>%
	summarise(year.min=min(year)+2,year.max=max(year)-1,.groups='drop')

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
str(cutoffs)

res.list=by(annual.hb.dist,annual.hb.dist[,c('country','sex','year')],function(df) {
	df=df[df$data.set=='donation0',]
	sex0=df$sex[1]
	country0=df$country[1]
	year0=df$year[1]
	cutoff=as.numeric(cutoffs[cutoffs$name==sex0&cutoffs$country==country0,'value'])

	hb.decimals=max(sapply(df$hb,decimalPlaces))
	rv=rectifyDistribution(data0=NULL,freq=df,cutoff=cutoff,hb.decimals=hb.decimals,plot=FALSE)

	return(data.frame(country=country0,sex=sex0,year=year0,mean0=rv$param$mean0,mean1=rv$param$mean))
})
rects=do.call(rbind,res.list)
rects = rects %>% 
	inner_join(conversions.df,join_by(country)) %>%
	mutate(diff=diff*rate)
rects$diff=rects$mean1-rects$mean0

pdf('results/hb-rectifications.pdf')
par(mfrow=c(1,2))
ycols=c('exp.coef','lower..95','upper..95')
ylim=c(min(rects[,ycols]),max(rects[,ycols]))
by(rects,rects[,'sex'],function(y) {
	sex0=y[1,'sex']
	main=paste(y$var,sex0)
	plotByGroups(y,group.cols=c(NA,'country'),xcol='year',ycols='diff',ylim=NULL,
		colours=colours,colour.col='country',trends='',main=main)
})
dev.off()
####### rectifyDistributions ends

annual.hb = annual.hb.dist %>%
	group_by(data.set,sex,country,year) %>%
	summarise(n.donor=sum(n),hb=sum(hb*n)/sum(n),.groups='drop') # %>%
	data.frame()

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
mar.res=list()
# xlab closer to the axis
# https://stackoverflow.com/questions/30265728/in-r-base-plot-move-axis-label-closer-to-axis
for (nm in names(margins)) {
	# Here, the comparison between monitored hb levels and annual means happens
	df=inner_join(margins[[nm]],annual.hb,join_by(country,sex,year,data.set)) %>% 
		filter(data.set=='donation0') %>% 
		mutate(hb.dev=mean-hb)

	df[[nm]]=as.character(df[[nm]])

	rlist=by(df,df[,c('sex','country')],function(x) {
			# 2026-05-24 Handle the case that hourly data is missing
			if (length(table(x[[nm]])) == 1) 
				return(NULL) 

			frml.char=paste0('hb.dev~',nm,'+0')
			m=lm(formula(frml.char),weights=n,data=x)
			sm=summary(m)

			df=data.frame(sm$coeff)
			tv=-qt(0.025,df=sm$df[2])
			df=data.frame(country=x$country[1],sex=x$sex[1],var=nm,level=as.integer(sub(nm,'',rownames(df))),df,
				lower=df$Estimate-tv*df$Std..Error,upper=df$Estimate+tv*df$Std..Error) %>%
				arrange(country,sex,level)
			return(df)
		})
	var.data=do.call(rbind,rlist)

	# 2026-05-27 conversions done here as well
	for (cn in names(conversions)) {
		convert.cols=c('Estimate','Std..Error','lower','upper')
		var.data[var.data$country==cn,convert.cols]= conversions[[cn]]*var.data[var.data$country==cn,convert.cols]
	}

	if (plot.old.style) {
		# pp.cols=list(Male='blue3',Female='red3') # not needed anymore
		xmin=min(var.data$level[var.data$level>=0])-1
		plot(x=NULL,xlim=c(xmin,max(var.data$level)),ylim=c(min(var.data$lower),max(var.data$upper)),
			main=paste(nm),xlab=nm,ylab='deviation from mean hb')
		by(var.data,var.data[,c('country','sex')],function(x) {
				sex0=x$sex[1]
				country0=x$country[1]

				# col0=pp.cols[[sex0]]
				col0=colours[[country0]]

				wh = which(x$level==-1)
				if (length(wh) > 0) {
					x0=min(x$level[-wh])-1+0.1*if(sex0=='Male') 0.2 else 0
					arrows(x0,x$lower[wh],x0,x$Estimate[wh],length=0.05,angle=90,code=1,col=col0,lwd=1.5)
					arrows(x0,x$Estimate[wh],x0,x$upper[wh],length=0.05,angle=90,code=2,col=col0,lwd=1.5)
					points(x0,x$Estimate[wh],pch=pchs[[sex0]],col=col0)
					x=x[-wh,]
				}

				lines(x$level,x$Estimate,col=col0,lwd=2,lty=if (sex0=='Female') 'solid' else 'dashed') # ltys[[sex0]])
				# lines(x$level,x$upper,col=col0,lwd=1,lty='dashed')
				# lines(x$level,x$lower,col=col0,lwd=1,lty='dashed')
			})
	}

	# Copy from below - not optimal but should suffice
	par(mfrow=c(1,2))
	ylim=lim=c(min(var.data[,'Estimate']),max(var.data[,'Estimate']))
	by(var.data,var.data$sex,function(y) {
		if (nm == 'age') 
			y = y %>% filter(level<=65)

		sex0=y$sex[1]
			
		local.plot=FALSE
		main=paste(nm,sex0)
		if (!is.null(file.pattern) && file.pattern!='') {
			# file.pattern='hb-¤phase-¤margin-¤sex.pdf'
			param=list(phase='margins',margin=nm,sex=sex0)
			filename=gsub('[](%,]','_',subFromList(file.pattern,param))
			local.plot=TRUE

			pdf(filename,width=7,heigh=5)
			# par(mar=c(0.1,5.5,0.5,0.6)) # no space at the top; bottom,left,top,right bottom 2.2->0
			par(mar=c(4.1,4.1,.1,0.1)) # no space at the top; bottom,left,top,right bottom 2.2->0
			par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
			main=''
		}

		xlim=as.numeric(c(min(y[,'level']),max(y[,'level'])))

		ylim=c(min(y[,'Estimate']),max(y[,'Estimate']))
		plot(NULL,xlim=xlim,ylim=ylim,ylab='hb',xlab=nm,main=main)
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

			lines(x$level,x$Estimate,col=col0,lwd=2)
		})

		if (local.plot) 
			dev.off()
	})
	# dev.off()

	# pdf('results/hb-levels.pdf')
	par(mfrow=c(1,2))
	
	marnm=margins[[nm]] %>%
		filter(data.set=='donation0') %>%
		mutate(level=as.integer(!!!syms(nm))) %>%
		inner_join(conversions.df,join_by(country)) %>%
		mutate(mean=mean*rate)

	by(marnm,marnm$sex,function(y) {
		sex0=y$sex[1]
		y = y %>% 
			# filter(data.set=='donation0') %>%
			# mutate(level=as.integer(!!!syms(nm))) %>%
			group_by(country,level) %>%
			summarise(mean=sum(n*mean)/sum(n),.groups='drop') %>%
			# inner_join(conversions.df,join_by(country)) %>%
			# mutate(mean=mean*rate) %>%
			arrange(country,level) %>%
			data.frame()

		if (nm == 'age') 
			y = y %>% filter(level<=65)
			
		local.plot=FALSE
		main=paste(nm,sex0)
		if (!is.null(file.pattern) && file.pattern!='') {
			# file.pattern='hb-¤phase-¤margin-¤sex.pdf'
			param=list(phase='levels',margin=nm,sex=sex0)
			filename=gsub('[](%,]','_',subFromList(file.pattern,param))
			local.plot=TRUE

			pdf(filename,width=7,heigh=5)
			par(mar=c(4.1,4.1,.1,0.1)) # no space at the top; bottom,left,top,right bottom 2.2->0
			par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
			main=''
		}

		xlim=as.numeric(c(min(y[,'level']),max(y[,'level'])))
		ylim=c(min(y[,'mean']),max(y[,'mean']))

		plot(NULL,xlim=xlim,ylim=ylim,ylab='hb',xlab=nm,main=main)
		by (y,y$country,function(x) {
			x$level=as.integer(x$level)

			# sex0=x$sex[1]
			country0=x$country[1]

			col0=colours[[country0]]

			wh = which(x$level==-1)
			if (length(wh) > 0) {
				x0=0 # min(x$level[-wh])-1+0.1*if(sex0=='Male') 0.2 else 0
				# arrows(x0,x$lower[wh],x0,x$Estimate[wh],length=0.05,angle=90,code=1,col=col0,lwd=1.5)
				# arrows(x0,x$Estimate[wh],x0,x$upper[wh],length=0.05,angle=90,code=2,col=col0,lwd=1.5)
				points(x0,x$mean[wh],pch=pchs[[sex0]],col=col0)
				x=x[-wh,]
			}

			lines(x$level,x$mean,col=col0,lwd=2)
		})

		if (local.plot) 
			dev.off()
	})
	# dev.off()

	mar.res[[nm]]=var.data
}
dev.off() # margins.pdf

crtn=do.call(rbind,mar.res)
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
	dplyr::select(!!!syms(colnames(crtn.mean)))

crtn.mean=rbind(crtn.mean0,crtn.rects)

crtn.annual=crtn.mean %>%
	group_by(country,data.set,sex,year) %>%
	summarise(correction=sum(correction),.groups='drop') %>%
	data.frame()
# plot(crtn.annual$correction)

conversions.df=data.frame(t(data.frame(conversions)))
colnames(conversions.df)='rate'
conversions.df$country=rownames(conversions.df)

# plotByGroups(crtn.annual,group.cols=c('sex','country'),xcol='year',ycols=c('correction'),colours=list(Male='blue3',Female='red3'))

hb.dummy=annual.hb %>%
	filter(data.set=='donation0') %>%
	inner_join(conversions.df,join_by(country)) %>% # nb! are the conversions applied again?
	mutate(hb=rate*hb) # %>%
	# inner_join(use.years,join_by(country,between(year,y$year.min,y$year.max))) %>%
	# select(-year.min) %>%
	# select(-year.max)

plotByGroups(hb.dummy,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=list(Male='blue3',Female='red3'))

hb.cmp=inner_join(crtn.annual,hb.dummy,join_by(data.set,sex,country,year,)) %>%
	mutate(hb=hb+correction,country=paste0(country,'-corrected')) %>%
	select(!!!syms(colnames(hb.dummy))) %>%
	rbind(hb.dummy)

# hb.cmp %>% filter(country=='fi') %>% summarize(min(year))
# crtn.annual %>% filter(country=='fi') %>% summarize(min(year))
# source('src/analysis-functions.r')

pdf('results/trends-corrected.pdf',width=12)
sms=plotByGroups(hb.cmp,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=colours,colour.col='country',trends='table')
dev.off()

trends.table = sms %>% 
	filter(par=='hb') %>%
	rowwise() %>%
	mutate(corrected=sub('^..','',sub('-corrected','yes',country))) %>%
	mutate(country=sub('-corrected','',country)) %>%
	# rename(p.value=Pr...t.) %>%
	dplyr::select(country,sex,corrected,Estimate,p.value) %>%
	mutate(p.value=round(p.value,4)) %>%
	arrange(desc(corrected))
wh=which(trends.table$p.value<0.05)
if (length(wh) > 0) {
	trends.table$p.value[wh]=paste0('¤',trends.table$p.value[wh],'%')
}

html.table=paste(capture.output(print(xtable(trends.table,digits=5),type='html',include.rownames=FALSE)),collapse='\n')
html.table=gsub('¤([^%]+)%','<b>\\1</b>',html.table)
cat(gsub('¤([^%]+)%','<b>\\1</b>',html.table))

caption='Estimated trends by country and sex for both corrected and uncorrected annual hemoglobin mean values'
html.file=sub('¤table¤',paste0(caption,'\n',html.table),html.template)
cat(html.file,file=paste0('results/','trends.html'))
cat(html.table)

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
# maybe ctb3 must 
plot.et.data = function(etd,col.widths=NULL,hadj=0,bold.first.row=TRUE) {
	# etd=vls.df
	ncols=length(table(etd$x))
	if (is.null(col.widths)) 
		col.widths=rep(1,ncols)

	cumwd=cumsum(col.widths)
	start.offset=cumwd-col.widths
	
	etd$x0=start.offset[etd$x] # 0.5+
	etd$x1=etd$x0+col.widths[etd$x]

	if (length(hadj) < ncols) {
		hadj=c(hadj,rep(0.5,ncols-length(hadj)))
	}

	etd$hadj=hadj[etd$x]

	if (nrow(etd)==0) {
		print('returning')
		return()
	}

	# etd$x-0.5, etd$x-0.5+1
	rect(etd$x0,etd$y-0.5,etd$x1,etd$y-0.5+1,col=col_vector[etd$col],border='white')
	# etd$x, (etd$x0+etd$x1)/2
	# text((etd$x0+etd$x1)/2,etd$y,labels=etd$value,cex=0.75)

	etd$font=0
	if (bold.first.row)
		etd$font[etd$y==1]=2

	by(etd,etd[,c('hadj','font')],function(etd.by) {
bsAssign('etd.by')
		# text((etd.by$x0+etd.by$x1)/2,etd.by$y,labels=etd.by$value,cex=0.75,adj=c(etd.by$hadj[1],0.5),font=etd.by$font[1]) # 1 

		ha=etd.by$hadj[1]
		text((1-ha)*etd.by$x0+ha*etd.by$x1,etd.by$y,labels=etd.by$value,cex=0.75,font=etd.by$font[1]) # 1 
		# text(etd.by$x0,etd.by$y,labels=etd.by$value,cex=0.75) # round(etd$value,1) # ,adj=c(etd.by$hadj[1],0.5)
	})
}

library(RColorBrewer)

# ctb4=ctb3
# ctb4$y=1:nrow(ctb3)
inx=expand.grid(row=1:nrow(ctb3),col=1:ncol(ctb3))
vls=lapply(1:nrow(inx),function(x) data.frame(y=inx[x,'row'],x=inx[x,'col'],value=as.character(ctb3[inx[x,'row'],inx[x,'col']])))
vls.df=do.call(rbind,vls)
vls.df[1:10,]
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
plot.et.data(vls.df,col.widths,hadj=0.5) # )
# rect(0,1,sum(col.widths),nrow(ctb3),lwd=3,col='red')
dev.off()

# pdata=pivot_longer(dfdona,cols=starts_with('X'),names_to='year',names_prefix='X',values_to='donations') %>%
# pivot_longer(ctb3,cols=colnames(ctb3),names_to=

# nb! Should produce the heatmap tables in R instead to make it automatic