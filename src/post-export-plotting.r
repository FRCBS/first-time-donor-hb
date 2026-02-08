file.paths = sub('\\.xlsx','-hb.xlsx',param$result.file) # paste(param$data.dir,file.names,sep='')
plotByGroups = function(data,group.cols=c('sex','country'),xcol='level',ycols=c('Estimate','lower','upper'),
		ltys=list(cm='dashed',fi='solid'),colours=list(Male='blue3',Female='red3'),main='',colour.col='sex') {

	xmin=min(data[[xcol]][data[[xcol]]>=0])-1
	ylim=c(min(data[,ycols]),max(data[,ycols]))
	yspan=(ylim[2]-ylim[1])
	plot(x=NULL,xlim=c(xmin,max(data[[xcol]])),ylim=c(ylim[1]-0.5*yspan,ylim[2]),
		main=if(main!='') main else '',xlab=xcol,ylab=ycols[1])
	lgnd=by(data,data[,group.cols[!is.na(group.cols)]],function(x) {
			sex0=x[1,group.cols[1]] 
			country0=x[1,group.cols[2]] 

			if (grepl('-corrected',country0)) {
				country0=sub('-.+$','',country0)
				ltys[[country0]]='dashed'
			}

			col0=NA
			if (!is.null(sex0) && !is.na(sex0) && colour.col=='sex') {
				col0=pp.cols[[sex0]]
			} else {
				col0=colours[[country0]]
			}

			wh = which(x[[xcol]]==-1)
			if (length(wh) > 0) {
				x0=min(x[[xcol]][-wh])-1+0.1*if(sex0=='Male') 0.2 else 0
				arrows(x0,x[[ycols[2]]][wh],x0,x[[ycols[1]]][wh],length=0.05,angle=90,code=3,col=col0,lwd=1.5)
				arrows(x0,x[[ycols[1]]][wh],x0,x[[ycols[3]]][wh],length=0.05,angle=90,code=3,col=col0,lwd=1.5)
				x=x[-wh,]
			}

			m=lm(x[[ycols[1]]]~x[[xcol]])

			if (!country0 %in% names(ltys))
				ltys[[country0]]='solid'

			lines(x[[xcol]],x[[ycols[1]]],col=col0,lwd=2,lty=ltys[[country0]])
			lines(x[[xcol]],x[[ycols[3]]],col=col0,lwd=1,lty='dotted')
			lines(x[[xcol]],x[[ycols[2]]],col=col0,lwd=1,lty='dotted')

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
	if (identifier=='exported') {
		identifier=param$country
	}

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
pdf('results/hb-margins.pdf')
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

# plot(crtn.mean$correction)
# summary(crtn.mean)
# crtn.mean # abs(min)=abs(max)~0.37 # no huge corrections

crtn.annual=crtn.mean %>%
	group_by(country,data.set,sex,year) %>%
	summarise(correction=sum(correction),.groups='drop') %>%
	data.frame()

is.test.plots=FALSE
if (is.test.plots) {
	plot(crtn.annual$correction)
	plotByGroups(crtn.annual,group.cols=c('sex','country'),xcol='year',ycols=c('correction'),colours=list(Male='blue3',Female='red3'))
}

hb.dummy=annual.hb %>%
	filter(data.set=='donation0')

if (is.test.plots) {
	plotByGroups(hb.dummy,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=list(Male='blue3',Female='red3'))
}

hb.cmp=inner_join(crtn.annual,hb.dummy,join_by(data.set,sex,country,year,)) %>%
	mutate(hb=hb+correction,country='cm') %>%
	select(!!!syms(colnames(hb.dummy))) %>%
	rbind(hb.dummy) %>%
	dplyr::filter(year>=2002,year<2024)

pdf('results/hb-trends-corrected.pdf')
plotByGroups(hb.cmp,group.cols=c('sex','country'),xcol='year',ycols=c('hb'),colours=list(Male='blue3',Female='red3'),ltys=list(cm='dashed',fi='solid'))
dev.off()

### end of hb

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

#####################
# survival plotting

#####
# Below, some plots are drawn for survival data
colours=list()
colours[['top 10%']]='blue3'
colours[['top 10-25%']]='lightblue'
colours[['bottom 10-25%']]='pink'
colours[['bottom 10%']]='red3'
colours[['(25,40]']]='green3'
colours[['(40,100]']]='gray3'
colours[['O-']]='blue3'
colours[['general']]='black'

lvs=unique(res.models$level[res.models$var=='age.group.t']) # levels(dlink$age.group.t)
palette=colorRampPalette(c("blue4", "white"))(length(lvs)+3)

library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category=='qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# the qual palette as constants (the brewer package must be installed separately, want to avoid that)
col_vector=c('#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17','#666666','#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E','#E6AB02','#A6761D','#666666','#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928','#FBB4AE','#B3CDE3','#CCEBC5','#DECBE4','#FED9A6','#FFFFCC','#E5D8BD','#FDDAEC','#F2F2F2','#B3E2CD','#FDCDAC','#CBD5E8','#F4CAE4','#E6F5C9','#FFF2AE','#F1E2CC','#CCCCCC','#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#FFFF33','#A65628','#F781BF','#999999','#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3','#8DD3C7','#FFFFB3','#BEBADA','#FB8072','#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD','#CCEBC5','#FFED6F')

for (i in 1:length(lvs)) {
	colours[[lvs[i]]]=if (i<length(lvs)) col_vector[i] else 'white' # palette[i]
}

getIntervals = function(breaks) {
	brs=strsplit(breaks,',')[[1]]
	mid.bits=sapply(2:(length(brs)-2),function(x) paste0('(',brs[x],',',brs[x+1],']'))
	lower=paste0('<',brs[2])
	upper=paste0('>',brs[length(brs)-1])
	return(c(lower,mid.bits[-2],upper))
}

# y=res.models.all %>% filter(sex=='Female',var=='ord.group.full')

res.models %>% filter(var=='sex') %>% arrange(country,sex)

pdf('results/survival-figures.pdf')
by(res.models,res.models[,c('sex','var')],function(y) {
	if (length(unique(y$level))==1)
		y$level='general'

	y=y %>% filter(!is.na(ord))

	if (nrow(y) == 1)
		return(NULL)

	ylim=c(min(y$exp.coef,na.rm=TRUE),max(y$exp.coef,na.rm=TRUE)) #c(0.70,1.40)
	y.delta=ylim[2]-ylim[1]
	ylim=c(ylim[1]-0.3*y.delta,ylim[2]+0.2*y.delta)
	plot(x=NULL,type='n',xlim=c(1,max(y$ord)),ylim=ylim,main=paste(y$sex[1],y$var[1]),
		xlab='number of donation',ylab='relative likelihood of donation')
	abline(h=1,lwd=1,lty='dashed')
	breaks=y$breaks[1]

	# factor out the age.group.t breaks
	if (grepl(';',breaks))
		breaks='-' 

	col.fun=function(x) {if (x %in% names(colours)) colours[[x]] else 'white'}

	if (max(y$ord) == param$max.ord.group.number+1) 
		abline(v=max(y$ord)-0.5,lwd=1,lty='dashed')

	brk.labels=if (breaks!='-') getIntervals(breaks) else ''
	levels=(unique(y$level))
	if (length(levels) > 1) {
		fill=sapply(levels,col.fun)
		levels=levels[fill!='white']
		fill=fill[fill!='white']
		legend(x='bottomright',legend=paste(levels,brk.labels),fill=sapply(levels,col.fun))
	}

	res.void=by(y,y[,c('var','level')],function(x) {
			gr=x$level[1]
			col=col.fun(gr)
			if (col=='white')
				return(NULL)
			ordint=x$ord
			lines(ordint,x$exp.coef,lwd=2,col=col)
			lines(ordint,x$lower..95,lwd=2,lty='dotted',col=col)
			lines(ordint,x$upper..95,lwd=2,lty='dotted',col=col)
		})
	})
dev.off()

pdf('results/survival-curves.pdf')
dummy=by(res.curves,res.curves[,c('ord','sex')],function(df) {
		wh=min(which(df$surv<0.99))
		len0=length(df$surv)
		df=df[wh:len0,]
		df$time=df$time[wh:len0]-wh
		df$sqrt.x=sqrt(df$time)

		# This is promising based on description, but seems not to work after all
		# m.ss=nls(surv~SSweibull(time,yf,y0,log_alpha,poweri),data=df)
		m.ss=nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df)
		sm=summary(m.ss)

		plot(surv~time,data=df,type='l',lwd=2,xlim=c(0,2*365),ylim=c(0,1),
			main=paste(df$sex[1],df$ord[1]))
		pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))
		lines(df$time,pred.ss,col='red2',lwd=2.5)

		tv=qt(0.025,df=sm$df[2])
		return(data.frame(sex=df$sex[1],ord=df$ord[1],var=rownames(sm$coeff),sm$coeff,lower=sm$coeff[,1]-tv*sm$coeff[,2],upper=sm$coeff[,1]+tv*sm$coeff[,2]))
	})
dev.off()

ce=do.call(rbind,dummy)

# nb! should use the similar implementation in the previous project to enable easier and more parametric
# plotting with countries etc.
plotParameters = function(ce,xvar='log_alpha',yvar='yf',col.var='sex',pp.cols=list(Male='blue3',Female='red3'),shade.var='ord') {
	lims = ce %>% 
		group_by(var) %>%
		summarise(min=min(Estimate),max=max(Estimate)) %>%
		data.frame()
	rownames(lims)=lims$var
	lims$var=NULL

	# plot(x=NULL,xlim=c(-3,-1),ylim=c(0,0.33),xlab='exponent',ylab='asymptote')
	plot(x=NULL,xlim=t(lims[xvar,]),ylim=t(lims[yvar,]),xlab='exponent',ylab='asymptote')
	for (gr in names(pp.cols)) {
		xdata=ce %>%
			filter(as.character(!!!syms(col.var))==gr,var==xvar) %>%
			arrange(!!!syms(shade.var))
		ydata=ce %>%
			filter(as.character(!!!syms(col.var))==gr,var==yvar) %>%
			arrange(!!!syms(shade.var))

		shades=colorRampPalette(c(pp.cols[[gr]],"white"))(5+nrow(xdata))

		col0=col=shades[xdata$ord]
		points(xdata$Estimate,ydata$Estimate,col=col0,lwd=3,pch=10)
		lines(xdata$Estimate,ydata$Estimate,col=col0,lwd=1,lty='dashed')

		# This is done to make the error bars at least somewhat visible
		# col0='black'
		arrows(xdata$Estimate,ydata$lower,xdata$Estimate,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
		arrows(xdata$Estimate,ydata$Estimate,xdata$Estimate,ydata$upper,length=0.05,angle=90,code=3,col=col0)

		arrows(xdata$lower,ydata$Estimate,xdata$Estimate,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
		arrows(xdata$Estimate,ydata$Estimate,xdata$upper,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
	}
}

pdf('results/survival-parameters.pdf')
plotParameters(ce)
dev.off()
