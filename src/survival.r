library(dplyr)
library(survival)
library(RColorBrewer)
library(openxlsx)

# setwd('C:\\git_repot\\first-time-donor-hb\\src')

param=list()
param$data.file = "c:/git_repot/DATA/donationdata.fortimo.rdata" # fi
param$donation.type.keys = c('Whole Blood (K)')
param$max.sample.size=1e7 # This is still reasonably fast (< 1 min)
param$cutoff.male=135
param$cutoff.female=125
param$max.ord.group.number=15

param$omit.data=list()

# For Finland, omit the donations from garrisons to create a second version of the data set
param$omit.data$DonationPlaceType='Garrison'

# getwd() might behave differently depending on the environment where it is run (console vs. Rmd),
# therefore checking if working directory is set to the src folder and moving up if yes.
param$wd = getwd()
if (grepl('[/\\]src[/\\]?',param$wd)) {
   param$wd = sub('[/\\]src([/\\]?)$','\\1',param$wd)
}

setwd(param$wd)

dir.create(file.path(param$wd,"results"),showWarnings = FALSE)
# dir.create(file.path(param$wd,"log"),showWarnings = FALSE)
param$result.file = file.path(param$wd,"results","exported-survival-data.xlsx")

if (!exists('donationdata'))
	load(param$data.file)

####
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

donation.simple = donationdata$donation[donationdata$donation$BloodDonationTypeKey %in% param$donation.type.keys,c('rowid','numid','DonationDate','Hb')] %>% 
	inner_join(donationdata$donor[,c('numid','Sex','BloodGroup','DateOfBirth')],join_by(numid)) %>%
	arrange(numid,DonationDate)

# donation.simple$dtEnd = as.Date("2020-01-01") # nb! This is set to NA later
# donation.simple$type = 'donation'
# donation.simple$age= as.numeric(difftime(donation.simple$DonationDate,donation.simple$DateOfBirth),unit="weeks")/52.25
donation.simple = donation.simple %>% 
	group_by(numid) %>%
	mutate(ord = row_number()) %>%
	ungroup() %>%
	# dplyr::select(-DateOfBirth) %>%
	rename(date=DonationDate)

str(donation.simple)
str(donation0)
donation0=donation.simple %>% filter(ord==1) %>% dplyr::select(numid,date) %>% rename(date0=date)
donation.simple=donation.simple %>%
	inner_join(donation0,join_by(numid))

colnames(donation.simple)=c('rowid','numid','date','hb','sex','bloodgroup','dateofbirth','ord','date0')

donation.simple$bloodgr='other'
donation.simple$bloodgr[donation.simple$bloodgroup=='O-']='O-'
donation.simple$bloodgr=as.factor(donation.simple$bloodgr)
donation.simple$bloodgr=relevel(donation.simple$bloodgr,ref='other')

donation.simple$age=as.numeric(difftime(donation.simple$date0,donation.simple$dateofbirth),unit="weeks")/52.25
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

dlink = donation.simple %>%
	left_join(donation.simple[,c('numid','ord','date')],join_by(numid,x$ord.next==y$ord)) %>%
	left_join(donation.simple[,c('numid','ord','hb')],join_by(numid,x$ord.prev==y$ord),suffix=c('','.prev')) %>%
	dplyr::select(-ord.next,-ord.prev) %>%
	mutate(hb.change=hb-hb.prev)

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

do.coxph.inner = function(data0) {
bsAssign('data0')
	hb.var=colnames(data0)[ncol(data0)]
	sex0=data0$sex[1]

	breaks.str='-'

	data=data0[[hb.var]]
	if (!is.factor(data) && length(unique(data)) > 10) {
		breaks.ord=quantile(data,prob=c(0,0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE)
		print(breaks.ord)
		breaks.str=df.breaks %>% filter(sex==sex0,var==hb.var) %>% dplyr::select(breaks) %>% as.character()
		breaks.common=strsplit(breaks.str,',')[[1]]
		print(paste('***',breaks.ord))
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

	data0 = data0 %>%
		dplyr::select(-ord.group,-sex)

	frml.char=paste0('Surv(diff,event)~.')
	m=coxph(formula(frml.char),data=data0)

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
vars = c('avg.diff','hb.surplus','hb.change','age.group','bloodgr')
hb.vars = c('avg.diff','hb.surplus','hb.change')
spec=expand.grid(grp.var=c(NA,'sex'),hb.var=vars)
cols.prefix=c('diff','event','sex','ord.group')

spec=data.frame(hb.var=vars)
spec.curves=data.frame(hb.var='-')

# compute the breaks used to group hb-variables in the cox regressions
res.breaks=lapply(hb.vars,function(x) {
		min.x=min(dlink[!is.na(dlink[[x]]),'ord']) # 1 or 2
		data.br=dlink[dlink$ord==min.x,c('sex',x)]
		br.list=lapply(c('Male','Female'),function(y) {
				brs=quantile(data.br[data.br$sex==y,x],prob=c(0,0.1,0.25,0.75,0.9,1),names=FALSE,na.rm=TRUE)
				data.frame(var=x,sex=y,breaks=paste(brs,collapse=','))
			})
		do.call(rbind,br.list)
	})
df.breaks=do.call(rbind,res.breaks)

getResults=function(dlink,spec) {
	res=by(spec,spec,function(x) {
		if (x=='-') {
			x=NULL
		} else
			x=c(t(x))
		
		coeff.list=by(dlink[,c(cols.prefix,x)],dlink[,c('sex','ord.group')],do.coxph.inner)
		res=do.call(rbind,coeff.list)
		return(res)
	})
	return(do.call(rbind,res))
}

res.models=getResults(dlink,spec) # do.call(rbind,dcox.list)
res.curves=getResults(dlink,spec.curves)

# writing the results
write.xlsx(list(models=res.models,curves=res.curves),file=param$result.file,rowNames=FALSE)

#####
# Below, some plots are drawn
colours=list()
colours[['top 10%']]='blue3'
colours[['top 10-25%']]='lightblue'
colours[['bottom 10-25%']]='pink'
colours[['bottom 10%']]='red3'
colours[['(25,40]']]='green3'
colours[['(40,100]']]='gray3'
colours[['O-']]='blue3'

getIntervals = function(breaks) {
	brs=strsplit(breaks,',')[[1]]
	mid.bits=sapply(2:(length(brs)-2),function(x) paste0('(',brs[x],',',brs[x+1],']'))
	lower=paste0('<',brs[2])
	upper=paste0('>',brs[length(brs)-1])
	return(c(lower,mid.bits[-2],upper))
}

pdf('results/figures.pdf')
by(res.models,res.models[,c('sex','var')],function(y) {
	plot(x=NULL,type='n',xlim=c(1,max(y$ord)),ylim=c(0.70,1.40),main=paste(y$sex[1],y$var[1]),
		xlab='number of donation',ylab='relative likelihood of donation')
	abline(h=1,lwd=2,lty='dotted')
	breaks=y$breaks[1]
	brk.labels=if (breaks!='-') getIntervals(breaks) else ''
	levels=unique(y$level)
	legend(x='bottomright',legend=paste(levels,brk.labels),fill=sapply(levels,function(x) colours[[x]]))
	by(y,y[,c('var','level')],function(x) {
			gr=x$level[1]
			col=colours[[gr]]
			ordint=x$ord
			lines(ordint,x$exp.coef,lwd=2,col=col)
			lines(ordint,x$lower..95,lwd=2,lty='dotted',col=col)
			lines(ordint,x$upper..95,lwd=2,lty='dotted',col=col)
		})
	})
dev.off()

pdf('results/curves.pdf')
dummy=by(res.curves,res.curves[,c('sex','ord')],function(df) {
bsAssign('df')
		wh=min(which(df$surv<0.99))
		len0=length(df$surv)
		df=df[wh:len0,]
		# df$surv=df$surv[wh:len0]
		df$time=df$time[wh:len0]-wh
		df$sqrt.x=sqrt(df$time)

		# This is promising, but seems not to work after all
		# m.ss=nls(surv~SSweibull(time,yf,y0,log_alpha,poweri),data=df)
		m.ss=nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df)
		sm=summary(m.ss)

		plot(surv~time,data=df,type='l',lwd=2,xlim=c(0,2*365),ylim=c(0,1),
			main=paste(df$sex[1],df$ord.group[1]))
		pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))
		lines(df$time,pred.ss,col='red2',lwd=2.5)

		tv=qt(0.025,df=sm$df[2])
		return(data.frame(sex=df$sex[1],ord=df$ord[1],var=rownames(sm$coeff),sm$coeff,lower=sm$coeff[,1]-tv*sm$coeff[,2],upper=sm$coeff[,1]+tv*sm$coeff[,2]))
	})
dev.off()

ce=do.call(rbind,dummy)
str(ce)

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

pdf('results/parameters.pdf')
plotParameters(ce)
dev.off()
