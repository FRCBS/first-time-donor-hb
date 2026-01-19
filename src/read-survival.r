setwd('c:/hy-version/first-time-donor-hb/src')

library(tidyverse)
library(openxlsx)
library(dplyr)

## ----parameters,echo=FALSE----------------------------------------------------
param=list()
param$data.dir = 'C:/Users/super/OneDrive - University of Helsinki/veripalvelu/paper-2 hemoglobin/data/'
param$shared.dir='C:/Users/super/OneDrive - University of Helsinki/veripalvelu/paper-2 hemoglobin/hb-paper-manuscripts/'
param$shared.dir="c:/hy-version/first-time-donor-h/submit/"

if (grepl('^VP',Sys.info()[4])) {
	setwd('C:\\git_repot\\first-time-donor-hb\\src')
	param$data.dir = 'C:\\git_repot\\first-time-donor-hb\\results\\'
	param$shared.dir = 'C:\\git_repot\\first-time-donor-hb\\results\\'
}

# source('analysis-functions.r')

param$png.resolution=1.4*150
param$figure.format='pdf'

## ----read-files,echo=FALSE----------------------------------------------------
file.names = dir(path=param$data.dir,pattern="*survival.xlsx")
file.names = file.names[!grepl('~',file.names)]
file.names = file.names[!grepl('^old',file.names)]
# file.names = file.names[!grepl('survival',file.names)]
file.names = file.names[grepl('.xlsx$',file.names)]
file.paths = paste(param$data.dir,file.names,sep='')

# Should be moved to a common functions file; now coexists in both read-* files
plotByGroups = function(data,group.cols=c('sex','country'),xcol='level',ycols=c('Estimate','lower','upper'),
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
	# bsAssign('lgnd')
	# legend=paste(lgnd)
	legend.data=do.call(rbind,lgnd)
	legend(x='bottom',legend=legend.data$text,col=legend.data$col,lty=legend.data$lty,lwd=2)
}

countries = list()
for (file in file.paths) {
	identifier = sub('.+[/\\]([a-z]+)[^/\\]+$','\\1',file) # gsub('.*\\\\(..).*\\.xlsx$','\\1',file)
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

str(countries$fi)

res.models=countries$fi$models
res.curves=countries$fi$curves

# res.models$ord=(as.integer(res.models$ord.group))
# res.curves$ord=(as.integer(res.curves$ord.group))

# nb! should check why the column name is wrong in that file
colnames(res.curves)=sub('^cur$','sex',colnames(res.curves))

param$wd = getwd()
if (grepl('[/\\]src[/\\]?',param$wd)) {
   param$wd = sub('[/\\]src([/\\]?)$','\\1',param$wd)
}
setwd(param$wd)