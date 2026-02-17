library(tidyverse)
library(openxlsx)

colours=list()
colours$fi='darkblue'
colours$nl='orange'
colours$fr='red3'
colours$au='#007F3B' # 'green3'
colours$nc='black'
colours$ct='purple'
colours$za='turquoise3' # 'violetred3'

conversions=list()
conversions$fi=1
conversions$nl=10 / 0.6206
conversions$nc=10

cn.names=list()
cn.names$fi='Finland'
cn.names$nl='Netherlands'
cn.names$fr='France'
cn.names$au='Australia'
cn.names$nc='Navarre'
cn.names$ct='Catalonia'
cn.names$za='South Africa'

pchs=list(Female=2,Male=6)

ltys=list(Female='solid',Male='A2')

bsAssign = function(name) {
	obj = get(name,envir=parent.frame())
	assign(name,obj,.GlobalEnv)
}

pp.cols=list(Male='blue3',Female='red3')

ltys=colours
for (cn in names(colours))
	ltys[[cn]]='solid'

# this is the version from post-export-plotting.r
# reverting that file to an old version from 2026-01-25
plotByGroups = function(data,group.cols=c('sex','country'),xcol='level',ycols=c('Estimate','lower','upper'),
		ltys=list(cm='dashed',fi='solid'),colours=list(Male='blue3',Female='red3'),main='',colour.col='sex') {

	xmin=min(data[[xcol]][data[[xcol]]>=0])-1
	ylim=c(min(data[,ycols]),max(data[,ycols]))
	yspan=(ylim[2]-ylim[1])
	plot(x=NULL,xlim=c(xmin,max(data[[xcol]])+20),ylim=c(ylim[1],ylim[2]),
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
	legend(x='right',legend=sub('cm','corrected',legend.data$text),col=legend.data$col,lty=legend.data$lty,lwd=2)
}

# nb! should use the similar implementation in the previous project to enable easier and more parametric
# plotting with countries etc.
plotParameters2 = function(ce,xvar='log_alpha',yvar='yf',col.var='sex',group.by=c('country','sex'),pp.cols=list(Male='blue3',Female='red3'),shade.var='ord') {
	lims = ce %>% 
		group_by(var) %>%
		summarise(min=min(Estimate),max=max(Estimate)) %>%
		data.frame()
	rownames(lims)=lims$var
	lims$var=NULL

	# plot(x=NULL,xlim=c(-3,-1),ylim=c(0,0.33),xlab='exponent',ylab='asymptote')
	plot(x=NULL,xlim=t(lims[xvar,]),ylim=t(lims[yvar,]),xlab='exponent',ylab='asymptote')
	by(ce,ce[,group.by],function(x) {
	# for (gr in names(pp.cols)) {
		xdata=x %>%
			# filter(as.character(!!!syms(col.var))==gr,var==xvar) %>%
			filter(var==xvar) %>%
			arrange(!!!syms(shade.var))
		ydata=x %>%
			# filter(as.character(!!!syms(col.var))==gr,var==yvar) %>%
			filter(var==yvar) %>%
			arrange(!!!syms(shade.var))

		col.var0=x[1,col.var]
		sex0=x[1,'sex']

		shades=colorRampPalette(c(pp.cols[[col.var0]],"white"))(5+nrow(xdata))

		col0=shades[xdata$ord]
		points(xdata$Estimate,ydata$Estimate,col=col0,lwd=3,pch=pchs[[sex0]])
		lines(xdata$Estimate,ydata$Estimate,col=col0,lwd=1,lty='dashed')

		# This is done to make the error bars at least somewhat visible
		# col0='black'
		arrows(xdata$Estimate,ydata$lower,xdata$Estimate,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
		arrows(xdata$Estimate,ydata$Estimate,xdata$Estimate,ydata$upper,length=0.05,angle=90,code=3,col=col0)

		arrows(xdata$lower,ydata$Estimate,xdata$Estimate,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
		arrows(xdata$Estimate,ydata$Estimate,xdata$upper,ydata$Estimate,length=0.05,angle=90,code=3,col=col0)
	})
}

