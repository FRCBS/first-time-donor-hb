library(tidyverse)
library(openxlsx)
library(xtable)

colours=list()
colours$fi='darkblue'
colours$nl='orange'
colours$fr='red3'
colours$au='#007F3B' # 'green3'
colours$nc='black'
colours$ct='purple'
colours$za='turquoise3' # 'violetred3'

colfun = function(x) {
	colours[[x]]
}

conversions=list()
conversions$fi=1
conversions$fr=10
conversions$nl=10 / 0.6206
conversions$nc=10
conversions$au=1

conversions.df=data.frame(t(data.frame(conversions)))
colnames(conversions.df)='rate'
conversions.df$country=rownames(conversions.df)

cn.names=list()
cn.names$fi='Finland'
cn.names$nl='Netherlands'
cn.names$fr='France'
cn.names$au='Australia'
cn.names$nc='Navarre'
cn.names$ct='Catalonia'
cn.names$za='South Africa'

colours[['top 10%']]='blue3'
colours[['top 10-25%']]='lightblue'
colours[['bottom 10-25%']]='pink'
colours[['bottom 10%']]='red3'
colours[['(25,40]']]='green3'
colours[['(40,100]']]='gray3'
colours[['O-']]='blue3'
colours[['general']]='black'

lvs=c('(15,20]','(20,25]','(25,30]','(30,35]','(35,40]','(45,50]','(50,55]','(55,60]','(60,100]')
# lvs=levels(dlink$age.group.t)
# palette=colorRampPalette(c("blue4", "white"))(length(lvs)+3)

# library(RColorBrewer)
# qual_col_pals = brewer.pal.info[brewer.pal.info$category=='qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# the qual palette as constants (the brewer package must be installed separately, want to avoid that)
col_vector=c('#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17','#666666','#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E','#E6AB02','#A6761D','#666666','#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928','#FBB4AE','#B3CDE3','#CCEBC5','#DECBE4','#FED9A6','#FFFFCC','#E5D8BD','#FDDAEC','#F2F2F2','#B3E2CD','#FDCDAC','#CBD5E8','#F4CAE4','#E6F5C9','#FFF2AE','#F1E2CC','#CCCCCC','#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#FFFF33','#A65628','#F781BF','#999999','#66C2A5','#FC8D62','#8DA0CB','#E78AC3','#A6D854','#FFD92F','#E5C494','#B3B3B3','#8DD3C7','#FFFFB3','#BEBADA','#FB8072','#80B1D3','#FDB462','#B3DE69','#FCCDE5','#D9D9D9','#BC80BD','#CCEBC5','#FFED6F')

for (i in 1:length(lvs)) {
	colours[[lvs[i]]]=if (i<length(lvs)) col_vector[i] else 'white' # palette[i]
}

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
		ltys=list(cm='dashed',fi='solid'),colours=list(Male='blue3',Female='red3'),main='',colour.col='sex',
		trends='legend',legend.position='',y.lim=NULL,extras.fun=NULL,x.max=NA) {

	if (is.na(x.max))
		x.max=max(data[[xcol]])

	xmin=min(data[[xcol]][data[[xcol]]>=0])-1
	if (is.null(y.lim)) 
		y.lim=c(min(data[,ycols]),max(data[,ycols]))
	yspan=(y.lim[2]-y.lim[1])
	plot(x=NULL,xlim=c(xmin,x.max + if (trends=='legend') 10 else 0),ylim=c(y.lim[1],y.lim[2]),
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

			if (!is.null(trends))
				m=lm(x[[ycols[1]]]~x[[xcol]])

			if (!country0 %in% names(ltys))
				ltys[[country0]]='solid'

			lines(x[[xcol]],x[[ycols[1]]],col=col0,lwd=2,lty=ltys[[country0]])
			lines(x[[xcol]],x[[ycols[3]]],col=col0,lwd=1,lty='dotted')
			lines(x[[xcol]],x[[ycols[2]]],col=col0,lwd=1,lty='dotted')

			if (trends=='legend') {
				sm=summary(m)
				cf=round(sm$coeff,3)
				print(summary(m))
				text=paste0('b=',sprintf(cf[2,1],fmt='%.3f'),', p=',cf[2,4])
				return(data.frame(text=paste(text,country0),b=cf[2,1],p=cf[2,4],lty=ltys[[country0]],col=col0))
			} else if (trends=='table') {
				rdf=data.frame(summary(m)$coeff)
				for (gc in group.cols) 
					rdf[[gc]]=x[[gc]][1]
				colnames(rdf)[4]='p.value'
				rdf$par='(Intercept)'
				rdf$par[2]=ycols[1]

				cn.col=which(group.cols=='country')
				if (length(cn.col) > 0 && grepl('corrected',x[[group.cols[cn.col]]][1]) && rdf$p.value[2] < 0.05) {
					a=rdf[1,1]
					b=rdf[2,1]
					n=length(x[[xcol]])
					# abline(a=rdf[1,1],b=rdf[2,1])
					lines(c(x[[xcol]][1],x[[xcol]][n]),c(a+b*x[[xcol]][1],a+b*x[[xcol]][n]),col=col0,lwd=1,lty='dashed')
				}

				return(rdf)
			}
		})

	# grps$country
	if ('country' %in% group.cols && legend.position!='') {
		cn.ids=sort(unique(data$country))
		cn.ids=cn.ids[!grepl('corrected',cn.ids)]
		legend(legend.position,fill=unlist(sapply(cn.ids,FUN=colfun)),legend=sapply(cn.ids,FUN=function(cn) {
			paste0(cn.names[[cn]])}))
	}

	if (trends == 'legend') {
		legend.data=do.call(rbind,lgnd)
		legend(x='bottomright',legend=sub('cm','corrected',legend.data$text),col=legend.data$col,lty=legend.data$lty,lwd=2)
	}

	if (trends == 'table') {
		return(do.call(rbind,lgnd))
	}

	if (!is.null(extras.fun)) {
		extras.fun()
	}
}

# nb! should use the similar implementation in the previous project to enable easier and more parametric
# plotting with countries etc.
plotParameters2 = function(ce,xvar='log_alpha',yvar='yf',col.var='sex',
	group.by=c('country','sex'),pp.cols=list(Male='blue3',Female='red3'),shade.var='ord',ylim=NULL) {
	lims = ce %>% 
		group_by(var) %>%
		summarise(min=min(Estimate),max=max(Estimate)) %>%
		data.frame()
	rownames(lims)=lims$var
	lims$var=NULL

	if (is.null(ylim)) 
		ylim=t(lims[yvar,])

	plot(x=NULL,xlim=t(lims[xvar,]),ylim=ylim,xlab='exponent',ylab='asymptote')
	by(ce,ce[,group.by],function(x) {
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

		cn.ids=sort(unique(ce$country))
		cn.ids=cn.ids[!grepl('corrected',cn.ids)]
		legend.text=sapply(cn.ids,FUN=function(cn) {paste0(cn.names[[cn]])})
		legend.text=c(legend.text,names(pchs))
		legend.fill=c(unlist(sapply(cn.ids,FUN=colfun)),rep(NA,length(pchs)))
		legend.pchs=c(rep(NA,length(cn.ids)),unlist(pchs))
		legend.border=c(rep('black',length(cn.ids)),rep(NA,length(pchs)))
		legend('topright',fill=legend.fill,legend=legend.text,pch=legend.pchs,border=legend.border)

}

html.template="<!DOCTYPE html>
<html>
<head>
<style>
html *
{
   font-size: 1em !important;
   color: #000 !important;
   font-family: Arial !important;
}

table {
  border-collapse: collapse;
}

td, th {
  border: 1px solid #ffffff;
  text-align: right;
  padding: 4px;
  font-size: 11px;
}

table td:first-child {
    text-align: left;
}​

tr:nth-child(even) {
  background-color: #ffffff;
}
</style>
</head>
<body>
¤table¤
</body>
</html>"

# Copied the main bits from post-export-plotting.r
# The idea is to produce the single-country survival plots 
plotSurvivalCurvesByCountry = function(cn,pdf.internal=TRUE,plot.what='all',filters=list(),draw.confint=TRUE) {
	res.models.all = res.models %>% filter(country==cn)
	res.curves = res.curves %>% filter(country==cn)
	# country=res.models.all$country[1]

bsAssign('res.models.all')
	for (fr in names(filters)) {
		print(paste(fr,filters[[fr]]))
		res.models.all = res.models.all[res.models.all[[fr]]==filters[[fr]],]
		# res.models.all = res.models.all %>%
		#	filter(!!!syms(fr)==filters[[fr]])
		str(res.models.all)
	}
	
	if (pdf.internal)
		pdf(paste0('results/survival-figures-',cn,'.pdf'))
	by(res.models.all,res.models.all[,c('sex','var')],function(y) {
		if (plot.what != 'all' && plot.what != 'figures')
			return(NULL)

		if (length(unique(y$level))==1)
			y$level='general'

		y=y %>% filter(!is.na(ord))

		if (nrow(y) == 1)
			return(NULL)

		ylim=c(min(y$exp.coef,na.rm=TRUE),max(y$exp.coef,na.rm=TRUE)) #c(0.70,1.40)
		y.delta=ylim[2]-ylim[1]
		ylim=c(ylim[1]-0.3*y.delta,ylim[2]+0.2*y.delta)
		plot(x=NULL,type='n',xlim=c(1,max(y$ord)),ylim=ylim,main=if (pdf.internal) paste(y$sex[1],y$var[1]) else '',
			xlab='number of donations',ylab='relative likelihood of next donation')
		abline(h=1,lwd=1,lty='dashed')
		breaks=y$breaks[1]

		# factor out the age.group.t breaks
		if (grepl(';',breaks))
			breaks='-' 

		col.fun=function(x) {if (x %in% names(colours)) colours[[x]] else 'white'}

		# if (max(y$ord) == param$max.ord.group.number+1) 
		#	abline(v=max(y$ord)-0.5,lwd=1,lty='dashed')

		brk.labels=if (breaks!='-') rev(getIntervals(breaks)) else ''
		if (length(brk.labels) > 1) {
			levels=rev(unique(y$level))
		} else {
			levels=(unique(y$level))
		}

		if (length(levels) > 1) {
			fill=sapply(levels,col.fun)
			levels=levels[fill!='white']
			fill=fill[fill!='white']
			lt0=paste(levels,brk.labels)
bsAssign('lt0')
			if (grepl('^.([0-9]+),([0-9]+).',lt0[1])) {
				lt0=sub('.([0-9]+),([0-9]+). ','\\1<age≤\\2',lt0)
				print(lt0)
			}
			legend(x='bottom',legend=lt0,fill=sapply(levels,col.fun),ncol=4,bg='white')
		}

		res.void=by(y,y[,c('var','level')],function(x) {
				gr=x$level[1]
				col=col.fun(gr)
				if (col=='white')
					return(NULL)
				ordint=x$ord
				lines(ordint,x$exp.coef,lwd=2,col=col)

				if (draw.confint) {
					lines(ordint,x$lower..95,lwd=2,lty='dotted',col=col)
					lines(ordint,x$upper..95,lwd=2,lty='dotted',col=col)
				}
			})
		})
	if (pdf.internal)
		dev.off()

	if (pdf.internal)
		pdf(paste0('results/survival-curves-',cn,'.pdf'))
	dummy=by(res.curves,res.curves[,c('sex','ord')],function(df) {
		if (plot.what != 'all' && plot.what != 'curves')
			return(NULL)

			wh=min(which(df$surv<0.99))
			len0=length(df$surv)
			df=df[wh:len0,]
			df$time=df$time[wh:len0]-wh
			df$sqrt.x=sqrt(df$time)

			# This is promising based on description, but seems not to work after all
			# m.ss=nls(surv~SSweibull(time,yf,y0,log_alpha,poweri),data=df)

			m.ss=NULL
			# This fitting is copied, need to do in one place only
			try(m.ss<-nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df))
			if (is.null(m.ss)) {
				return(NULL)
			}

			# m.ss=nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df)
			sm=summary(m.ss)

			plot(surv~time,data=df,type='l',lwd=2,xlim=c(0,2*365),ylim=c(0,1),
				main=paste(df$sex[1],df$ord[1]))
			pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))
			lines(df$time,pred.ss,col='red2',lwd=2.5)

			tv=qt(0.025,df=sm$df[2])
			return(data.frame(sex=df$sex[1],ord=df$ord[1],var=rownames(sm$coeff),sm$coeff,lower=sm$coeff[,1]-tv*sm$coeff[,2],upper=sm$coeff[,1]+tv*sm$coeff[,2]))
		})

	if (pdf.internal)
		dev.off()
}

convertOutput = function(html,file,page.width=20) {
	if (param$figure.format == 'png') {
		cat(html,file)
	} else {
bsAssign('html.file')
		html.0=html.file
		tex=sub('.+[<]body[>]','',html.file)
		tex=gsub('[<]table[>].tr.','\\\\begin{tabular}{cc}',tex)
		tex=gsub('[<]/table[>]','\\\\end{tabular}\n\\\\end{center}\n',tex)
		tex=gsub('[<]/tr[>][<]tr[>]','\\\\\\\\\n',tex)
		tex=gsub('[<]/td[>][\n ]*[<]td[^>]*[>]',' & ',tex)
		# tex=gsub('[<]/td[>][\n ]*[^>]+td[^>]*[>]',' & ',tex)
		tex=gsub('[<]/tr[>]','\\\\\\\\\n',tex)
		tex=gsub('[<]b[>]([^<]+)[<]/b[>]','\\\\textbf{\\1}',tex)
		tex=gsub('[<]/body[>].+','\n\\\\end{document}',tex)
		tex=gsub('&nbsp;','\\\\ ',tex)
		tex=gsub('&frac12;','$\\\\frac{1}{2}$',tex)
		tex=gsub('&ndash;','--',tex)
		tex=gsub('~','\\\\sim',tex)
		tex=gsub('&middot;','\\\\cdot',tex)
		tex=gsub('(log|exp)[(]','\\\\\\1(',tex)
		tex=gsub('src=.([^>]+).[>]','>\\\\includegraphics[width=9cm]{\\1}',tex)

		# 1800 is used to code two-column graphics
		if (grepl('1800',html.0)) 
			tex=gsub('width=9cm','width=18cm',tex)

		tex=gsub('[.]png','.pdf',tex)
		tex=gsub('[<]span[>]([^<]+)[<]/span[>]','\\$\\1\\$',tex)
		tex=gsub('[<][^>]+[>]','',tex)


		if (!grepl('end.center',tex)) {
			tex=sub('.textbf','\\\\end{center}\n\\\\textbf',tex)
			tex=gsub('9cm','16cm',tex)
		}

		tex.pre=paste0('\\documentclass[varwidth=',page.width,'cm,border=2mm]{standalone}\n\\usepackage[pdftex]{color,graphicx}\n\\begin{document} \\begin{center}')

		wd=sub('^(.+[/\\]).+','\\1',file)
		bare.file=sub(wd,'',file,fixed=TRUE)
		bare.file=sub('.html','.tex',bare.file)
		tex=paste(tex.pre,tex,collapse='\n')
		latexCompile(tex,param$shared.dir,bare.file)
	}

	suffix=c('aux','log','tex')
	dev.null=sapply(suffix,FUN=function(x) file.remove(sub('[.][a-z]+$',paste0('.',x),file)) )
}

getIntervals = function(breaks) {
	brs=strsplit(breaks,',')[[1]]
	mid.bits=sapply(2:(length(brs)-2),function(x) paste0('(',brs[x],',',brs[x+1],']'))
	lower=paste0('<',brs[2])
	upper=paste0('>',brs[length(brs)-1])
	return(c(lower,mid.bits[-2],upper))
}

### copied from main-with-export.Rmd
# This is the function that correct the kink in a distriution (data0)
# The function can be either called with data0 != NULL; data0 should then have the structure of simple, donation0 or donation.r
#	 and can be a subset of these.
# *or* with freq != NULL, with the structure of hb.freq
# Nb! This function operates on a single distribution
rectifyDistribution = function (data0,sex=NULL,cutoff=NULL,plot=TRUE,freq=NULL,hb.decimals=0) {
	if (is.null(cutoff)) {
		if (is.null(sex)) {
			if (is.null(data0)) {
				sex = unique(freq$Sex)[1]
			} else
				sex = unique(data0$Sex)[1]
		} 
		cutoff = if (sex=='Female') param$cutoff.female else param$cutoff.male
	}

	if (!is.null(data0)) {
		# sex0=data0[data0$Sex==sex&!is.na(data0$hb)&data0$BloodDonationTypeKey %in% c('Whole Blood (K)','No Donation (E)'),c('hb')]	
		freq = data0 %>%
			group_by(hb) %>%
			summarise(n=n()) %>%
			mutate(prop=n/sum(n))
	} else {
		freq = freq %>%
			group_by(hb) %>%
			summarise(n=sum(n),.groups='drop')
	}
	
	freq = freq[!is.na(freq$hb),]
	
	if (!'prop' %in% colnames(freq)) {
		sum.n = sum(freq$n) # -coalesce(freq$nas,0))
		freq$prop = freq$n / sum.n
	}
	
	freq.mean = freq %>%
		filter(!is.na(hb)) %>% # These are the summary rows for NA; leave them out at this point
		summarise(n2=sum(n),mean0=sum(hb*(n))/n2,.groups='drop')
	
	freq.stats = freq %>%
		filter(!is.na(hb)) %>% # These are the summary rows for NA; leave them out at this point
		mutate(dev=hb-freq.mean$mean0) %>% # ,prop=n/n2) %>%
		group_by() %>%
		summarise(mean=min(freq.mean$mean0),n0=min(n),var=sum(prop*dev^2),sd=sqrt(var),
							skewness=sum(prop*(dev/sd)^3),kurtosis=sum(prop*(dev/sd)^4),.groups='drop')

	mean0= freq.stats$mean
	sd0 = freq.stats$sd

	hb.values = sort(unique(freq$hb))
	if (FALSE) {
		dx <- 1. #depends on units, but should now all be converted to g/L
	} else	{
		if (hb.decimals == 1){
			dx <- 0.1 #depends on units, but should now all be converted to g/L
		} else{
			dx = 1.
		}
	}
	ndist=dnorm(hb.values,mean=mean0,sd=sd0)*dx
	
	# Plotting the unchanged distribution
	if (plot && FALSE) {
		plot(prop~hb,data=freq,type='l',lwd=2)
		lines(hb.values,ndist)
	
		# A plot showing the difference between the distribution defined by data0 and the
		# normal distribution estimated from that data
		plot(freq$prop-ndist~hb.values)
		abline(v=cutoff,col='blue')
		abline(h=c(-1,1)*0.0005,col='red')
	}

	# This is the part that 
	cnt = 0
	freq2 = freq
	# cover the case that there is no data at the cutoff value (should not occur with reasonable data volumes)
	# k = which(freq$hb==cutoff)
	k = max(which(freq$hb<=cutoff))
	if (length(k) == 0 || k == -Inf) 
		k = min(which(freq$hb>=cutoff))
	while (TRUE) {
		diff = (freq2$prop-ndist)[1:(k-1)]
		wh = which(diff < -0.0005)
		if (length(wh) == 0)
			break
		wh0 = max(wh)
		ds0 = diff[wh0]
		
		diff.plus = (freq2$prop-ndist)[k:nrow(freq2)]
		wh = which(diff.plus > 0.0005)
		if (length(wh) == 0)
			break
		wh1 = min(wh)
		ds1 = diff.plus[wh1]
		
		to.adjust = min(-ds0,ds1)
		
		freq2$prop[wh0] = freq2$prop[wh0] + to.adjust
		freq2$prop[wh1+(k-1)] = freq2$prop[wh1+(k-1)] - to.adjust
		
		cnt = cnt + 1
		if (cnt > 1000) {
			print('max iterations exceeded (back-stop)')
			break
		}
	}

	mean1 = freq2 %>% 
			mutate(mom=hb*prop) %>%
			summarise(mean=sum(mom))
	mean1=as.numeric(mean1)
	sd1 =	freq2 %>% 
			mutate(mom=(hb-as.numeric(mean1))^2*prop) %>%
			summarise(sdx=sum(mom)) 
	sd1=sqrt(as.numeric(sd1))
	
	# The theoretical deferred proportion is computed here
	deferred.prop = pnorm(cutoff-0.5,mean1,sd1)

	if (plot) {
		par(mar=c(4,4,0.5,0.6)) # no space at the top; bottom,left,top,right bottom 2.2->0
		plot(prop~hb,data=freq2,type='l',lwd=3,col='red3',xlim=NULL,xlab='hemoglobin (g/L)',ylab='probability density')
		abline(v=cutoff,col='limegreen',lty='dashed')
		lines(freq$hb,freq$prop,lwd=2,col='black')

		abline(v=mean0,col='red3',lty='dashed')
		abline(v=mean1,col='black',lty='dashed')

		ndist2=dnorm(hb.values,mean=mean1,sd=sd1)*dx
		lines(hb.values,ndist2,col='red3',lty='dotted',lwd=2)
	}

	if (FALSE) {
		plot(prop~hb,data=freq2,type='l',lwd=3,xlim=NULL)
		# lines(prop~hb,data=freq2,col='green', lwd=2)

		ndist2=dnorm(hb.values,mean=mean1,sd=sd1)*dx
		lines(hb.values,ndist2,col='red',lty='dotted',lwd=2)

		abline(v=cutoff,col='blue',lty='dashed')
		rect(mean1-5,0,mean1+5,0.001,col='pink',lwd=2)
		abline(v=mean1,lty='dotted',lwd=3)
		
		plot(freq2$prop-ndist2~hb.values)
		abline(v=cutoff,col='blue',lty='dashed')
		abline(h=c(-1,1)*0.0005,col='red',lty='dashed')
	}

	return(list(dist=freq2,params=list(mean0=freq.mean$mean0,mean=mean1,sd=sd1,deferred.prop=deferred.prop)))
}

firstUp <- function(x) {
	substr(x,1,1) <- toupper(substr(x,1,1))
	return(x)
}

decimalPlaces <- function(x) {
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}

subFromList = function(ptrn,lst) {
	for (nm in names(lst)) {
		if (is.na(lst[[nm]]))
			lst[[nm]]='NA'
		ptrn=gsub(paste0('¤',nm),lst[[nm]],ptrn)
	}
	return(ptrn)
}

latexCompile = function(content,workdir,filename) {
bsAssign('content')
bsAssign('workdir')
bsAssign('filename')
	oldwd = getwd()
	setwd(workdir)

	tex.file = sub('.pdf$','.tex',filename)

	pdflatex = 'C:\\Users\\super\\AppData\\Local\\Programs\\MiKTeX\\miktex\\bin\\x64\\pdflatex.exe'

	cat(content,file=tex.file)
	system(paste(pdflatex,paste0('"',tex.file,'"')),intern=TRUE)
	setwd(oldwd)
}
