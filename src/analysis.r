source('src/analysis-functions.r')

library(survival)

# 2026-06-05 new content: estimate country-specific hr's from the curve data
redat.list=lapply(names(countries.surv),function(x) {
	# x='fi'
	countries.surv[[x]]$curves %>%
		dplyr::select(country,sex,ord,time,n.event,n.censor) %>%
		# head(10) %>%
		pivot_longer(cols=c('n.event','n.censor')) %>%
		inner_join(data.frame(event=c(1,0),name=c('n.event','n.censor')),join_by(name)) %>%
		dplyr::select(-name) %>%
		rename(weight=value) %>%
		filter(weight>0)
})
redat=do.call(rbind,redat.list)

cn.models.list=by(redat,redat[,c('sex','ord')],function(x) {
bsAssign('x')
	hb.var='country'
	m=coxph(Surv(time,event)~country,data=x,weights=weight)
	sm=summary(m)
	df=data.frame(sm$coeff)
	df=cbind(df,sm$conf.int)
	colnames(df)=sub(' \\.','..',colnames(df))

	var='country'
	level=sub(hb.var,'',rownames(df))
	sex0=x$sex[1]
	df=cbind(var=hb.var,level=level,ord.group=x$ord[1],df)
	df$country=df$level
	df$sex=sex0
	# colnames(df)=c('var','level','ord.group','coef','exp.coef','se.coef','z','p.value')
	df
})
cn.models=do.call(rbind,cn.models.list)

#cn.models %>%
#	filter(country=='fi')
	
source('src/analysis-functions.r')
ylim=c(min(cn.models[['exp.coef.']]),max(cn.models[['exp.coef.']]))
by(cn.models,cn.models$sex,function(cnm0) {
	sex0=cnm0$sex[1]
	pdf(paste0('results/survival-cn0-',sex0,'.pdf'),width=7,heigh=5)
	par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
	par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
	plotByGroups(cnm0,xcol='ord.group',ycols=c('exp.coef.','lower..95','upper..95'),group.cols=c('sex','country'),trends='',y.lim=ylim,draw.confint=TRUE,
		colours=colours,colour.col='country',xlab='number of donations',ylab='relative likelihood of next donation')
	abline(h=1,lty='dashed')
	dev.off()
})

#plotByGroups = function(data,group.cols=c('sex','country'),xcol='level',ycols=c('Estimate','lower','upper'),
#		ltys=list(cm='dashed',fi='solid'),colours=list(Male='blue3',Female='red3'),main='',colour.col='sex',
#		trends='legend',legend.position='',y.lim=NULL,extras.fun=NULL,x.max=NA,xlab=NULL,ylab=NULL) {

# rbind(res.models,cn.models) %>% dim
str(res.models)
str(cn.models)

###
vars2=c('ord.group.full','bloodgr','age.group.t','hb.surplus','sex')
vl.comb=left_join(data.frame(var=vars2),unique((res.models %>% mutate(level=coalesce(level,'-')))[,c('var','level')]),join_by(var))
singles = vl.comb %>% 
	group_by(var) %>%
	summarise(n=n()) %>%
	filter(n==1) %>%
	dplyr::select(var)
vl.comb$level[vl.comb$var %in% unlist(singles)]=NA
vl.comb$legend.position=NA
vl.comb$legend.position[1]='topleft'
vl.comb$x.max=NA
vl.comb[vl.comb$var=='ord.group.full','x.max']=40

hr.plot.extras.fun = function() {
	abline(h=1,lty='dashed')
	# abline(v=15.5,lty='dashed')
}

pdf('results\\survival-joint-with-levels.pdf',width=12,height=6)
file.pattern='results/survival-joint-¤var-¤sex-¤level.pdf'
lapply(rownames(vl.comb),function(x) {
		# x=vars.levels[[x]]
		x=unclass(vl.comb[x,])
		df=res.models %>% filter(var==x$var)

		level=''
		if ('level' %in% names(x) && !is.na(x$level)) {
			df=df %>% filter(level==x$level)
			level=x$level
		}

		legend.position=''
		if ('legend.position' %in% names(x) && !is.na(x$legend.position)) {
			legend.position=x$legend.position
		}

		if (x$var!='sex') {
			par(mfrow=c(1,2))
		} else {
			# nb! must fetch new data and remove this hard-coding
			df$sex='Female'
		}

		if (!is.na(x$x.max)) 
			df=df %>% filter(ord<=x$x.max)

		ycols=c('exp.coef','lower..95','upper..95')
		ylim=c(min(df[,ycols]),max(df[,ycols]))
		by(df,df[,'sex'],function(y) {
			sex0=y[1,'sex']
			x$sex=sex0

			main=paste(x$var,sex0,level)

			local.plot=FALSE
			if (!is.null(file.pattern) && file.pattern!='') {
				filename=gsub('[](%,]','-',subFromList(file.pattern,x))
				filename=gsub(' ','-',filename)

				local.plot=TRUE

				pdf(filename,width=7,heigh=5)
				# par(mar=c(0.1,5.5,0.5,0.6)) # no space at the top; bottom,left,top,right bottom 2.2->0
				par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
				par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
				main=''
			}
			

bsAssign('y')
bsAssign('ycols')
			plotByGroups(y,group.cols=c(NA,'country'),xcol='ord',ycols=ycols,y.lim=ylim,draw.confint=TRUE,
				colours=colours,ltys=ltys,main=main,trends='',legend.position=legend.position,
				extras.fun=hr.plot.extras.fun,x.max=x$x.max,xlab='number of donations',ylab='relative likelihood of next donation')

			if (local.plot)
				dev.off()
		})
	})

dev.off()

# nb! using sqrt(t) implies steeper initial decline than in the standard exponential model
t=0:500
y=exp(-0.002*t)
y2=exp(-0.002*sqrt(500)*sqrt(t))
plot(y~t,type='l',col='blue3',lwd=3)
lines(t,y2,lwd=3)

res.curves=res.curves %>% arrange(country,ord,sex,time)
dummy=by(res.curves,res.curves[,c('country','ord','sex')],function(df) {
		wh=min(which(df$surv<0.99))
		len0=length(df$surv)
		df=df[wh:len0,]
		df$time=df$time[wh:len0]-wh
		df$sqrt.x=sqrt(df$time)

		# This is promising based on description, but seems not to work after all
		# m.ss=nls(surv~SSweibull(time,yf,y0,log_alpha,poweri),data=df)
		m.ss=NULL
		# SSasymp(input, Asym, R0, lrc)
		try(m.ss<-nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df))

		if (is.null(m.ss)) {
			return(NULL)
		}
		sm=summary(m.ss)

		pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))

		tv=qt(0.025,df=sm$df[2])
		return(list(parameters=data.frame(country=df$country[1],sex=df$sex[1],ord=df$ord[1],var=rownames(sm$coeff),sm$coeff,lower=sm$coeff[,1]-tv*sm$coeff[,2],upper=sm$coeff[,1]+tv*sm$coeff[,2]),
			fitted.data=data.frame(country=df$country[1],sex=df$sex[1],ord=df$ord[1],time=df$time,surv=df$surv,fitted=pred.ss)))
	})
ce=do.call(rbind,lapply(dummy,function(x) x$parameters))
fitted=do.call(rbind,lapply(dummy,function(x) x$fitted.data))

pdf('results\\survival-joint-curves.pdf')
by(fitted,fitted$ord,function(x) {
	# plot here
	plot(x=NULL,xlim=c(0,2*365),ylim=c(0,1),main=x$ord[1],ylab='survival',xlab='time')
	by(x,x[,c('sex','country')],function(y) {
		country0=y$country[1]
		sex0=y$sex[1]
		col=colours[[country0]]
		# pch=pchs[[sex0]]

		y$surv=1-y$surv
		y$fitted=1-y$fitted

		lines(fitted~time,data=y,col=col,lwd=2,lty='dotted')
		lines(surv~time,data=y,col=col,lwd=2,lty=if (sex0=='Female') 'solid' else '8282')

		# wh=seq(1,y$time[nrow(y],by=50)
		# points(y$surv[wh],)
	})
})
dev.off()

# In single files, separately for males and females
# pdf('results\\survival-joint-curves.pdf')
fitted.116=fitted %>% filter(ord %in% c(1,16))
by(fitted.116,fitted.116$ord,function(x) {
	# plot here
	ord0=x$ord[1]

	by(x,x[,c('sex')],function(y) {
		sex0=y$sex[1]
		pdf(paste0('results/survival-curves-',ord0,'-',sex0,'.pdf'),width=7,height=5.5)
		par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
		par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
		plot(x=NULL,xlim=c(0,2*365),ylim=c(0,1),main='',ylab='retention',xlab='time (days)') # main=x$ord[1]
		by(y,y$country,function(z) {
			country0=z$country[1]
			sex0=z$sex[1]
			col=colours[[country0]]
			# pch=pchs[[sex0]]

			z$surv=1-z$surv
			z$fitted=1-z$fitted

			lines(fitted~time,data=z,col=col,lwd=2,lty='dotted')
			lines(surv~time,data=z,col=col,lwd=2) 
			abline(h=0,lty='dashed')
		})
		dev.off()
	})
})
# dev.off()

# Both sexes in the same plot
pdf('results/survival-parameters.pdf',width=7,height=6)
par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
plotParameters2(ce,xvar='log_alpha',yvar='yf',group.by=c('country','sex'),col.var='country',pp.cols=colours,shade.var='ord')
dev.off()

lims = ce %>% 
	group_by(var) %>%
	summarise(min=min(Estimate),max=max(Estimate)) %>%
	data.frame()
rownames(lims)=lims$var
lims$var=NULL
lims

source('src/analysis-functions.r')
# Sexes in different plots
by(ce,ce$sex,function(ce0) {
bsAssign('ce0')
	sex0=ce0$sex[1]
	pdf(paste0('results/survival-parameters-',sex0,'.pdf'),width=7,height=6)
	par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
	par(cex=1.25,cex.axis=1.25,cex.lab=1.25)
	plotParameters2(ce0,xvar='log_alpha',yvar='yf',group.by=c('country','sex'),col.var='country',pp.cols=colours,shade.var='ord',ylim=t(lims['yf',]))
	dev.off()
})

sapply(unique(res.models$country),plotSurvivalCurvesByCountry)

cairo_pdf('results/survival-sample-age.group.t-fi-female.pdf',width=7,heigh=5)
par(mar=c(4.1,4.1,0.2,0.1)) # modifired measures with left and bottom margins for labels and some at top for y-axis labels
par(cex=1.10,cex.axis=1.10,cex.lab=1.10)
plotSurvivalCurvesByCountry('fi',plot.what='figures',filters=list(sex='Female',var='age.group.t'),pdf.internal=FALSE,draw.confint=TRUE)
dev.off()