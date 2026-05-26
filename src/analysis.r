source('src/analysis-functions.r')

vars=c('ord.group.full','bloodgr','sex')
vars.levels=list(
	a=list(var='ord.group.full',legend.position='topleft'),
	b=list(var='bloodgr'),
	c=list(var='age.group.t',level=c('(15,20]')),
	c2=list(var='hb.surplus',level=c('bottom 10%')),
	c3=list(var='hb.surplus',level=c('bottom 10-25%')),
	d=list(var='bloodgr'))
# lapply(vars,function(x) {

vars2=c('ord.group.full','bloodgr','age.group.t','hb.surplus')
vl.comb=left_join(data.frame(var=vars2),unique((res.models %>% mutate(level=coalesce(level,'-')))[,c('var','level')]),join_by(var))
singles = vl.comb %>% 
	group_by(var) %>%
	summarise(n=n()) %>%
	filter(n==1) %>%
	dplyr::select(var)
vl.comb$level[vl.comb$var %in% unlist(singles)]=NA
vl.comb$legend.position=NA
vl.comb$legend.position[1]='topleft'
x=unclass(vl.comb[1,])

pdf('results\\survival-joint-with-levels.pdf',width=12,height=6)
lapply(rownames(vl.comb),function(x) {
		# x=vars.levels[[x]]
		x=unclass(vl.comb[x,])
		df=res.models %>% filter(var==x$var)

		level=''
		if ('level' %in% names(x) && !is.na(x$level)) {
			df=df %>% filter(level==x$level)
			level=x$level
		}

		ylim=

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

		ycols=c('exp.coef','lower..95','upper..95')
		ylim=c(min(df[,ycols]),max(df[,ycols]))
		by(df,df[,'sex'],function(y) {
			sex0=y[1,'sex']
			plotByGroups(y,group.cols=c(NA,'country'),xcol='ord',ycols=ycols,ylim=ylim,
				colours=colours,ltys=ltys,main=paste(x$var,sex0,level),trends='',legend.position=legend.position)
		})
	})
dev.off()

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
		try(m.ss<-nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df))

		if (is.null(m.ss)) {
			return(NULL)
		}
		sm=summary(m.ss)

		# plot(surv~time,data=df,type='l',lwd=2,xlim=c(0,2*365),ylim=c(0,1),
		#	main=paste(df$sex[1],df$ord[1]))
		pred.ss=predict(m.ss,data.frame(sqrt.x=df$sqrt.x))
		# lines(df$time,pred.ss,col='red2',lwd=2.5)

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

		lines(fitted~time,data=y,col=col,lwd=2,lty='dotted')
		lines(surv~time,data=y,col=col,lwd=2,lty=if (sex0=='Female') 'solid' else '8282')

		# wh=seq(1,y$time[nrow(y],by=50)
		# points(y$surv[wh],)
	})
})
dev.off()
pdf('results\\survival-joint-curves.pdf')
by(fitted,fitted$ord,function(x) {
	# plot here
	plot(x=NULL,xlim=c(0,2*365),ylim=c(0,1),main=x$ord[1],ylab='survival',xlab='time')
	by(x,x[,c('sex','country')],function(y) {
		country0=y$country[1]
		sex0=y$sex[1]
		col=colours[[country0]]
		# pch=pchs[[sex0]]

		lines(fitted~time,data=y,col=col,lwd=2,lty='dotted')
		lines(surv~time,data=y,col=col,lwd=2,lty=if (sex0=='Female') 'solid' else '8282')

		# wh=seq(1,y$time[nrow(y],by=50)
		# points(y$surv[wh],)
	})
})
dev.off()

pdf('results/survival-parameters.pdf')
# plotParameters(ce)
# list(Male='blue3',Female='red3')
plotParameters2(ce,xvar='log_alpha',yvar='yf',group.by=c('country','sex'),col.var='country',pp.cols=colours,shade.var='ord')
dev.off()

sapply(unique(res.models$country),plotSurvivalCurvesByCountry)