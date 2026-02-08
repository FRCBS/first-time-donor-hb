source('src/analysis-functions.r')

pdf('results\\survival-joint.pdf',width=12,height=6)
vars=c('ord.group.full','bloodgr','sex')
lapply(vars,function(x) {
bsAssign('x')
		df=res.models %>% filter(var==x)

		if (x!='sex') {
			par(mfrow=c(1,2))
		} else {
			# nb! must fetch new data and remove this hard-coding
			df$sex='Female'
		}
		by(df,df[,'sex'],function(y) {
			sex0=y[1,'sex']
			plotByGroups(y,group.cols=c(NA,'country'),xcol='ord',ycols=c('exp.coef','lower..95','upper..95')
				,colours=colours,ltys=ltys,main=paste(x,sex0))
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
		m.ss=nls(surv~SSasymp(sqrt.x,yf,y0,log_alpha),data=df)
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

pdf('results/survival-parameters.pdf')
# plotParameters(ce)
# list(Male='blue3',Female='red3')
plotParameters2(ce,xvar='log_alpha',yvar='yf',group.by=c('country','sex'),col.var='country',pp.cols=colours,shade.var='ord')
dev.off()
